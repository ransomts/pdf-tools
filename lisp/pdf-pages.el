;;; pdf-pages.el --- Delete, extract, move, rotate and insert PDF pages. -*- lexical-binding: t -*-

;; Copyright (C) 2026  Tim Ransom

;; Author: Tim Ransom
;; Keywords: files, multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Page operations on the PDF file visited by a `pdf-view-mode'
;; buffer: delete, extract, move and rotate pages, and insert pages of
;; another PDF.  Poppler, and therefore the epdfinfo server, cannot
;; rewrite a document's page tree, so the work is done by the qpdf
;; command line program (version 8.4 or later), see
;; `pdf-pages-qpdf-program'.
;;
;; Every operation that changes the visited file
;;
;; - refuses to run on a buffer with unsaved changes (it offers to
;;   save them first), because they would otherwise be lost,
;; - asks for confirmation, unless `pdf-pages-confirm' is nil,
;; - lets qpdf write a new file next to the original, checks qpdf's
;;   exit status and the page count of the result, and only then
;;   renames it over the original, so a failure leaves the file as it
;;   was,
;; - keeps a copy of the previous version, which `pdf-pages-undo'
;;   restores, and
;; - reverts the buffer, which makes the epdfinfo server reopen the
;;   new file.
;;
;; Commands take the current page.  A numeric prefix argument N
;; selects N pages starting at the current one, and a plain C-u
;; prompts for a page range in qpdf syntax,
;; e.g. "3-5,8,10-z" (z is the last page).

(require 'pdf-view)
(require 'pdf-info)
(require 'pdf-util)
(require 'cl-lib)
(require 'subr-x)

;;; Code:

(defvar pdf-view--buffer-file-name)



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-pages nil
  "Delete, extract, move, rotate and insert PDF pages."
  :group 'pdf-tools)

(defcustom pdf-pages-qpdf-program "qpdf"
  "The qpdf program used for page operations.

Either a program name looked up in variable `exec-path', or an
absolute file name.  qpdf 8.4 or later is required."
  :type 'string)

(defcustom pdf-pages-confirm 'destructive
  "Whether to ask before an operation changes the visited PDF file.

If t, ask before every change.  If `destructive', ask only before
operations that remove content: deleting pages and
`pdf-pages-undo'.  If nil, never ask.  Every change can be
reverted with `pdf-pages-undo' regardless.

Extracting pages into a new file never asks, except before
overwriting an existing file."
  :type '(choice (const :tag "Before every change" t)
                 (const :tag "Before deleting pages and undoing" destructive)
                 (const :tag "Never" nil)))

(defcustom pdf-pages-undo-limit 10
  "How many previous versions of a file `pdf-pages-undo' can restore.

The previous versions are copies of the whole file, kept in
`pdf-pages-backup-directory'."
  :type 'integer)

(defcustom pdf-pages-backup-directory nil
  "Directory for the copies that `pdf-pages-undo' restores.

If nil, a directory below variable `temporary-file-directory' is used,
which is deleted when Emacs exits.  If a directory is given, the
copies are kept there and outlive the Emacs session."
  :type '(choice (const :tag "Temporary, deleted on exit" nil)
                 directory))

(defcustom pdf-pages-minor-mode-map-prefix (kbd "C-c C-o")
  "The prefix to use for `pdf-pages-minor-mode-map'.

Setting this after the package was loaded has no effect."
  :type 'key-sequence)



;; * ================================================================== *
;; * Page ranges
;; * ================================================================== *

(defun pdf-pages-parse-range (string npages)
  "Return the list of pages described by STRING in a NPAGES document.

STRING uses qpdf's syntax: comma separated page numbers and
ranges, where z stands for the last page and r N for the N-th
page from the end, e.g. \"1,3-5,z\".  A range whose first page
is larger than its last one is in reverse order.  The pages are
returned in the order given, duplicates included."
  (let (pages)
    (dolist (item (split-string string "," t "[ \t\n]+"))
      (unless (string-match
               "\\`\\(z\\|r?[0-9]+\\)\\(?:[ \t]*-[ \t]*\\(z\\|r?[0-9]+\\)\\)?\\'"
               item)
        (user-error "Invalid page range `%s', use e.g. 3-5,8,10-z" item))
      (let* ((to-page
              (lambda (spec)
                (let ((n (cond ((equal spec "z") npages)
                               ((string-prefix-p "r" spec)
                                (- (1+ npages)
                                   (string-to-number (substring spec 1))))
                               (t (string-to-number spec)))))
                  (unless (<= 1 n npages)
                    (user-error "No page %s, the document has %d pages"
                                spec npages))
                  n)))
             (from (funcall to-page (match-string 1 item)))
             (to (if (match-string 2 item)
                     (funcall to-page (match-string 2 item))
                   from)))
        (if (<= from to)
            (cl-loop for p from from to to do (push p pages))
          (cl-loop for p downfrom from to to do (push p pages)))))
    (unless pages
      (user-error "No pages given"))
    (nreverse pages)))

(defun pdf-pages-range-string (pages)
  "Return a qpdf page range for the list PAGES, preserving their order."
  (let (runs)
    (dolist (p pages)
      (if (and runs (= p (1+ (cdar runs))))
          (setcdr (car runs) p)
        (push (cons p p) runs)))
    (mapconcat (lambda (run)
                 (if (= (car run) (cdr run))
                     (number-to-string (car run))
                   (format "%d-%d" (car run) (cdr run))))
               (nreverse runs) ",")))

(defun pdf-pages--describe (pages)
  "Return a description of PAGES for prompts and messages."
  (format "page%s %s"
          (if (cdr pages) "s" "")
          (pdf-pages-range-string pages)))

(defun pdf-pages--current-page ()
  "Return the current page, 1 if there is none yet."
  (or (pdf-view-current-page) 1))

(defun pdf-pages-read-pages (prompt &optional arg)
  "Return the pages a command should work on.

ARG is the raw prefix argument.  Without it, return the current
page.  A number N selects N pages starting at the current one,
and \\[universal-argument] reads a page range with PROMPT."
  (let ((current (pdf-pages--current-page))
        (npages (pdf-cache-number-of-pages)))
    (cond
     ((consp arg)
      (pdf-pages-parse-range
       (read-string (format "%s (e.g. 3-5,8,10-z): " prompt)
                    nil nil (number-to-string current))
       npages))
     (arg
      (let ((n (prefix-numeric-value arg)))
        (if (> n 0)
            (number-sequence current (min npages (+ current n -1)))
          (number-sequence (max 1 (+ current n 1)) current))))
     (t (list current)))))



;; * ================================================================== *
;; * Running qpdf
;; * ================================================================== *

(defun pdf-pages--qpdf-program ()
  "Return the absolute file name of qpdf, or signal an error."
  (or (and pdf-pages-qpdf-program
           (if (file-name-absolute-p pdf-pages-qpdf-program)
               (and (file-executable-p pdf-pages-qpdf-program)
                    pdf-pages-qpdf-program)
             (executable-find pdf-pages-qpdf-program)))
      (error "Cannot find the qpdf program `%s'; install qpdf or \
customize `pdf-pages-qpdf-program'" pdf-pages-qpdf-program)))

(defun pdf-pages--call-qpdf (password &rest args)
  "Run qpdf with ARGS, decrypting with PASSWORD unless it is nil.

Return a cons (STATUS . OUTPUT) of qpdf's exit status and its
combined standard output and error."
  (let ((program (pdf-pages--qpdf-program)))
    (with-temp-buffer
      (let* ((default-directory temporary-file-directory)
             (status (apply #'call-process program nil t nil
                            (append (when password
                                      (list (concat "--password=" password)))
                                    args))))
        (cons status (string-trim (buffer-string)))))))

(defun pdf-pages--ok-p (result)
  "Return non-nil if qpdf RESULT means success.

Exit status 3 means qpdf succeeded with warnings."
  (memq (car result) '(0 3)))

(defun pdf-pages--password-error-p (result)
  "Return non-nil if qpdf RESULT failed for want of a password."
  (and (not (pdf-pages--ok-p result))
       (string-match-p "invalid password\\|incorrect password" (cdr result))))

(defun pdf-pages--password-key (file)
  "Return the `password-cache' key of FILE, as `pdf-view-decrypt-document' does."
  (concat "/pdf-tools" file))

(defun pdf-pages--run (file &rest args)
  "Run qpdf with ARGS on the possibly encrypted FILE.

The password, if FILE needs one, is taken from the password cache
\(see `pdf-view-decrypt-document') or read from the user.  Return
the password that worked, or nil if none was needed.  Signal an
error if qpdf fails."
  (let* ((key (pdf-pages--password-key file))
         (password (password-read-from-cache key))
         (tries 3)
         (result (apply #'pdf-pages--call-qpdf password args)))
    (while (and (pdf-pages--password-error-p result) (> tries 0))
      (when password (password-cache-remove key))
      (setq tries (1- tries)
            password (password-read
                      (format (if password
                                  "Invalid password for `%s', try again: "
                                "Enter password for `%s': ")
                              (abbreviate-file-name file)))
            result (apply #'pdf-pages--call-qpdf password args)))
    (unless (pdf-pages--ok-p result)
      (error "The qpdf program failed (exit status %s): %s" (car result) (cdr result)))
    (when password
      (password-cache-add key password))
    password))

(defun pdf-pages--number-of-pages (file &optional password)
  "Return the number of pages of FILE, as counted by qpdf.

Use PASSWORD if FILE is encrypted.  Signal an error if FILE can
not be read."
  (let ((result (pdf-pages--call-qpdf password "--show-npages" file)))
    (unless (and (pdf-pages--ok-p result)
                 (string-match "^\\([0-9]+\\)$" (cdr result)))
      (error "The qpdf program cannot read `%s': %s"
             (abbreviate-file-name file) (cdr result)))
    (string-to-number (match-string 1 (cdr result)))))

(defun pdf-pages--make-temp-file (file)
  "Return a new empty temporary file next to FILE.

It is in the same directory, so that renaming it over FILE is
atomic."
  (make-temp-file (expand-file-name
                   (concat "." (file-name-base file) "-pdf-pages-")
                   (file-name-directory file))
                  nil ".pdf"))

(defun pdf-pages--write (source target npages args &optional before-replace)
  "Let qpdf write a version of SOURCE with NPAGES pages to TARGET.

ARGS are the qpdf arguments after the input file name, the output
file is appended to them.  qpdf writes to a temporary file, which
replaces TARGET only if qpdf succeeded and the result has NPAGES
pages.  On failure TARGET is left untouched and an error is
signaled.  BEFORE-REPLACE, if non-nil, is called without
arguments just before the rename.  Return the password SOURCE
needed, or nil."
  (let ((temp (pdf-pages--make-temp-file target))
        done password)
    (unwind-protect
        (progn
          (setq password
                (apply #'pdf-pages--run source
                       (append (list source) args (list temp))))
          (let ((n (pdf-pages--number-of-pages temp password)))
            (unless (= n npages)
              (error "The qpdf program produced %d pages instead of %d; `%s' unchanged"
                     n npages (abbreviate-file-name target))))
          (when (file-exists-p target)
            (set-file-modes temp (file-modes target)))
          (when before-replace
            (funcall before-replace))
          (rename-file temp target t)
          (setq done t)
          password)
      (unless done
        (when (file-exists-p temp)
          (delete-file temp))))))



;; * ================================================================== *
;; * Checks
;; * ================================================================== *

(defun pdf-pages--assert-file (&optional read-only)
  "Check that the current buffer can be worked on and return its file.

The buffer must be a `pdf-view-mode' buffer visiting a regular,
local, uncompressed PDF file, which must be writable unless
READ-ONLY is non-nil.  Return the file's true name."
  (unless (derived-mode-p 'pdf-view-mode)
    (user-error "Not in a PDF buffer"))
  (when (derived-mode-p 'pdf-virtual-view-mode)
    (user-error "Page operations do not work on virtual PDF buffers"))
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer `%s' is not visiting a file; page operations \
need a PDF file on disk" (buffer-name)))
    (when (or (file-remote-p file)
              pdf-view--buffer-file-name
              (not (file-regular-p file)))
      (user-error "`%s' is not a local PDF file (remote, compressed or in \
an archive); page operations need a plain file on disk"
                  (abbreviate-file-name file)))
    (setq file (file-truename file))
    (unless (or read-only
                (and (file-writable-p file)
                     (file-writable-p (file-name-directory file))))
      (user-error "`%s' or its directory is not writable"
                  (abbreviate-file-name file)))
    file))

(defun pdf-pages--assert-saved ()
  "Make sure the buffer has no unsaved changes and matches its file.

Offer to save unsaved changes (annotations, form fields), and
signal an error if the user declines, since a page operation
would lose them."
  (when (buffer-modified-p)
    (if (y-or-n-p (format "`%s' has unsaved changes (annotations or \
form fields); save them first? " (buffer-name)))
        (save-buffer)
      (user-error "Not touching the pages: save or revert the buffer first")))
  (unless (verify-visited-file-modtime (current-buffer))
    (user-error "`%s' changed on disk; revert the buffer first (%s)"
                (buffer-name)
                (substitute-command-keys "\\[revert-buffer]"))))

(defun pdf-pages--capitalize (string)
  "Return STRING with its first character upcased."
  (if (string-empty-p string)
      string
    (concat (upcase (substring string 0 1)) (substring string 1))))

(defun pdf-pages--confirm (destructive format-string &rest args)
  "Ask the question FORMAT-STRING with ARGS, as `pdf-pages-confirm' says.

DESTRUCTIVE non-nil means the operation removes content.  Signal
a `user-error' if the answer is no."
  (when (and (if destructive pdf-pages-confirm (eq pdf-pages-confirm t))
             (not (y-or-n-p (apply #'format format-string args))))
    (user-error "Page operation cancelled")))



;; * ================================================================== *
;; * Undo
;; * ================================================================== *

(defvar pdf-pages--undo-alist nil
  "Alist of (FILE . ENTRIES) for `pdf-pages-undo'.

ENTRIES is a list, most recent first, of plists with the keys
:backup (a copy of FILE before the operation), :npages (its
page count), :page (the page shown before it), :modtime (FILE's
modification time after it) and :description.")

(defvar pdf-pages--session-directory nil
  "Temporary directory of this session's backups, if one was created.")

(defun pdf-pages--backup-directory ()
  "Return the directory for backups, creating it if needed."
  (if pdf-pages-backup-directory
      (let ((dir (expand-file-name pdf-pages-backup-directory)))
        (make-directory dir t)
        dir)
    (unless (and pdf-pages--session-directory
                 (file-directory-p pdf-pages--session-directory))
      (setq pdf-pages--session-directory
            (make-temp-file "pdf-pages-undo-" t))
      (add-hook 'kill-emacs-hook #'pdf-pages--delete-session-directory))
    pdf-pages--session-directory))

(defun pdf-pages--delete-session-directory ()
  "Delete this session's backup directory."
  (when (and pdf-pages--session-directory
             (file-directory-p pdf-pages--session-directory))
    (delete-directory pdf-pages--session-directory t)))

(defun pdf-pages--file-modtime (file)
  "Return the modification time of FILE."
  (file-attribute-modification-time (file-attributes file)))

(defun pdf-pages--make-backup (file)
  "Copy FILE to the backup directory and return the copy's name."
  (let ((backup (make-temp-file
                 (expand-file-name (concat (file-name-base file) "-")
                                   (pdf-pages--backup-directory))
                 nil ".pdf")))
    (copy-file file backup t t)
    backup))

(defun pdf-pages--push-undo (file backup npages page description)
  "Record BACKUP as FILE's version before DESCRIPTION.

BACKUP has NPAGES pages, and PAGE was shown."
  (let* ((cell (or (assoc file pdf-pages--undo-alist)
                   (car (push (list file) pdf-pages--undo-alist))))
         (entries (cons (list :backup backup
                              :npages npages
                              :page page
                              :modtime (pdf-pages--file-modtime file)
                              :description description)
                        (cdr cell))))
    (when (> (length entries) (max 0 pdf-pages-undo-limit))
      (dolist (old (nthcdr (max 0 pdf-pages-undo-limit) entries))
        (ignore-errors (delete-file (plist-get old :backup))))
      (setq entries (cl-subseq entries 0 (max 0 pdf-pages-undo-limit))))
    (setcdr cell entries)))

(defun pdf-pages-undo ()
  "Restore the version of the file before the last page operation.

Only operations of this Emacs session can be undone, up to
`pdf-pages-undo-limit' of them."
  (interactive)
  (let* ((file (pdf-pages--assert-file))
         (cell (assoc file pdf-pages--undo-alist))
         (entry (cadr cell)))
    (unless entry
      (user-error "No page operation to undo for `%s'"
                  (abbreviate-file-name file)))
    (pdf-pages--assert-saved)
    (unless (file-exists-p (plist-get entry :backup))
      (setcdr cell nil)
      (error "The backup of `%s' is gone; cannot undo"
             (abbreviate-file-name file)))
    (if (equal (pdf-pages--file-modtime file) (plist-get entry :modtime))
        (pdf-pages--confirm t "Undo %s? " (plist-get entry :description))
      (unless (y-or-n-p (format "`%s' changed since %s; restore the version \
before it anyway? " (abbreviate-file-name file) (plist-get entry :description)))
        (user-error "Undo cancelled")))
    (let ((backup (plist-get entry :backup))
          (temp (pdf-pages--make-temp-file file))
          done)
      (unwind-protect
          (progn
            (copy-file backup temp t t)
            (set-file-modes temp (file-modes file))
            (pdf-pages--close-document)
            (rename-file temp file t)
            (setq done t))
        (unless done
          (when (file-exists-p temp)
            (delete-file temp))))
      (setcdr cell (cddr cell))
      (delete-file backup)
      (pdf-pages--revert (plist-get entry :npages) (plist-get entry :page))
      (message "Undid %s" (plist-get entry :description)))))



;; * ================================================================== *
;; * Changing the visited file
;; * ================================================================== *

(defun pdf-pages--close-document ()
  "Close the document in the epdfinfo server, if it is running."
  (when (pdf-info-running-p)
    (ignore-errors (pdf-info-close))))

(defun pdf-pages--revert (npages page)
  "Revert the buffer to a file of NPAGES pages, then show PAGE.

Reverting closes the document in the epdfinfo server, so the next
request opens the new file."
  (let ((page (max 1 (min page npages))))
    ;; Point every window at an existing page before the revert
    ;; redisplays them.
    (dolist (win (get-buffer-window-list nil nil t))
      (setf (pdf-view-current-page win)
            (if (eq win (selected-window))
                page
              (max 1 (min (or (pdf-view-current-page win) 1) npages)))))
    (pdf-view-revert-buffer nil t)
    (pdf-view-goto-page page)))

(defun pdf-pages--modify (description npages page args &optional destructive)
  "Rewrite the visited file with qpdf and revert the buffer.

DESCRIPTION describes the operation for questions and messages.
NPAGES is the expected page count of the result, PAGE the page
to show afterwards, and ARGS the qpdf arguments after the input
file.  DESTRUCTIVE non-nil means the operation removes content,
see `pdf-pages-confirm'.  The previous version is kept for
`pdf-pages-undo'."
  (let* ((file (pdf-pages--assert-file))
         (old-page (pdf-pages--current-page))
         (old-npages (pdf-cache-number-of-pages))
         backup done)
    (pdf-pages--assert-saved)
    (pdf-pages--confirm destructive "%s? " (pdf-pages--capitalize description))
    (setq backup (pdf-pages--make-backup file))
    (unwind-protect
        (progn
          (pdf-pages--write file file npages args
                            #'pdf-pages--close-document)
          (setq done t))
      (unless done
        (delete-file backup)))
    (pdf-pages--push-undo file backup old-npages old-page description)
    (pdf-pages--revert npages page)
    (message "Done: %s (%s to undo)" description
             (substitute-command-keys "\\[pdf-pages-undo]"))))



;; * ================================================================== *
;; * Commands
;; * ================================================================== *

(defun pdf-pages--prepare (&optional read-only)
  "Check the buffer before reading a command's arguments.

See `pdf-pages--assert-file' for READ-ONLY."
  (pdf-pages--assert-file read-only)
  (pdf-pages--assert-saved))

(defun pdf-pages--check-pages (pages npages)
  "Signal an error unless PAGES is a non-empty list of pages in 1..NPAGES."
  (unless (and pages
               (cl-every (lambda (p) (and (integerp p) (<= 1 p npages))) pages))
    (user-error "Pages must be between 1 and %d: %S" npages pages)))

(defun pdf-pages-delete (pages)
  "Delete PAGES from the visited PDF file.

Interactively, delete the current page; a numeric prefix argument
N deletes N pages starting at it, and \\[universal-argument] reads a
page range.  Afterwards, show the page that followed the first
deleted one."
  (interactive
   (progn (pdf-pages--prepare)
          (list (pdf-pages-read-pages "Delete pages" current-prefix-arg))))
  (pdf-pages--assert-file)
  (let* ((npages (pdf-cache-number-of-pages))
         (pages (sort (delete-dups (copy-sequence pages)) #'<))
         (keep (cl-remove-if (lambda (p) (memq p pages))
                             (number-sequence 1 npages))))
    (pdf-pages--check-pages pages npages)
    (unless keep
      (user-error "Cannot delete all pages of the document"))
    (pdf-pages--modify
     (format "delete %s of %d" (pdf-pages--describe pages) npages)
     (length keep) (car pages)
     (list "--pages" "." (pdf-pages-range-string keep) "--")
     t)))

(defun pdf-pages-move (pages position)
  "Move PAGES so that the first of them becomes page POSITION.

The moved pages keep the order in which they are given.
Interactively, move the current page; a numeric prefix argument N
moves N pages starting at it, and \\[universal-argument] reads a
page range.  Afterwards, show the first moved page."
  (interactive
   (progn
     (pdf-pages--prepare)
     (let* ((pages (pdf-pages-read-pages "Move pages" current-prefix-arg))
            (last (- (pdf-cache-number-of-pages)
                     (length (delete-dups (copy-sequence pages)))
                     -1)))
       (list pages
             (read-number (format "Move %s to position (1-%d): "
                                  (pdf-pages--describe pages) last))))))
  (pdf-pages--assert-file)
  (let* ((npages (pdf-cache-number-of-pages))
         (pages (delete-dups (copy-sequence pages)))
         (rest (cl-remove-if (lambda (p) (memq p pages))
                             (number-sequence 1 npages)))
         (last (1+ (length rest))))
    (pdf-pages--check-pages pages npages)
    (unless (and (integerp position) (<= 1 position last))
      (user-error "Position must be between 1 and %d" last))
    (let ((order (append (cl-subseq rest 0 (1- position))
                         pages
                         (nthcdr (1- position) rest))))
      (when (equal order (number-sequence 1 npages))
        (user-error "Pages are already in that order"))
      (pdf-pages--modify
       (format "move %s to position %d" (pdf-pages--describe pages) position)
       npages position
       (list "--pages" "." (pdf-pages-range-string order) "--")))))

(defun pdf-pages-rotate (pages angle)
  "Rotate PAGES clockwise by ANGLE degrees, a multiple of 90.

A negative ANGLE rotates counterclockwise.  This changes the file,
unlike `pdf-view-rotate', which only changes the display.
Interactively, rotate the current page by 90 degrees; a numeric
prefix argument N rotates N pages starting at it, and
\\[universal-argument] reads a page range and the angle."
  (interactive
   (progn
     (pdf-pages--prepare)
     (list (pdf-pages-read-pages "Rotate pages" current-prefix-arg)
           (if (consp current-prefix-arg)
               (string-to-number
                (completing-read "Rotate clockwise by degrees: "
                                 '("90" "180" "270") nil t nil nil "90"))
             90))))
  (pdf-pages--assert-file)
  (let ((npages (pdf-cache-number-of-pages))
        (pages (sort (delete-dups (copy-sequence pages)) #'<))
        (angle (and (integerp angle) (mod angle 360))))
    (pdf-pages--check-pages pages npages)
    (unless (memq angle '(90 180 270))
      (user-error "The angle must be a multiple of 90 degrees"))
    (pdf-pages--modify
     (format "rotate %s %s" (pdf-pages--describe pages)
             (pcase angle
               (90 "clockwise")
               (270 "counterclockwise")
               (_ "by 180 degrees")))
     npages (pdf-pages--current-page)
     (list (format "--rotate=+%d:%s" angle (pdf-pages-range-string pages))
           "--"))))

(defun pdf-pages-rotate-counterclockwise (pages)
  "Rotate PAGES counterclockwise by 90 degrees.

See `pdf-pages-rotate' for the interactive use."
  (interactive
   (progn (pdf-pages--prepare)
          (list (pdf-pages-read-pages "Rotate pages" current-prefix-arg))))
  (pdf-pages-rotate pages -90))

(defun pdf-pages--read-pdf-file (prompt)
  "Read the name of an existing PDF file with PROMPT."
  (expand-file-name
   (read-file-name prompt nil nil t nil
                   (lambda (f)
                     (or (file-directory-p f)
                         (string-match-p "\\.pdf\\'" f))))))

(defun pdf-pages--read-file-pages (file)
  "Read a page range of FILE."
  (read-string (format "Pages of %s (e.g. 3-5,8,10-z): "
                       (file-name-nondirectory file))
               nil nil "1-z"))

(defun pdf-pages-insert-file (file after &optional file-pages)
  "Insert the pages of the PDF FILE after page AFTER.

AFTER 0 inserts them before the first page.  FILE-PAGES, a qpdf
page range string such as \"1-3,5\", selects pages of FILE; the
default is all of them.  Interactively, insert after the current
page, and with a prefix argument read FILE-PAGES.  Afterwards,
show the first inserted page.

If FILE is encrypted, decrypt it first: only the visited file's
password is supported."
  (interactive
   (progn
     (pdf-pages--prepare)
     (let* ((file (pdf-pages--read-pdf-file "Insert pages of PDF file: "))
            (range (when current-prefix-arg
                     (pdf-pages--read-file-pages file))))
       (list file
             (read-number (format "Insert after page (0-%d, 0 = before the \
first): " (pdf-cache-number-of-pages))
                          (pdf-pages--current-page))
             range))))
  (pdf-pages--assert-file)
  (let* ((npages (pdf-cache-number-of-pages))
         (file (expand-file-name file))
         (file-pages (or file-pages "1-z"))
         (count (length (pdf-pages-parse-range
                         file-pages (pdf-pages--number-of-pages file)))))
    (unless (and (integerp after) (<= 0 after npages))
      (user-error "AFTER must be between 0 and %d" npages))
    (pdf-pages--modify
     (format "insert %d page%s of %s after page %d"
             count (if (= count 1) "" "s")
             (file-name-nondirectory file) after)
     (+ npages count) (1+ after)
     (append (list "--pages")
             (when (> after 0)
               (list "." (format "1-%d" after)))
             (list file file-pages)
             (when (< after npages)
               (list "." (format "%d-z" (1+ after))))
             (list "--")))))

(defun pdf-pages-append-file (file &optional file-pages)
  "Append the pages of the PDF FILE to the visited file.

FILE-PAGES and the interactive use are as in
`pdf-pages-insert-file'."
  (interactive
   (progn
     (pdf-pages--prepare)
     (let ((file (pdf-pages--read-pdf-file "Append pages of PDF file: ")))
       (list file
             (when current-prefix-arg
               (pdf-pages--read-file-pages file))))))
  (pdf-pages--assert-file)
  (pdf-pages-insert-file file (pdf-cache-number-of-pages) file-pages))

(defun pdf-pages-extract (pages file)
  "Write PAGES to the new PDF FILE and display it.

The pages keep the order in which they are given, so this can
also reorder them.  The visited file is not changed.  The new
file keeps the document information and the encryption of the
visited one.  Interactively, read a page range (the default is
the current page) and the file name; a numeric prefix argument N
takes N pages starting at the current one without asking.
Return FILE's buffer."
  (interactive
   (progn
     (pdf-pages--prepare t)
     (let* ((arg current-prefix-arg)
            (pages (pdf-pages-read-pages "Extract pages"
                                         (if (and arg (not (consp arg)))
                                             arg
                                           '(4))))
            (source (buffer-file-name))
            (default (expand-file-name
                      (format "%s-p%s.pdf" (file-name-base source)
                              (pdf-pages-range-string pages))
                      (file-name-directory source))))
       (list pages
             (read-file-name (format "Write %s to file: "
                                     (pdf-pages--describe pages))
                             (file-name-directory default) default nil
                             (file-name-nondirectory default))))))
  (let* ((source (pdf-pages--assert-file t))
         (npages (pdf-cache-number-of-pages))
         (file (expand-file-name file))
         (buffer (find-buffer-visiting file)))
    (pdf-pages--check-pages pages npages)
    (pdf-pages--assert-saved)
    (when (file-directory-p file)
      (user-error "`%s' is a directory" (abbreviate-file-name file)))
    (when (and (file-exists-p file)
               (equal (file-truename file) source))
      (user-error "Cannot extract pages into the visited file itself"))
    (when (and buffer (buffer-modified-p buffer))
      (user-error "Buffer `%s' visits `%s' and has unsaved changes"
                  (buffer-name buffer) (abbreviate-file-name file)))
    (when (and (file-exists-p file)
               (not (y-or-n-p (format "Overwrite `%s'? "
                                      (abbreviate-file-name file)))))
      (user-error "Page operation cancelled"))
    (pdf-pages--write source file (length pages)
                      (list "--pages" "." (pdf-pages-range-string pages) "--")
                      ;; The server may have the old FILE open.
                      (lambda ()
                        (when (pdf-info-running-p)
                          (ignore-errors (pdf-info-close file)))))
    (if buffer
        (with-current-buffer buffer
          (revert-buffer nil t))
      (setq buffer (find-file-noselect file)))
    (when (called-interactively-p 'any)
      (pop-to-buffer buffer))
    (message "Wrote %s to %s" (pdf-pages--describe pages)
             (abbreviate-file-name file))
    buffer))



;; * ================================================================== *
;; * Minor mode
;; * ================================================================== *

(defvar pdf-pages-minor-mode-map
  (let ((kmap (make-sparse-keymap))
        (smap (make-sparse-keymap)))
    (define-key kmap pdf-pages-minor-mode-map-prefix smap)
    (define-key smap "d" #'pdf-pages-delete)
    (define-key smap "x" #'pdf-pages-extract)
    (define-key smap "m" #'pdf-pages-move)
    (define-key smap "r" #'pdf-pages-rotate)
    (define-key smap "l" #'pdf-pages-rotate-counterclockwise)
    (define-key smap "i" #'pdf-pages-insert-file)
    (define-key smap "a" #'pdf-pages-append-file)
    (define-key smap "u" #'pdf-pages-undo)
    kmap)
  "Keymap used for `pdf-pages-minor-mode'.")

(easy-menu-define pdf-pages-menu pdf-pages-minor-mode-map
  "Menu for PDF page operations."
  '("Pages"
    ["Delete Page(s)" pdf-pages-delete]
    ["Extract Pages to File..." pdf-pages-extract]
    ["Move Page(s)..." pdf-pages-move]
    ["Rotate Page(s) Clockwise" pdf-pages-rotate]
    ["Rotate Page(s) Counterclockwise" pdf-pages-rotate-counterclockwise]
    ["Insert PDF File..." pdf-pages-insert-file]
    ["Append PDF File..." pdf-pages-append-file]
    "--"
    ["Undo Last Page Operation" pdf-pages-undo]))

;;;###autoload
(define-minor-mode pdf-pages-minor-mode
  "Delete, extract, move, rotate and insert PDF pages.

The commands change the visited file with the qpdf program, see
`pdf-pages-qpdf-program'.  They work on the current page, on N
pages starting at it with a numeric prefix argument N, or on a
page range read with \\[universal-argument].  Every change can be
reverted with `pdf-pages-undo'.

\\{pdf-pages-minor-mode-map}"
  :group 'pdf-pages)

(provide 'pdf-pages)

;;; pdf-pages.el ends here

