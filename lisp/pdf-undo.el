;;; pdf-undo.el --- Undo changes to a PDF document. -*- lexical-binding: t -*-

;; Copyright (C) 2026  Tim Ransom

;; Author: Tim Ransom
;; Keywords: files, convenience

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
;; A PDF document is changed in two places: annotations, form fields
;; and document metadata live in the document that the epdfinfo
;; server holds and writes when the buffer is saved, while the page
;; operations of `pdf-pages' (and the flattening that signing with an
;; image ends in) rewrite the whole file with qpdf.  Neither poppler
;; nor qpdf can undo a single change, so `pdf-undo' works with whole
;; versions of the file instead:
;;
;; - if the buffer has unsaved changes, it reverts to the file on
;;   disk, i.e. to the version before them;
;; - otherwise it restores the file as it was before the last page
;;   operation, from a copy that `pdf-pages' keeps in
;;   `pdf-undo-backup-directory'.
;;
;; The two fit together, because a page operation refuses to run while
;; the buffer has unsaved changes: the newest change comes first in
;; either case.
;;
;; `pdf-undo' is on `C-/' and `C-_' in `pdf-view-mode', on `u' behind
;; the prefix of `pdf-pages-minor-mode', and is also `pdf-thumbs-undo'
;; in the thumbnail grid.

;;; Code:

(require 'pdf-view)
(require 'pdf-info)
(require 'pdf-util)
(require 'cl-lib)

(defvar pdf-view--buffer-file-name)
(declare-function pdf-pages--undo-page-operation "pdf-pages" (file))



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-undo nil
  "Undo changes to a PDF document."
  :group 'pdf-tools)

(defcustom pdf-undo-limit 10
  "How many previous versions of a file `pdf-undo' can restore.

The previous versions are copies of the whole file, kept in
`pdf-undo-backup-directory'."
  :group 'pdf-undo
  :type 'integer)

(defcustom pdf-undo-backup-directory nil
  "Directory for the copies that `pdf-undo' restores.

If nil, a directory below variable `temporary-file-directory' is used,
which is deleted when Emacs exits.  If a directory is given, the
copies are kept there and outlive the Emacs session."
  :group 'pdf-undo
  :type '(choice (const :tag "Temporary, deleted on exit" nil)
                 directory))



;; * ================================================================== *
;; * Versions of the file
;; * ================================================================== *

(defvar pdf-undo--alist nil
  "Alist of (FILE . ENTRIES) of the versions `pdf-undo' can restore.

ENTRIES is a list, most recent first, of plists with the keys
:backup (a copy of FILE before the operation), :npages (its page
count), :page (the page shown before it), :modtime (FILE's
modification time after it) and :description.")

(defvar pdf-undo--session-directory nil
  "Temporary directory of this session's backups, if one was created.")

(defun pdf-undo--backup-directory ()
  "Return the directory for backups, creating it if needed."
  (if pdf-undo-backup-directory
      (let ((dir (expand-file-name pdf-undo-backup-directory)))
        (make-directory dir t)
        dir)
    (unless (and pdf-undo--session-directory
                 (file-directory-p pdf-undo--session-directory))
      (setq pdf-undo--session-directory
            (make-temp-file "pdf-undo-" t))
      (add-hook 'kill-emacs-hook #'pdf-undo--delete-session-directory))
    pdf-undo--session-directory))

(defun pdf-undo--delete-session-directory ()
  "Delete this session's backup directory."
  (when (and pdf-undo--session-directory
             (file-directory-p pdf-undo--session-directory))
    (delete-directory pdf-undo--session-directory t)))

(defun pdf-undo--file-modtime (file)
  "Return the modification time of FILE."
  (file-attribute-modification-time (file-attributes file)))

(defun pdf-undo--make-backup (file)
  "Copy FILE to the backup directory and return the copy's name."
  (let ((backup (make-temp-file
                 (expand-file-name (concat (file-name-base file) "-")
                                   (pdf-undo--backup-directory))
                 nil ".pdf")))
    (copy-file file backup t t)
    backup))

(defun pdf-undo--push (file backup npages page description)
  "Record BACKUP as FILE's version before DESCRIPTION.

BACKUP has NPAGES pages, and PAGE was shown.  FILE must already
have been written, so that its modification time is the one of the
new version."
  (let* ((cell (or (assoc file pdf-undo--alist)
                   (car (push (list file) pdf-undo--alist))))
         (entries (cons (list :backup backup
                              :npages npages
                              :page page
                              :modtime (pdf-undo--file-modtime file)
                              :description description)
                        (cdr cell))))
    (when (> (length entries) (max 0 pdf-undo-limit))
      (dolist (old (nthcdr (max 0 pdf-undo-limit) entries))
        (ignore-errors (delete-file (plist-get old :backup))))
      (setq entries (cl-subseq entries 0 (max 0 pdf-undo-limit))))
    (setcdr cell entries)))

(defun pdf-undo--entry (file)
  "Return the newest version of FILE that `pdf-undo' can restore.

Return nil, if there is none."
  (cadr (assoc file pdf-undo--alist)))

(defun pdf-undo--pop (file)
  "Drop the newest version of FILE from the versions of `pdf-undo'."
  (let ((cell (assoc file pdf-undo--alist)))
    (when cell
      (setcdr cell (cddr cell)))))



;; * ================================================================== *
;; * The visited file
;; * ================================================================== *

(defun pdf-undo--assert-file (&optional read-only)
  "Check that the current buffer can be worked on and return its file.

The buffer must be a `pdf-view-mode' buffer visiting a regular,
local, uncompressed PDF file, which must be writable unless
READ-ONLY is non-nil.  Return the file's true name."
  (unless (derived-mode-p 'pdf-view-mode)
    (user-error "Not in a PDF buffer"))
  (when (derived-mode-p 'pdf-virtual-view-mode)
    (user-error "This does not work on virtual PDF buffers"))
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer `%s' is not visiting a file; this needs a PDF \
file on disk" (buffer-name)))
    (when (or (file-remote-p file)
              pdf-view--buffer-file-name
              (not (file-regular-p file)))
      (user-error "`%s' is not a local PDF file (remote, compressed or in \
an archive); this needs a plain file on disk"
                  (abbreviate-file-name file)))
    (setq file (file-truename file))
    (unless (or read-only
                (and (file-writable-p file)
                     (file-writable-p (file-name-directory file))))
      (user-error "`%s' or its directory is not writable"
                  (abbreviate-file-name file)))
    file))

(defun pdf-undo--close-document ()
  "Close the document in the epdfinfo server, if it is running."
  (when (pdf-info-running-p)
    (ignore-errors (pdf-info-close))))

(defun pdf-undo--revert (npages page)
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



;; * ================================================================== *
;; * Undo
;; * ================================================================== *

(defun pdf-undo ()
  "Undo the last change to the visited PDF file.

Only whole versions can be undone, because neither the epdfinfo
server nor qpdf can take back a single change:

- If the buffer has unsaved changes, i.e. annotations, form fields
  or metadata that were not written to the file yet, the buffer
  reverts to the file on disk and the changes are lost.  This asks
  first, and there is no redo: save the buffer if you want to keep
  them.
- Otherwise, the file is restored as it was before the last page
  operation (`pdf-pages'), which also asks first, see
  `pdf-pages-confirm'.

Versions of the second kind are kept for the current Emacs
session, up to `pdf-undo-limit' of them, in
`pdf-undo-backup-directory'.

It is bound to \[pdf-undo] in `pdf-view-mode', and to `u' behind the
prefix of `pdf-pages-minor-mode', see `pdf-pages-minor-mode-map-prefix'."
  (interactive)
  (let ((file (pdf-undo--assert-file (buffer-modified-p))))
    (cond
     ((buffer-modified-p)
      (pdf-undo--discard-changes file))
     ((pdf-undo--entry file)
      (pdf-pages--undo-page-operation file))
     (t
      (user-error "Nothing to undo in `%s'" (abbreviate-file-name file))))))

(defun pdf-undo--discard-changes (file)
  "Discard the unsaved changes to FILE, reverting the current buffer."
  (unless (verify-visited-file-modtime (current-buffer))
    (user-error "`%s' changed on disk; revert the buffer first (%s)"
                (abbreviate-file-name file)
                (substitute-command-keys "\\[revert-buffer]")))
  (unless (y-or-n-p (format "Discard the unsaved changes to `%s'? "
                            (abbreviate-file-name file)))
    (user-error "Undo cancelled"))
  (let ((npages (pdf-cache-number-of-pages))
        (page (or (pdf-view-current-page) 1)))
    (pdf-undo--revert npages page))
  (message "Discarded the unsaved changes to `%s'"
           (abbreviate-file-name file)))

(provide 'pdf-undo)

;;; pdf-undo.el ends here
