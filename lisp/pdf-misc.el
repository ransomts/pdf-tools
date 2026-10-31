;;; pdf-misc.el --- Miscellaneous commands for PDF buffer.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
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


(require 'pdf-view)
(require 'pdf-util)
;; The qpdf helpers of the page operations are reused for document-level
;; attachments, see the "Attachments" section below.
(require 'pdf-pages)
(require 'pdf-undo)
(require 'imenu)
(require 'subr-x)
(require 'xml)



;;; Code:

(defvar pdf-misc-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "I") 'pdf-misc-display-metadata)
    (define-key map (kbd "C-c C-p") 'pdf-misc-print-document)
    map)
  "Keymap used in `pdf-misc-minor-mode'.")

;;;###autoload
(define-minor-mode pdf-misc-minor-mode
  "FIXME:  Not documented."
  :group 'pdf-misc)

;;;###autoload
(define-minor-mode pdf-misc-size-indication-minor-mode
  "Provide a working size indication in the mode-line."
  :group 'pdf-misc
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-misc-size-indication-minor-mode
    (unless (assq 'pdf-misc-size-indication-minor-mode
                  mode-line-position)
      (setq mode-line-position
            `((pdf-misc-size-indication-minor-mode
               (:eval (pdf-misc-size-indication)))
              ,@mode-line-position))))
   (t
    (setq mode-line-position
          (cl-remove 'pdf-misc-size-indication-minor-mode
                     mode-line-position :key 'car-safe)))))

(defun pdf-misc-size-indication ()
  "Return size indication string for the mode-line."
  (let ((top (= (window-vscroll nil t) 0))
        (bot (>= (+ (- (nth 3 (window-inside-pixel-edges))
                       (nth 1 (window-inside-pixel-edges)))
                    (window-vscroll nil t))
                 (cdr (pdf-view-image-size t)))))
    (cond
     ((and top bot) " All")
     (top " Top")
     (bot " Bot")
     (t (format
         " %d%%%%"
         (ceiling
          (* 100 (/ (float (window-vscroll nil t))
                    (cdr (pdf-view-image-size t))))))))))

(defvar pdf-misc-menu-bar-minor-mode-map (make-sparse-keymap)
  "The keymap used in `pdf-misc-menu-bar-minor-mode'.")

(easy-menu-define nil pdf-misc-menu-bar-minor-mode-map
  "Menu for PDF Tools."
  `("PDF Tools"
    ["Go Backward" pdf-history-backward
     :visible (bound-and-true-p pdf-history-minor-mode)
     :active (and (bound-and-true-p pdf-history-minor-mode)
                  (not (pdf-history-end-of-history-p)))]
    ["Go Forward" pdf-history-forward
     :visible (bound-and-true-p pdf-history-minor-mode)
     :active (not (pdf-history-end-of-history-p))]
    ["--" nil
     :visible (derived-mode-p 'pdf-virtual-view-mode)]
    ["Next file" pdf-virtual-buffer-forward-file
     :visible  (derived-mode-p 'pdf-virtual-view-mode)
     :active (pdf-virtual-document-next-file
              (pdf-view-current-page))]
    ["Previous file" pdf-virtual-buffer-backward-file
     :visible (derived-mode-p 'pdf-virtual-view-mode)
     :active (not (eq 1 (pdf-view-current-page)))]
    ["--" nil
     :visible (bound-and-true-p pdf-history-minor-mode)]
    ["Add text annotation" pdf-annot-mouse-add-text-annotation
     :visible (bound-and-true-p pdf-annot-minor-mode)
     :keys "\\[pdf-annot-add-text-annotation]"]
    ("Add markup annotation"
     :active (pdf-view-active-region-p)
     :visible (and (bound-and-true-p pdf-annot-minor-mode)
                   (pdf-info-markup-annotations-p))
     ["highlight" pdf-annot-add-highlight-markup-annotation]
     ["squiggly" pdf-annot-add-squiggly-markup-annotation]
     ["underline" pdf-annot-add-underline-markup-annotation]
     ["strikeout" pdf-annot-add-strikeout-markup-annotation])
    ("Add shape annotation"
     :active (pdf-view-active-region-p)
     :visible (and (bound-and-true-p pdf-annot-minor-mode)
                   (pdf-info-shape-annotations-p))
     ["square" pdf-annot-add-square-annotation]
     ["circle" pdf-annot-add-circle-annotation]
     ["line" pdf-annot-add-line-annotation]
     ["stamp" pdf-annot-add-stamp-annotation
      :visible (pdf-info-stamp-annotations-p)])
    ["Add free text annotation" pdf-annot-add-free-text-annotation
     :help "Write text on the page, in the region or where you click"
     :visible (and (bound-and-true-p pdf-annot-minor-mode)
                   (pdf-info-free-text-annotations-p))]
    ["Add ink annotation" pdf-annot-add-ink-annotation
     :help "Draw free-hand with the mouse"
     :visible (and (bound-and-true-p pdf-annot-minor-mode)
                   (pdf-info-ink-annotations-p))]
    ["--" nil :visible (bound-and-true-p pdf-annot-minor-mode)]
    ["Display Annotations" pdf-annot-list-annotations
     :help "List all annotations"
     :visible (bound-and-true-p pdf-annot-minor-mode)]
    ["Display Annotation Attachments" pdf-annot-attachment-dired
     :help "List the files attached to annotations in a dired buffer"
     :visible (featurep 'pdf-annot)]
    ["List Document Attachments" pdf-misc-attachments-dired
     :help "Extract the document's attachments and list them in a dired buffer"
     :visible (featurep 'pdf-misc)]
    ["Add Attachment..." pdf-misc-add-attachment
     :help "Attach a file to the document with the qpdf program"
     :visible (featurep 'pdf-misc)
     :active (and (pdf-view-buffer-file-name)
                  (file-writable-p (pdf-view-buffer-file-name)))]
    ["Remove Attachment..." pdf-misc-remove-attachment
     :help "Remove an attached file from the document with the qpdf program"
     :visible (featurep 'pdf-misc)
     :active (and (pdf-view-buffer-file-name)
                  (file-writable-p (pdf-view-buffer-file-name)))]
    ["Display Metadata" pdf-misc-display-metadata
     :help "Display information about the document"
     :visible (featurep 'pdf-misc)]
    ["Edit Metadata" pdf-misc-edit-metadata
     :help "Edit the title, author, subject, keywords and creator"
     :visible (featurep 'pdf-misc)
     :active (and (pdf-info-writable-metadata-p)
                  (not (derived-mode-p 'pdf-virtual-view-mode)))]
    ["Display Outline" pdf-outline
     :help "Display documents outline"
     :visible (featurep 'pdf-outline)]
    "--"
    ("Render Options"
     ["Printed Mode" (lambda ()
                       (interactive)
                       (pdf-view-printer-minor-mode 'toggle))
      :style toggle
      :selected pdf-view-printer-minor-mode
      :help "Display the PDF as it would be printed."]
     ["Midnight Mode" (lambda ()
                        (interactive)
                        (pdf-view-midnight-minor-mode 'toggle))
      :style toggle
      :selected pdf-view-midnight-minor-mode
      :help "Apply a color-filter appropriate for past midnight reading."]
     ["Mark Form Fields" pdf-form-show-fields
      :style toggle
      :selected (bound-and-true-p pdf-form-show-fields)
      :visible (bound-and-true-p pdf-form-minor-mode)
      :help "Mark the form fields you can interact with."])
    "--"
    ["Copy region" pdf-view-kill-ring-save
     :keys "\\[kill-ring-save]"
     :active (pdf-view-active-region-p)]
    ["Copy image" pdf-view-copy-image
     :keys "\\[pdf-view-copy-image]"
     :help "Copy the page, slice or region to the clipboard as a picture"]
    ("Selection style"
     ["Glyph" (pdf-view-set-selection-style 'glyph)
      :style radio
      :selected (eq pdf-view-selection-style 'glyph)
      :help "When dragging the mouse, select individual characters."]
     ["Word" (pdf-view-set-selection-style 'word)
      :style radio
      :selected (eq pdf-view-selection-style 'word)
      :help "When dragging the mouse, select entire words."]
     ["Line" (pdf-view-set-selection-style 'line)
      :style radio
      :selected (eq pdf-view-selection-style 'line)
      :help "When dragging the mouse, select entire lines."])
    "--"
    ["Isearch document" isearch-forward
     :visible (bound-and-true-p pdf-isearch-minor-mode)]
    ["Occur document" pdf-occur
     :visible (featurep 'pdf-occur)]
    "--"
    ["Locate TeX source" pdf-sync-backward-search-mouse
     :visible (and (featurep 'pdf-sync)
                   (equal last-command-event
                          last-nonmenu-event))]
    ["--" nil :visible (and (featurep 'pdf-sync)
                            (equal last-command-event
                                   last-nonmenu-event))]
    ["Print" pdf-misc-print-document
     :active (and (pdf-view-buffer-file-name)
                  (file-readable-p (pdf-view-buffer-file-name)))]
    ["Create image" pdf-view-extract-region-image
     :help "Create an image of the page or the selected region(s)."]
    ["Create virtual PDF" pdf-virtual-buffer-create
     :help "Create a PDF containing all documents in this directory."
     :visible (bound-and-true-p pdf-virtual-global-minor-mode)]
    "--"
    ["Revert buffer" pdf-view-revert-buffer
     :visible (pdf-info-writable-annotations-p)]
    "--"
    ["Customize" pdf-tools-customize]))

;;;###autoload
(define-minor-mode pdf-misc-menu-bar-minor-mode
  "Display a PDF Tools menu in the menu-bar."
  :group 'pdf-misc
  (pdf-util-assert-pdf-buffer))

(defvar pdf-misc-context-menu-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [down-mouse-3] 'pdf-misc-popup-context-menu)
    kmap))

;;;###autoload
(define-minor-mode pdf-misc-context-menu-minor-mode
  "Provide a right-click context menu in PDF buffers.

\\{pdf-misc-context-menu-minor-mode-map}"
  :group 'pdf-misc
  (pdf-util-assert-pdf-buffer))

(defun pdf-misc-popup-context-menu (_event)
  "Popup a context menu at position."
  (interactive "@e")
  (popup-menu
   (cons 'keymap
         (cddr (or (lookup-key pdf-misc-menu-bar-minor-mode-map
                               [menu-bar PDF\ Tools])
                   (lookup-key pdf-misc-menu-bar-minor-mode-map
                               [menu-bar pdf\ tools]))))))

(defvar-local pdf-misc-metadata--buffer nil
  "The PDF buffer whose metadata is displayed in this buffer.")

(defvar pdf-metadata-buffer-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "e") #'pdf-misc-edit-metadata)
    kmap)
  "Keymap used in `pdf-metadata-buffer-mode'.")

(define-derived-mode pdf-metadata-buffer-mode special-mode "PDF Metadata"
  "View and traverse the metadata of a PDF file.

\\<pdf-metadata-buffer-mode-map>\
Press \\[pdf-misc-edit-metadata] to edit the metadata and \
\\[revert-buffer] to redisplay it.

\\{pdf-metadata-buffer-mode-map}"
  (setq-local revert-buffer-function #'pdf-misc-metadata--revert))

(defun pdf-misc-metadata--revert (&rest _)
  "Redisplay the metadata in the current `pdf-metadata-buffer-mode' buffer."
  (unless (buffer-live-p pdf-misc-metadata--buffer)
    (user-error "The PDF buffer of this metadata is gone"))
  (with-current-buffer pdf-misc-metadata--buffer
    (pdf-misc-display-metadata)))

(defun pdf-misc-display-metadata ()
  "Display all available metadata in a separate buffer."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (let* ((buffer (current-buffer))
         (md (pdf-info-metadata)))
    (with-current-buffer (get-buffer-create "*PDF-Metadata*")
      (unless (derived-mode-p 'pdf-metadata-buffer-mode)
        (pdf-metadata-buffer-mode))
      (setq pdf-misc-metadata--buffer buffer)
      (let* ((inhibit-read-only t)
             (pad (apply' max (mapcar (lambda (d)
                                        (length (symbol-name (car d))))
                                      md)))
             (fmt (format "%%%ds:%%s\n" pad)))
        (erase-buffer)
        (setq header-line-format (buffer-name buffer)
              buffer-read-only t)
        (font-lock-mode 1)
        (font-lock-add-keywords nil
          '(("^ *\\(\\(?:\\w\\|-\\)+\\):"
             (1 font-lock-keyword-face))))
        (dolist (d md)
          (let ((key (car d))
                (val (cdr d)))
            (cl-case key
              (keywords
               (setq val (mapconcat 'identity val ", "))))
            (let ((beg (+ (length (symbol-name key)) (point) 1))
                  (fill-prefix
                   (make-string (1+ pad) ?\s)))
              (insert (format fmt key val))
              (fill-region beg (point) )))))
      (goto-char 1)
      (display-buffer (current-buffer)))
    md))

(defgroup pdf-misc nil
  "Miscellaneous options for PDF documents."
  :group 'pdf-tools)

(define-obsolete-variable-alias 'pdf-misc-print-programm
  'pdf-misc-print-program-executable "1.0")
(defcustom pdf-misc-print-program-executable nil
  "The program used for printing.

It is called with one argument, the PDF file."
  :group 'pdf-misc
  :type 'file)

(define-obsolete-variable-alias 'pdf-misc-print-programm-args
  'pdf-misc-print-program-args "1.0")
(defcustom pdf-misc-print-program-args nil
  "List of additional arguments passed to `pdf-misc-print-program'."
  :group 'pdf-misc
  :type '(repeat string))

(define-obsolete-function-alias 'pdf-misc-print-programm
  'pdf-misc-print-program "1.0")
(defun pdf-misc-print-program (&optional interactive-p)
  "Return the program used to print PDFs (if the executable is installed).

If INTERACTIVE-P is non-nil, ask the user for which program to
use when printing the PDF. Optionally, save the choice"
  (or (and pdf-misc-print-program-executable
           (executable-find pdf-misc-print-program-executable))
      (when interactive-p
        (let* ((default (car (delq nil (mapcar
                                        'executable-find
                                        '("gtklp" "xpp" "gpr")))))
               buffer-file-name
               (program
                (expand-file-name
                 (read-file-name
                  "Print with: " default nil t nil 'file-executable-p))))
          (when (and program
                     (executable-find program))
            (when (y-or-n-p "Save choice using customize? ")
              (customize-save-variable
               'pdf-misc-print-program-executable program))
            (setq pdf-misc-print-program-executable program))))))

(defun pdf-misc-print-document (filename &optional interactive-p)
  "Print the PDF doc FILENAME.

`pdf-misc-print-program' handles the print program, which see for
definition of INTERACTIVE-P."
  (interactive
   (list (pdf-view-buffer-file-name) t))
  (cl-check-type filename (and string (satisfies file-readable-p)))
  (let ((program (pdf-misc-print-program interactive-p))
        (args (append pdf-misc-print-program-args (list filename))))
    (unless program
      (error "No print program available"))
    (apply #'start-process "printing" nil program args)
    (message "Print job started: %s %s"
             program (mapconcat #'identity args " "))))


;; * ================================================================== *
;; * Editing metadata
;; * ================================================================== *

(defconst pdf-misc-metadata-xmp-namespaces
  '((rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (dc . "http://purl.org/dc/elements/1.1/")
    (xml . "http://www.w3.org/XML/1998/namespace"))
  "XML namespaces used when reading XMP metadata.")

(defun pdf-misc--xmp-name (prefix local)
  "Return the qualified name of LOCAL in the namespace PREFIX.

PREFIX is a key of `pdf-misc-metadata-xmp-namespaces'.  The name
is a symbol, as returned by `xml-parse-region' with
`symbol-qnames'."
  (intern (concat (cdr (assq prefix pdf-misc-metadata-xmp-namespaces))
                  local)))

(defun pdf-misc-metadata-xmp-title (&optional buffer)
  "Return the title in the XMP metadata of the PDF in BUFFER.

This is the \"x-default\" (or else the first) entry of the
dc:title property in the document's XMP metadata stream, which
PDF/UA and accessibility checkers read instead of, or in addition
to, the title in the document information dictionary.

Return nil if the document has no XMP metadata stream.  Return
the empty string if it has one, but there is no dc:title in it,
or the XMP can not be parsed."
  (with-current-buffer (or buffer (current-buffer))
    (let ((xmp (pdf-info-xmp-metadata)))
      (when xmp
        (or (ignore-errors
              (with-temp-buffer
                (insert xmp)
                ;; xml.el chokes on the byte order mark in the xpacket
                ;; processing instruction.
                (goto-char 1)
                (while (re-search-forward "<\\?xpacket[^>]*>" nil t)
                  (replace-match ""))
                (pdf-misc--xmp-find-title
                 (xml-parse-region (point-min) (point-max)
                                   nil nil 'symbol-qnames))))
            "")))))

(defun pdf-misc--xmp-find-title (nodes)
  "Return the dc:title found in the parsed XML NODES, or nil."
  (let ((title-name (pdf-misc--xmp-name 'dc "title"))
        (li-name (pdf-misc--xmp-name 'rdf "li"))
        (lang-name (pdf-misc--xmp-name 'xml "lang"))
        result)
    (cl-labels
        ((items (node)
           ;; All rdf:li descendants of NODE.
           (cl-loop for child in (xml-node-children node)
                    when (consp child)
                    nconc (if (eq (xml-node-name child) li-name)
                              (list child)
                            (items child))))
         (text (node)
           (string-trim
            (mapconcat (lambda (c) (if (stringp c) c ""))
                       (xml-node-children node) "")))
         (walk (node)
           (when (and (consp node) (not result))
             (if (eq (xml-node-name node) title-name)
                 (let ((lis (items node)))
                   (setq result
                         (if lis
                             (text (or (cl-find "x-default" lis
                                                :key (lambda (li)
                                                       (xml-get-attribute-or-nil
                                                        li lang-name))
                                                :test #'equal)
                                       (car lis)))
                           (text node))))
               (mapc #'walk (xml-node-children node))))))
      (mapc #'walk nodes))
    result))

(defun pdf-misc--metadata-value-string (key value)
  "Return metadata VALUE of KEY as the string sent to the server."
  (cond
   ((null value) "")
   ((and (eq key 'keywords) (consp value))
    (mapconcat #'identity value ", "))
   (t value)))

(defun pdf-misc--metadata-current-string (key metadata)
  "Return the value of KEY in METADATA as a string.

METADATA is an alist as returned by `pdf-info-metadata'."
  (or (cdr (assq (if (eq key 'keywords) 'keywords-raw key) metadata))
      ""))

(defun pdf-misc--metadata-refresh-display (buffer)
  "Redisplay the metadata buffer, if it shows the metadata of BUFFER."
  (let ((md-buffer (get-buffer "*PDF-Metadata*")))
    (when (and md-buffer
               (eq buffer (buffer-local-value
                           'pdf-misc-metadata--buffer md-buffer))
               (get-buffer-window md-buffer t))
      (with-current-buffer md-buffer
        (pdf-misc-metadata--revert)))))

(defun pdf-misc-update-metadata (metadata &optional buffer)
  "Change the metadata of the PDF in BUFFER according to METADATA.

METADATA is an alist of fields and their new values.  The fields
are `title', `author', `subject', `keywords' and `creator'.  A
value is a string, or for `keywords' also a list of strings; nil
or the empty string removes the field from the document.  Fields
whose value does not change are ignored.

If anything changed, BUFFER is marked as modified; \
\\[save-buffer] writes
the new metadata to the file and reverting the buffer discards
it.

Poppler changes only the document information dictionary, not
the XMP metadata stream, which PDF/UA and accessibility checkers
read the title from.  If the title changes and the document has
an XMP stream whose title (see `pdf-misc-metadata-xmp-title')
differs from the new one, a warning of type (pdf-misc xmp) is
displayed.  Add it to `warning-suppress-types' to silence it.

BUFFER defaults to the current buffer.  Return the document's
metadata after the change, as `pdf-info-metadata' does."
  (with-current-buffer (or buffer (current-buffer))
    (pdf-util-assert-pdf-buffer)
    (when (derived-mode-p 'pdf-virtual-view-mode)
      (user-error "Can not edit the metadata of a virtual PDF"))
    (pdf-info-assert-writable-metadata)
    (let* ((current (pdf-info-metadata))
           (changes
            (cl-loop for (key . value) in metadata
                     for new = (pdf-misc--metadata-value-string key value)
                     unless (memq key pdf-info-writable-metadata-keys)
                     do (error "Metadata field is not editable: %s" key)
                     unless (equal new (pdf-misc--metadata-current-string
                                        key current))
                     collect (cons key new))))
      (if (null changes)
          current
        (let ((md (pdf-info-setmetadata changes)))
          (set-buffer-modified-p t)
          (when (assq 'title changes)
            (pdf-misc--metadata-check-xmp-title (cdr (assq 'title md))))
          (pdf-misc--metadata-refresh-display (current-buffer))
          md)))))

(defun pdf-misc--metadata-check-xmp-title (title)
  "Warn if the XMP title of the current document disagrees with TITLE."
  (let ((xmp-title (pdf-misc-metadata-xmp-title)))
    (when (and xmp-title (not (equal xmp-title (or title ""))))
      (display-warning
       '(pdf-misc xmp)
       (format "%s: the title was changed in the document information \
dictionary only; the XMP metadata, which accessibility checkers read, \
%s"
               (buffer-name)
               (if (equal xmp-title "")
                   "has no title"
                 (format "still has the title %S" xmp-title)))))))

(defun pdf-misc-set-metadata (field value &optional buffer)
  "Set the metadata FIELD of the PDF in BUFFER to VALUE.

FIELD is one of `title', `author', `subject', `keywords' or
`creator'.  An empty VALUE removes the field.  This is a
convenience wrapper around `pdf-misc-update-metadata', which see,
also for the handling of XMP metadata.

Interactively, read FIELD and VALUE in the minibuffer, using the
current value as initial input."
  (interactive
   (progn
     (pdf-util-assert-pdf-buffer)
     (let* ((field (intern
                    (completing-read
                     "Metadata field: "
                     (mapcar #'symbol-name pdf-info-writable-metadata-keys)
                     nil t nil nil "title")))
            (current (pdf-misc--metadata-current-string
                      field (pdf-info-metadata))))
       (list field
             (read-string (format "%s: " (capitalize (symbol-name field)))
                          current)))))
  (prog1 (pdf-misc-update-metadata (list (cons field value)) buffer)
    (when (called-interactively-p 'any)
      (message "%s"
               (substitute-command-keys
                "Metadata changed; \\[save-buffer] writes it to the file")))))

(defvar-local pdf-misc-edit-metadata--buffer nil
  "The PDF buffer whose metadata is edited in this buffer.")

(defvar-local pdf-misc-edit-metadata--original nil
  "Alist of the field values as they were inserted into this buffer.")

(defvar pdf-misc-edit-metadata-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-c C-c") #'pdf-misc-edit-metadata-commit)
    (define-key kmap (kbd "C-c C-q") #'pdf-misc-edit-metadata-abort)
    (define-key kmap (kbd "C-c C-k") #'pdf-misc-edit-metadata-abort)
    kmap)
  "Keymap used in `pdf-misc-edit-metadata-mode'.")

(define-derived-mode pdf-misc-edit-metadata-mode text-mode "PDF Metadata Edit"
  "Major mode for editing the metadata of a PDF document.

Each line has the form FIELD: VALUE.  The order of the lines does
not matter, and a field whose line is deleted is left unchanged.
An empty value removes the field from the document.

See `pdf-misc-update-metadata' for what is changed in the
document and what is not.

\\{pdf-misc-edit-metadata-mode-map}"
  (setq-local header-line-format
              (substitute-command-keys "\
Press \\[pdf-misc-edit-metadata-commit] to apply your changes, \
\\[pdf-misc-edit-metadata-abort] to abandon them."))
  (setq-local font-lock-defaults
              '((("^\\([[:alpha:]]+\\):" 1 font-lock-keyword-face)) t)))

(defun pdf-misc-edit-metadata--parse ()
  "Return the alist of fields and values in the current buffer."
  (let (result)
    (save-excursion
      (goto-char 1)
      (while (not (eobp))
        (cond
         ((looking-at "[ \t]*$"))
         ((looking-at "\\([[:alpha:]]+\\):[ \t]*\\(.*?\\)[ \t]*$")
          (let ((field (intern (downcase (match-string-no-properties 1))))
                (value (match-string-no-properties 2)))
            (unless (memq field pdf-info-writable-metadata-keys)
              (user-error "Line %d: Unknown metadata field: %s"
                          (line-number-at-pos) (match-string 1)))
            (when (assq field result)
              (user-error "Line %d: Field given twice: %s"
                          (line-number-at-pos) (match-string 1)))
            (push (cons field value) result)))
         (t
          (user-error "Line %d: Expected FIELD: VALUE"
                      (line-number-at-pos))))
        (forward-line)))
    (nreverse result)))

(defun pdf-misc-edit-metadata-commit ()
  "Apply the edited metadata to the PDF and close the edit buffer."
  (interactive)
  (let* ((fields (pdf-misc-edit-metadata--parse))
         (original pdf-misc-edit-metadata--original)
         (buffer pdf-misc-edit-metadata--buffer)
         (changes (cl-remove-if
                   (lambda (elt)
                     (equal (cdr elt) (cdr (assq (car elt) original))))
                   fields)))
    (unless (buffer-live-p buffer)
      (user-error "The PDF buffer of this metadata is gone"))
    (when changes
      (pdf-misc-update-metadata changes buffer))
    (pdf-misc-edit-metadata--quit)
    (message "%s"
             (if changes
                 (with-current-buffer buffer
                   (substitute-command-keys
                    "Metadata changed; \\[save-buffer] writes it to the file"))
               "Metadata unchanged"))))

(defun pdf-misc-edit-metadata-abort ()
  "Abandon the edits and close the edit buffer."
  (interactive)
  (pdf-misc-edit-metadata--quit))

(defun pdf-misc-edit-metadata--quit ()
  "Quit all windows showing the current edit buffer and kill it."
  (let ((buffer (current-buffer)))
    (set-buffer-modified-p nil)
    (dolist (win (get-buffer-window-list buffer nil t))
      (quit-window nil win))
    (kill-buffer buffer)))

(defun pdf-misc-edit-metadata-noselect (&optional buffer)
  "Prepare and return a buffer for editing the metadata of BUFFER.

BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (pdf-util-assert-pdf-buffer)
    (when (derived-mode-p 'pdf-virtual-view-mode)
      (user-error "Can not edit the metadata of a virtual PDF"))
    (pdf-info-assert-writable-metadata)
    (let* ((pdf-buffer (current-buffer))
           (md (pdf-info-metadata))
           (original
            (mapcar (lambda (key)
                      (cons key
                            (string-trim
                             (replace-regexp-in-string
                              "[\n\r]+" " "
                              (pdf-misc--metadata-current-string key md)))))
                    pdf-info-writable-metadata-keys)))
      (with-current-buffer (get-buffer-create
                            (format "*Edit Metadata %s*" (buffer-name)))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (unless (derived-mode-p 'pdf-misc-edit-metadata-mode)
            (pdf-misc-edit-metadata-mode))
          (setq pdf-misc-edit-metadata--buffer pdf-buffer
                pdf-misc-edit-metadata--original original)
          (dolist (elt original)
            (insert (format "%s: %s\n"
                            (capitalize (symbol-name (car elt)))
                            (cdr elt))))
          (goto-char 1)
          (end-of-line)
          (set-buffer-modified-p nil))
        (current-buffer)))))

(defun pdf-misc-edit-metadata (&optional buffer)
  "Edit the metadata of the PDF in BUFFER.

Pop up a buffer listing the title, author, subject, keywords and
creator of the document.  Edit the values and press
\\<pdf-misc-edit-metadata-mode-map>\\[pdf-misc-edit-metadata-commit] \
to apply them or \\[pdf-misc-edit-metadata-abort] to abandon them.

Applying marks the PDF buffer as modified; save it to write the
metadata to the file.  Only the document information dictionary
is changed, not the XMP metadata stream (see
`pdf-misc-update-metadata').

BUFFER defaults to the current buffer or, in the buffer displayed
by `pdf-misc-display-metadata', to the PDF buffer it describes."
  (interactive)
  (pop-to-buffer
   (pdf-misc-edit-metadata-noselect
    (or buffer
        (and (derived-mode-p 'pdf-metadata-buffer-mode)
             (or (and (buffer-live-p pdf-misc-metadata--buffer)
                      pdf-misc-metadata--buffer)
                 (user-error "The PDF buffer of this metadata is gone")))
        (current-buffer)))))


;; * ================================================================== *
;; * Attachments
;; * ================================================================== *
;;
;; A PDF can carry files, either attached to an individual annotation or
;; to the document as a whole.  Poppler, and therefore the epdfinfo
;; server, only reads them: the file names and contents come from
;; `pdf-info-getattachments'.  Putting a file into the document or taking
;; one out means rewriting it, which qpdf does.  The page operations in
;; pdf-pages solved the same problem already, so their private helpers
;; are called from here on purpose: they run qpdf, let it write the new
;; version next to the original, check the result, replace the original
;; only if that succeeded, and make the server reopen the new file.  The
;; version before the change is kept for `pdf-undo', as for the page
;; operations.
;;
;; The document-level attachments are a different collection from the
;; files attached to annotations, which have their own cache in
;; `pdf-annot-attachment-base-directory' and are listed by
;; `pdf-annot-attachment-dired'.

(defun pdf-misc--attachment-keys (file)
  "Return the keys of the document-level attachments of the PDF FILE.

The keys are the names the qpdf program uses to identify the
attachments; they are not necessarily the file names they are
displayed with.  Return nil if FILE has no attachments."
  (let ((result (pdf-pages--call-qpdf
                 (password-read-from-cache (pdf-pages--password-key file))
                 "--list-attachments" file)))
    (unless (pdf-pages--ok-p result)
      (error "The qpdf program cannot list the attachments of `%s': %s"
             (abbreviate-file-name file) (cdr result)))
    (cl-loop for line in (split-string (cdr result) "\n" t)
             when (string-match "\\`\\(.+\\) -> [0-9]+,[0-9]+\\'" line)
             collect (match-string 1 line))))

(defun pdf-misc--unique-attachment-name (name names)
  "Return NAME, or a variant of it that is not in the list NAMES.

A clash is resolved the way `pdf-annot-attachment-unique-filename'
resolves it, by appending -1, -2 and so on before the extension,
so that two attachments called rubric.md become rubric.md and
rubric-1.md.  The empty string is taken as the name attachment."
  (when (string-empty-p name)
    (setq name "attachment"))
  (let* ((sans-ext (file-name-sans-extension name))
         (extension (file-name-extension name))
         (unique name)
         (i 0))
    (while (member unique names)
      (setq unique (format "%s-%d%s" sans-ext (cl-incf i)
                           (if extension (concat "." extension) ""))))
    unique))

(defun pdf-misc--signature-fields-p ()
  "Return non-nil if the current document has a signature field.

Signing a PDF signs the bytes of the file, so any rewrite of the
document makes the signature invalid, even if the signature itself
is left alone."
  (cl-some (lambda (page)
             (cl-some (lambda (field)
                        (eq (alist-get 'type field) 'signature))
                      (pdf-info-formfields page)))
           (number-sequence 1 (pdf-cache-number-of-pages))))

(defun pdf-misc--confirm-signature-loss ()
  "Ask before rewriting a signed document, which invalidates its signature.

This is a question rather than a hard stop, since a reader may not
care about the signature.  Answering no signals a `user-error'."
  (when (pdf-misc--signature-fields-p)
    (unless (y-or-n-p "Rewriting the file invalidates the document's \
signature; continue? ")
      (user-error "Attachment not changed: the signature stays valid"))))

(defun pdf-misc--write-attachments (args description)
  "Rewrite the visited PDF with qpdf ARGS and revert its buffer.

DESCRIPTION describes the operation for the message the user sees.
ARGS are the qpdf arguments after the input file, terminated by a
\"--\" if they belong to an option group.  The original file is
replaced only if qpdf succeeds, see `pdf-pages--write', whose page
count check also makes sure no pages were lost.  The previous version
is kept for `pdf-undo', as for the page operations.

Return the document's attachment list after the change, as
`pdf-info-getattachments' does."
  (let* ((file (pdf-undo--assert-file))
         (npages (pdf-cache-number-of-pages))
         (page (pdf-pages--current-page))
         backup done)
    (pdf-pages--assert-saved)
    (pdf-misc--confirm-signature-loss)
    (setq backup (pdf-undo--make-backup file))
    (unwind-protect
        (progn
          (pdf-pages--write file file npages args
                            ;; The server may have the old file open.
                            #'pdf-undo--close-document)
          (setq done t))
      (unless done
        (delete-file backup)))
    (pdf-undo--push file backup npages page description)
    (pdf-undo--revert npages page)
    (let ((attachments (pdf-info-getattachments)))
      (when (called-interactively-p 'any)
        (message "%s; the document has %s (%s to undo)" description
                 (if attachments
                     (format "%d attachment%s" (length attachments)
                             (if (cdr attachments) "s" ""))
                   "no attachments")
                 (substitute-command-keys "\\[pdf-undo]")))
      attachments)))

(defun pdf-misc-attachments-dired (&optional regenerate-p)
  "List the document-level attachments of the current PDF in Dired.

Extract every attachment into a temporary directory named after
the document -- the counterpart of
`pdf-annot-attachment-dired', which lists the files attached to
annotations -- and pop to a Dired buffer showing it, where
\\<dired-mode-map>\\[dired-find-file] opens the file at point.  Two
attachments whose names clash are made unique the way
`pdf-annot-attachment-unique-filename' does it, so two attachments
called rubric.md appear as rubric.md and rubric-1.md.

If REGENERATE-P is non-nil, extract the files again even if they
are already there; interactively it is non-nil if a prefix
argument was given.  Return the Dired buffer."
  (interactive (list current-prefix-arg))
  (pdf-util-assert-pdf-buffer)
  (let ((attachments (pdf-info-getattachments t))
        names)
    (unwind-protect
        (progn
          (unless attachments
            (user-error "Document has no attachments"))
          (let ((dir (pdf-util-expand-file-name "document-attachments")))
            (make-directory dir t)
            (dolist (attachment attachments)
              (let* ((source (cdr (assq 'file attachment)))
                     (name (pdf-misc--unique-attachment-name
                            (or (cdr (assq 'filename attachment))
                                "attachment")
                            names))
                     (target (expand-file-name name dir)))
                (push name names)
                (when (or regenerate-p
                          (not (file-exists-p target)))
                  (copy-file source target nil nil t t))))
            (dired dir)))
      ;; The extracted files were written by the server for us alone.
      (dolist (attachment attachments)
        (let ((source (cdr (assq 'file attachment))))
          (when (and source (file-exists-p source))
            (delete-file source)))))))

(defun pdf-misc-add-attachment (file &optional description)
  "Attach FILE to the visited PDF document with the qpdf program.

DESCRIPTION is the description readers see for the attachment;
nil, or the empty string, means none.  The attachment is named
after FILE unless that name is taken, in which case a number is
appended, so the same file can be attached twice.

Rewriting the document invalidates a signature, so if it has a
signature field, ask before going ahead.  Return the document's
attachment list after the change, as `pdf-info-getattachments'
does."
  (interactive
   (progn
     (pdf-undo--assert-file)
     (pdf-pages--assert-saved)
     (list (read-file-name "Attach file: " nil nil t)
           (let ((description (read-string "Description (optional): ")))
             (unless (string-empty-p description)
               description)))))
  (let* ((document (pdf-undo--assert-file))
         (attachment (expand-file-name file)))
    (unless (file-regular-p attachment)
      (user-error "`%s' is not a regular file"
                  (abbreviate-file-name attachment)))
    (let* ((keys (pdf-misc--attachment-keys document))
           (key (pdf-misc--unique-attachment-name
                 (file-name-nondirectory attachment) keys)))
      (pdf-misc--write-attachments
       (append (list "--add-attachment" attachment (concat "--key=" key))
               (unless (string-empty-p (or description ""))
                 (list (concat "--description=" description)))
               (list "--"))
       (format "Attached `%s' as `%s'"
               (file-name-nondirectory attachment) key)))))

(defun pdf-misc-remove-attachment (name)
  "Remove the document-level attachment NAME from the visited PDF.

NAME is the attachment's key, as the qpdf program lists it;
interactively it is read with completion over the document's
attachments.  Signal a `user-error' if there is no such
attachment.

Rewriting the document invalidates a signature, so if it has a
signature field, ask before going ahead.  Return the document's
attachment list after the change, as `pdf-info-getattachments'
does."
  (interactive
   (progn
     (let ((keys (pdf-misc--attachment-keys (pdf-undo--assert-file))))
       (pdf-pages--assert-saved)
       (unless keys
         (user-error "Document has no attachments"))
       (list (completing-read "Remove attachment: " keys nil t)))))
  (let ((file (pdf-undo--assert-file)))
    (unless (member name (pdf-misc--attachment-keys file))
      (user-error "No attachment named `%s'" name))
    (pdf-misc--write-attachments
     (list (concat "--remove-attachment=" name) "--")
     (format "Removed attachment `%s'" name))))

(provide 'pdf-misc)

;;; pdf-misc.el ends here
