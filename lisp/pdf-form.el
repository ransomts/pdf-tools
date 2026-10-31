;;; pdf-form.el --- Fill in PDF forms. -*- lexical-binding: t -*-

;; Author: Tim Ransom <ransomtim8078@gmail.com>
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
;; Fill in the fields of interactive (AcroForm) PDF forms.  Click on a
;; field to change its value, move from field to field with TAB and
;; S-TAB, or use C-c C-e to pick one of the visible fields by name.
;; The values are kept in the buffer until it is saved.
;;
;; The fields you can interact with are marked on the page; turn the
;; marks off and on with C-c C-s.
;;
;; Clicking on a signature field, or M-x pdf-form-signatures, shows
;; whether its signature is valid.
;;
;; M-x pdf-form-sign signs the document with a certificate from an
;; NSS database, see `pdf-info-nss-directory', into an empty
;; signature field or a rectangle of the page.  Clicking on an empty
;; signature field offers to sign it.  Poppler writes the signed
;; document to a new file, so signing
;;
;; - refuses to run on a buffer with unsaved changes (it offers to
;;   save them first), because the signature covers the file on disk
;;   and they would otherwise be lost,
;; - lets poppler write the signed version next to the original and
;;   only then renames it over the original, so a failure leaves the
;;   file as it was,
;; - keeps a copy of the unsigned version in
;;   `pdf-form-backup-directory', and
;; - reverts the buffer.
;;
;; From Lisp, `pdf-form-values' returns the values of a form and
;; `pdf-form-fill' fills one in, both by field name.

(require 'pdf-view)
(require 'pdf-info)
(require 'pdf-cache)
(require 'pdf-util)
(require 'pdf-annot)
(require 'pdf-pages)
(require 'let-alist)
(require 'cl-lib)
(require 'crm)
(require 'subr-x)

;;; Code:

(defvar pdf-view--buffer-file-name)



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-form nil
  "Filling in PDF forms."
  :group 'pdf-tools)

(defcustom pdf-form-highlight-color '("white" "steel blue" 0.35)
  "How to highlight the field being filled in.

A list of the highlight's background and foreground colors and
its opacity, see `pdf-info-renderpage-highlight'."
  :group 'pdf-form
  :type '(list (color :tag "Background")
               (color :tag "Foreground")
               (number :tag "Opacity")))

(defcustom pdf-form-show-fields t
  "Whether to mark the fields you can interact with on displayed pages.

Turn the marks off and on in a buffer with \\[pdf-form-show-fields];
`pdf-form-show-fields-color' says how to mark them."
  :group 'pdf-form
  :type 'boolean)

(defcustom pdf-form-show-fields-color '("steel blue" "steel blue" 0.15)
  "How to mark the fields you can interact with.

A list of the mark's background and foreground colors and its
opacity, see `pdf-info-renderpage-highlight'."
  :group 'pdf-form
  :type '(list (color :tag "Background")
               (color :tag "Foreground")
               (number :tag "Opacity")))

(defcustom pdf-form-backup-directory nil
  "Directory for the copies of documents as they were before signing.

If nil, a directory below variable `temporary-file-directory' is used,
which is deleted when Emacs exits.  If a directory is given, the
copies are kept there and outlive the Emacs session."
  :group 'pdf-form
  :type '(choice (const :tag "Temporary, deleted on exit" nil)
                 directory))


;; * ================================================================== *
;; * Minor Mode
;; * ================================================================== *

(defvar pdf-form-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-c C-e") 'pdf-form-fill-field)
    (define-key kmap (kbd "C-c C-s") 'pdf-form-show-fields)
    (define-key kmap (kbd "TAB") 'pdf-form-next-field)
    (define-key kmap (kbd "<backtab>") 'pdf-form-previous-field)
    kmap))

;;;###autoload
(define-minor-mode pdf-form-minor-mode
  "Fill in PDF forms.

If this mode is enabled, the fields of a form may be filled in by
clicking on them, by moving from field to field with
\\[pdf-form-next-field] and \\[pdf-form-previous-field], or by pressing \\[pdf-form-fill-field] and
selecting one of the visible fields.  Save the buffer to keep the
values.  Clicking on a signature field shows whether its
signature is valid, or offers to sign it if it is empty, see
`pdf-form-sign'.

The fields you can interact with are marked on the page;
\\[pdf-form-show-fields] turns the marks off and on.

\\{pdf-form-minor-mode-map}"
  :group 'pdf-form
  (pdf-util-assert-pdf-buffer)
  (cond
   ((and pdf-form-minor-mode
         (not (memq 'form-fields (pdf-info-features))))
    (setq pdf-form-minor-mode nil)
    (message "Filling in forms is not supported by this version of epdfinfo"))
   (pdf-form-minor-mode
    (pdf-view-add-hotspot-function 'pdf-form-hotspots-function 5)
    ;; Never removed when a buffer turns the mode off: the advice
    ;; applies to every buffer, while `pdf-form-minor-mode' below says
    ;; which ones mark their fields.
    (advice-add 'pdf-view-create-page :around #'pdf-form--create-page))
   (t
    (pdf-view-remove-hotspot-function 'pdf-form-hotspots-function)))
  (pdf-view-redisplay t))

(defun pdf-form-fillable-p (field)
  "Return non-nil, if the value of FIELD may be changed."
  (let-alist field
    (and (not .read-only)
         (memq .type '(text multiline password file-select
                       checkbox radio combo list)))))

(defun pdf-form-signature-p (field)
  "Return non-nil, if FIELD is a signature field."
  (eq (alist-get 'type field) 'signature))

(defun pdf-form-reset-button-p (field)
  "Return non-nil, if FIELD is a push button that resets the form."
  (and (eq (alist-get 'type field) 'push)
       (memq 'reset (alist-get 'flags field))))

(defun pdf-form-actionable-p (field)
  "Return non-nil, if FIELD may be filled in, pressed or validated."
  (or (pdf-form-fillable-p field)
      (and (pdf-form-reset-button-p field)
           (not (alist-get 'read-only field)))
      (and (pdf-form-signature-p field)
           (pdf-info-signatures-p))))

(defun pdf-form-label (field)
  "Return the name to show for FIELD: its tooltip, else its name."
  (or (alist-get 'tooltip field) (alist-get 'name field)))

(defun pdf-form--actionable-fields (page)
  "Return the fields on PAGE you can interact with, in document order.

Leave out invisible fields, e.g. most signatures, which have no
area."
  (cl-remove-if-not
   (lambda (f)
     (let ((e (alist-get 'edges f)))
       (and (pdf-form-actionable-p f)
            (< (nth 0 e) (nth 2 e))
            (< (nth 1 e) (nth 3 e)))))
   (pdf-cache-formfields page)))

(defun pdf-form-hotspots-function (page size)
  "Create hotspots for the form fields on PAGE using SIZE."
  (let (hotspots)
    (dolist (f (pdf-form--actionable-fields page))
      (let* ((e (pdf-util-scale (alist-get 'edges f) size 'round))
             (id (intern (format "form-field-%d" (alist-get 'id f)))))
        (push `((rect . ((,(nth 0 e) . ,(nth 1 e))
                         . (,(nth 2 e) . ,(nth 3 e))))
                ,id
                (pointer hand help-echo ,(pdf-form-field-to-string f)))
              hotspots)
        (local-set-key
         (vector id 'mouse-1)
         (lambda nil
           (interactive "@")
           (pdf-form-edit-field f)))
        (local-set-key
         (vector id t)
         'pdf-util-image-map-mouse-event-proxy)))
    (nreverse hotspots)))


;; * ================================================================== *
;; * Marking fields
;; * ================================================================== *

(defun pdf-form--field-edges (page)
  "Return the edges of the fields on PAGE you can interact with."
  (mapcar (lambda (f) (alist-get 'edges f))
          (pdf-form--actionable-fields page)))

(defun pdf-form--create-image (page size window &rest groups)
  "Create an image of PAGE at SIZE for WINDOW, with GROUPS highlighted.

SIZE is the image's size in pixel, as from `pdf-view-image-size'.
Each of GROUPS is a list of a background and a foreground color, an
opacity and the edges to highlight with them, see
`pdf-info-renderpage-highlight'.  The fields on PAGE you can
interact with are marked as well, when `pdf-form-show-fields' is
non-nil and the page has any."
  (let ((marks (and pdf-form-show-fields
                    (pdf-form--field-edges page))))
    (pdf-view-create-image
        (apply #'pdf-cache-renderpage-highlight
               page (car size)
               (if marks
                   (cons `(,@pdf-form-show-fields-color ,@marks) groups)
                 groups))
      :width (car size)
      :rotation (or pdf-view--current-rotation 0)
      :map (pdf-view-apply-hotspot-functions window page size)
      :pointer 'arrow)))

(defun pdf-form--create-page (create-page page &optional window)
  "CREATE-PAGE is `pdf-view-create-page', applied to PAGE and WINDOW.

Mark the fields on PAGE you can interact with first, when
`pdf-form-minor-mode' and `pdf-form-show-fields' are non-nil."
  (if (not (and pdf-form-minor-mode pdf-form-show-fields))
      (funcall create-page page window)
    (if (not (pdf-form--field-edges page))
        (funcall create-page page window)
      (let ((size (pdf-view-desired-image-size page window)))
        (pdf-form--create-image page size window)))))

(defun pdf-form-show-fields (&optional arg)
  "Toggle marking the fields you can interact with on displayed pages.

With a prefix argument, turn the marks on when it is positive and
off otherwise.  See `pdf-form-show-fields' for what the marks are."
  (interactive (list current-prefix-arg))
  (pdf-util-assert-pdf-buffer)
  (setq-local pdf-form-show-fields
              (if (null arg)
                  (not pdf-form-show-fields)
                (> (prefix-numeric-value arg) 0)))
  (pdf-view-redisplay t))


;; * ================================================================== *
;; * Finding fields
;; * ================================================================== *

(defvar-local pdf-form--current-field nil
  "The field last filled in, as a cons (PAGE . ID).")

(defun pdf-form-visible-pages (&optional window)
  "Return the pages displayed in WINDOW."
  (if pdf-view-roll-minor-mode
      (sort (copy-sequence (image-mode-window-get 'displayed-pages window))
            #'<)
    (list (pdf-view-current-page window))))

(defun pdf-form-fields (pages &optional predicate)
  "Return the fields on PAGES satisfying PREDICATE, in document order."
  (cl-remove-if-not (or predicate #'identity)
                    (apply #'append (mapcar #'pdf-cache-formfields pages))))

(defun pdf-form-field-index (field fields)
  "Return the position of FIELD in FIELDS."
  (cl-position (alist-get 'id field) fields
               :key (lambda (f) (alist-get 'id f))))


;; * ================================================================== *
;; * Editing fields
;; * ================================================================== *

(defun pdf-form-field-to-string (field)
  "Return a string describing FIELD and its value."
  (let-alist field
    (format "%s: %s" (pdf-form-label field)
            (cl-case .type
              (checkbox (if .value "on" "off"))
              ;; The buttons of a group share its name, number them.
              (radio
               (let ((group (cl-remove-if-not
                             (lambda (f)
                               (and (eq (alist-get 'type f) 'radio)
                                    (equal (alist-get 'name f) .name)))
                             (pdf-cache-formfields .page))))
                 (format "option %d of %d%s"
                         (1+ (cl-position .id (sort (mapcar (lambda (f)
                                                              (alist-get 'id f))
                                                            group)
                                                    #'<)))
                         (length group)
                         (if .value " (selected)" ""))))
              ((combo list)
               (if (and (null .value) (> (length .text) 0))
                   .text
                 (mapconcat (lambda (i) (nth i .items)) .value ", ")))
              (password (make-string (length .value) ?*))
              (push "button")
              (signature "signature")
              (t .value)))))

(defvar pdf-form-multiline-map
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap minibuffer-local-map)
    (define-key kmap (kbd "RET") 'newline)
    (define-key kmap (kbd "C-c C-c") 'exit-minibuffer)
    kmap)
  "Keymap for reading the value of a multiline field.")

(defun pdf-form-read-value (field)
  "Read a new value for FIELD from the user."
  (let-alist field
    (let ((prompt (concat (pdf-form-label field)
                          (and .max-length
                               (format " (at most %d characters)" .max-length))
                          ": ")))
      (cl-case .type
        (text (pdf-form--check-length
               field (read-string prompt .value)))
        (multiline
         (pdf-form--check-length
          field
          (read-from-minibuffer
           (format "%s (C-c C-c to finish): " (string-remove-suffix ": " prompt))
           .value pdf-form-multiline-map)))
        (password (pdf-form--check-length field (read-passwd prompt)))
        (file-select (read-file-name prompt nil .value))
        (checkbox (not .value))
        (radio t)
        ((combo list)
         (cond
          ((memq 'multiple .flags)
           (mapcar (lambda (item) (cl-position item .items :test #'equal))
                   (completing-read-multiple
                    prompt .items nil t
                    (mapconcat (lambda (i) (nth i .items)) .value ","))))
          (t
           (let* ((editable (memq 'editable .flags))
                  (item (completing-read
                         prompt .items nil (not editable) nil nil
                         (if .value (nth (car .value) .items) .text)))
                  (index (cl-position item .items :test #'equal)))
             ;; An editable field takes text that is not one of its items.
             (if index (list index) item)))))))))

(defun pdf-form--check-length (field text)
  "Return TEXT, or signal an error if it is too long for FIELD."
  (let ((max (alist-get 'max-length field)))
    (when (and max (> (length text) max))
      (user-error "`%s' takes at most %d characters"
                  (pdf-form-label field) max))
    text))

(defun pdf-form-show-field (field &optional window)
  "Turn to FIELD's page in WINDOW and highlight it."
  (save-selected-window
    (when window (select-window window 'norecord))
    (pdf-util-assert-pdf-window)
    (let ((page (alist-get 'page field)))
      (unless (memq page (pdf-form-visible-pages))
        (pdf-view-goto-page page))
      (let ((size (pdf-view-image-size))
            (edges (alist-get 'edges field)))
        (pdf-view-display-image
          (pdf-form--create-image page size (selected-window)
                                  `(,@pdf-form-highlight-color ,edges))
          (when pdf-view-roll-minor-mode page))
        (pdf-util-scroll-to-edges
         (pdf-util-scale-relative-to-pixel edges))))))

(defun pdf-form-edit-field (field)
  "Read a new value for FIELD and set it.

Show the validity of a signature field's signature instead, or
offer to sign it if it is empty."
  (cond
   ((pdf-form-signature-p field)
    ;; Signing an empty field puts a new one on top of it.
    (let ((fields (pdf-form--signatures-at field)))
      (if (and (pdf-info-signing-p)
               (cl-every #'pdf-form-unsigned-p fields)
               (y-or-n-p (format "`%s' is not signed; sign it? "
                                 (pdf-form-label field))))
          (pdf-form-sign field)
        (pdf-form-describe-signatures fields))))
   ((pdf-form-reset-button-p field)
    (when (y-or-n-p (format "Press `%s' and reset the form? "
                            (pdf-form-label field)))
      (pdf-info-resetform (alist-get 'id field))
      (pdf-form--changed)))
   ((not (pdf-form-fillable-p field))
    (user-error "Field `%s' can't be filled in" (pdf-form-label field)))
   (t
    (setq pdf-form--current-field
          (cons (alist-get 'page field) (alist-get 'id field)))
    (let (done)
      (unwind-protect
          (progn
            (when (memq (alist-get 'type field)
                        '(text multiline password file-select combo list))
              (pdf-form-show-field field))
            (pdf-form-set-field field (pdf-form-read-value field))
            (setq done t))
        ;; Remove the highlight, if the user quit.
        (unless done
          (pdf-view-redisplay-pages (alist-get 'page field))))))))

(defun pdf-form-set-field (field value)
  "Set FIELD to VALUE and redisplay its page.

VALUE has the form described in `pdf-info-formfields'.  A string
sets the text of an editable combo field."
  (pdf-form--set field value)
  (pdf-form--changed (list (alist-get 'page field))))

(defun pdf-form--set (field value)
  "Set FIELD to VALUE in the server, see `pdf-form-set-field'."
  (let ((id (alist-get 'id field)))
    (if (and (stringp value)
             (memq (alist-get 'type field) '(combo list)))
        (pdf-info-setformfield-text id value)
      (pdf-info-setformfield id value))))

(defun pdf-form--changed (&optional pages)
  "Mark the buffer modified and redisplay PAGES, or all pages."
  (set-buffer-modified-p t)
  (cond
   (pages
    (apply #'pdf-cache-clear-data-of-pages pages)
    (apply #'pdf-cache-clear-images-of-pages pages)
    (apply #'pdf-view-redisplay-pages pages))
   (t
    (pdf-cache-clear-data)
    (pdf-cache-clear-images)
    (pdf-view-redisplay t))))

(defun pdf-form-reset ()
  "Reset all fields of the form to their default values."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (when (y-or-n-p "Reset all fields of the form? ")
    (pdf-info-resetform)
    (pdf-form--changed)))

(defun pdf-form-fill-field (field)
  "Fill in FIELD, one of the fields on the displayed pages.

Interactively, read FIELD by name."
  (interactive
   (let* ((fields (pdf-form-fields (pdf-form-visible-pages)
                                   #'pdf-form-actionable-p))
          (choices (mapcar (lambda (f)
                             (cons (pdf-form-field-to-string f) f))
                           fields)))
     (unless fields
       (user-error "No fields to fill in on this page"))
     (list (cdr (assoc (completing-read "Field: " choices nil t)
                       choices)))))
  (pdf-form-edit-field field))

(defun pdf-form-next-field (&optional n)
  "Fill in the N'th next field of the document.

Start from the field last filled in, if it is visible, otherwise
from the top of the displayed pages, or their bottom if N is
negative.  Wrap around at the end of the document."
  (interactive "p")
  (let* ((fields (pdf-form-fields
                  (number-sequence 1 (pdf-cache-number-of-pages))
                  #'pdf-form-fillable-p))
         (visible (pdf-form-visible-pages))
         (n (or n 1))
         (start
          (or (and pdf-form--current-field
                   (memq (car pdf-form--current-field) visible)
                   (cl-position (cdr pdf-form--current-field) fields
                                :key (lambda (f) (alist-get 'id f))))
              ;; Pretend to be just before the first visible field,
              ;; or just after the last one.
              (let ((after (or (cl-position-if
                                (lambda (f)
                                  (if (> n 0)
                                      (>= (alist-get 'page f) (car visible))
                                    (> (alist-get 'page f) (car (last visible)))))
                                fields)
                               (length fields))))
                (if (> n 0) (1- after) after)))))
    (unless fields
      (user-error "No fields to fill in"))
    (pdf-form-edit-field (nth (mod (+ start n) (length fields)) fields))))

(defun pdf-form-previous-field (&optional n)
  "Fill in the N'th previous field of the document.

See `pdf-form-next-field'."
  (interactive "p")
  (pdf-form-next-field (- (or n 1))))


;; * ================================================================== *
;; * Values from and for Lisp
;; * ================================================================== *

(defun pdf-form--named-fields ()
  "Return the fillable fields of the document, grouped by name.

The result is an alist (NAME . WIDGETS) in document order.  The
buttons of a radio group, and widgets showing the same field, share
a name."
  (let (groups)
    (dolist (f (pdf-form-fields
                (number-sequence 1 (pdf-cache-number-of-pages))
                (lambda (f)
                  (memq (alist-get 'type f)
                        '(text multiline password file-select
                          checkbox radio combo list)))))
      (let ((group (assoc (alist-get 'name f) groups)))
        (if group
            (setcdr group (append (cdr group) (list f)))
          (push (list (alist-get 'name f) f) groups))))
    (nreverse groups)))

(defun pdf-form--radio-buttons (widgets)
  "Return the radio buttons among WIDGETS, sorted by id."
  (sort (cl-remove-if-not (lambda (f) (eq (alist-get 'type f) 'radio))
                          (copy-sequence widgets))
        (lambda (a b) (< (alist-get 'id a) (alist-get 'id b)))))

(defun pdf-form--lisp-value (widgets)
  "Return the value of the field shown by WIDGETS, for Lisp."
  (let-alist (car widgets)
    (cl-case .type
      (radio
       (let ((n (cl-position-if (lambda (f) (alist-get 'value f))
                                (pdf-form--radio-buttons widgets))))
         (and n (1+ n))))
      ((combo list)
       (let ((items (mapcar (lambda (i) (nth i .items)) .value)))
         (cond
          ((memq 'multiple .flags) items)
          (items (car items))
          ((> (length .text) 0) .text))))
      (t .value))))

(defun pdf-form-values ()
  "Return the values of the form in the current buffer.

The result is an alist (NAME . VALUE) in document order, where NAME
is a field's fully qualified name and VALUE is

- a string for a text field,
- t or nil for a checkbox,
- for a radio group, the number of the selected button, counting
  from 1 in the order of `pdf-info-formfields', or nil,
- for a combo box or list, the selected item, or the text typed into
  an editable combo box, or nil; a list of the selected items if
  more than one may be selected.

Push buttons and signatures are left out.  `pdf-form-fill' takes
the same values."
  (pdf-util-assert-pdf-buffer)
  (mapcar (lambda (group)
            (cons (car group) (pdf-form--lisp-value (cdr group))))
          (pdf-form--named-fields)))

(defun pdf-form--server-value (widgets value)
  "Return the widget and value to set to give WIDGETS VALUE.

The result is a cons (WIDGET . SERVER-VALUE), see
`pdf-form-set-field', or nil if there is nothing to set.  Signal an
error if VALUE does not fit."
  (let* ((f (car widgets))
         (name (alist-get 'name f)))
    (if (alist-get 'read-only f)
        ;; Accept its own value, so that `pdf-form-values' round-trips.
        (unless (equal value (pdf-form--lisp-value widgets))
          (error "Form field `%s' is read-only" name))
      (pdf-form--server-value-1 widgets value))))

(defun pdf-form--server-value-1 (widgets value)
  "Like `pdf-form--server-value' for a field that is not read-only."
  (let* ((f (car widgets))
         (name (alist-get 'name f)))
    (let-alist f
      (cl-case .type
        ((text multiline password file-select)
         (unless (stringp value)
           (error "Form field `%s' takes a string: %S" name value))
         (when (and .max-length (> (length value) .max-length))
           (error "Form field `%s' takes at most %d characters" name .max-length))
         (cons f value))
        (checkbox (cons f (and value t)))
        (radio
         (let ((buttons (pdf-form--radio-buttons widgets)))
           (cond
            ((null value)
             (cons (or (cl-find-if (lambda (b) (alist-get 'value b)) buttons)
                       (car buttons))
                   nil))
            ((and (natnump value) (<= 1 value (length buttons)))
             (cons (nth (1- value) buttons) t))
            (t (error "Form field `%s' takes a button number from 1 to %d: %S"
                      name (length buttons) value)))))
        ((combo list)
         (let ((values (if (listp value) value (list value))))
           (when (and (cdr values) (not (memq 'multiple .flags)))
             (error "Form field `%s' takes one item: %S" name value))
           (cond
            ((cl-every (lambda (v) (member v .items)) values)
             (cons f (mapcar (lambda (v) (cl-position v .items :test #'equal))
                             values)))
            ((and (stringp value) (memq 'editable .flags))
             (cons f value))
            (t (error "Form field `%s' has no item %S; it has %S"
                      name value .items)))))))))

(defun pdf-form-fill (values)
  "Fill in the form in the current buffer with VALUES.

VALUES is an alist (NAME . VALUE) as returned by `pdf-form-values';
fields it does not name keep their values.  All VALUES are checked
before any field is changed, so an error leaves the form as it was."
  (pdf-util-assert-pdf-buffer)
  (let* ((groups (pdf-form--named-fields))
         (settings
          (mapcar (lambda (nv)
                    (let ((group (assoc (car nv) groups)))
                      (unless group
                        (error "No fillable form field named `%s'" (car nv)))
                      (pdf-form--server-value (cdr group) (cdr nv))))
                  values)))
    (setq settings (delq nil settings))
    (dolist (setting settings)
      (pdf-form--set (car setting) (cdr setting)))
    (when settings
      (pdf-form--changed
       (delete-dups (mapcar (lambda (setting) (alist-get 'page (car setting)))
                            settings))))))

;; * ================================================================== *
;; * Signatures
;; * ================================================================== *

(defun pdf-form-format-time (time)
  "Format the ISO 8601 string TIME for display."
  (replace-regexp-in-string "T" " " time))

(defun pdf-form-describe-signatures (fields)
  "Show whether the signatures of FIELDS are valid."
  (let ((buffer (current-buffer)))
    (with-help-window "*PDF Signatures*"
      (dolist (f fields)
        (let-alist (with-current-buffer buffer
                     (pdf-info-validatesignature (alist-get 'id f)))
          (princ (format "%s (page %d)\n" (alist-get 'name f)
                         (alist-get 'page f)))
          (princ (format "  Signature:   %s\n"
                         (cl-case .signature-status
                           (valid "valid")
                           (invalid "invalid")
                           (digest-mismatch
                            "INVALID, the document changed after signing")
                           (decoding-error "could not be decoded")
                           (not-found "unsigned")
                           (t "could not be checked"))))
          (unless (eq .signature-status 'not-found)
            (princ (format "  Certificate: %s\n"
                           (cl-case .certificate-status
                             (trusted "trusted")
                             (untrusted-issuer "issuer is not trusted")
                             (unknown-issuer "issuer is unknown")
                             (revoked "REVOKED")
                             (expired "expired")
                             (not-verified "not verified")
                             (t "could not be checked"))))
            (dolist (line `(("Signer" ,.signer)
                            ("Signed" ,(and .signing-time
                                            (pdf-form-format-time .signing-time)))
                            ("Subject" ,(string-join
                                         (delq nil (list .subject
                                                         .subject-organization))
                                         ", "))
                            ("Issuer" ,(string-join
                                        (delq nil (list .issuer
                                                        .issuer-organization))
                                        ", "))
                            ("Expires" ,(and .expires
                                             (pdf-form-format-time .expires)))))
              (when (> (length (cadr line)) 0)
                (princ (format "  %-12s %s\n" (concat (car line) ":")
                               (cadr line))))))
          (terpri))))))

(defun pdf-form-signatures ()
  "Show whether the signatures of the document are valid."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (unless (pdf-info-signatures-p)
    (user-error "Validating signatures is not supported by this version of epdfinfo"))
  (let ((fields (pdf-form-fields
                 (number-sequence 1 (pdf-cache-number-of-pages))
                 #'pdf-form-signature-p)))
    (unless fields
      (user-error "This document has no signature fields"))
    (pdf-form-describe-signatures fields)))

(defun pdf-form-unsigned-p (field)
  "Return non-nil, if FIELD is a signature field nobody signed."
  (and (pdf-form-signature-p field)
       (eq (alist-get 'signature-status
                      (pdf-info-validatesignature (alist-get 'id field)))
           'not-found)))

(defun pdf-form--signatures-at (field)
  "Return the signature fields in the same place as FIELD, FIELD first."
  (cons field
        (cl-remove-if-not
         (lambda (f)
           (and (pdf-form-signature-p f)
                (not (eql (alist-get 'id f) (alist-get 'id field)))
                (equal (alist-get 'edges f) (alist-get 'edges field))))
         (pdf-cache-formfields (alist-get 'page field)))))

(defun pdf-form-unsigned-fields ()
  "Return the signature fields of the document nobody signed.

Leave out those under a signed field, which is where signing them
put the signature."
  (cl-remove-if-not
   (lambda (f) (cl-every #'pdf-form-unsigned-p (pdf-form--signatures-at f)))
   (pdf-form-fields (number-sequence 1 (pdf-cache-number-of-pages))
                    #'pdf-form-signature-p)))


;; * ================================================================== *
;; * Signing
;; * ================================================================== *

(defun pdf-form--assert-file ()
  "Check that the visited file can be signed and return its true name.

The buffer must visit a regular, local, uncompressed PDF file,
which must be writable, as must be its directory."
  (pdf-util-assert-pdf-buffer)
  (when (derived-mode-p 'pdf-virtual-view-mode)
    (user-error "Virtual PDF buffers can't be signed"))
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer `%s' is not visiting a file" (buffer-name)))
    (when (or (file-remote-p file)
              pdf-view--buffer-file-name
              (not (file-regular-p file)))
      (user-error "`%s' is not a local PDF file (remote, compressed or in \
an archive); signing needs a plain file on disk"
                  (abbreviate-file-name file)))
    (setq file (file-truename file))
    (unless (and (file-writable-p file)
                 (file-writable-p (file-name-directory file)))
      (user-error "`%s' or its directory is not writable"
                  (abbreviate-file-name file)))
    file))

(defun pdf-form--assert-saved ()
  "Make sure the buffer has no unsaved changes and matches its file.

The signature covers the file on disk, so offer to save unsaved
changes (form fields, annotations) first, and signal an error if
the user declines."
  (when (buffer-modified-p)
    (if (y-or-n-p (format "`%s' has unsaved changes (form fields or \
annotations); save them before signing? " (buffer-name)))
        (save-buffer)
      (user-error "Not signing: save or revert the buffer first")))
  (unless (verify-visited-file-modtime (current-buffer))
    (user-error "`%s' changed on disk; revert the buffer first (%s)"
                (buffer-name)
                (substitute-command-keys "\\[revert-buffer]"))))

(defun pdf-form--signing-certificates ()
  "Return the signing certificates and the NSS password they needed.

The result is a cons (CERTIFICATES . PASSWORD), see
`pdf-info-signingcertificates'.  Read the password, if the NSS
database has one."
  (let (password certificates (tries 0))
    (while (null certificates)
      (condition-case err
          (setq certificates (list (pdf-info-signingcertificates password)))
        (error
         (let ((msg (error-message-string err)))
           (cond
            ((and (string-match-p "NSS password required\\|Wrong NSS password" msg)
                  (< tries 3))
             (cl-incf tries)
             (setq password
                   (read-passwd
                    (if (string-match-p "Wrong" msg)
                        "Wrong password, try again: "
                      "Password of the NSS certificate database: "))))
            (t (signal (car err) (cdr err))))))))
    (cons (car certificates) password)))

(defun pdf-form--certificate-to-string (certificate)
  "Return a string describing CERTIFICATE for the user."
  (let-alist certificate
    (concat (or .subject .id)
            (and .subject-email (format " <%s>" .subject-email))
            (and .subject-organization (format ", %s" .subject-organization))
            (format " (%s" .id)
            (and .issuer (not (equal .issuer .subject))
                 (format ", issued by %s" .issuer))
            (and .expires (format ", expires %s"
                                  (substring (pdf-form-format-time .expires)
                                             0 (min 10 (length .expires)))))
            ")")))

(defun pdf-form-read-certificate (certificates)
  "Let the user choose one of the signing CERTIFICATES."
  (unless certificates
    (user-error "There are no signing certificates in the NSS database %s"
                (if pdf-info-nss-directory
                    (abbreviate-file-name pdf-info-nss-directory)
                  "(poppler's default, see `pdf-info-nss-directory')")))
  (let* ((choices (mapcar (lambda (c)
                            (cons (pdf-form--certificate-to-string c) c))
                          certificates))
         (default (caar choices)))
    (cdr (assoc (completing-read (format "Sign with certificate (default %s): "
                                         default)
                                 choices nil t nil nil default)
                choices))))

(defun pdf-form--read-corner (prompt)
  "Read a corner of a rectangle with the mouse using PROMPT.

Return a cons (PAGE . (X . Y)), X and Y relative to the page."
  (let* ((posn (pdf-util-read-image-position prompt))
         (window (posn-window posn))
         (page (if pdf-view-roll-minor-mode
                   (1+ (/ (posn-point posn) 4))
                 (pdf-view-current-page window)))
         (size (with-selected-window window (pdf-view-image-size)))
         (xy (posn-object-x-y posn)))
    (cons page (cons (/ (float (car xy)) (car size))
                     (/ (float (cdr xy)) (cdr size))))))

(defun pdf-form-read-rectangle ()
  "Read a rectangle of a page for a new signature.

Use the active region, if there is one, otherwise let the user click
on two opposite corners.  Return a cons (PAGE . EDGES), EDGES
relative to the page."
  (if (pdf-view-active-region-p)
      (let ((region (pdf-view-active-region t)))
        (cons (car region)
              (apply #'pdf-util-edges-union
                     (mapcar (lambda (e)
                               (pdf-util-with-edges (e)
                                 (list (min e-left e-right) (min e-top e-bot)
                                       (max e-left e-right) (max e-top e-bot))))
                             (cdr region)))))
    (let* ((a (pdf-form--read-corner
               "Click on a corner of the new signature (or select a region first)"))
           (b (pdf-form--read-corner "Click on the opposite corner")))
      (unless (eql (car a) (car b))
        (user-error "The corners must be on the same page"))
      (let ((x1 (cadr a)) (y1 (cddr a)) (x2 (cadr b)) (y2 (cddr b)))
        (cons (car a) (list (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)))))))

(defun pdf-form--read-signature-place ()
  "Read where to sign: an empty signature field or a rectangle.

Return a field, or a cons (PAGE . EDGES)."
  (let ((fields (and (not (pdf-view-active-region-p))
                     (pdf-form-unsigned-fields)))
        (new "A new field in a rectangle of the page"))
    (if (null fields)
        (pdf-form-read-rectangle)
      (let* ((choices
              (append (mapcar (lambda (f)
                                (cons (format "%s (page %d)" (pdf-form-label f)
                                              (alist-get 'page f))
                                      f))
                              fields)
                      (list (cons new nil))))
             (choice (completing-read
                      (format "Sign (default %s): " (caar choices))
                      choices nil t nil nil (caar choices))))
        (or (cdr (assoc choice choices))
            (pdf-form-read-rectangle))))))

(defun pdf-form--new-field-name (base)
  "Return a field name starting with BASE that the document lacks."
  (let ((names (mapcar (lambda (f) (alist-get 'name f))
                       (pdf-form-fields
                        (number-sequence 1 (pdf-cache-number-of-pages)))))
        (name base)
        (n 1))
    (while (member name names)
      (setq name (format "%s%d" base (cl-incf n))))
    name))

(defun pdf-form-signature-text (certificate &optional reason location)
  "Return the text shown in a signature with CERTIFICATE.

REASON and LOCATION are the signature's, or nil."
  (let ((signer (or (alist-get 'subject certificate)
                    (alist-get 'id certificate))))
    (concat (format "Digitally signed by %s\nDate: %s" signer
                    (format-time-string "%Y-%m-%d %H:%M:%S %z"))
            (and reason (format "\nReason: %s" reason))
            (and location (format "\nLocation: %s" location)))))

(defvar pdf-form--backup-session-directory nil
  "Temporary directory of this session's backups, if one was created.")

(defun pdf-form--backup-directory ()
  "Return the directory for backups, creating it if needed."
  (if pdf-form-backup-directory
      (let ((dir (expand-file-name pdf-form-backup-directory)))
        (make-directory dir t)
        dir)
    (unless (and pdf-form--backup-session-directory
                 (file-directory-p pdf-form--backup-session-directory))
      (setq pdf-form--backup-session-directory
            (make-temp-file "pdf-form-backup-" t))
      (add-hook 'kill-emacs-hook #'pdf-form--delete-backup-session-directory))
    pdf-form--backup-session-directory))

(defun pdf-form--delete-backup-session-directory ()
  "Delete this session's backup directory."
  (when (and pdf-form--backup-session-directory
             (file-directory-p pdf-form--backup-session-directory))
    (delete-directory pdf-form--backup-session-directory t)))

(defun pdf-form--make-backup (file)
  "Copy FILE to the backup directory and return the copy's name."
  (let ((backup (make-temp-file
                 (expand-file-name (concat (file-name-base file) "-unsigned-")
                                   (pdf-form--backup-directory))
                 nil ".pdf")))
    (copy-file file backup t t)
    backup))

(defun pdf-form--replace-file (file signed)
  "Replace FILE by SIGNED, keeping a backup, and revert the buffer.

Return the backup's name."
  (let ((backup (pdf-form--make-backup file))
        (page (pdf-view-current-page))
        done)
    (unwind-protect
        (progn
          (set-file-modes signed (file-modes file))
          (when (pdf-info-running-p)
            (ignore-errors (pdf-info-close)))
          (rename-file signed file t)
          (setq done t))
      (unless done
        (delete-file backup)))
    (pdf-view-revert-buffer nil t)
    (when page
      (pdf-view-goto-page page))
    backup))

(defun pdf-form-sign (place &optional certificate reason location)
  "Sign the document at PLACE with CERTIFICATE.

PLACE is an empty signature field or a cons (PAGE . EDGES) of a
rectangle of relative EDGES.  Interactively, choose one of the
empty fields of the document, or a new field in the active region
or in a rectangle selected with the mouse.  Poppler can only add
new signature fields, so signing an empty field puts a new one
with the signature on top of it.

CERTIFICATE is the id of one of `pdf-info-signingcertificates'.
If it is nil, read it from the user, as well as REASON and
LOCATION, which are optional.  Read the password of the NSS
database `pdf-info-nss-directory', if it has one.

Unsaved changes have to be saved first, since the signature covers
the file on disk.  The signed document replaces the visited file
and the buffer is reverted; a copy of the unsigned version is kept
in `pdf-form-backup-directory'."
  (interactive
   (progn
     (pdf-form--prepare-signing)
     (list (pdf-form--read-signature-place))))
  (pdf-form--prepare-signing)
  (let* ((file (pdf-form--assert-file))
         (field (and (pdf-form-signature-p place) place))
         (page (if field (alist-get 'page field) (car place)))
         (edges (if field (alist-get 'edges field) (cdr place)))
         (certs+password (pdf-form--signing-certificates))
         (password (cdr certs+password))
         (cert (if certificate
                   (or (cl-find certificate (car certs+password)
                                :key (lambda (c) (alist-get 'id c))
                                :test #'equal)
                       (error "No signing certificate %s" certificate))
                 (pdf-form-read-certificate (car certs+password))))
         (reason (if certificate
                     reason
                   (read-string "Reason for signing (optional): ")))
         (location (if certificate
                       location
                     (read-string "Location (optional): ")))
         (name (pdf-form--new-field-name
                (if field
                    (concat (alist-get 'name field) "-signed")
                  "Signature")))
         (temp (make-temp-file (expand-file-name
                                (concat "." (file-name-base file) "-pdf-form-")
                                (file-name-directory file))
                               nil ".pdf"))
         backup)
    (when (equal reason "") (setq reason nil))
    (when (equal location "") (setq location nil))
    (unwind-protect
        (progn
          (pdf-info-sign page edges (alist-get 'id cert) name temp
                         reason location
                         (pdf-form-signature-text cert reason location)
                         (or (alist-get 'subject cert) (alist-get 'id cert))
                         password)
          (unless (> (or (file-attribute-size (file-attributes temp)) 0) 0)
            (error "Signing produced no file; `%s' unchanged"
                   (abbreviate-file-name file)))
          (setq backup (pdf-form--replace-file file temp)))
      (when (file-exists-p temp)
        (delete-file temp)))
    (message "Signed `%s' as %s; the unsigned version is %s"
             (file-name-nondirectory file)
             (or (alist-get 'subject cert) (alist-get 'id cert))
             (abbreviate-file-name backup))))

(defcustom pdf-form-signature-image nil
  "The PNG file with an image of your signature.
`pdf-form-sign-with-image' places it on the page and flattens it
into the document.  If nil, the command reads the file name every
time."
  :type '(choice (const :tag "Ask for the file every time" nil)
                 (file :tag "Image file")))

(defun pdf-form--read-signature-image ()
  "Read the name of a PNG file with a signature image."
  (read-file-name "Signature image (PNG file): "))

(defun pdf-form-sign-with-image (place &optional image)
  "Sign the document at PLACE with an image of your signature.

PLACE is an empty signature field or a cons (PAGE . EDGES) of a
rectangle of relative EDGES, as in `pdf-form-sign'.  Interactively,
choose one of the empty fields of the document, or a new place in
the active region or in a rectangle selected with the mouse.

The signature is a stamp annotation showing IMAGE, a PNG file
(`pdf-form-signature-image' by default, read from the user
otherwise), that fills PLACE and keeps the aspect ratio of the
image.  It is flattened into the page right away, so that the
signed document looks the same in every PDF viewer; the previous
version is kept for `pdf-undo'.

This is a signature in the everyday sense, a picture of your
handwritten name, not a digital one: unlike `pdf-form-sign', it
does not certify who signed.  It needs the epdfinfo server to
support image stamps and the qpdf program, version 11.8 or
later, see `pdf-pages-qpdf-program'."
  (interactive
   (progn
     (pdf-util-assert-pdf-buffer)
     (unless (pdf-info-image-stamps-p)
       (user-error "Image stamps are not supported by this version of \
epdfinfo (requires poppler-glib >= 22.07 and PNG support in cairo)"))
     (pdf-form--assert-file)
     (list (pdf-form--read-signature-place)
           (or pdf-form-signature-image
               (pdf-form--read-signature-image)))))
  (unless (pdf-info-image-stamps-p)
    (user-error "Image stamps are not supported by this version of epdfinfo"))
  (let ((file (pdf-form--assert-file))
        (image (or image pdf-form-signature-image)))
    (unless image
      (user-error "No signature image given"))
    (setq image (expand-file-name image))
    ;; Check that flattening can run before changing anything.
    (unless (pdf-pages--qpdf-version-at-least '(11 8))
      (user-error "Signing with an image needs the qpdf program version \
11.8 or later, not %s; see `pdf-pages-qpdf-program'"
                  (mapconcat #'number-to-string
                             (pdf-pages--qpdf-version) ".")))
    (pdf-annot-add-image-stamp-annotation
     (if (pdf-form-signature-p place)
         (cons (alist-get 'page place)
               (list (alist-get 'edges place)))
       (cons (car place) (list (cdr place))))
     image)
    ;; Write the stamp into the file and flatten it into the page.
    (save-buffer)
    (let (pdf-pages-confirm)
      (pdf-pages-flatten-annotations))
    (message "Signed `%s' with an image; the unsigned version is kept \
for `pdf-undo'"
             (file-name-nondirectory file))))

(defun pdf-form--prepare-signing ()
  "Check that the buffer can be signed, before asking anything."
  (pdf-util-assert-pdf-buffer)
  (unless (pdf-info-signing-p)
    (user-error "Signing documents is not supported by this version of epdfinfo \
(requires poppler-glib >= 23.07)"))
  (pdf-form--assert-file)
  (pdf-form--assert-saved))

(provide 'pdf-form)

;;; pdf-form.el ends here
