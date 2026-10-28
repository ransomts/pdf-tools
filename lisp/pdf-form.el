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
;; From Lisp, `pdf-form-values' returns the values of a form and
;; `pdf-form-fill' fills one in, both by field name.

(require 'pdf-view)
(require 'pdf-info)
(require 'pdf-cache)
(require 'pdf-util)
(require 'let-alist)
(require 'cl-lib)
(require 'crm)
(require 'subr-x)

;;; Code:



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
signature is valid.

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

Show the validity of a signature field's signature instead."
  (cond
   ((pdf-form-signature-p field)
    (pdf-form-describe-signatures (list field)))
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

(provide 'pdf-form)

;;; pdf-form.el ends here
