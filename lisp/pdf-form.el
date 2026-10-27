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
;; Clicking on a signature field, or M-x pdf-form-signatures, shows
;; whether its signature is valid.

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


;; * ================================================================== *
;; * Minor Mode
;; * ================================================================== *

(defvar pdf-form-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-c C-e") 'pdf-form-fill-field)
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

\\{pdf-form-minor-mode-map}"
  :group 'pdf-form
  (pdf-util-assert-pdf-buffer)
  (cond
   ((and pdf-form-minor-mode
         (not (memq 'form-fields (pdf-info-features))))
    (setq pdf-form-minor-mode nil)
    (message "Filling in forms is not supported by this version of epdfinfo"))
   (pdf-form-minor-mode
    (pdf-view-add-hotspot-function 'pdf-form-hotspots-function 5))
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

(defun pdf-form-actionable-p (field)
  "Return non-nil, if FIELD may be filled in or its signature validated."
  (or (pdf-form-fillable-p field)
      (and (pdf-form-signature-p field)
           (pdf-info-signatures-p))))

(defun pdf-form-hotspots-function (page size)
  "Create hotspots for the form fields on PAGE using SIZE."
  (let (hotspots)
    (dolist (f (pdf-cache-formfields page))
      (let ((e (pdf-util-scale (alist-get 'edges f) size 'round)))
        ;; Invisible fields, e.g. most signatures, have no area.
        (when (and (pdf-form-actionable-p f)
                   (< (nth 0 e) (nth 2 e))
                   (< (nth 1 e) (nth 3 e)))
          (let ((id (intern (format "form-field-%d" (alist-get 'id f)))))
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
             'pdf-util-image-map-mouse-event-proxy)))))
    (nreverse hotspots)))


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
    (format "%s: %s" .name
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
    (let ((prompt (format "%s: " .name)))
      (cl-case .type
        (text (read-string prompt .value))
        (multiline
         (read-from-minibuffer
          (format "%s (C-c C-c to finish): " .name)
          .value pdf-form-multiline-map))
        (password (read-passwd prompt))
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
         (pdf-view-create-image
             (pdf-cache-renderpage-highlight
              page (car size)
              `(,@pdf-form-highlight-color ,edges))
           :map (pdf-view-apply-hotspot-functions
                 (selected-window) page size)
           :width (car size))
         (when pdf-view-roll-minor-mode page))
        (pdf-util-scroll-to-edges
         (pdf-util-scale-relative-to-pixel edges))))))

(defun pdf-form-edit-field (field)
  "Read a new value for FIELD and set it.

Show the validity of a signature field's signature instead."
  (cond
   ((pdf-form-signature-p field)
    (pdf-form-describe-signatures (list field)))
   ((not (pdf-form-fillable-p field))
    (user-error "Field `%s' can't be filled in" (alist-get 'name field)))
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
  (let ((page (alist-get 'page field))
        (id (alist-get 'id field)))
    (if (and (stringp value)
             (memq (alist-get 'type field) '(combo list)))
        (pdf-info-setformfield-text id value)
      (pdf-info-setformfield id value))
    (set-buffer-modified-p t)
    (pdf-cache-clear-data-of-pages page)
    (pdf-cache-clear-images-of-pages page)
    (pdf-view-redisplay-pages page)))

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
