;; -*- lexical-binding: t -*-

(require 'pdf-form)
(require 'ert)

(defun pdf-form-test-field (name &optional fields)
  (cl-find name (or fields (pdf-info-formfields 1))
           :key (lambda (f) (alist-get 'name f))
           :test #'equal))

(ert-deftest pdf-form-formfields ()
  (pdf-test-with-pdf "forms.pdf"
    (let ((fields (pdf-info-formfields 1)))
      (should (= 13 (length fields)))
      (should (cl-every (lambda (f)
                          (pdf-test-relative-edges-p (alist-get 'edges f)))
                        fields))
      (should (equal (mapcar (lambda (f) (alist-get 'type f)) fields)
                     '(text multiline password text checkbox
                       combo radio list combo list text push radio)))
      (let-alist (pdf-form-test-field "fixed" fields)
        (should .read-only)
        (should (equal .value "locked")))
      (let-alist (pdf-form-test-field "color" fields)
        (should (equal .items '("Red" "Green" "Blue")))
        (should (equal .value '(1)))
        (should-not .flags))
      (should (equal (alist-get 'flags (pdf-form-test-field "pet" fields))
                     '(editable)))
      (should (equal (alist-get 'flags (pdf-form-test-field "tags" fields))
                     '(multiple))))))

(ert-deftest pdf-form-setformfield ()
  (pdf-test-with-pdf "forms.pdf"
    (let ((id (lambda (name) (alist-get 'id (pdf-form-test-field name)))))
      (pdf-info-setformfield (funcall id "name") "Ada: Lovelace")
      (pdf-info-setformfield (funcall id "notes") "line1\nline2")
      (pdf-info-setformfield (funcall id "agree") t)
      (pdf-info-setformfield (funcall id "color") '(2))
      (should-error (pdf-info-setformfield (funcall id "fixed") "x"))
      (should-error (pdf-info-setformfield (funcall id "color") '(7)))
      (let ((fields (pdf-info-formfields 1)))
        (should (equal (alist-get 'value (pdf-form-test-field "name" fields))
                       "Ada: Lovelace"))
        (should (equal (alist-get 'value (pdf-form-test-field "notes" fields))
                       "line1\nline2"))
        (should (alist-get 'value (pdf-form-test-field "agree" fields)))
        (should (equal (alist-get 'value (pdf-form-test-field "color" fields))
                       '(2))))
      (pdf-info-setformfield (funcall id "agree") nil)
      (should-not (alist-get 'value (pdf-form-test-field "agree"))))))

(ert-deftest pdf-form-save ()
  (pdf-test-with-pdf "forms.pdf"
    (let ((saved (progn
                   (pdf-info-setformfield
                    (alist-get 'id (pdf-form-test-field "name")) "Ada")
                   (pdf-info-save))))
      (unwind-protect
          (should (equal (alist-get 'value
                                    (pdf-form-test-field
                                     "name" (pdf-info-formfields 1 saved)))
                         "Ada"))
        (pdf-info-close saved)
        (delete-file saved)))))

(ert-deftest pdf-form-radio-group ()
  (pdf-test-with-pdf "forms.pdf"
    (let* ((radios (cl-remove-if-not
                    (lambda (f) (eq (alist-get 'type f) 'radio))
                    (pdf-info-formfields 1)))
           (first (alist-get 'id (car radios)))
           (second (alist-get 'id (cadr radios))))
      (pdf-info-setformfield first t)
      (pdf-info-setformfield second t)
      (let ((radios (pdf-info-formfields 1)))
        (should (alist-get 'value (cl-find second radios
                                           :key (lambda (f) (alist-get 'id f)))))
        (should-not (alist-get 'value (cl-find first radios
                                               :key (lambda (f) (alist-get 'id f)))))))))

(ert-deftest pdf-form-set-field-modifies-buffer ()
  (pdf-test-with-pdf "forms.pdf"
    (let ((field (pdf-form-test-field "name"))
          (before (pdf-cache-renderpage 1 600)))
      (should-not (buffer-modified-p))
      (pdf-form-set-field field "Ada")
      (should (buffer-modified-p))
      (should-not (equal before (pdf-cache-renderpage 1 600)))
      (should (equal (alist-get 'value
                                (pdf-form-test-field
                                 "name" (pdf-cache-formfields 1)))
                     "Ada")))))

(ert-deftest pdf-form-multiple-and-editable ()
  (pdf-test-with-pdf "forms.pdf"
    (pdf-form-set-field (pdf-form-test-field "tags") '(0 2))
    (pdf-form-set-field (pdf-form-test-field "pet") "Ferret")
    (should-error (pdf-info-setformfield-text
                   (alist-get 'id (pdf-form-test-field "color")) "x"))
    (let ((fields (pdf-cache-formfields 1)))
      (should (equal (alist-get 'value (pdf-form-test-field "tags" fields))
                     '(0 2)))
      (let-alist (pdf-form-test-field "pet" fields)
        (should-not .value)
        (should (equal .text "Ferret"))
        (should (equal (pdf-form-field-to-string (pdf-form-test-field "pet" fields))
                       "pet: Ferret"))))))

(ert-deftest pdf-form-read-value ()
  (pdf-test-with-pdf "forms.pdf"
    (let ((fields (pdf-info-formfields 1)))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) '("Blue" "Red")))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _items _pred require-match &rest _)
                   (if require-match "Blue" "Ferret"))))
        (should (equal (pdf-form-read-value (pdf-form-test-field "tags" fields))
                       '(2 0)))
        (should (equal (pdf-form-read-value (pdf-form-test-field "color" fields))
                       '(2)))
        (should (equal (pdf-form-read-value (pdf-form-test-field "pet" fields))
                       "Ferret"))))))

(ert-deftest pdf-form-next-field ()
  (pdf-test-with-pdf "forms.pdf"
    (let (visited)
      (cl-letf (((symbol-function 'pdf-form-visible-pages)
                 (lambda (&rest _) '(1)))
                ((symbol-function 'pdf-form-edit-field)
                 (lambda (f)
                   (push (alist-get 'name f) visited)
                   (setq pdf-form--current-field
                         (cons (alist-get 'page f) (alist-get 'id f))))))
        (pdf-form-next-field)
        (pdf-form-next-field)
        (pdf-form-previous-field)
        (pdf-form-previous-field)
        (pdf-form-previous-field)
        ;; Without a current field, start from the bottom of the page.
        (setq pdf-form--current-field nil)
        (pdf-form-previous-field)
        ;; `fixed' is read-only.
        (setq pdf-form--current-field nil)
        (pdf-form-next-field 4))
      (should (equal (nreverse visited)
                     '("name" "notes" "name" "size" "zip" "size" "agree"))))))

(ert-deftest pdf-form-validatesignature ()
  (skip-unless (pdf-info-signatures-p))
  (pdf-test-with-pdf "signed.pdf"
    (let ((sig (cl-find 'signature (pdf-info-formfields 1)
                        :key (lambda (f) (alist-get 'type f)))))
      (should sig)
      (let-alist (pdf-info-validatesignature (alist-get 'id sig))
        (should (eq .signature-status 'valid))
        ;; The test certificate is self-signed.
        (should (memq .certificate-status '(untrusted-issuer unknown-issuer)))
        (should (equal .signer "pdf-tools test"))
        (should (equal .issuer-organization "pdf-tools"))
        (should (string-match-p "\\`[0-9]\\{4\\}-" .signing-time)))
      (should-error (pdf-info-validatesignature
                     (alist-get 'id (pdf-form-test-field "name")))))))

(ert-deftest pdf-form-tooltip-and-max-length ()
  (pdf-test-with-pdf "forms.pdf"
    (let ((fields (pdf-info-formfields 1)))
      (let-alist (pdf-form-test-field "name" fields)
        (should (equal .tooltip "Your full name"))
        (should-not .max-length))
      (should (equal (pdf-form-label (pdf-form-test-field "name" fields))
                     "Your full name"))
      (should (equal (pdf-form-label (pdf-form-test-field "notes" fields))
                     "notes"))
      (should (string-prefix-p "Your full name: "
                               (pdf-form-field-to-string
                                (pdf-form-test-field "name" fields))))
      (let ((zip (pdf-form-test-field "zip" fields)))
        (should (= (alist-get 'max-length zip) 5))
        (should (equal (pdf-form--check-length zip "29634") "29634"))
        (should-error (pdf-form--check-length zip "296340") :type 'user-error)))))

(ert-deftest pdf-form-reset ()
  (skip-unless (pdf-info-form-reset-p))
  (pdf-test-with-pdf "forms.pdf"
    (let ((reset (pdf-form-test-field "Reset")))
      (should (pdf-form-reset-button-p reset))
      (should (pdf-form-actionable-p reset))
      (should-not (pdf-form-fillable-p reset))
      ;; The button.
      (pdf-info-setformfield (alist-get 'id (pdf-form-test-field "name")) "Ada")
      (pdf-info-setformfield (alist-get 'id (pdf-form-test-field "agree")) t)
      (pdf-info-resetform (alist-get 'id reset))
      (should (equal (alist-get 'value (pdf-form-test-field "name")) ""))
      (should-not (alist-get 'value (pdf-form-test-field "agree")))
      ;; The whole form, as a command.
      (pdf-info-setformfield (alist-get 'id (pdf-form-test-field "name")) "Bob")
      (set-buffer-modified-p nil)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (pdf-form-reset))
      (should (buffer-modified-p))
      (should (equal (alist-get 'value (pdf-form-test-field "name")) ""))
      ;; Only reset buttons reset.
      (should-error (pdf-info-resetform
                     (alist-get 'id (pdf-form-test-field "name")))))))

(ert-deftest pdf-form-values ()
  (pdf-test-with-pdf "forms.pdf"
    (should (equal (pdf-form-values)
                   '(("name" . "") ("notes" . "") ("secret" . "")
                     ("fixed" . "locked") ("agree" . nil) ("color" . "Green")
                     ("size" . nil) ("fruit" . nil) ("pet" . nil)
                     ("tags" . nil) ("zip" . ""))))))

(ert-deftest pdf-form-fill ()
  (pdf-test-with-pdf "forms.pdf"
    (pdf-form-fill '(("name" . "Ada") ("agree" . t) ("size" . 2)
                     ("color" . "Blue") ("pet" . "Ferret")
                     ("tags" . ("Red" "Blue")) ("zip" . "29634")))
    (should (buffer-modified-p))
    (let ((values (pdf-form-values)))
      (should (equal (cdr (assoc "name" values)) "Ada"))
      (should (eq (cdr (assoc "agree" values)) t))
      (should (= (cdr (assoc "size" values)) 2))
      (should (equal (cdr (assoc "color" values)) "Blue"))
      (should (equal (cdr (assoc "pet" values)) "Ferret"))
      (should (equal (cdr (assoc "tags" values)) '("Red" "Blue")))
      (should (equal (cdr (assoc "zip" values)) "29634")))
    ;; The values of one call round-trip.
    (let ((values (pdf-form-values)))
      (pdf-form-fill values)
      (should (equal (pdf-form-values) values)))
    ;; Selecting the other radio button deselects the first.
    (pdf-form-fill '(("size" . 1)))
    (should (= (cdr (assoc "size" (pdf-form-values))) 1))
    ;; Errors change nothing, even for the valid values before them.
    (let ((before (pdf-form-values)))
      (dolist (bad '((("name" . "Bob") ("nope" . "x"))
                     (("name" . "Bob") ("fixed" . "x"))
                     (("name" . "Bob") ("zip" . "296340"))
                     (("name" . "Bob") ("size" . 3))
                     (("name" . "Bob") ("color" . "Mauve"))
                     (("name" . "Bob") ("color" . ("Red" "Blue")))
                     (("name" . "Bob") ("name" . 42))))
        (should-error (pdf-form-fill bad))
        (should (equal (pdf-form-values) before))))))
