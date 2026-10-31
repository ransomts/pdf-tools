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

(ert-deftest pdf-form-actionable-fields ()
  (pdf-test-with-pdf "forms.pdf"
    ;; `fixed' is read-only, the other twelve fields can be used.
    (should (equal (mapcar (lambda (f) (alist-get 'name f))
                            (pdf-form--actionable-fields 1))
                   '("name" "notes" "secret" "agree" "color" "size"
                     "fruit" "pet" "tags" "zip" "Reset" "size")))))

(defun pdf-form-test-widget-annots (page)
  "Return the widget annotations of PAGE."
  (cl-remove-if-not (lambda (a) (eq (pdf-annot-get a 'type) 'widget))
                    (pdf-annot-getannots page)))

(ert-deftest pdf-form-widget-annotation-is-a-field ()
  "A widget annotation is the visible part of a form field.

`pdf-annot' must leave its hotspot to `pdf-form', and activating
the annotation must edit the field instead of its comment."
  (pdf-test-with-pdf "forms.pdf"
    (let ((annots (pdf-form-test-widget-annots 1)))
      (should annots)
      ;; Every widget annotation is drawn where its field is.
      (dolist (a annots)
        (should (pdf-form--field-at-edges (pdf-annot-get a 'page)
                                          (pdf-annot-get a 'edges))))
      ;; The annotation hotspots leave the widgets alone...
      (let ((spots (pdf-annot-hotspot-function 1 '(1000 . 1000))))
        (dolist (a annots)
          (should-not (memq (pdf-annot-get-id a) (mapcar #'cadr spots)))))
      ;; ...while every field you can use gets a hotspot of its own.
      (let ((spots (mapcar #'cadr
                           (pdf-form-hotspots-function 1 '(1000 . 1000)))))
        (dolist (field (pdf-form--actionable-fields 1))
          (should (memq (intern (format "form-field-%d"
                                        (alist-get 'id field)))
                        spots))))
      ;; Activating a widget edits its field and handles the click.
      (let* ((widget (cl-find-if
                      (lambda (a)
                        (equal "name"
                               (alist-get 'name
                                          (pdf-form--field-at-edges
                                           (pdf-annot-get a 'page)
                                           (pdf-annot-get a 'edges)))))
                      annots))
             edited)
        (should widget)
        (cl-letf (((symbol-function 'pdf-form-edit-field)
                   (lambda (f) (setq edited (alist-get 'name f)))))
          (should (pdf-form-activate-widget widget)))
        (should (equal edited "name"))))))

(ert-deftest pdf-form-mode-activates-widgets ()
  "The mode takes the widget activation handler and leaves no trace."
  (pdf-test-with-pdf "forms.pdf"
    (pdf-form-minor-mode 1)
    (should (memq 'pdf-form-activate-widget
                  pdf-annot-activate-handler-functions))
    (pdf-form-minor-mode -1)
    (should-not (memq 'pdf-form-activate-widget
                      pdf-annot-activate-handler-functions))
    (pdf-form-minor-mode 1)
    (should (memq 'pdf-form-activate-widget
                  pdf-annot-activate-handler-functions))))

(defun pdf-form-test-image-data (image)
  "Return the data of the IMAGE created by `pdf-view-create-page'."
  (plist-get (cdr image) :data))

(defmacro pdf-form-test-with-images (&rest body)
  "Run BODY so that page images can be created without PNG support.

These tests only compare the image data, and the Emacs builds CI
uses cannot create PNG images: `pdf-view-image-type' signals an
error, and so would `create-image'."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'pdf-view-image-type)
              (lambda () 'png))
             ((symbol-function 'create-image)
              (lambda (data &optional type _data-p &rest props)
                `(image :type ,type :data ,data ,@props))))
     ,@body))

(ert-deftest pdf-form-create-page-marks-fields ()
  (pdf-form-test-with-images
    (pdf-test-with-pdf "forms.pdf"
      (let* ((pdf-view-inhibit-hotspots t)
             (size (pdf-view-desired-image-size 1)))
        (should pdf-form-show-fields)
        ;; With marks on, the page shows the highlighted fields.
        (should (equal (pdf-form-test-image-data (pdf-view-create-page 1))
                       (apply #'pdf-cache-renderpage-highlight 1 (car size)
                              (list `(,@pdf-form-show-fields-color
                                      ,@(pdf-form--field-edges 1))))))
        ;; With marks off, it is the plain rendering.
        (let ((pdf-form-show-fields nil))
          (should (equal (pdf-form-test-image-data (pdf-view-create-page 1))
                         (pdf-cache-renderpage 1 (car size)))))))))

(ert-deftest pdf-form-create-page-marks-no-fields ()
  (pdf-form-test-with-images
    (pdf-test-with-test-pdf-view
      ;; A document without fields is rendered as before.
      (let* ((pdf-view-inhibit-hotspots t)
             (size (pdf-view-desired-image-size 1)))
        (should (equal (pdf-form-test-image-data (pdf-view-create-page 1))
                       (pdf-cache-renderpage 1 (car size))))))))

(ert-deftest pdf-form-show-field-shows-marks-too ()
  (pdf-form-test-with-images
    (pdf-test-with-pdf "forms.pdf"
      (let* ((pdf-view-inhibit-hotspots t)
             (size (pdf-view-desired-image-size 1))
             (field (pdf-form-test-field "name"))
             (image (pdf-form--create-image 1 size nil
                                            `(,@pdf-form-highlight-color
                                              ,(alist-get 'edges field)))))
        (should (equal (pdf-form-test-image-data image)
                       (apply #'pdf-cache-renderpage-highlight 1 (car size)
                              (list `(,@pdf-form-show-fields-color
                                      ,@(pdf-form--field-edges 1))
                                    `(,@pdf-form-highlight-color
                                      ,(alist-get 'edges field))))))))))

(ert-deftest pdf-form-show-fields-toggle ()
  (pdf-form-test-with-images
    (pdf-test-with-pdf "forms.pdf"
      (let* ((pdf-view-inhibit-hotspots t)
             (width (car (pdf-view-desired-image-size 1))))
        (should pdf-form-show-fields)
        (pdf-form-show-fields)
        (should-not pdf-form-show-fields)
        ;; Marks follow the variable.
        (should (equal (pdf-form-test-image-data (pdf-view-create-page 1))
                       (pdf-cache-renderpage 1 width)))
        (pdf-form-show-fields)
        (should pdf-form-show-fields)
        (should-not (equal (pdf-form-test-image-data (pdf-view-create-page 1))
                           (pdf-cache-renderpage 1 width)))
        (pdf-form-show-fields 0)
        (should-not pdf-form-show-fields)
        (pdf-form-show-fields 1)
        (should pdf-form-show-fields)))))

(ert-deftest pdf-form-create-page-gated-by-mode ()
  (pdf-form-test-with-images
    (pdf-test-with-pdf "forms.pdf"
      (let* ((pdf-view-inhibit-hotspots t)
             (width (car (pdf-view-desired-image-size 1))))
        ;; The advice stays installed, but a buffer with the mode off
        ;; shows the plain rendering.
        (pdf-form-minor-mode -1)
        (should (advice-member-p 'pdf-form--create-page 'pdf-view-create-page))
        (should-not pdf-form-minor-mode)
        (let ((pdf-form-show-fields t))
          (should (equal (pdf-form-test-image-data (pdf-view-create-page 1))
                         (pdf-cache-renderpage 1 width))))
        (pdf-form-minor-mode 1)
        (should pdf-form-minor-mode)))))

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

;; Signing.  The tests create a throwaway NSS database with a
;; self-signed certificate, and sign temporary copies of the fixtures.

(defvar pdf-form-test-dir nil
  "The temporary directory of the current signing test.")

(defun pdf-form-test-run (program &rest args)
  "Run PROGRAM with ARGS, signal an error if it fails."
  (with-temp-buffer
    (unless (eq 0 (apply #'call-process program nil t nil args))
      (error "%s failed: %s" program (buffer-string)))))

(defun pdf-form-test-make-nss (dir &optional password)
  "Make an NSS database in DIR with the certificate `signer'.

Give the database PASSWORD, if non-nil."
  (let ((key (expand-file-name "key.pem" dir))
        (cert (expand-file-name "cert.pem" dir))
        (p12 (expand-file-name "cert.p12" dir))
        (pwfile (expand-file-name "password" dir))
        (db (expand-file-name "nssdb" dir)))
    (make-directory db)
    (pdf-form-test-run "openssl" "req" "-x509" "-newkey" "rsa:2048" "-nodes"
                       "-keyout" key "-out" cert "-days" "30"
                       "-subj" "/CN=Sign Tester/O=pdf-tools")
    (pdf-form-test-run "openssl" "pkcs12" "-export" "-inkey" key "-in" cert
                       "-name" "signer" "-out" p12 "-passout" "pass:")
    (if (null password)
        (pdf-form-test-run "certutil" "-N" "--empty-password" "-d" db)
      (with-temp-file pwfile (insert password "\n"))
      (pdf-form-test-run "certutil" "-N" "-f" pwfile "-d" db))
    (apply #'pdf-form-test-run "pk12util" "-i" p12 "-d" db "-W" ""
           (and password (list "-k" pwfile)))
    db))

(defmacro pdf-form-test-with-signing (fixture password &rest body)
  "Run BODY visiting a copy of FIXTURE, with a test NSS database.

The database has the password PASSWORD, if it is non-nil."
  (declare (indent 2) (debug t))
  `(progn
     (skip-unless (and (executable-find "openssl")
                       (executable-find "certutil")
                       (executable-find "pk12util")))
     (let* ((pdf-form-test-dir (make-temp-file "pdf-form-test" t))
            (file (expand-file-name ,fixture pdf-form-test-dir))
            (pdf-info-nss-directory
             (pdf-form-test-make-nss pdf-form-test-dir ,password))
            (pdf-form-backup-directory
             (expand-file-name "backups" pdf-form-test-dir)))
       (copy-file ,fixture file)
       (unwind-protect
           (pdf-test-with-pdf file
             (skip-unless (pdf-info-signing-p))
             ;; Start the mode now that the server runs, even if
             ;; `auto-mode-alist' started it when the server did not,
             ;; which leaves it half set up.
             (pdf-view-mode)
             ;; Batch mode has no window to show the buffer.
             (when (eq image-mode-winprops-alist t)
               (setq image-mode-winprops-alist nil))
             ,@body)
         (delete-directory pdf-form-test-dir t)))))

(defun pdf-form-test-signatures ()
  "Return the validations of the signed fields of the document by name."
  (delq nil
        (mapcar (lambda (f)
                  (let ((v (pdf-info-validatesignature (alist-get 'id f))))
                    (unless (eq (alist-get 'signature-status v) 'not-found)
                      (cons (alist-get 'name f) v))))
                (pdf-form-fields '(1) #'pdf-form-signature-p))))

(defun pdf-form-test-file-contents (file)
  "Return the bytes of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun pdf-form-test-assert-signed (validation)
  "Assert that VALIDATION is the intact signature of the test certificate."
  (let-alist validation
    (should (eq .signature-status 'valid))
    ;; The test certificate is self-signed.
    (should (memq .certificate-status '(untrusted-issuer unknown-issuer)))
    (should (equal .signer "Sign Tester"))
    (should (equal .subject-organization "pdf-tools"))))

(ert-deftest pdf-form-signingcertificates ()
  (pdf-form-test-with-signing "forms.pdf" nil
    (let ((certs (pdf-info-signingcertificates)))
      (should (= 1 (length certs)))
      (let-alist (car certs)
        (should (equal .id "signer"))
        (should (equal .subject "Sign Tester"))
        (should (equal .subject-organization "pdf-tools"))
        (should (equal .issuer "Sign Tester"))
        (should (string-match-p "\\`[0-9]\\{4\\}-" .expires))))))

(ert-deftest pdf-form-sign-rectangle ()
  (pdf-form-test-with-signing "forms.pdf" nil
    (let ((before (pdf-form-test-file-contents file)))
      (should-not (pdf-form-test-signatures))
      (pdf-form-sign '(1 0.1 0.05 0.4 0.12) "signer" "Testing" "Clemson")
      (should-not (buffer-modified-p))
      (let ((sigs (pdf-form-test-signatures)))
        (should (equal (mapcar #'car sigs) '("Signature")))
        (pdf-form-test-assert-signed (cdar sigs)))
      (let ((field (car (pdf-form-fields '(1) #'pdf-form-signature-p))))
        (should (equal (mapcar (lambda (x) (/ (round (* x 100)) 100.0))
                               (alist-get 'edges field))
                       '(0.1 0.05 0.4 0.12))))
      ;; The form is still there, and the unsigned version is kept.
      (should (= 13 (length (pdf-form-fields '(1) (lambda (f)
                                                   (not (pdf-form-signature-p f)))))))
      (let ((backups (directory-files pdf-form-backup-directory t "\\.pdf\\'")))
        (should (= 1 (length backups)))
        (should (equal (pdf-form-test-file-contents (car backups)) before)))
      ;; No temporary files are left next to the document.
      (should (equal (directory-files pdf-form-test-dir nil "\\.pdf\\'")
                     '("forms.pdf")))
      ;; Signing again adds a second signature and keeps the first.
      (pdf-form-sign '(1 0.5 0.05 0.8 0.12) "signer")
      (let ((sigs (pdf-form-test-signatures)))
        (should (equal (sort (mapcar #'car sigs) #'string<)
                       '("Signature" "Signature2")))
        (mapc (lambda (s) (pdf-form-test-assert-signed (cdr s))) sigs)))))

(ert-deftest pdf-form-sign-field ()
  (pdf-form-test-with-signing "sigfield.pdf" nil
    (let ((field (pdf-form-test-field "approval")))
      (should (pdf-form-unsigned-p field))
      (should (equal (mapcar (lambda (f) (alist-get 'name f))
                             (pdf-form-unsigned-fields))
                     '("approval")))
      (pdf-form-sign field "signer")
      (let ((sigs (pdf-form-test-signatures)))
        (should (equal (mapcar #'car sigs) '("approval-signed")))
        (pdf-form-test-assert-signed (cdar sigs)))
      ;; The signature covers the empty field, which no longer counts.
      (let ((signed (pdf-form-test-field "approval-signed")))
        (should (equal (alist-get 'edges signed) (alist-get 'edges field)))
        (should (= 2 (length (pdf-form--signatures-at signed)))))
      ;; The empty field is labelled as the field under the signature.
      (let ((fields (pdf-form-fields '(1) #'pdf-form-signature-p)))
        (should (equal (mapcar #'pdf-form--signature-heading fields)
                       '("approval (page 1, empty, under approval-signed)\n"
                         "approval-signed (page 1)\n")))
        (pdf-form-describe-signatures fields)
        (with-current-buffer "*PDF Signatures*"
          (should (string-match-p
                   (regexp-quote "approval (page 1, empty, under approval-signed)")
                   (buffer-string)))
          (should (string-match-p (regexp-quote "approval-signed (page 1)")
                                  (buffer-string)))))
      (should-not (pdf-form-unsigned-fields)))))

(ert-deftest pdf-form-sign-password ()
  (pdf-form-test-with-signing "forms.pdf" "secret"
    (should (string-match-p
             "NSS password required"
             (error-message-string
              (should-error (pdf-info-signingcertificates)))))
    (should (string-match-p
             "Wrong NSS password"
             (error-message-string
              (should-error (pdf-info-signingcertificates "wrong")))))
    (let ((answers '("wrong" "secret")))
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _) (pop answers))))
        (pdf-form-sign '(1 0.1 0.05 0.4 0.12) "signer"))
      (should-not answers))
    (pdf-form-test-assert-signed (cdar (pdf-form-test-signatures)))))

(ert-deftest pdf-form-sign-unsaved ()
  (pdf-form-test-with-signing "forms.pdf" nil
    (let ((before (pdf-form-test-file-contents file)))
      (pdf-form-fill '(("name" . "Ada")))
      (should (buffer-modified-p))
      ;; Declining to save refuses to sign and changes nothing.
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (should-error (pdf-form-sign '(1 0.1 0.05 0.4 0.12) "signer")
                      :type 'user-error))
      (should (buffer-modified-p))
      (should (equal (pdf-form-test-file-contents file) before))
      ;; Saving first signs the filled in form.
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (pdf-form-sign '(1 0.1 0.05 0.4 0.12) "signer"))
      (should-not (buffer-modified-p))
      (should (equal (cdr (assoc "name" (pdf-form-values))) "Ada"))
      (pdf-form-test-assert-signed (cdar (pdf-form-test-signatures))))))

(ert-deftest pdf-form-sign-interactive ()
  (pdf-form-test-with-signing "sigfield.pdf" nil
    ;; Take the defaults: the empty field and the only certificate.
    (let (prompts)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt _coll &optional _pred _req _init _hist def &rest _)
                   (push prompt prompts)
                   def))
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (push prompt prompts)
                   (if (string-prefix-p "Reason" prompt) "Approved" ""))))
        (call-interactively #'pdf-form-sign))
      (should (string-prefix-p "Sign (default approval (page 1))" (car (last prompts))))
      (should (= 4 (length prompts))))
    (let ((sigs (pdf-form-test-signatures)))
      (should (equal (mapcar #'car sigs) '("approval-signed")))
      (pdf-form-test-assert-signed (cdar sigs)))
    ;; Clicking on the signed field shows the signatures there.
    (pdf-form-edit-field (pdf-form-test-field "approval"))
    (with-current-buffer "*PDF Signatures*"
      (should (string-match-p "approval-signed (page 1)\n  Signature: +valid"
                              (buffer-string)))
      (should (string-match-p
               (concat (regexp-quote
                        "approval (page 1, empty, under approval-signed)")
                       "\n  Signature: +unsigned")
               (buffer-string))))))

(defmacro pdf-form-test-with-image-signing (fixture &rest body)
  "Run BODY visiting a copy of FIXTURE, ready for image signing."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (pdf-info-image-stamps-p))
     (skip-unless (and (executable-find pdf-pages-qpdf-program)
                       (ignore-errors (pdf-pages--qpdf-version-at-least
                                       '(11 8)))))
     (let* ((pdf-form-test-dir (make-temp-file "pdf-form-test" t))
            (file (expand-file-name ,fixture pdf-form-test-dir))
            (pdf-undo--alist nil)
            (pdf-pages-confirm nil)
            (pdf-undo-backup-directory
             (expand-file-name "backups" pdf-form-test-dir)))
       (copy-file ,fixture file)
       (unwind-protect
           (pdf-test-with-pdf file
             (pdf-view-mode)
             ;; Batch mode has no window to show the buffer.
             (when (eq image-mode-winprops-alist t)
               (setq image-mode-winprops-alist nil))
             ,@body)
         (delete-directory pdf-form-test-dir t)))))

(ert-deftest pdf-form-sign-with-image ()
  (let ((image (expand-file-name "signature.png")))
    (pdf-form-test-with-image-signing "forms.pdf"
      (let ((npages (pdf-cache-number-of-pages)))
        (pdf-form-sign-with-image '(1 0.6 0.05 0.9 0.2) image)
        ;; The stamp was flattened into the page.
        (should (= (pdf-cache-number-of-pages) npages))
        (should-not (cl-some (lambda (a) (eq (alist-get 'type a) 'stamp))
                             (pdf-info-getannots nil)))
        (should-not (buffer-modified-p))
        ;; The image is part of the file now.
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally file)
          (goto-char (point-min))
          (should (search-forward "/Subtype /Image" nil t))
          (should-not (search-forward "/Subtype /Stamp" nil t)))
        ;; Undo restores the version with the unflattened stamp.
        (pdf-undo)
        (should (cl-some (lambda (a) (eq (alist-get 'type a) 'stamp))
                         (pdf-info-getannots nil)))))))

;; The certificate store.  These tests do not need poppler: the
;; management commands run certutil and pk12util on their own, and
;; the pure ones build argument lists without running anything.

(defun pdf-form-test-iso-time (days)
  "Return an ISO 8601 string DAYS days from now, in UTC."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (time-add (current-time) (* days 24 60 60))
                      t))

(ert-deftest pdf-form-certificate-string-expiry ()
  "Say in `pdf-form--certificate-to-string' when a certificate has expired."
  (let ((expired (pdf-form--certificate-to-string
                  `((id . "old") (subject . "Old Signer")
                    (expires . ,(pdf-form-test-iso-time -1)))))
        (soon (pdf-form--certificate-to-string
               `((id . "soon") (subject . "Soon Signer")
                 (expires . ,(pdf-form-test-iso-time 10)))))
        (valid (pdf-form--certificate-to-string
                `((id . "new") (subject . "New Signer")
                  (expires . ,(pdf-form-test-iso-time 200))))))
    (should (string-match-p ", expired)\\'" expired))
    (should (string-match-p ", expires [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\})"
                            soon))
    (should-not (string-match-p ", expired" soon))
    (should (string-match-p "expires [0-9]\\{4\\}-" valid))
    (should-not (string-match-p ", expired" valid))))

(ert-deftest pdf-form-certificate-state ()
  "Describe the three states of a certificate's expiry."
  (let ((now (current-time))
        (certificate (lambda (days)
                       `((id . "nick")
                         (expires . ,(pdf-form-test-iso-time days))))))
    (should (equal (pdf-form--certificate-state (funcall certificate -1) now)
                   "expired"))
    (should (equal (pdf-form--certificate-state (funcall certificate 0.5) now)
                   "expires today"))
    (should (equal (pdf-form--certificate-state (funcall certificate 10.5) now)
                   "expires in 10 days"))
    (should (equal (pdf-form--certificate-state (funcall certificate 200) now)
                   "valid"))
    ;; The warning threshold is a customization.
    (let ((pdf-form-certificate-warning-days 5))
      (should (equal (pdf-form--certificate-state (funcall certificate 10.5)
                                                  now)
                     "valid")))))

(ert-deftest pdf-form-certificate-command ()
  "Build the certutil and pk12util argument lists without running them."
  (let ((pdf-info-nss-directory nil))
    (should (equal (pdf-form--certificate-command 'list)
                   (list "certutil" "-L" "-d"
                         (concat "sql:"
                                 (expand-file-name "~/.pki/nssdb"))))))
  (let ((pdf-info-nss-directory "/tmp/example-nss"))
    (let ((list-command
           (pdf-form--certificate-command 'list :password-file "/tmp/pw")))
      (should (equal (car list-command) "certutil"))
      (should (member "-L" list-command))
      (should (equal (cadr (member "-d" list-command)) "sql:/tmp/example-nss"))
      (should (equal (cadr (member "-f" list-command)) "/tmp/pw")))
    (let ((create (pdf-form--certificate-command
                   'create :nickname "nick" :subject "CN=Tim,O=Clemson"
                   :days 365 :password-file "/tmp/pw" :noise-file "/tmp/noise")))
      (should (equal (car create) "certutil"))
      (should (member "-S" create))
      (should (member "-x" create))
      (should (equal (cadr (member "-n" create)) "nick"))
      (should (equal (cadr (member "-s" create)) "CN=Tim,O=Clemson"))
      (should (equal (cadr (member "-t" create)) "u,u,u"))
      (should (equal (cadr (member "-d" create)) "sql:/tmp/example-nss"))
      (should (equal (cadr (member "-f" create)) "/tmp/pw"))
      (should (equal (cadr (member "-z" create)) "/tmp/noise"))
      (should (member "--keyUsage" create))
      ;; certutil counts months, not days.
      (should (equal (cadr (member "-v" create)) "12")))
    (let ((delete (pdf-form--certificate-command
                   'delete :nickname "nick" :password-file "/tmp/pw")))
      (should (equal (car delete) "certutil"))
      (should (member "-D" delete))
      (should (equal (cadr (member "-n" delete)) "nick"))
      (should (equal (cadr (member "-d" delete)) "sql:/tmp/example-nss"))
      (should (equal (cadr (member "-f" delete)) "/tmp/pw")))
    (let ((import (pdf-form--certificate-command
                   'import :file "/tmp/cert.p12"
                   :file-password-file "/tmp/file-pw"
                   :password-file "/tmp/pw")))
      (should (equal (car import) "pk12util"))
      (should (equal (cadr (member "-i" import)) "/tmp/cert.p12"))
      (should (equal (cadr (member "-d" import)) "sql:/tmp/example-nss"))
      (should (equal (cadr (member "-w" import)) "/tmp/file-pw"))
      (should (equal (cadr (member "-k" import)) "/tmp/pw")))))

(ert-deftest pdf-form-certificates-list ()
  "List certificates with expiry, expired ones first and red."
  (let ((certificates
         (list `((id . "new") (subject . "New Signer")
                 (issuer . "New Signer")
                 (expires . ,(pdf-form-test-iso-time 200)))
               `((id . "old") (subject . "Old Signer")
                 (issuer . "Old Signer")
                 (expires . ,(pdf-form-test-iso-time -1)))
               `((id . "soon") (subject . "Soon Signer")
                 (issuer . "Someone Else")
                 (expires . ,(pdf-form-test-iso-time 5.5))))))
    (unwind-protect
        (cl-letf (((symbol-function 'pdf-info-signingcertificates)
                   (lambda (&optional _password) certificates)))
          (let ((buffer (pdf-form-certificates nil)))
            (with-current-buffer buffer
              (should (derived-mode-p 'pdf-form-certificates-mode))
              ;; Expired first, then by expiry.
              (should (equal (mapcar #'car tabulated-list-entries)
                             '("old" "soon" "new")))
              ;; Point starts on the expired one.
              (should (looking-at-p " *Old Signer"))
              ;; It is red, and its state says so.
              (search-forward "Old Signer")
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'error))
              (search-forward "expired")
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'error))
              ;; The soon-expiring one is not red, but visible.
              (search-forward "expires in 5 days")
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'warning))
              ;; The Issuer column is there because one differs, but
              ;; a self-signed certificate leaves its cell empty.
              (should (member "Issuer"
                              (mapcar #'car tabulated-list-format)))
              (should (string-match-p "Soon Signer +Someone Else"
                                      (buffer-substring-no-properties
                                       (point-min) (point-max))))))
      (when (get-buffer "*PDF Certificates*")
        (kill-buffer "*PDF Certificates*"))))))

(ert-deftest pdf-form-read-certificate-default ()
  "Offer the default certificate first and as the default answer."
  (let ((certificates
         '(((id . "a") (subject . "A") (expires . "2030-01-01T00:00:00Z"))
           ((id . "b") (subject . "B") (expires . "2031-01-01T00:00:00Z"))))
        (pdf-form-default-certificate "b")
        (prompt nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (p _collection &optional _predicate _require _initial
                          _history default _inherit)
                 (setq prompt p)
                 default)))
      (let-alist (pdf-form-read-certificate certificates)
        (should (equal .id "b")))
      (should (string-match-p "default B (" prompt)))))

(ert-deftest pdf-form-certificate-create-and-list ()
  "Create a self-signed certificate and list it with certutil.

The database is made with `certutil -N --empty-password', which
this NSS version accepts.  If a version wanted a password, the
test makes the database with one through a 0600 password file
instead, and answers `read-passwd' with that password."
  (skip-unless (executable-find "certutil"))
  (let* ((dir (make-temp-file "pdf-form-nss-test" t))
         (db (concat "sql:" dir))
         (pdf-info-nss-directory dir)
         (pdf-form-default-certificate nil)
         (password nil)
         (password-file (expand-file-name "password" dir)))
    (unwind-protect
        (progn
          (unless (eq 0 (call-process "certutil" nil nil nil
                                      "-N" "-d" db "--empty-password"))
            (setq password "secret")
            (write-region "secret\n" nil password-file nil 'silent)
            (set-file-modes password-file #o600)
            (should (eq 0 (call-process "certutil" nil nil nil
                                        "-N" "-f" password-file "-d" db))))
          (cl-letf (((symbol-function 'read-passwd)
                     (lambda (&rest _) password)))
            (pdf-form-certificate-create "test signer" "Test Signer"
                                         "pdf-tools" "test@example.com" 30))
          ;; certutil lists the new certificate; this test does not
          ;; involve poppler, which may refuse an unfinished store.
          (let ((listing (with-temp-buffer
                           (call-process "certutil" nil t nil "-L" "-d" db)
                           (buffer-string))))
            (should (string-match-p "test signer" listing))
            (should (equal (pdf-form--certificate-nicknames listing)
                           '("test signer")))))
      (delete-directory dir t))))
