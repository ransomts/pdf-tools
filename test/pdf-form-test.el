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

;; CSV tables.  A table of values is a way to fill in a form for many
;; people at once, so the tests cover the format itself, the round trip
;; through a file, and filling in a copy of the fixture per row.

(ert-deftest pdf-form-csv-keys-do-not-shadow-other-modes ()
  ;; `pdf-form-minor-mode' is looked up before `pdf-misc-minor-mode',
  ;; so a plain key here would hide the one of the other mode in every
  ;; document with form fields.  `I' is `pdf-misc-display-metadata'.
  (should (eq 'pdf-form-export-csv
              (lookup-key pdf-form-minor-mode-map (kbd "C-c C-f e"))))
  (should (eq 'pdf-form-import-csv
              (lookup-key pdf-form-minor-mode-map (kbd "C-c C-f i"))))
  (should (eq 'pdf-form-import-csv-batch
              (lookup-key pdf-form-minor-mode-map (kbd "C-c C-f b"))))
  (should-not (lookup-key pdf-form-minor-mode-map (kbd "E")))
  (should-not (lookup-key pdf-form-minor-mode-map (kbd "I"))))

(ert-deftest pdf-form-csv-round-trip ()
  (let ((rows '(("name" "value")
                ("a,b" "c")
                ("quote" "he said \"hi\"")
                ("newline" "two\nlines")
                ("empty" "")
                ("spaced" "  padded  "))))
    (should (equal (pdf-form--csv-parse
                    (mapconcat #'pdf-form--csv-line rows "\n"))
                   rows))
    ;; A line break at the end does not start an empty row.
    (should (equal (pdf-form--csv-parse "a,b\n") '(("a" "b"))))
    (should-not (pdf-form--csv-parse ""))
    ;; CRLF, and a double quote inside a quoted field is written twice.
    (should (equal (pdf-form--csv-parse "a,\"b\"\"c\"\r\nd,e\r\n")
                   '(("a" "b\"c") ("d" "e"))))
    ;; A field is quoted only when it has to be.
    (should (equal (pdf-form--csv-line '("a" "b")) "a,b"))
    (should (equal (pdf-form--csv-line '("a,b")) "\"a,b\""))
    (should (equal (pdf-form--csv-line '("a\"b")) "\"a\"\"b\""))
    (should (equal (pdf-form--csv-line '("a\nb")) "\"a\nb\""))
    (should (equal (pdf-form--csv-line '("")) ""))
    ;; A space at either end survives the round trip.
    (should (equal (pdf-form--csv-line '(" a")) "\" a\""))
    (should (equal (pdf-form--csv-parse (pdf-form--csv-line '(" a ")))
                   '((" a "))))))

(ert-deftest pdf-form-csv-export-and-import ()
  (let* ((dir (make-temp-file "pdf-form-csv" t))
         (file (expand-file-name "values.csv" dir)))
    (unwind-protect
        (pdf-test-with-pdf "forms.pdf"
          (let* ((original (pdf-form-values))
                 (names (mapcar (lambda (group) (car group))
                                (cl-remove-if-not
                                 (lambda (group)
                                   (pdf-form-fillable-p (cadr group)))
                                 (pdf-form--named-fields)))))
            (pdf-form-export-csv file)
            (should (file-exists-p file))
            (let* ((rows (pdf-form--csv-parse
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))))
                   (header (car rows))
                   (cells (cadr rows)))
              (should (= 2 (length rows)))
              (should (equal (length header) (length cells)))
              ;; The header names the fields you can fill in, in
              ;; document order, and leaves the read-only one out.
              (should (equal header names))
              (should-not (member "fixed" header))
              ;; The cells hold the values, read as text.
              (dolist (name header)
                (should (equal (nth (cl-position name header :test #'equal)
                                    cells)
                               (pdf-form--csv-cell
                                (cadr (assoc name (pdf-form--named-fields)))
                                (cdr (assoc name original))))))
              ;; Change some values, then read the file back.
              (pdf-form-fill '(("name" . "Ada") ("agree" . t)
                               ("size" . 1) ("color" . "Blue")
                               ("tags" . ("Red" "Blue")) ("zip" . "29634")))
              (should-not (equal (pdf-form-values) original))
              (pdf-form-import-csv file)
              (let ((imported (pdf-form-values)))
                ;; The file brings back the values it holds.  The
                ;; `size' group had no button selected when it was
                ;; written, and a viewer cannot clear a group, so it
                ;; keeps the button this test chose.
                (dolist (name header)
                  (unless (equal name "size")
                    (should (equal (cdr (assoc name imported))
                                   (cdr (assoc name original))))))
                (should (= 1 (cdr (assoc "size" imported))))))))
      (delete-directory dir t))))

(ert-deftest pdf-form-csv-import-unknown-field ()
  (let ((file (make-temp-file "pdf-form-csv" nil ".csv")))
    (unwind-protect
        (pdf-test-with-pdf "forms.pdf"
          (let ((before (pdf-form-values)))
            ;; A name the form does not have.
            (with-temp-file file
              (insert "name,nope\nAda,x\n"))
            (should-error (pdf-form-import-csv file) :type 'user-error)
            (should (equal (pdf-form-values) before))
            ;; A name it has but cannot fill in.
            (with-temp-file file
              (insert "name,fixed\nAda,x\n"))
            (let ((err (should-error (pdf-form-import-csv file)
                                     :type 'user-error)))
              (should (string-match-p "fixed" (error-message-string err))))
            (should (equal (pdf-form-values) before))))
      (delete-file file))))

(ert-deftest pdf-form-csv-batch ()
  (let* ((dir (make-temp-file "pdf-form-csv" t))
         (first (expand-file-name "first.pdf" dir))
         (second (expand-file-name "second.pdf" dir))
         (table (expand-file-name "roster.csv" dir)))
    (unwind-protect
        (progn
          (copy-file "forms.pdf" first)
          (copy-file "forms.pdf" second)
          (with-temp-file table
            (insert "File,name,agree\r\n"
                    "first.pdf,Ada,true\r\n"
                    "second.pdf,Bob,false\r\n"))
          ;; The command runs in a PDF buffer, and asks first.
          (pdf-test-with-pdf "forms.pdf"
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (should (= 2 (pdf-form-import-csv-batch table)))))
          (dolist (expected `((,first "Ada" t) (,second "Bob" nil)))
            (pdf-test-with-pdf (car expected)
              (let ((values (pdf-form-values)))
                (should (equal (cdr (assoc "name" values)) (nth 1 expected)))
                ;; The column of a checkbox is read as true or false.
                (should (equal (and (cdr (assoc "agree" values)) t)
                               (nth 2 expected))))))
          ;; A row that fails does not stop the others.
          (with-temp-file table
            (insert "file,name\n"
                    "first.pdf,Charles\n"
                    "missing.pdf,Denise\n"
                    "second.pdf,Edith\n"))
          (pdf-test-with-pdf "forms.pdf"
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (should (= 2 (pdf-form-import-csv-batch table))))
            (with-current-buffer "*PDF form CSV*"
              (should (string-match-p "missing\\.pdf" (buffer-string)))))
          (dolist (expected `((,first . "Charles") (,second . "Edith")))
            (pdf-test-with-pdf (car expected)
              (should (equal (cdr (assoc "name" (pdf-form-values)))
                             (cdr expected))))))
      (delete-directory dir t))))

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
      (should (string-match-p "approval (page 1)\n  Signature: +unsigned"
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
            (pdf-pages--undo-alist nil)
            (pdf-pages-confirm nil)
            (pdf-pages-backup-directory
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
        (pdf-pages-undo)
        (should (cl-some (lambda (a) (eq (alist-get 'type a) 'stamp))
                         (pdf-info-getannots nil)))))))
