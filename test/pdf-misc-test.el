;; -*- lexical-binding: t -*-

(require 'pdf-misc)
(require 'ert)
(require 'cl-lib)

;; Tests for editing document metadata.  Every test works on a copy
;; of a PDF in a temporary directory, since saving writes the file.

(defconst pdf-misc-test-unicode-title "Café Straße 日本"
  "A title with Latin-1, non Latin-1 and CJK characters.")

(defmacro pdf-misc-test-with-copy (file-var source &rest body)
  "Copy SOURCE into a temporary directory and run BODY visiting it.

FILE-VAR is bound to the copy's file name.  SOURCE is a file
name, or a function that writes a PDF to the file name it is
called with.

Unlike `pdf-test-with-pdf', this starts the server before
visiting the file and binds `auto-mode-alist' to select
`pdf-view-mode', so that the mode is set up completely even when
an earlier test quit the server or uninstalled the alists."
  (declare (indent 2) (debug t))
  (let ((dir (make-symbol "dir"))
        (buffer (make-symbol "buffer")))
    `(let* ((,dir (make-temp-file "pdf-misc-test" t))
            (,file-var (expand-file-name "copy.pdf" ,dir))
            (pdf-info-epdfinfo-error-filename
             (expand-file-name "epdfinfo.log" ,dir))
            ,buffer)
       (unwind-protect
           (progn
             (if (functionp ,source)
                 (funcall ,source ,file-var)
               (copy-file ,source ,file-var))
             (pdf-info-quit)
             (pdf-info-process-assert-running t)
             (let ((auto-mode-alist (cons '("\\.[pP][dD][fF]\\'" . pdf-view-mode)
                                          auto-mode-alist)))
               (setq ,buffer (find-file-noselect ,file-var)))
             (with-current-buffer ,buffer
               (should pdf-view--server-file-name)
               ,@body))
         (when (buffer-live-p ,buffer)
           (with-current-buffer ,buffer
             (set-buffer-modified-p nil)
             (let (kill-buffer-hook)
               (kill-buffer))))
         (pdf-info-quit)
         (delete-directory ,dir t)))))

(defun pdf-misc-test-metadata-of-file (file)
  "Return the metadata of FILE, as freshly read from disk."
  (pdf-info-close file)
  (prog1 (pdf-info-metadata file)
    (pdf-info-close file)))

(defun pdf-misc-test-write-xmp-pdf (file title)
  "Write a one page PDF to FILE, with TITLE in Info and XMP.

TITLE should be ASCII."
  (let* ((xmp (concat
               "<?xpacket begin=\"\357\273\277\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n"
               "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">"
               "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">"
               "<rdf:Description rdf:about=\"\""
               " xmlns:dc=\"http://purl.org/dc/elements/1.1/\">"
               "<dc:title><rdf:Alt>"
               "<rdf:li xml:lang=\"de\">Anderer Titel</rdf:li>"
               "<rdf:li xml:lang=\"x-default\">" title "</rdf:li>"
               "</rdf:Alt></dc:title>"
               "</rdf:Description></rdf:RDF></x:xmpmeta>\n"
               "<?xpacket end=\"w\"?>"))
         (objects
          (list "<< /Type /Catalog /Pages 2 0 R /Metadata 4 0 R >>"
                "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
                "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 200 200] >>"
                (format "<< /Type /Metadata /Subtype /XML /Length %d >>\n\
stream\n%s\nendstream"
                        (length xmp) xmp)
                (format "<< /Title (%s) >>" title)))
         offsets)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert "%PDF-1.5\n")
      (cl-loop for obj in objects
               for i from 1
               do (push (1- (point)) offsets)
               (insert (format "%d 0 obj\n%s\nendobj\n" i obj)))
      (let ((xref (1- (point))))
        (insert (format "xref\n0 %d\n0000000000 65535 f \n"
                        (1+ (length objects))))
        (dolist (offset (nreverse offsets))
          (insert (format "%010d 00000 n \n" offset)))
        (insert (format "trailer\n<< /Size %d /Root 1 0 R /Info 5 0 R >>\n\
startxref\n%d\n%%%%EOF\n"
                        (1+ (length objects)) xref)))
      (let ((coding-system-for-write 'binary))
        (write-region nil nil file nil 'silent)))))

(defun pdf-misc-test-xmp-pdf (file)
  (pdf-misc-test-write-xmp-pdf file "Original Title"))

;; * Server level

(ert-deftest pdf-info-setmetadata-save-roundtrip ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (let ((md (pdf-info-setmetadata
               `((title . ,pdf-misc-test-unicode-title)
                 (author . "A. U. Thor")
                 (subject . "Colons: and \\ backslashes")
                 (keywords . ("one" "two"))
                 (creator . "pdf-misc-test")))))
      ;; The response reflects the change ...
      (let-alist md
        (should (equal .title pdf-misc-test-unicode-title))
        (should (equal .author "A. U. Thor"))
        (should (equal .subject "Colons: and \\ backslashes"))
        (should (equal .keywords '("one" "two")))
        (should (equal .creator "pdf-misc-test")))
      ;; ... but the file is unchanged until saved.
      (should (equal "" (cdr (assq 'title (pdf-misc-test-metadata-of-file
                                           file)))))
      (pdf-info-setmetadata `((title . ,pdf-misc-test-unicode-title)
                              (author . "A. U. Thor")))
      (let ((saved (pdf-info-save)))
        (unwind-protect
            (let-alist (pdf-misc-test-metadata-of-file saved)
              (should (equal .title pdf-misc-test-unicode-title))
              (should (equal .author "A. U. Thor"))
              ;; Untouched fields survive.
              (should (equal .producer "pdfTeX-1.40.13")))
          (delete-file saved))))))

(ert-deftest pdf-info-setmetadata-empty-removes ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file #'pdf-misc-test-xmp-pdf
    (should (equal "Original Title"
                   (cdr (assq 'title (pdf-info-metadata)))))
    (should (equal "" (cdr (assq 'title (pdf-info-setmetadata
                                         '((title . "")))))))
    (should (equal "" (cdr (assq 'title (pdf-info-setmetadata
                                         '((title . nil)))))))))

(ert-deftest pdf-info-setmetadata-errors ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (should-error (pdf-info-setmetadata '((producer . "x"))))
    (should-error (pdf-info-query 'setmetadata (pdf-view-buffer-file-name)
                                  "bogus" "x"))
    (should-error (pdf-info-query 'setmetadata (pdf-view-buffer-file-name)
                                  "title"))
    ;; A failed request changes nothing, even if earlier pairs were valid.
    (should-error (pdf-info-query 'setmetadata (pdf-view-buffer-file-name)
                                  "title" "x" "bogus" "y"))
    (should (equal "" (cdr (assq 'title (pdf-info-metadata)))))))

(ert-deftest pdf-info-xmp-metadata ()
  (pdf-misc-test-with-copy file "test.pdf"
    (should-not (pdf-info-xmp-metadata)))
  (pdf-misc-test-with-copy file #'pdf-misc-test-xmp-pdf
    (let ((xmp (pdf-info-xmp-metadata)))
      (should (stringp xmp))
      (should (string-match-p "<dc:title>" xmp)))))

;; * XMP

(ert-deftest pdf-misc-metadata-xmp-title ()
  (pdf-misc-test-with-copy file "test.pdf"
    (should-not (pdf-misc-metadata-xmp-title)))
  (pdf-misc-test-with-copy file #'pdf-misc-test-xmp-pdf
    (should (equal "Original Title" (pdf-misc-metadata-xmp-title)))))

(ert-deftest pdf-misc-metadata-xmp-not-updated ()
  "Poppler changes the Info title only; the XMP title stays and we warn."
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file #'pdf-misc-test-xmp-pdf
    (let (warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (type message &rest _)
                   (push (cons type message) warnings))))
        ;; Same title as in the XMP: no warning.
        (pdf-misc-update-metadata '((title . "Changed")))
        (setq warnings nil)
        (pdf-misc-update-metadata '((title . "Original Title")))
        (should-not warnings)
        ;; Other fields: no warning.
        (pdf-misc-update-metadata '((author . "Somebody")))
        (should-not warnings)
        (pdf-misc-update-metadata '((title . "New Title")))
        (should (= 1 (length warnings)))
        (should (equal '(pdf-misc xmp) (caar warnings)))
        (should (string-match-p "Original Title" (cdar warnings)))
        (save-buffer)
        (let-alist (pdf-misc-test-metadata-of-file file)
          (should (equal .title "New Title")))
        ;; The saved file still carries the old XMP title.
        (should (equal "Original Title" (pdf-misc-metadata-xmp-title)))))))

;; * Lisp commands

(ert-deftest pdf-misc-update-metadata-modifies-buffer ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (should-not (buffer-modified-p))
    ;; No change, no modification.
    (pdf-misc-update-metadata '((title . "") (author . nil)))
    (should-not (buffer-modified-p))
    (should-error (pdf-misc-update-metadata '((producer . "x"))))
    (pdf-misc-set-metadata 'title pdf-misc-test-unicode-title)
    (should (buffer-modified-p))
    (save-buffer)
    (should-not (buffer-modified-p))
    (should (equal pdf-misc-test-unicode-title
                   (cdr (assq 'title (pdf-misc-test-metadata-of-file
                                      file)))))))

(ert-deftest pdf-misc-metadata-revert-discards ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (pdf-misc-set-metadata 'title "Unsaved")
    (should (equal "Unsaved" (cdr (assq 'title (pdf-info-metadata)))))
    (should (buffer-modified-p))
    (revert-buffer t t)
    (should-not (buffer-modified-p))
    (should (equal "" (cdr (assq 'title (pdf-info-metadata)))))
    (should (equal "" (cdr (assq 'title (pdf-misc-test-metadata-of-file
                                         file)))))))

(defun pdf-misc-test-edit-field (field value)
  "Replace the value of FIELD in the current edit buffer with VALUE."
  (goto-char 1)
  (re-search-forward (format "^%s:.*$" (capitalize (symbol-name field))))
  (replace-match (format "%s: %s" (capitalize (symbol-name field)) value)
                 t t))

(ert-deftest pdf-misc-edit-metadata-commit ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (let ((pdf-buffer (current-buffer))
          (edit (pdf-misc-edit-metadata-noselect)))
      (with-current-buffer edit
        (should (derived-mode-p 'pdf-misc-edit-metadata-mode))
        (should (equal (buffer-string)
                       "Title: \nAuthor: \nSubject: \nKeywords: \n\
Creator: LaTeX with hyperref package\n"))
        (pdf-misc-test-edit-field 'title pdf-misc-test-unicode-title)
        (pdf-misc-test-edit-field 'keywords "alpha, beta")
        (pdf-misc-test-edit-field 'creator "")
        (pdf-misc-edit-metadata-commit))
      (should-not (buffer-live-p edit))
      (should (eq (current-buffer) pdf-buffer))
      (should (buffer-modified-p))
      (let-alist (pdf-info-metadata)
        (should (equal .title pdf-misc-test-unicode-title))
        (should (equal .keywords '("alpha" "beta")))
        (should (equal .creator ""))
        (should (equal .author "")))
      (save-buffer)
      (let-alist (pdf-misc-test-metadata-of-file file)
        (should (equal .title pdf-misc-test-unicode-title))
        (should (equal .keywords '("alpha" "beta")))
        (should (equal .creator "")))
      ;; The edit buffer starts from the saved values.
      (with-current-buffer (pdf-misc-edit-metadata-noselect)
        (goto-char 1)
        (should (looking-at (regexp-quote
                             (concat "Title: " pdf-misc-test-unicode-title))))
        ;; Committing without edits changes nothing.
        (pdf-misc-edit-metadata-commit))
      (should-not (buffer-modified-p)))))

(ert-deftest pdf-misc-edit-metadata-abort ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (let ((edit (pdf-misc-edit-metadata-noselect)))
      (with-current-buffer edit
        (pdf-misc-test-edit-field 'title "Abandoned")
        (pdf-misc-edit-metadata-abort))
      (should-not (buffer-live-p edit))
      (should-not (buffer-modified-p))
      (should (equal "" (cdr (assq 'title (pdf-info-metadata))))))))

(ert-deftest pdf-misc-edit-metadata-malformed ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (let ((edit (pdf-misc-edit-metadata-noselect)))
      (unwind-protect
          (with-current-buffer edit
            (goto-char (point-max))
            (insert "Producer: me\n")
            (should-error (pdf-misc-edit-metadata-commit)
                          :type 'user-error)
            (delete-region (line-beginning-position 0) (point-max))
            (insert "no colon here\n")
            (should-error (pdf-misc-edit-metadata-commit)
                          :type 'user-error)
            ;; Deleting a line leaves the field alone.
            (erase-buffer)
            (insert "Title: Only this\n")
            (pdf-misc-edit-metadata-commit))
        (when (buffer-live-p edit)
          (kill-buffer edit)))
      (let-alist (pdf-info-metadata)
        (should (equal .title "Only this"))
        (should (equal .creator "LaTeX with hyperref package"))))))

(ert-deftest pdf-misc-display-metadata-edit-key ()
  (pdf-misc-test-with-copy file "test.pdf"
    (should (eq (lookup-key pdf-metadata-buffer-mode-map (kbd "e"))
                #'pdf-misc-edit-metadata))
    (let ((pdf-buffer (current-buffer)))
      (pdf-misc-display-metadata)
      (with-current-buffer "*PDF-Metadata*"
        (should (eq pdf-misc-metadata--buffer pdf-buffer))
        (when (pdf-info-writable-metadata-p)
          (let ((edit (save-window-excursion
                        (call-interactively #'pdf-misc-edit-metadata)
                        (current-buffer))))
            (should (eq pdf-buffer (buffer-local-value
                                    'pdf-misc-edit-metadata--buffer edit)))
            (kill-buffer edit))))
      (kill-buffer "*PDF-Metadata*"))))
