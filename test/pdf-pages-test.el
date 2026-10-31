;; -*- lexical-binding: t -*-

(require 'pdf-pages)
(require 'ert)

;; Tests for pdf-pages.el.  They need the qpdf program and work on
;; temporary copies of the fixtures, never on the fixtures themselves.

(defvar pdf-pages-test-file nil
  "The temporary copy of the fixture visited by the current test.")

(defmacro pdf-pages-test-with-copy (fixture &rest body)
  "Run BODY in a buffer visiting a temporary copy of FIXTURE."
  (declare (indent 1) (debug t))
  (let ((dir (make-symbol "dir")))
    `(let* ((,dir (make-temp-file "pdf-pages-test" t))
            (pdf-pages-test-file (expand-file-name ,fixture ,dir))
            (pdf-pages-confirm nil)
            (pdf-undo--alist nil)
            (pdf-undo-backup-directory (expand-file-name "backups" ,dir)))
       (copy-file ,fixture pdf-pages-test-file)
       (unwind-protect
           (pdf-test-with-pdf pdf-pages-test-file
             ;; Other tests may have changed `auto-mode-alist'.
             (unless (derived-mode-p 'pdf-view-mode)
               (pdf-view-mode))
             ;; Batch mode has no window to show the buffer, so set up
             ;; the window properties for the window-less case (t).
             (when (eq image-mode-winprops-alist t)
               (setq image-mode-winprops-alist nil))
             ,@body)
         (delete-directory ,dir t)))))

(defun pdf-pages-test-page-ids ()
  "Return the section title of every page of the current buffer.

The pages of test.pdf are titled Text, Annotations, Links,
Attachments, Outline and nothing."
  (cl-loop for page from 1 to (pdf-info-number-of-pages)
           collect (let ((text (pdf-info-gettext page '(0 0 1 1))))
                     (and (string-match "Text\\|Annotations\\|Links\\|\
Attachments\\|Outline" text)
                          (match-string 0 text)))))

(defconst pdf-pages-test-ids
  '("Text" "Annotations" "Links" "Attachments" "Outline" nil)
  "The result of `pdf-pages-test-page-ids' for test.pdf.")

(defun pdf-pages-test-ids (&rest pages)
  "Return the page ids of test.pdf's PAGES."
  (mapcar (lambda (p) (nth (1- p) pdf-pages-test-ids)) pages))

(defun pdf-pages-test-file-contents (file)
  "Return the bytes of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun pdf-pages-test-stray-files ()
  "Return temporary files left next to `pdf-pages-test-file'."
  (directory-files (file-name-directory pdf-pages-test-file) nil
                   "-pdf-pages-"))

;;; Page ranges

(ert-deftest pdf-pages-test-parse-range ()
  (should (equal (pdf-pages-parse-range "3-5,8" 10) '(3 4 5 8)))
  (should (equal (pdf-pages-parse-range " 1 , 3 - 4 " 6) '(1 3 4)))
  (should (equal (pdf-pages-parse-range "z" 6) '(6)))
  (should (equal (pdf-pages-parse-range "5-z" 6) '(5 6)))
  (should (equal (pdf-pages-parse-range "r1,r2" 6) '(6 5)))
  (should (equal (pdf-pages-parse-range "5-3,1,1" 6) '(5 4 3 1 1)))
  (should-error (pdf-pages-parse-range "0" 6) :type 'user-error)
  (should-error (pdf-pages-parse-range "7" 6) :type 'user-error)
  (should-error (pdf-pages-parse-range "1-x" 6) :type 'user-error)
  (should-error (pdf-pages-parse-range "" 6) :type 'user-error))

(ert-deftest pdf-pages-test-range-string ()
  (should (equal (pdf-pages-range-string '(1 2 3 5 7 8)) "1-3,5,7-8"))
  (should (equal (pdf-pages-range-string '(3 1 2)) "3,1-2"))
  (should (equal (pdf-pages-range-string '(4)) "4")))

;;; Operations

(ert-deftest pdf-pages-test-delete ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-pages-test-with-copy "test.pdf"
    (should (equal (pdf-pages-test-page-ids) pdf-pages-test-ids))
    (pdf-view-goto-page 2)
    ;; test.pdf makes qpdf warn (exit status 3), which is a success.
    (pdf-pages-delete '(3 2))
    (should (= (pdf-cache-number-of-pages) 4))
    (should (equal (pdf-pages-test-page-ids) (pdf-pages-test-ids 1 4 5 6)))
    ;; The page after the deleted ones.
    (should (= (pdf-view-current-page) 2))
    (should-not (buffer-modified-p))
    (should (verify-visited-file-modtime))
    (should-not (pdf-pages-test-stray-files))
    ;; Deleting the last page shows the new last page.
    (pdf-pages-delete '(4))
    (should (= (pdf-view-current-page) 3))
    (should-error (pdf-pages-delete '(1 2 3)) :type 'user-error)
    (should-error (pdf-pages-delete '(9)) :type 'user-error)))

(ert-deftest pdf-pages-test-move ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-pages-test-with-copy "test.pdf"
    (pdf-pages-move '(5) 1)
    (should (= (pdf-cache-number-of-pages) 6))
    (should (equal (pdf-pages-test-page-ids) (pdf-pages-test-ids 5 1 2 3 4 6)))
    (should (= (pdf-view-current-page) 1))
    ;; Moving a block keeps its order.
    (pdf-pages-move '(1 2) 5)
    (should (equal (pdf-pages-test-page-ids) (pdf-pages-test-ids 2 3 4 6 5 1)))
    (should (= (pdf-view-current-page) 5))
    (should-error (pdf-pages-move '(1) 7) :type 'user-error)
    (should-error (pdf-pages-move '(1) 1) :type 'user-error)))

(ert-deftest pdf-pages-test-rotate ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-pages-test-with-copy "test.pdf"
    (let ((portrait (pdf-info-pagesize 2)))
      (should (< (car portrait) (cdr portrait)))
      (pdf-view-goto-page 3)
      (pdf-pages-rotate '(2 4) 90)
      (should (= (pdf-view-current-page) 3))
      (should (equal (pdf-info-pagesize 2)
                     (cons (cdr portrait) (car portrait))))
      (should (equal (pdf-info-pagesize 4)
                     (cons (cdr portrait) (car portrait))))
      (should (equal (pdf-info-pagesize 3) portrait))
      (should (equal (pdf-pages-test-page-ids) pdf-pages-test-ids))
      (pdf-pages-rotate-counterclockwise '(2))
      (should (equal (pdf-info-pagesize 2) portrait))
      (pdf-pages-rotate '(1) 180)
      (should (equal (pdf-info-pagesize 1) portrait))
      (should-error (pdf-pages-rotate '(1) 45) :type 'user-error))))

(ert-deftest pdf-pages-test-insert-and-append ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-pages-test-with-copy "test.pdf"
    (let ((other (expand-file-name "other.pdf"
                                   (file-name-directory pdf-pages-test-file))))
      (copy-file "test.pdf" other)
      (pdf-pages-insert-file other 3 "4-5")
      (should (= (pdf-cache-number-of-pages) 8))
      (should (equal (pdf-pages-test-page-ids)
                     (pdf-pages-test-ids 1 2 3 4 5 4 5 6)))
      (should (= (pdf-view-current-page) 4))
      (pdf-pages-insert-file other 0 "z")
      (should (equal (pdf-pages-test-page-ids)
                     (pdf-pages-test-ids 6 1 2 3 4 5 4 5 6)))
      (should (= (pdf-view-current-page) 1))
      (pdf-pages-append-file other)
      (should (= (pdf-cache-number-of-pages) 15))
      (should (= (pdf-view-current-page) 10))
      (should (equal (last (pdf-pages-test-page-ids) 6) pdf-pages-test-ids)))))

(ert-deftest pdf-pages-test-extract ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-pages-test-with-copy "test.pdf"
    (let* ((before (pdf-pages-test-file-contents pdf-pages-test-file))
           (target (expand-file-name "part.pdf"
                                     (file-name-directory pdf-pages-test-file)))
           (buffer (pdf-pages-extract '(3 1) target)))
      (unwind-protect
          (progn
            (should (buffer-live-p buffer))
            (should (equal (buffer-file-name buffer) target))
            (with-current-buffer buffer
              (should (= (pdf-info-number-of-pages) 2))
              (should (equal (pdf-pages-test-page-ids)
                             (pdf-pages-test-ids 3 1))))
            ;; The visited file is untouched and has nothing to undo.
            (should (equal (pdf-pages-test-file-contents pdf-pages-test-file)
                           before))
            (should (= (pdf-cache-number-of-pages) 6))
            (should-error (pdf-undo) :type 'user-error)
            (should-error (pdf-pages-extract '(1) pdf-pages-test-file)
                          :type 'user-error)
            ;; Overwriting an existing file asks, and the buffer
            ;; visiting it is reverted.
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (pdf-pages-extract '(5) target))
            (with-current-buffer buffer
              (should (equal (pdf-pages-test-page-ids)
                             (pdf-pages-test-ids 5)))))
        (with-current-buffer buffer
          (let (kill-buffer-hook) (kill-buffer)))))))

(ert-deftest pdf-pages-test-undo ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-pages-test-with-copy "test.pdf"
    (let ((original (pdf-pages-test-file-contents pdf-pages-test-file)))
      (should-error (pdf-undo) :type 'user-error)
      (pdf-view-goto-page 4)
      (pdf-pages-delete '(4))
      (pdf-pages-move '(1) 5)
      (should (equal (pdf-pages-test-page-ids) (pdf-pages-test-ids 2 3 5 6 1)))
      (pdf-undo)
      (should (equal (pdf-pages-test-page-ids) (pdf-pages-test-ids 1 2 3 5 6)))
      (should (= (pdf-view-current-page) 4))
      (pdf-undo)
      (should (equal (pdf-pages-test-page-ids) pdf-pages-test-ids))
      (should (= (pdf-view-current-page) 4))
      (should (equal (pdf-pages-test-file-contents pdf-pages-test-file)
                     original))
      (should-error (pdf-undo) :type 'user-error)
      (should-not (pdf-pages-test-stray-files))
      ;; The backups are gone too.
      (should-not (directory-files pdf-undo-backup-directory nil "\\.pdf\\'")))))

(ert-deftest pdf-pages-test-undo-limit ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-pages-test-with-copy "test.pdf"
    (let ((pdf-undo-limit 2))
      (pdf-pages-rotate '(1) 90)
      (pdf-pages-rotate '(1) 90)
      (pdf-pages-rotate '(1) 90)
      (should (= 2 (length (cdr (assoc (file-truename pdf-pages-test-file)
                                       pdf-undo--alist)))))
      (should (= 2 (length (directory-files pdf-undo-backup-directory
                                            nil "\\.pdf\\'")))))))

;;; Safety

(ert-deftest pdf-pages-test-refuse-unsaved-changes ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-pages-test-with-copy "test.pdf"
    (let ((original (pdf-pages-test-file-contents pdf-pages-test-file))
          asked)
      (set-buffer-modified-p t)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq asked t) nil)))
        (should-error (pdf-pages-delete '(1)) :type 'user-error)
        (should-error (pdf-pages-rotate '(1) 90) :type 'user-error)
        (should-error (pdf-pages-extract
                       '(1) (expand-file-name
                             "x.pdf" (file-name-directory pdf-pages-test-file)))
                      :type 'user-error))
      (should asked)
      (should (buffer-modified-p))
      (should (equal (pdf-pages-test-file-contents pdf-pages-test-file)
                     original))
      (should (= (pdf-cache-number-of-pages) 6)))))

(ert-deftest pdf-pages-test-refuse-cancelled ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-pages-test-with-copy "test.pdf"
    (let ((pdf-pages-confirm 'destructive)
          questions)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (push prompt questions) nil)))
        (should-error (pdf-pages-delete '(2)) :type 'user-error)
        (should (= (length questions) 1))
        ;; Rotating removes nothing, so it does not ask.
        (pdf-pages-rotate '(2) 90)
        (should (= (length questions) 1)))
      (should (= (pdf-cache-number-of-pages) 6)))))

(ert-deftest pdf-pages-test-failure-leaves-file-intact ()
  (skip-unless (executable-find "false"))
  (pdf-pages-test-with-copy "test.pdf"
    (let ((original (pdf-pages-test-file-contents pdf-pages-test-file)))
      (let ((pdf-pages-qpdf-program (executable-find "false")))
        (should-error (pdf-pages-delete '(1)))
        (should-error (pdf-pages-extract
                       '(1) (expand-file-name
                             "x.pdf" (file-name-directory pdf-pages-test-file)))))
      ;; A program that succeeds without writing a valid PDF.
      (let ((pdf-pages-qpdf-program (executable-find "true")))
        (should-error (pdf-pages-move '(1) 2)))
      (should (equal (pdf-pages-test-file-contents pdf-pages-test-file)
                     original))
      (should-not (pdf-pages-test-stray-files))
      (should-not (file-exists-p
                   (expand-file-name
                    "x.pdf" (file-name-directory pdf-pages-test-file))))
      (should-not (cdr (assoc (file-truename pdf-pages-test-file)
                              pdf-undo--alist)))
      (should (= (pdf-cache-number-of-pages) 6)))))

(ert-deftest pdf-pages-test-missing-qpdf ()
  (pdf-pages-test-with-copy "test.pdf"
    (let ((pdf-pages-qpdf-program "/nonexistent/qpdf"))
      (should (string-match-p
               "pdf-pages-qpdf-program"
               (cadr (should-error (pdf-pages-delete '(1)))))))))

(ert-deftest pdf-pages-test-requires-a-file ()
  (with-temp-buffer
    (should-error (pdf-pages-delete '(1)) :type 'user-error))
  (pdf-pages-test-with-copy "test.pdf"
    (set-visited-file-name nil)
    (set-buffer-modified-p nil)
    (should (string-match-p
             "not visiting a file"
             (cadr (should-error (pdf-pages-delete '(1))
                                 :type 'user-error))))))

(ert-deftest pdf-pages-test-encrypted ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (let ((password-cache nil)
        (passwords nil))
    (cl-letf (((symbol-function 'password-read)
               (lambda (&rest _) (pop passwords))))
      (setq passwords (list "pdftool" "pdftool"))
      (pdf-pages-test-with-copy "encrypted.pdf"
        ;; The test macro restarts the server, open the document again.
        (pdf-view-decrypt-document)
        (let ((original (pdf-pages-test-file-contents pdf-pages-test-file))
              (npages (pdf-cache-number-of-pages)))
          ;; Wrong passwords: qpdf fails and the file is untouched.
          (setq passwords (list "wrong" "wrong" "wrong" "wrong"))
          (should-error (pdf-pages-delete '(1)))
          (should (equal (pdf-pages-test-file-contents pdf-pages-test-file)
                         original))
          ;; The right password, read again for reopening the result.
          (setq passwords (list "pdftool" "pdftool"))
          (pdf-pages-delete '(1))
          (should (= (pdf-cache-number-of-pages) (1- npages)))
          ;; The result is still encrypted.
          (should-not (eq 0 (car (pdf-pages--call-qpdf
                                  nil "--check" pdf-pages-test-file))))
          (should (eq 0 (car (pdf-pages--call-qpdf
                              "pdftool" "--check" pdf-pages-test-file))))
          ;; Undo, with the password asked again for reopening.
          (setq passwords (list "pdftool"))
          (pdf-undo)
          (should (= (pdf-cache-number-of-pages) npages))
          (should (equal (pdf-pages-test-file-contents pdf-pages-test-file)
                         original)))))))


;;; Flattening

(defun pdf-pages-test-qpdf-at-least (version)
  "Return non-nil if a qpdf of at least VERSION runs and reports itself."
  (and (executable-find pdf-pages-qpdf-program)
       (ignore-errors (pdf-pages--qpdf-version-at-least version))))

(ert-deftest pdf-pages-test-qpdf-version ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (should (equal (pdf-pages--qpdf-version) (pdf-pages--qpdf-version)))
  (should (pdf-pages--qpdf-version-at-least '(1)))
  (should-not (pdf-pages--qpdf-version-at-least '(1000))))

(ert-deftest pdf-pages-test-flatten-annotations ()
  (skip-unless (pdf-pages-test-qpdf-at-least '(11 8)))
  (pdf-pages-test-with-copy "test.pdf"
    (let ((npages (pdf-cache-number-of-pages))
          ;; The markup and text annotations of page 2 have no
          ;; appearance streams, which the qpdf program cannot
          ;; flatten.
          (markup-types '(highlight squiggly underline strike-out text)))
      (save-buffer)
      (pdf-pages-flatten-annotations)
      (should (= (pdf-cache-number-of-pages) npages))
      ;; The file attachment of page 4 has an appearance stream and
      ;; is flattened into the page.
      (should-not (cl-some (lambda (a) (eq (alist-get 'type a) 'file))
                           (pdf-info-getannots nil)))
      ;; The annotations without one are not touched.
      (should (cl-every (lambda (a)
                          (memq (alist-get 'type a) markup-types))
                        (pdf-info-getannots 2)))
      (should (= 5 (length (pdf-info-getannots 2))))
      (should-not (buffer-modified-p))
      (should (verify-visited-file-modtime))
      (should-not (pdf-pages-test-stray-files))
      ;; Undo brings the file attachment back.
      (pdf-undo)
      (should (cl-some (lambda (a) (eq (alist-get 'type a) 'file))
                       (pdf-info-getannots nil)))
      (should (= (pdf-cache-number-of-pages) npages)))))

(ert-deftest pdf-pages-test-flatten-form-fields ()
  (skip-unless (pdf-pages-test-qpdf-at-least '(11 8)))
  (pdf-pages-test-with-copy "forms.pdf"
    (let ((field (cl-find "name" (pdf-info-formfields 1)
                          :key (lambda (f) (alist-get 'name f))
                          :test #'equal)))
      (pdf-info-setformfield (alist-get 'id field) "Ada Lovelace")
      (set-buffer-modified-p t)
      (save-buffer)
      (pdf-pages-flatten-annotations)
      ;; The filled-in value is part of the page now, and the field
      ;; is gone; only the Reset push button, which the qpdf program
      ;; cannot flatten, remains.
      (should (string-match-p "Ada Lovelace"
                              (pdf-info-gettext 1 '(0 0 1 1))))
      (should-not (cl-find "name" (pdf-info-formfields 1)
                           :key (lambda (f) (alist-get 'name f))
                           :test #'equal))
      (should (equal (mapcar (lambda (f) (alist-get 'name f))
                             (pdf-info-formfields 1))
                     '("Reset")))
      (should-not (buffer-modified-p))
      ;; Undo restores the field and its value.
      (pdf-undo)
      (let ((field (cl-find "name" (pdf-info-formfields 1)
                            :key (lambda (f) (alist-get 'name f))
                            :test #'equal)))
        (should (equal (alist-get 'value field) "Ada Lovelace")))
      (should-not (string-match-p
                   "Ada Lovelace" (pdf-info-gettext 1 '(0 0 1 1)))))))

(ert-deftest pdf-pages-test-flatten-needs-qpdf ()
  (pdf-pages-test-with-copy "test.pdf"
    (let ((pdf-pages--qpdf-version '(8 4 3)))
      (should (string-match-p
               "11.8 or later"
               (cadr (should-error (pdf-pages-flatten-annotations))))))))
