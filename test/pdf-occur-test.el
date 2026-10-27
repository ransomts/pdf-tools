;; -*- lexical-binding: t -*-

(require 'pdf-occur)
(require 'ert)

;; Tests for pdf-occur.el, against the text of test.pdf: page 1 holds
;; the same paragraph twice ("stored" four times on three lines),
;; "line" occurs on page 2 ("4. underline") and page 5 ("Outline"),
;; and page 3 has "Links", "Internal link" and "External link".
;;
;; The search runs asynchronously in a server local to the occur
;; buffer; `pdf-occur-test--wait' waits for it to finish.

(defconst pdf-occur-test--buffer "*PDF-Occur*")

(defmacro pdf-occur-test-with-pdf (&rest body)
  "Run BODY in a buffer visiting test.pdf; kill the occur buffer after."
  (declare (indent 0) (debug t))
  `(pdf-test-with-test-pdf-view
     (unwind-protect
         (progn ,@body)
       (when (get-buffer pdf-occur-test--buffer)
         ;; Also kills the buffer's local server.
         (kill-buffer pdf-occur-test--buffer)))))

(defmacro pdf-occur-test-with-displayed-pdf (&rest body)
  "Like `pdf-occur-test-with-pdf', with the PDF in the selected window.
Rendering needs a window system, so page display is stubbed out and
drawing the match highlight is recorded instead."
  (declare (indent 0) (debug t))
  `(pdf-occur-test-with-pdf
     (cl-letf (((symbol-function 'pdf-view-display-page) #'ignore)
               ((symbol-function 'pdf-view-image-size)
                (lambda (&rest _) '(612 . 792)))
               ((symbol-function 'pdf-isearch-hl-matches)
                (lambda (&rest args)
                  (push (cons 'hl args) pdf-occur-test--drawn)))
               ((symbol-function 'pdf-isearch-focus-match-batch)
                (lambda (&rest args)
                  (push (cons 'focus args) pdf-occur-test--drawn))))
       (setq pdf-occur-test--drawn nil)
       (switch-to-buffer (current-buffer))
       (delete-other-windows)
       (pdf-view-goto-page 1)
       (unwind-protect
           (progn ,@body)
         (delete-other-windows)))))

(defvar pdf-occur-test--drawn nil
  "Calls to the stubbed highlighting functions, most recent first.")

(defun pdf-occur-test--wait ()
  "Wait for the search in the occur buffer to finish."
  (with-current-buffer pdf-occur-test--buffer
    (let ((deadline (+ (float-time) 10)))
      (while (and (or (pdf-occur-search-in-progress-p)
                      (not (equal (cadr mode-line-process) ":exit")))
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (equal (cadr mode-line-process) ":exit"))))

(defun pdf-occur-test--search (documents string &optional regexp-p)
  "Search DOCUMENTS for STRING and wait for the result."
  (pdf-occur-search documents string regexp-p)
  (pdf-occur-test--wait))

(defun pdf-occur-test--lines ()
  "The lines of the occur buffer, without properties."
  (with-current-buffer pdf-occur-test--buffer
    (split-string (buffer-substring-no-properties (point-min) (point-max))
                  "\n" t)))

(defun pdf-occur-test--ids ()
  "The ids of the entries of the occur buffer, in order."
  (with-current-buffer pdf-occur-test--buffer
    (mapcar #'car tabulated-list-entries)))

(defun pdf-occur-test--header ()
  "The header line of the occur buffer.
`format-mode-line' returns \"\" in batch, so evaluate the (:eval FORM)
directly."
  (with-current-buffer pdf-occur-test--buffer
    (should (eq (car header-line-format) :eval))
    (substring-no-properties (eval (cadr header-line-format) t))))

;;; Searching one document

(ert-deftest pdf-occur-string-search ()
  (pdf-occur-test-with-pdf
    (let ((file (buffer-file-name)))
      (pdf-occur "stored")
      (pdf-occur-test--wait)
      (with-current-buffer pdf-occur-test--buffer
        (should (derived-mode-p 'pdf-occur-buffer-mode))
        (should (= pdf-occur-number-of-matches 4))
        (should (equal pdf-occur-search-string "stored"))
        (should-not pdf-occur-search-regexp-p)
        (should (equal pdf-occur-search-documents (list (list file))))
        ;; One document: just the page and line columns.
        (should (equal (mapcar #'car tabulated-list-format) '("Page" "Line"))))
      (should (equal (pdf-occur-test--header)
                     "4 matches in document `test.pdf'"))
      ;; One line per match, in document order; a line with two matches
      ;; is listed twice.
      (should (equal (pdf-occur-test--lines)
                     '("   1 key difference is, that pages are not prerendered by e.g. ghostscript and stored"
                       "   1 in the file-system, but rather created on-demand and stored in memory."
                       "   1 stored in the file-system, but rather created on-demand and stored in memory."
                       "   1 stored in the file-system, but rather created on-demand and stored in memory.")))
      (let ((ids (pdf-occur-test--ids))
            (expected (pdf-info-search-string "stored" 1)))
        (should (= 4 (length ids)))
        (cl-loop for id in ids
                 for match in expected
                 do (should (equal (plist-get id :document) file))
                 (should (= (plist-get id :page) 1))
                 (should (equal (plist-get id :match-edges)
                                (alist-get 'edges match)))
                 (should (equal-including-properties
                          (plist-get id :match-text)
                          (alist-get 'text match))))))))

(ert-deftest pdf-occur-match-highlighting ()
  (pdf-occur-test-with-pdf
    (pdf-occur "stored")
    (pdf-occur-test--wait)
    (with-current-buffer pdf-occur-test--buffer
      (goto-char (point-min))
      (forward-line 2)
      ;; Both matches in the line are highlighted, the page in its face.
      (let ((text (buffer-substring (line-beginning-position)
                                    (line-end-position))))
        (should (string-match "stored" text))
        (should (eq (get-text-property (match-beginning 0) 'face text) 'match))
        (should (string-match "stored" text (match-end 0)))
        (should (eq (get-text-property (match-beginning 0) 'face text) 'match))
        (should (string-match "1" text))
        (should (eq (get-text-property (match-beginning 0) 'face text)
                    'pdf-occur-page-face))))))

(ert-deftest pdf-occur-regexp-vs-string ()
  (pdf-occur-test-with-pdf
    (pdf-occur "under?line" t)
    (pdf-occur-test--wait)
    (should (equal (pdf-occur-test--lines) '("   2 4. underline")))
    (should (equal (pdf-occur-test--header) "1 match in document `test.pdf'"))
    ;; The same pattern as a plain string matches nothing.
    (with-current-buffer pdf-occur-test--buffer
      (pdf-occur-revert-buffer-with-args "under?line" nil))
    (pdf-occur-test--wait)
    (should-not (pdf-occur-test--lines))
    (should (equal (pdf-occur-test--header) "0 matches in document `test.pdf'"))))

(ert-deftest pdf-occur-several-pages ()
  (pdf-occur-test-with-pdf
    (pdf-occur "line")
    (pdf-occur-test--wait)
    (should (equal (pdf-occur-test--lines) '("   2 4. underline" "   5 Outline")))
    (should (equal (mapcar (lambda (id) (plist-get id :page))
                           (pdf-occur-test--ids))
                   '(2 5)))))

(ert-deftest pdf-occur-case-folding ()
  (pdf-occur-test-with-pdf
    (let ((case-fold-search t))
      (pdf-occur "link"))
    (pdf-occur-test--wait)
    (should (= 3 (length (pdf-occur-test--lines))))
    (kill-buffer pdf-occur-test--buffer)
    (let ((case-fold-search nil))
      (pdf-occur "link"))
    (pdf-occur-test--wait)
    (should (equal (pdf-occur-test--lines)
                   '("   3 1. Internal link: 1"
                     "   3 2. External link: http://www.gnu.org")))))

(ert-deftest pdf-occur-page-range ()
  (pdf-occur-test-with-pdf
    (let ((file (buffer-file-name)))
      (pdf-occur-test--search (list (cons file 5)) "line")
      (should (equal (pdf-occur-test--lines) '("   5 Outline")))
      (pdf-occur-test--search (list (cons file '(3 . 4))) "line")
      (should-not (pdf-occur-test--lines)))))

(ert-deftest pdf-occur-revert-with-new-string ()
  (pdf-occur-test-with-pdf
    (pdf-occur "stored")
    (pdf-occur-test--wait)
    (with-current-buffer pdf-occur-test--buffer
      (pdf-occur-revert-buffer-with-args "Outline"))
    (pdf-occur-test--wait)
    (should (equal (pdf-occur-test--lines) '("   5 Outline")))
    (with-current-buffer pdf-occur-test--buffer
      (should (equal pdf-occur-search-string "Outline"))
      ;; Plain revert repeats the search.
      (revert-buffer))
    (pdf-occur-test--wait)
    (should (equal (pdf-occur-test--lines) '("   5 Outline")))))

(ert-deftest pdf-occur-errors ()
  (should-error (pdf-occur-search nil "x"))
  (pdf-occur-test-with-pdf
    (should-error (pdf-occur-search (list (current-buffer)) ""))
    (should-error (pdf-occur-search (list (current-buffer)) nil)))
  (with-temp-buffer
    (should-error (pdf-occur "x"))
    (should-error (pdf-occur-revert-buffer))
    (should-error (pdf-occur-abort-search))))

;;; Searching several documents

(ert-deftest pdf-occur-several-documents ()
  (let ((other (make-temp-file "pdf-occur-test" nil ".pdf")))
    (unwind-protect
        (pdf-occur-test-with-pdf
          (copy-file "test.pdf" other t)
          (let ((file (buffer-file-name)))
            ;; A buffer and a file name; only page 2 of the copy.
            (pdf-occur-test--search (list (current-buffer) (cons other 2))
                                    "line")
            (with-current-buffer pdf-occur-test--buffer
              (should (equal (mapcar #'car tabulated-list-format)
                             '("Document" "Page" "Line")))
              ;; Documents are kept sorted by name.
              (should (equal (mapcar #'car pdf-occur-search-documents)
                             (sort (list file other) #'string-lessp))))
            (should (equal (pdf-occur-test--header) "3 matches in 2 documents"))
            (should (equal (sort (mapcar (lambda (id)
                                           (list (plist-get id :document)
                                                 (plist-get id :page)))
                                         (pdf-occur-test--ids))
                                 (lambda (a b) (string-lessp (format "%s" a)
                                                             (format "%s" b))))
                           (sort (list (list file 2) (list file 5)
                                       (list other 2))
                                 (lambda (a b) (string-lessp (format "%s" a)
                                                             (format "%s" b))))))
            ;; The document column shows the file's base name.
            (should (cl-every (lambda (line)
                                (string-match-p
                                 (concat "\\`\\(test\\.pdf\\|"
                                         (regexp-quote (file-name-nondirectory other))
                                         "\\) +[25] \\(4\\. underline\\|Outline\\)\\'")
                                 line))
                              (pdf-occur-test--lines)))))
      (delete-file other))))

(ert-deftest pdf-occur-several-documents-without-matches ()
  (let ((other (make-temp-file "pdf-occur-test" nil ".pdf")))
    (unwind-protect
        (pdf-occur-test-with-pdf
          (copy-file "test.pdf" other t)
          ;; Pages 3-4 of the copy have no "line".
          (pdf-occur-test--search (list (current-buffer) (cons other '(3 . 4)))
                                  "line")
          (should (equal (pdf-occur-test--header) "2 matches in 2 documents"))
          ;; A document without matches still gets a line.
          (let ((ids (pdf-occur-test--ids)))
            (should (= 3 (length ids)))
            (let ((empty (cl-find other ids
                                  :key (lambda (id) (plist-get id :document))
                                  :test #'equal)))
              (should empty)
              (should-not (plist-get empty :match-edges))))
          (should (cl-some (lambda (line)
                             (string-match-p "\\[No matches\\]\\'" line))
                           (pdf-occur-test--lines))))
      (delete-file other))))

(ert-deftest pdf-occur-remove-document ()
  (let ((other (make-temp-file "pdf-occur-test" nil ".pdf")))
    (unwind-protect
        (pdf-occur-test-with-pdf
          (copy-file "test.pdf" other t)
          (pdf-occur-test--search (list (current-buffer) other) "line")
          (should (equal (pdf-occur-test--header) "4 matches in 2 documents"))
          (with-current-buffer pdf-occur-test--buffer
            (goto-char (point-min))
            (while (not (equal (plist-get (tabulated-list-get-id) :document)
                               other))
              (forward-line))
            (cl-letf (((symbol-function 'tablist-yes-or-no-p)
                       (lambda (&rest _) t)))
              (pdf-occur-tablist-do-delete))
            (should (= pdf-occur-number-of-matches 2))
            (should (equal (mapcar #'car pdf-occur-search-documents)
                           (list (buffer-file-name
                                  (get-file-buffer "test.pdf")))))
            (should-not (cl-find other (pdf-occur-test--ids)
                                 :key (lambda (id) (plist-get id :document))
                                 :test #'equal))))
      (delete-file other))))

;;; Visiting matches

(ert-deftest pdf-occur-goto-occurrence ()
  (pdf-occur-test-with-displayed-pdf
    (let ((pdf-buffer (current-buffer)))
      (pdf-occur "line")
      (pdf-occur-test--wait)
      (select-window (get-buffer-window pdf-occur-test--buffer))
      (goto-char (point-min))
      (forward-line 1)
      (pdf-occur-goto-occurrence)
      (should (eq (current-buffer) pdf-buffer))
      (should (= (pdf-view-current-page) 5))
      ;; The match is highlighted and scrolled into view.
      (let ((edges (pdf-util-scale-relative-to-pixel
                    (alist-get 'edges (car (pdf-info-search-string "line" 5))))))
        (should (equal (assq 'focus pdf-occur-test--drawn)
                       (list 'focus edges)))
        (should (equal (assq 'hl pdf-occur-test--drawn)
                       (list 'hl edges nil t)))))))

(ert-deftest pdf-occur-view-occurrence ()
  (pdf-occur-test-with-displayed-pdf
    (let ((pdf-window (selected-window)))
      (pdf-occur "line")
      (pdf-occur-test--wait)
      (let ((occur-window (get-buffer-window pdf-occur-test--buffer)))
        (select-window occur-window)
        (goto-char (point-min))
        (pdf-occur-view-occurrence)
        ;; The occur window stays selected.
        (should (eq (selected-window) occur-window))
        (with-selected-window pdf-window
          (should (= (pdf-view-current-page) 2)))))))

(ert-deftest pdf-occur-next-error ()
  (pdf-occur-test-with-displayed-pdf
    (pdf-occur "line")
    (pdf-occur-test--wait)
    (next-error 1 t)
    (should (= (pdf-view-current-page) 2))
    (next-error 1)
    (should (= (pdf-view-current-page) 5))
    (should-error (next-error 1))
    (should (= (pdf-view-current-page) 5))))

;;; Helpers

(ert-deftest pdf-occur-create-entry ()
  (let ((pdf-occur-search-documents '(("/a/one.pdf"))))
    (should (equal (pdf-occur-create-entry "/a/one.pdf" 3 '("two\nlines" (0 0 1 1)))
                   (list '(:document "/a/one.pdf" :page 3
                                     :match-text "two\nlines"
                                     :match-edges ((0 0 1 1)))
                         ;; Line breaks are shown as \n.
                         (vector "3" "two\\nlines")))))
  (let* ((pdf-occur-search-documents '(("/a/one.pdf") ("/b/two.pdf")))
         (entry (pdf-occur-create-entry "/b/two.pdf" 1)))
    (should (equal (car entry) '(:document "/b/two.pdf" :page 1
                                           :match-text nil :match-edges nil)))
    (should (equal (cadr entry) (vector "two.pdf" "" "[No matches]")))
    (should (eq (get-text-property 0 'face (aref (cadr entry) 2))
                'font-lock-warning-face))))

(ert-deftest pdf-occur-create-batches ()
  (pdf-occur-test-with-pdf
    (let ((file (buffer-file-name)))
      (should (equal (pdf-occur-create-batches (list (list file)) 4)
                     `((,file (1 . 4)) (,file (5 . 6)))))
      (should (equal (pdf-occur-create-batches (list (list file)) 16)
                     `((,file (1 . 6)))))
      (should (equal (pdf-occur-create-batches
                      (list (cons file 3) (cons "x.pdf" '(2 . 7))) 3)
                     `((,file (3 . 3)) ("x.pdf" (2 . 4)) ("x.pdf" (5 . 7))))))))

(ert-deftest pdf-occur-normalize-documents ()
  (pdf-occur-test-with-pdf
    (let ((file (buffer-file-name))
          (scratch (get-buffer-create " *pdf-occur-test*")))
      (unwind-protect
          (should (equal (pdf-occur-normalize-documents
                          (list (cons "zz.pdf" 2) (current-buffer) scratch))
                         ;; Buffers visiting files become file names,
                         ;; names are expanded and everything is sorted.
                         (list (list scratch)
                               (list file)
                               (cons (expand-file-name "zz.pdf") 2))))
        (kill-buffer scratch)))))

(ert-deftest pdf-occur-abbrev-document ()
  (should (equal (pdf-occur-abbrev-document "/a/b/doc.pdf") "doc.pdf"))
  (should (equal (pdf-occur-abbrev-document "/a/b/") "/a/b/"))
  (with-temp-buffer
    (should (equal (pdf-occur-abbrev-document (current-buffer))
                   (buffer-name)))))

(ert-deftest pdf-occur-want-regexp-search-p ()
  (let ((current-prefix-arg nil) (pdf-occur-prefer-string-search nil))
    (should (pdf-occur-want-regexp-search-p)))
  (let ((current-prefix-arg '(4)) (pdf-occur-prefer-string-search nil))
    (should-not (pdf-occur-want-regexp-search-p)))
  (let ((current-prefix-arg nil) (pdf-occur-prefer-string-search t))
    (should-not (pdf-occur-want-regexp-search-p)))
  (let ((current-prefix-arg '(4)) (pdf-occur-prefer-string-search t))
    (should (pdf-occur-want-regexp-search-p))))

(provide 'pdf-occur-test)

;;; pdf-occur-test.el ends here
