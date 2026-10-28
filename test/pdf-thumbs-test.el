;; -*- lexical-binding: t -*-

(require 'pdf-thumbs)
(require 'ert)

;; Tests for pdf-thumbs.el.  Batch Emacs cannot show images, so the
;; grid shows the page labels as text, which exercises the same data
;; model: one cell per page, navigation, marks and page operations.
;; Page operations need qpdf and work on a temporary copy of
;; test.pdf.

(defmacro pdf-thumbs-test-with-grid (&rest body)
  "Run BODY in the grid of a temporary copy of test.pdf.

The grid has 4 columns.  The variable `pdf' is bound to the PDF
buffer and `file' to its file."
  (declare (indent 0) (debug t))
  (let ((dir (make-symbol "dir"))
        (grid (make-symbol "grid")))
    `(let* ((,dir (make-temp-file "pdf-thumbs-test" t))
            (file (expand-file-name "test.pdf" ,dir))
            (pdf-thumbs-columns 4)
            (pdf-pages-confirm nil)
            (pdf-pages--undo-alist nil)
            (pdf-pages-backup-directory (expand-file-name "backups" ,dir)))
       (copy-file "test.pdf" file)
       (unwind-protect
           (save-current-buffer
            (save-window-excursion
             (delete-other-windows)
             (pdf-test-with-pdf file
               ;; The mode may have failed half way while
               ;; `pdf-test-with-pdf' restarted the server.
               (pdf-view-mode)
               (let ((pdf (current-buffer)))
                 (switch-to-buffer pdf)
                 ;; Batch mode sets up the window properties for the
                 ;; window-less case (t); there is a window here.
                 (when (eq image-mode-winprops-alist t)
                   (setq image-mode-winprops-alist nil))
                 ;; Batch mode cannot display the page images, so
                 ;; only the window's page number changes.
                 (cl-letf (((symbol-function 'pdf-view-redisplay) #'ignore)
                           ((symbol-function 'pdf-view-display-page) #'ignore))
                   (let ((,grid (pdf-thumbs)))
                     (unwind-protect
                         (with-current-buffer ,grid ,@body)
                       (when (buffer-live-p ,grid)
                         (kill-buffer ,grid)))))))))
         (delete-directory ,dir t)))))

(defun pdf-thumbs-test-cell-text (page)
  "Return the text shown for PAGE's thumbnail, without properties."
  (substring-no-properties
   (get-text-property (pdf-thumbs--cell page) 'display)))

(defun pdf-thumbs-test-pdf-page (pdf)
  "Return the current page of PDF's window."
  (with-current-buffer pdf
    (pdf-view-current-page (get-buffer-window pdf))))

(ert-deftest pdf-thumbs-test-grid ()
  (pdf-thumbs-test-with-grid
    (should (derived-mode-p 'pdf-thumbs-mode))
    (should (eq pdf-thumbs-pdf-buffer pdf))
    (should (eq (with-current-buffer pdf pdf-thumbs--buffer) (current-buffer)))
    (should (equal (buffer-name) (format "*Thumbs %s*" (buffer-name pdf))))
    ;; No images in batch mode.
    (should-not pdf-thumbs--graphic)
    (should (= pdf-thumbs--npages 6))
    ;; One cell per page, labelled, four to a row.
    (dotimes (i 6)
      (let ((page (1+ i)))
        (should (= (get-text-property (pdf-thumbs--cell page) 'pdf-thumbs-page)
                   page))
        (should (string-match-p (format "\\[ *%d *\\]" page)
                                (pdf-thumbs-test-cell-text page)))))
    (should (= (count-lines (point-min) (point-max)) 2))
    (should (= (line-number-at-pos (pdf-thumbs--cell 4)) 1))
    (should (= (line-number-at-pos (pdf-thumbs--cell 5)) 2))
    ;; The grid starts at the current page, which is highlighted.
    (should (= (pdf-thumbs-page-at-point) 1))
    (should (= pdf-thumbs--current 1))
    (should (memq 'pdf-thumbs-current
                  (get-text-property 0 'face (get-text-property
                                              (pdf-thumbs--cell 1)
                                              'display))))
    ;; Asking again reuses the grid.
    (should (eq (pdf-thumbs-noselect pdf) (current-buffer)))))

(ert-deftest pdf-thumbs-test-labels ()
  (pdf-thumbs-test-with-grid
    (should (equal (pdf-thumbs--label 2) "2"))
    ;; A page label other than the number shows both.
    (aset pdf-thumbs--labels 2 "ii (2)")
    (pdf-thumbs--update-cell 2)
    (should (string-match-p "ii (2)" (pdf-thumbs-test-cell-text 2)))))

(ert-deftest pdf-thumbs-test-navigation ()
  (pdf-thumbs-test-with-grid
    (pdf-thumbs-next)
    (should (= (pdf-thumbs-page-at-point) 2))
    (pdf-thumbs-next 2)
    (should (= (pdf-thumbs-page-at-point) 4))
    (pdf-thumbs-previous)
    (should (= (pdf-thumbs-page-at-point) 3))
    ;; Rows of four; the last row is short.
    (pdf-thumbs-next-row)
    (should (= (pdf-thumbs-page-at-point) 6))
    (pdf-thumbs-previous-row)
    (should (= (pdf-thumbs-page-at-point) 2))
    ;; The first row is short, too.
    (pdf-thumbs-previous-row)
    (should (= (pdf-thumbs-page-at-point) 1))
    (should-error (pdf-thumbs-previous-row) :type 'user-error)
    (should-error (pdf-thumbs-previous) :type 'user-error)
    (pdf-thumbs-last)
    (should (= (pdf-thumbs-page-at-point) 6))
    (should-error (pdf-thumbs-next) :type 'user-error)
    (should-error (pdf-thumbs-next-row) :type 'user-error)
    (pdf-thumbs-first)
    (should (= (pdf-thumbs-page-at-point) 1))
    ;; The keys.
    (should (eq (key-binding (kbd "<right>")) #'pdf-thumbs-next))
    (should (eq (key-binding (kbd "C-n")) #'pdf-thumbs-next-row))
    (should (eq (key-binding (kbd "p")) #'pdf-thumbs-previous-row))
    (should (eq (key-binding (kbd "b")) #'pdf-thumbs-previous))
    (should (eq (key-binding (kbd "RET")) #'pdf-thumbs-show-page))
    (should (eq (key-binding (kbd "q")) #'pdf-thumbs-quit))))

(ert-deftest pdf-thumbs-test-show-page ()
  (pdf-thumbs-test-with-grid
    (let ((grid (current-buffer)))
      (pdf-thumbs-goto-thumbnail 3)
      ;; SPC shows the page and stays in the grid.
      (pdf-thumbs-display-page)
      (should (= (pdf-thumbs-test-pdf-page pdf) 3))
      (should (= pdf-thumbs--current 3))
      (should (eq (window-buffer) grid))
      ;; RET selects the PDF window; the grid stays visible.
      (pdf-thumbs-goto-thumbnail 5)
      (pdf-thumbs-show-page)
      (should (eq (window-buffer) pdf))
      (should (get-buffer-window grid))
      (should (= (pdf-view-current-page) 5))
      (with-current-buffer grid
        (should (= pdf-thumbs--current 5)))
      ;; Paging in the PDF moves the highlight and point in the grid.
      (pdf-view-next-page)
      (with-current-buffer grid
        (should (= pdf-thumbs--current 6))
        (should (= (window-point (get-buffer-window grid)) (pdf-thumbs--cell 6))))
      (set-buffer grid))))

(ert-deftest pdf-thumbs-test-marks ()
  (pdf-thumbs-test-with-grid
    (should (equal (pdf-thumbs-target-pages) '(1)))
    (pdf-thumbs-goto-thumbnail 2)
    (pdf-thumbs-mark)
    (should (= (pdf-thumbs-page-at-point) 3))
    (pdf-thumbs-mark 2)
    (should (equal (pdf-thumbs-marked-pages) '(2 3 4)))
    (should (equal (pdf-thumbs-target-pages) '(2 3 4)))
    (should (string-match-p "\\* 3" (pdf-thumbs-test-cell-text 3)))
    (should (memq 'pdf-thumbs-marked
                  (get-text-property 0 'face (get-text-property
                                              (pdf-thumbs--cell 3)
                                              'display))))
    (pdf-thumbs-goto-thumbnail 3)
    (pdf-thumbs-unmark)
    (should (equal (pdf-thumbs-marked-pages) '(2 4)))
    (should-not (string-match-p "\\*" (pdf-thumbs-test-cell-text 3)))
    (pdf-thumbs-goto-thumbnail 3)
    (pdf-thumbs-unmark-backward)
    (should (equal (pdf-thumbs-marked-pages) '(4)))
    (should (= (pdf-thumbs-page-at-point) 2))
    (pdf-thumbs-toggle-marks)
    (should (equal (pdf-thumbs-marked-pages) '(1 2 3 5 6)))
    (pdf-thumbs-unmark-all)
    (should-not (pdf-thumbs-marked-pages))
    (should (eq (key-binding (kbd "m")) #'pdf-thumbs-mark))
    (should (eq (key-binding (kbd "u")) #'pdf-thumbs-unmark))
    (should (eq (key-binding (kbd "U")) #'pdf-thumbs-unmark-all))))

(ert-deftest pdf-thumbs-test-size ()
  (pdf-thumbs-test-with-grid
    (should (= pdf-thumbs--size pdf-thumbs-size))
    (pdf-thumbs-goto-thumbnail 5)
    (pdf-thumbs-enlarge)
    (should (= pdf-thumbs--size (round (* pdf-thumbs-size pdf-thumbs-size-factor))))
    (should (= (pdf-thumbs-page-at-point) 5))
    (pdf-thumbs-shrink 2)
    (should (< pdf-thumbs--size pdf-thumbs-size))
    (pdf-thumbs-reset-size)
    (should (= pdf-thumbs--size pdf-thumbs-size))
    (pdf-thumbs-shrink 100)
    (should (= pdf-thumbs--size 32))
    (should (eq (key-binding (kbd "+")) #'pdf-thumbs-enlarge))
    (should (eq (key-binding (kbd "-")) #'pdf-thumbs-shrink))))

(ert-deftest pdf-thumbs-test-reflow ()
  (pdf-thumbs-test-with-grid
    (pdf-thumbs-goto-thumbnail 5)
    (pdf-thumbs--layout 2)
    (should (= pdf-thumbs--columns 2))
    (should (= (count-lines (point-min) (point-max)) 3))
    (should (= (line-number-at-pos (pdf-thumbs--cell 5)) 3))
    (should (= (pdf-thumbs-page-at-point) 5))
    (pdf-thumbs-previous-row)
    (should (= (pdf-thumbs-page-at-point) 3))
    ;; Without a fixed number of columns, fit the window.
    (let ((pdf-thumbs-columns nil))
      (should (= (pdf-thumbs--columns-for (selected-window))
                 (max 1 (/ (1- (window-body-width))
                           (1+ (pdf-thumbs--cell-width)))))))))

(ert-deftest pdf-thumbs-test-delete-marked ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-thumbs-test-with-grid
    (let ((grid (current-buffer)))
      (pdf-thumbs-goto-thumbnail 2)
      (pdf-thumbs-mark)
      (pdf-thumbs-next)
      (pdf-thumbs-mark)
      (should (equal (pdf-thumbs-marked-pages) '(2 4)))
      (call-interactively #'pdf-thumbs-delete)
      (should (eq (current-buffer) grid))
      (should (= (pdf-info-number-of-pages file) 4))
      (should (= (with-current-buffer pdf (pdf-cache-number-of-pages)) 4))
      ;; The grid followed the revert.
      (should (= pdf-thumbs--npages 4))
      (should (= (length pdf-thumbs--cells) 5))
      (should-not (pdf-thumbs-marked-pages))
      (should (= (pdf-thumbs-page-at-point) 2))
      (should (= pdf-thumbs--current 2))
      ;; Without marks, the page at point.
      (pdf-thumbs-goto-thumbnail 4)
      (pdf-thumbs-delete (pdf-thumbs-target-pages))
      (should (= pdf-thumbs--npages 3))
      ;; And undo.
      (pdf-thumbs-undo)
      (should (= pdf-thumbs--npages 4)))))

(ert-deftest pdf-thumbs-test-move-rotate-extract ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-thumbs-test-with-grid
    (let ((portrait (pdf-info-pagesize 1 file))
          (extracted (expand-file-name "out.pdf" (file-name-directory file))))
      ;; Rotate the page at point.
      (pdf-thumbs-goto-thumbnail 2)
      (pdf-thumbs-rotate (pdf-thumbs-target-pages) 90)
      (should (equal (pdf-info-pagesize 2 file)
                     (cons (cdr portrait) (car portrait))))
      (pdf-thumbs-goto-thumbnail 2)
      (call-interactively #'pdf-thumbs-rotate-counterclockwise)
      (should (equal (pdf-info-pagesize 2 file) portrait))
      ;; Extract the marked pages; the PDF is unchanged.
      (pdf-thumbs-goto-thumbnail 5)
      (pdf-thumbs-mark)
      (pdf-thumbs-goto-thumbnail 1)
      (pdf-thumbs-mark)
      (let ((buffer (pdf-thumbs-extract (pdf-thumbs-target-pages) extracted)))
        (should (= (pdf-info-number-of-pages extracted) 2))
        (kill-buffer buffer))
      (should (= pdf-thumbs--npages 6))
      (should (equal (pdf-thumbs-marked-pages) '(1 5)))
      ;; Move the marked pages to the end.
      (pdf-thumbs-move (pdf-thumbs-target-pages) 5)
      (should (= pdf-thumbs--npages 6))
      (should (= (with-current-buffer pdf (pdf-view-current-page)) 5))
      (should (= (pdf-thumbs-page-at-point) 5))
      (should-not (pdf-thumbs-marked-pages)))))

(ert-deftest pdf-thumbs-test-follow-revert-and-kill ()
  (pdf-thumbs-test-with-grid
    (let ((grid (current-buffer)))
      (pdf-thumbs-mark)
      (with-current-buffer pdf
        (revert-buffer nil t))
      (should (buffer-live-p grid))
      (should-not (pdf-thumbs-marked-pages))
      (should (= pdf-thumbs--npages 6))
      (pdf-thumbs-quit)
      (should-not (buffer-live-p grid))
      (should-not (with-current-buffer pdf pdf-thumbs--buffer))
      ;; A new grid closes with its PDF buffer.
      (set-buffer pdf)
      (setq grid (pdf-thumbs-noselect))
      (should (buffer-live-p grid))
      (let (kill-buffer-query-functions)
        (kill-buffer pdf))
      (should-not (buffer-live-p grid)))))
