;; -*- lexical-binding: t -*-

(require 'pdf-view)
(require 'ert)
(require 'cl-lib)

;; Tests for pdf-view-register struct and cl-defmethod implementations.
;; These tests use mock bookmark data because the actual pdf-view-registerv-make
;; function requires a window context which isn't available in batch mode.

(ert-deftest pdf-view-register-struct-creation ()
  "Test that pdf-view-register struct can be created."
  (let* ((mock-bookmark '("test.pdf"
                          (filename . "/path/to/test.pdf")
                          (page . 1)
                          (origin . (0.0 . 0.0))
                          (handler . pdf-view-bookmark-jump-handler)))
         (reg (pdf-view-register--make mock-bookmark)))
    ;; Should return non-nil
    (should reg)
    ;; Should be a pdf-view-register struct
    (should (pdf-view-register-p reg))
    ;; Should store the bookmark
    (should (equal (pdf-view-register-bookmark reg) mock-bookmark))))

(ert-deftest pdf-view-register-val-describe ()
  "Test that register-val-describe works for PDF register entries."
  (let* ((mock-bookmark '("test.pdf"
                          (filename . "/path/to/test.pdf")
                          (page . 5)
                          (origin . (0.0 . 0.5))
                          (handler . pdf-view-bookmark-jump-handler)))
         (reg (pdf-view-register--make mock-bookmark)))
    ;; register-val-describe should return a non-empty string
    (should (stringp
             (with-output-to-string
               (register-val-describe reg nil))))
    ;; The description should mention the page number
    (should (string-match-p "page 5"
             (with-output-to-string
               (register-val-describe reg nil))))))

(ert-deftest pdf-view-register-val-insert ()
  "Test that register-val-insert works for PDF register entries."
  (let* ((mock-bookmark '("test.pdf"
                          (filename . "/path/to/test.pdf")
                          (page . 1)
                          (handler . pdf-view-bookmark-jump-handler)))
         (reg (pdf-view-register--make mock-bookmark)))
    ;; register-val-insert should insert text without error
    (with-temp-buffer
      (register-val-insert reg)
      (should (> (buffer-size) 0)))))

(ert-deftest pdf-view-handle-archived-file ()
  :expected-result :failed
  (skip-unless (executable-find "gzip"))
  (let ((tramp-verbose 0)
        (temp
         (make-temp-file "pdf-test")))
    (unwind-protect
        (progn
          (copy-file "test.pdf" temp t)
          (call-process "gzip" nil nil nil temp)
          (setq temp (concat temp ".gz"))
          (should (numberp (pdf-info-number-of-pages temp)))))
    (when (file-exists-p temp)
      (delete-file temp))))

;; Tests for mouse selection.  Batch Emacs cannot display images, so
;; the display side is stubbed: the page image is taken to be 1000x1000
;; pixels, which makes pixel positions map to relative ones by /1000.

(defun pdf-view-test--posn (x y)
  "Return a mouse position at pixel X, Y on the page image."
  (list (selected-window) 1 (cons x y) 0 nil 1 '(0 . 0)
        '(image :type png) (cons x y) '(1000 . 1000)))

(defun pdf-view-test--event (type x y &optional count)
  "Return a mouse event of TYPE at pixel X, Y with click COUNT."
  (if count
      (list type (pdf-view-test--posn x y) count)
    (list type (pdf-view-test--posn x y))))

(defmacro pdf-view-test--with-mouse (events &rest body)
  "Run BODY with the page display stubbed and EVENTS pending.
EVENTS are the events the mouse tracking loop reads after the
command's own down event."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'pdf-view-image-size)
              (lambda (&rest _) '(1000 . 1000)))
             ((symbol-function 'pdf-view-display-region) #'ignore)
             ((symbol-function 'pdf-view-redisplay) #'ignore)
             ((symbol-function 'pdf-util-scroll-to-edges) #'ignore)
             (this-command 'pdf-view-mouse-set-region)
             (unread-command-events ,events))
     ;; Show page 1, without needing a window displaying the image.
     (setq-local image-mode-winprops-alist nil)
     (image-mode-window-put 'page 1 t)
     (unwind-protect
         (progn ,@body)
       (setq unread-command-events nil))))

(ert-deftest pdf-view-mouse-click-leaves-region-inactive ()
  "A single click without dragging must not activate the region (#342)."
  (pdf-test-with-test-pdf
    (pdf-view-test--with-mouse
        (list (pdf-view-test--event 'mouse-1 520 195))
      (pdf-view-mouse-set-region
       (pdf-view-test--event 'down-mouse-1 520 195))
      (should-not (pdf-view-active-region-p))
      (should-not pdf-view-active-region))))

(ert-deftest pdf-view-mouse-drag-sets-region ()
  "Dragging selects the text between the press and the release."
  (pdf-test-with-test-pdf
    (let ((pdf-view-selection-style 'word))
      (pdf-view-test--with-mouse
          (list (pdf-view-test--event 'mouse-movement 590 195)
                (pdf-view-test--event 'mouse-1 590 195))
        (pdf-view-mouse-set-region
         (pdf-view-test--event 'down-mouse-1 480 195))
        (should (pdf-view-active-region-p))
        (should (equal (car pdf-view-active-region) 1))
        (should (= 1 (length (cdr pdf-view-active-region))))
        (should (equal (pdf-view-active-region-text)
                       '("replacement of DocView")))))))

(ert-deftest pdf-view-mouse-click-keeps-extended-region ()
  "A click with the extend modifier keeps the active region (#342)."
  (pdf-test-with-test-pdf
    (let ((pdf-view-selection-style 'word))
      (setq pdf-view-active-region (list 1 '(0.48 0.195 0.59 0.195)))
      (pdf-view-test--with-mouse
          (list (pdf-view-test--event 'mouse-1 250 195))
        (pdf-view-mouse-extend-region
         (pdf-view-test--event 'C-down-mouse-1 250 195))
        (should (equal pdf-view-active-region
                       (list 1 '(0.48 0.195 0.59 0.195))))))))

(defconst pdf-view-test--line
  "PDF Tools is, among other things, a replacement of DocView for PDF files. The"
  "The first line of text on page 1 of test.pdf.
The word \"replacement\" spans x from 477 to 563 pixel in it, at y 195.")

(ert-deftest pdf-view-mouse-double-click-selects-word ()
  "A double click selects the word under the mouse, whatever the style."
  (pdf-test-with-test-pdf
    (dolist (style '(glyph word line))
      (let ((pdf-view-selection-style style))
        (pdf-view-test--with-mouse
            (list (pdf-view-test--event 'double-mouse-1 520 195 2))
          (pdf-view-mouse-set-region
           (pdf-view-test--event 'double-down-mouse-1 520 195 2))
          (should (equal pdf-view-active-region
                         (list 1 '(0.52 0.195 0.52 0.195))))
          (should (eq (pdf-view-region-selection-style) 'word))
          (should (equal (pdf-view-active-region-text)
                         '("replacement"))))))))

(ert-deftest pdf-view-mouse-triple-click-selects-line ()
  "A triple click selects the line under the mouse."
  (pdf-test-with-test-pdf
    (let ((pdf-view-selection-style 'glyph))
      (pdf-view-test--with-mouse
          (list (pdf-view-test--event 'triple-mouse-1 520 195 3))
        (pdf-view-mouse-set-region
         (pdf-view-test--event 'triple-down-mouse-1 520 195 3))
        (should (eq (pdf-view-region-selection-style) 'line))
        (should (equal (pdf-view-active-region-text)
                       (list pdf-view-test--line)))))))

(ert-deftest pdf-view-mouse-double-click-drag-extends-by-words ()
  "Dragging after a double click selects whole words."
  (pdf-test-with-test-pdf
    (let ((pdf-view-selection-style 'glyph))
      (pdf-view-test--with-mouse
          (list (pdf-view-test--event 'mouse-movement 300 195)
                (pdf-view-test--event 'double-mouse-1 300 195 2))
        (pdf-view-mouse-set-region
         (pdf-view-test--event 'double-down-mouse-1 520 195 2))
        (should (equal (pdf-view-active-region-text)
                       '("is, among other things, a replacement")))))))

(ert-deftest pdf-view-mouse-single-click-ignores-style-of-last-region ()
  "A drag after a double click selects by `pdf-view-selection-style' again."
  (pdf-test-with-test-pdf
    (let ((pdf-view-selection-style 'glyph))
      (pdf-view-test--with-mouse
          (list (pdf-view-test--event 'double-mouse-1 520 195 2))
        (pdf-view-mouse-set-region
         (pdf-view-test--event 'double-down-mouse-1 520 195 2)))
      (pdf-view-test--with-mouse
          (list (pdf-view-test--event 'mouse-movement 540 195)
                (pdf-view-test--event 'mouse-1 540 195))
        (pdf-view-mouse-set-region
         (pdf-view-test--event 'down-mouse-1 500 195)))
      (should (eq (pdf-view-region-selection-style) 'glyph))
      (should (equal (pdf-view-active-region-text) '("lacem"))))))

(ert-deftest pdf-view-mouse-shift-click-extends-region ()
  "A shift click moves the end of the region, keeping its style."
  (pdf-test-with-test-pdf
    (let ((pdf-view-selection-style 'glyph))
      (pdf-view-test--with-mouse
          (list (pdf-view-test--event 'double-mouse-1 520 195 2))
        (pdf-view-mouse-set-region
         (pdf-view-test--event 'double-down-mouse-1 520 195 2)))
      (pdf-view-test--with-mouse
          (list (pdf-view-test--event 'S-mouse-1 620 195))
        (pdf-view-mouse-set-region-end
         (pdf-view-test--event 'S-down-mouse-1 620 195)))
      (should (equal pdf-view-active-region
                     (list 1 '(0.52 0.195 0.62 0.195))))
      (should (equal (pdf-view-active-region-text)
                     '("replacement of DocView"))))))

(ert-deftest pdf-view-mouse-shift-click-without-region ()
  "A shift click without an active region behaves like a click."
  (pdf-test-with-test-pdf
    (pdf-view-test--with-mouse
        (list (pdf-view-test--event 'S-mouse-1 620 195))
      (pdf-view-mouse-set-region-end
       (pdf-view-test--event 'S-down-mouse-1 620 195))
      (should-not (pdf-view-active-region-p)))))

(ert-deftest pdf-view-double-click-region-annotation ()
  "A markup annotation made from a double click covers just the word."
  (pdf-test-with-test-pdf
    (let ((pdf-view-selection-style 'glyph)
          (pdf-annot-activate-created-annotations nil)
          ;; The hooks redisplay the page, which needs a window.
          (pdf-annot-inhibit-modification-hooks t))
      (pdf-view-test--with-mouse
          (list (pdf-view-test--event 'double-mouse-1 520 195 2))
        (pdf-view-mouse-set-region
         (pdf-view-test--event 'double-down-mouse-1 520 195 2)))
      (let* ((region (pdf-view-active-region t))
             (annot (pdf-annot-add-annotation
                     'highlight (cdr region) nil (car region)))
             (edges (pdf-annot-get annot 'edges)))
        (should-not (pdf-view-active-region-p))
        ;; The word spans 0.477 to 0.563.
        (should (< 0.47 (nth 0 edges) 0.48))
        (should (< 0.56 (nth 2 edges) 0.57))))))

(ert-deftest pdf-view-sync-double-click-needs-synctex ()
  "Double click is only a SyncTeX search when there is a database."
  (pdf-test-with-test-pdf
    (should-not (pdf-sync--when-synctex 'pdf-sync-backward-search-mouse))
    (let ((pdf-sync-locate-synctex-file-functions
           (list (lambda (_) "/tmp/test.synctex.gz"))))
      (should (eq (pdf-sync--when-synctex 'pdf-sync-backward-search-mouse)
                  'pdf-sync-backward-search-mouse)))))
