;; -*- lexical-binding: t -*-

(require 'pdf-outline)
(require 'ert)

;; Tests for pdf-outline.el.  The outline of test.pdf is
;;
;;   1 Text          (page 1)
;;   2 Annotations   (page 2)
;;   3 Links         (page 3)
;;   4 Attachments   (page 4)
;;   5 Outline       (page 5)
;;     5.1 subsection     (page 5)
;;       5.1.1 subsubsection (page 5)
;;
;; and page 6 is empty.  Rendering pages and showing tooltips need a
;; window system, so where the PDF has to be shown in a window, page
;; display is stubbed out, the image size is fixed and the tooltip
;; arrow is recorded instead of shown.

(defconst pdf-outline-test--lines
  '("Text (1)"
    "Annotations (2)"
    "Links (3)"
    "Attachments (4)"
    "Outline (5)"
    "  subsection (5)"
    "    subsubsection (5)")
  "The lines of the outline buffer for test.pdf.")

(defconst pdf-outline-test--buffer-name "*Outline test.pdf*"
  "The name of the outline buffer for test.pdf.")

(defvar pdf-outline-test--arrows nil
  "Arguments passed to the stubbed `pdf-util-tooltip-arrow'.")

(defmacro pdf-outline-test-with-pdf (&rest body)
  "Run BODY in a buffer visiting test.pdf; kill its outline buffer after."
  (declare (indent 0) (debug t))
  `(pdf-test-with-test-pdf-view
     (unwind-protect
         (progn ,@body)
       (when (get-buffer pdf-outline-test--buffer-name)
         (kill-buffer pdf-outline-test--buffer-name)))))

(defmacro pdf-outline-test-with-displayed-pdf (&rest body)
  "Run BODY in test.pdf, shown on page 1 in the only window."
  (declare (indent 0) (debug t))
  `(pdf-outline-test-with-pdf
     (cl-letf (((symbol-function 'pdf-view-display-page) #'ignore)
               ((symbol-function 'pdf-view-image-size)
                (lambda (&rest _) '(612 . 792)))
               ((symbol-function 'pdf-util-tooltip-arrow)
                (lambda (top &rest _) (push top pdf-outline-test--arrows))))
       (setq pdf-outline-test--arrows nil)
       (switch-to-buffer (current-buffer))
       (delete-other-windows)
       (pdf-view-goto-page 1)
       (unwind-protect
           (progn ,@body)
         (delete-other-windows)))))

(defun pdf-outline-test--goto-item (title)
  "Move point to the start of the outline line for TITLE."
  (goto-char (point-min))
  (re-search-forward (concat "^ *" (regexp-quote title) " ("))
  (back-to-indentation))

;;; The outline buffer

(ert-deftest pdf-outline-noselect-creates-buffer ()
  (pdf-outline-test-with-pdf
    (let* ((pdf-buffer (current-buffer))
           (buffer (pdf-outline-noselect)))
      (should (equal (buffer-name buffer) "*Outline test.pdf*"))
      (with-current-buffer buffer
        (should (derived-mode-p 'pdf-outline-buffer-mode))
        (should buffer-read-only)
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (concat (mapconcat #'identity pdf-outline-test--lines "\n")
                               "\n")))
        (should (equal pdf-outline-pdf-document
                       (buffer-file-name pdf-buffer)))
        (should (eq other-window-scroll-buffer pdf-buffer)))
      ;; A second call reuses the buffer rather than rebuilding it.
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "marker")))
      (should (eq (pdf-outline-noselect pdf-buffer) buffer))
      (with-current-buffer buffer
        (should (string-suffix-p "marker" (buffer-string)))))))

(ert-deftest pdf-outline-noselect-requires-pdf-buffer ()
  (with-temp-buffer
    (should-error (pdf-outline-noselect))))

(ert-deftest pdf-outline-noselect-without-outline ()
  (pdf-outline-test-with-pdf
    (cl-letf (((symbol-function 'pdf-info-outline) (lambda (&rest _) nil)))
      (should (equal (cadr (should-error (pdf-outline-noselect)))
                     "PDF has no outline"))
      (should-not (get-buffer pdf-outline-test--buffer-name)))))

(ert-deftest pdf-outline-buffer-levels ()
  (pdf-outline-test-with-pdf
    (with-current-buffer (pdf-outline-noselect)
      (goto-char (point-min))
      (let (levels)
        (while (not (eobp))
          (should (looking-at outline-regexp))
          (push (funcall outline-level) levels)
          (forward-line))
        (should (equal (nreverse levels) '(1 1 1 1 1 2 3)))))))

(ert-deftest pdf-outline-buffer-indent ()
  (pdf-outline-test-with-pdf
    (let ((pdf-outline-buffer-indent 4))
      (with-current-buffer (pdf-outline-noselect)
        (should (equal (split-string (buffer-substring-no-properties
                                      (point-min) (point-max))
                                     "\n" t)
                       '("Text (1)" "Annotations (2)" "Links (3)"
                         "Attachments (4)" "Outline (5)"
                         "    subsection (5)"
                         "        subsubsection (5)")))
        ;; Levels follow the indent.
        (pdf-outline-test--goto-item "subsubsection")
        (beginning-of-line)
        (looking-at outline-regexp)
        (should (= 3 (funcall outline-level)))))))

(ert-deftest pdf-outline-buffer-labels ()
  (pdf-outline-test-with-pdf
    (let ((pdf-outline-display-labels t))
      (cl-letf (((symbol-function 'pdf-info-pagelabels)
                 (lambda (&rest _) '("i" "ii" "iii" "iv" "v" "vi"))))
        (with-current-buffer (pdf-outline-noselect)
          (should (equal (car (split-string (buffer-string) "\n"))
                         "Text (i)"))
          (pdf-outline-test--goto-item "Links")
          (should (looking-at "Links (iii)$")))))))

(ert-deftest pdf-outline-link-at-pos ()
  (pdf-outline-test-with-pdf
    (let ((outline (pdf-info-outline)))
      (with-current-buffer (pdf-outline-noselect)
        (goto-char (point-min))
        (dolist (item outline)
          (should (equal (pdf-outline-link-at-pos) item))
          ;; Also at the end of the line (just after the button).
          (should (equal (pdf-outline-link-at-pos (line-end-position)) item))
          (forward-line))
        (should (eobp))
        (should-not (pdf-outline-link-at-pos))))))

(ert-deftest pdf-outline-help-echo ()
  (pdf-outline-test-with-pdf
    (with-current-buffer (pdf-outline-noselect)
      (pdf-outline-test--goto-item "Attachments")
      (should (equal (get-char-property (point) 'help-echo)
                     "Goto page 4 (Attachments)")))))

(ert-deftest pdf-outline-end-of-buffer ()
  (pdf-outline-test-with-pdf
    (with-current-buffer (pdf-outline-noselect)
      (goto-char (point-min))
      (pdf-outline-end-of-buffer)
      ;; Stops on the last item, not on the empty line after it.
      (should (equal (alist-get 'title (pdf-outline-link-at-pos))
                     "subsubsection"))
      (should (= (mark t) (point-min))))))

(ert-deftest pdf-outline-up-heading ()
  (pdf-outline-test-with-pdf
    (with-current-buffer (pdf-outline-noselect)
      (pdf-outline-test--goto-item "subsubsection")
      (let ((start (point)))
        (pdf-outline-up-heading 1)
        (should (equal (alist-get 'title (pdf-outline-link-at-pos))
                       "subsection"))
        (should (= (mark t) start))
        (pdf-outline-up-heading 1)
        (should (equal (alist-get 'title (pdf-outline-link-at-pos))
                       "Outline"))))))

;;; Moving to a page's item

(ert-deftest pdf-outline-move-to-page ()
  (pdf-outline-test-with-pdf
    (with-current-buffer (pdf-outline-noselect)
      (dolist (spec '((1 . "Text") (2 . "Annotations") (3 . "Links")
                      (4 . "Attachments") (5 . "Outline")))
        (goto-char (point-max))
        (pdf-outline-move-to-page (car spec))
        (should (equal (alist-get 'title (pdf-outline-link-at-pos))
                       (cdr spec)))
        ;; Point is on the title, after the indentation.
        (should (= (point) (save-excursion (back-to-indentation) (point))))))))

(ert-deftest pdf-outline-move-to-page-after-last-item ()
  ;; A page after the last item belongs to the last item's section.
  (pdf-outline-test-with-pdf
    (with-current-buffer (pdf-outline-noselect)
      (goto-char (point-min))
      (pdf-outline-move-to-page 6)
      (should (pdf-outline-link-at-pos)))))

(ert-deftest pdf-outline-move-to-current-page ()
  (pdf-outline-test-with-displayed-pdf
    (pdf-view-goto-page 4)
    (pdf-outline nil t)
    (with-current-buffer pdf-outline-test--buffer-name
      (goto-char (point-min))
      (pdf-outline-move-to-current-page)
      (should (equal (alist-get 'title (pdf-outline-link-at-pos))
                     "Attachments")))))

;;; Following items

(ert-deftest pdf-outline-command-selects-outline-window ()
  (pdf-outline-test-with-displayed-pdf
    (let ((pdf-buffer (current-buffer))
          (pdf-window (selected-window)))
      (pdf-outline)
      (should (eq (window-buffer) (get-buffer pdf-outline-test--buffer-name)))
      (should (eq pdf-outline-pdf-window pdf-window))
      (should (eq (window-buffer pdf-window) pdf-buffer))
      ;; With a prefix argument the PDF window stays selected.
      (select-window pdf-window)
      (pdf-outline nil t)
      (should (eq (selected-window) pdf-window)))))

(ert-deftest pdf-outline-follow-link ()
  (pdf-outline-test-with-displayed-pdf
    (let ((pdf-buffer (current-buffer))
          (pdf-window (selected-window)))
      (pdf-outline)
      (pdf-outline-test--goto-item "Links")
      (pdf-outline-follow-link)
      (should (eq (selected-window) pdf-window))
      (should (eq (current-buffer) pdf-buffer))
      (should (= (pdf-view-current-page) 3))
      ;; The arrow marks the section heading.
      (should (equal pdf-outline-test--arrows
                     (list (alist-get 'top (nth 2 (pdf-info-outline)))))))))

(ert-deftest pdf-outline-follow-link-nothing-here ()
  (pdf-outline-test-with-displayed-pdf
    (pdf-outline)
    (goto-char (point-max))
    (should-error (pdf-outline-follow-link))
    (should (eq (window-buffer) (get-buffer pdf-outline-test--buffer-name)))))

(ert-deftest pdf-outline-display-link-keeps-outline-selected ()
  (pdf-outline-test-with-displayed-pdf
    (let ((pdf-window (selected-window)))
      (pdf-outline)
      (let ((outline-window (selected-window)))
        (pdf-outline-test--goto-item "subsection")
        (pdf-outline-display-link)
        (should (eq (selected-window) outline-window))
        (with-selected-window pdf-window
          (should (= (pdf-view-current-page) 5)))))))

(ert-deftest pdf-outline-follow-link-and-quit ()
  (pdf-outline-test-with-displayed-pdf
    (let ((pdf-window (selected-window)))
      (pdf-outline)
      (pdf-outline-test--goto-item "Annotations")
      (pdf-outline-follow-link-and-quit)
      (should (eq (selected-window) pdf-window))
      (should-not (get-buffer-window pdf-outline-test--buffer-name))
      (should (= (pdf-view-current-page) 2)))))

(ert-deftest pdf-outline-quit-and-kill ()
  (pdf-outline-test-with-displayed-pdf
    (let ((pdf-window (selected-window)))
      (pdf-outline)
      (pdf-outline-quit-and-kill)
      (should (eq (selected-window) pdf-window))
      (should-not (get-buffer pdf-outline-test--buffer-name)))))

;;; Imenu

(ert-deftest pdf-outline-treeify ()
  (let* ((item (lambda (title depth) `((depth . ,depth) (title . ,title))))
         (a (funcall item "a" 1)) (a1 (funcall item "a1" 2))
         (a1x (funcall item "a1x" 3)) (a2 (funcall item "a2" 2))
         (b (funcall item "b" 1)) (c (funcall item "c" 1)))
    (should-not (pdf-outline-treeify-outline-list nil))
    (should (equal (pdf-outline-treeify-outline-list (list a b))
                   (list a b)))
    (should (equal (pdf-outline-treeify-outline-list (list a a1 a1x a2 b c))
                   `((,a (,a1 ,a1x) ,a2) ,b ,c)))))

(ert-deftest pdf-outline-imenu-index-tree ()
  (pdf-outline-test-with-pdf
    (let ((index (pdf-outline-imenu-create-index-tree))
          (outline (pdf-info-outline)))
      (should (equal (mapcar #'car index)
                     '("Text (1)" "Annotations (2)" "Links (3)"
                       "Attachments (4)" "Outline")))
      ;; Leaf items are imenu "special" items: (NAME POS FUNCTION LINK).
      (should (equal (car index)
                     (list "Text (1)" 0 'pdf-outline-imenu-activate-link
                           (car outline))))
      ;; A node with children is a sub-menu, which starts with the
      ;; node's own item.
      (let ((sub (nth 4 index)))
        (should (equal (mapcar #'car (cdr sub))
                       '("Outline (5)" "subsection")))
        (should (equal (mapcar #'car (cdr (nth 2 sub)))
                       '("subsection (5)" "subsubsection (5)")))))))

(ert-deftest pdf-outline-imenu-index-flat ()
  (pdf-outline-test-with-pdf
    (let ((index (pdf-outline-imenu-create-index-flat)))
      (should (equal (mapcar #'car index)
                     (mapcar (lambda (l) (string-trim-left l))
                             pdf-outline-test--lines)))
      (should (cl-every (lambda (e) (eq (nth 2 e)
                                        'pdf-outline-imenu-activate-link))
                        index)))))

(ert-deftest pdf-outline-imenu-labels ()
  (pdf-outline-test-with-pdf
    (let ((pdf-outline-display-labels t))
      (cl-letf (((symbol-function 'pdf-info-pagelabels)
                 (lambda (&rest _) '("i" "ii" "iii" "iv" "v" "vi"))))
        (should (equal (car (nth 2 (pdf-outline-imenu-create-index-flat)))
                       "Links (iii)"))))))

(ert-deftest pdf-outline-imenu-activate-link ()
  (pdf-outline-test-with-displayed-pdf
    (let ((index (pdf-outline-imenu-create-index-flat)))
      (imenu (assoc "Attachments (4)" index))
      (should (= (pdf-view-current-page) 4))
      ;; Also when called the way older imenu versions did, with the
      ;; function name in front of the link.
      (let ((item (assoc "Links (3)" index)))
        (apply #'pdf-outline-imenu-activate-link
               (car item) (cadr item) (cddr item)))
      (should (= (pdf-view-current-page) 3)))))

(ert-deftest pdf-outline-imenu-split-menu-keeps-order ()
  (pdf-outline-test-with-pdf
    (let* ((index (pdf-outline-imenu-create-index-tree))
           (menu (imenu--split-menu index "PDF Outline")))
      (should (equal (car menu) "PDF Outline"))
      ;; The sub-menu "Outline" stays last instead of being moved to
      ;; the top, and nothing is sorted.
      (should (equal (mapcar #'car (cdr menu))
                     (mapcar #'car index))))
    (let ((imenu-max-items 2)
          (index (pdf-outline-imenu-create-index-flat)))
      (should (equal (mapcar #'car (cdr (imenu--split-menu index "PDF Outline")))
                     '("From: Text (1)" "From: Links (3)" "From: Outline (5)"
                       "From: subsubsection (5)"))))))

(ert-deftest pdf-outline-minor-mode-imenu ()
  (pdf-outline-test-with-pdf
    (pdf-outline-minor-mode 1)
    (should (eq imenu-create-index-function
                'pdf-outline-imenu-create-index-tree))
    (pdf-outline-minor-mode -1)
    (should-not imenu-create-index-function)
    (let ((pdf-outline-imenu-use-flat-menus t))
      (pdf-outline-minor-mode 1)
      (should (eq imenu-create-index-function
                  'pdf-outline-imenu-create-index-flat))
      (pdf-outline-minor-mode -1))))

(provide 'pdf-outline-test)

;;; pdf-outline-test.el ends here
