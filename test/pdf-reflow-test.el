;; -*- lexical-binding: t -*-

;; Fixtures: reflow.pdf is a tagged PDF/UA-2 document built from
;; reflow.tex; a11y.pdf is described in pdf-a11y-test.el;
;; a11y-untagged.pdf and test.pdf have no structure tree.

(require 'pdf-reflow)
(require 'ert)

(defmacro pdf-reflow-test-with-reflow (pdf &rest body)
  "Run BODY in the reflow buffer of PDF."
  (declare (indent 1) (debug t))
  `(pdf-test-with-pdf ,pdf
     (pdf-view-mode)
     (let ((reflow (pdf-reflow)))
       (unwind-protect
           (with-current-buffer reflow ,@body)
         (kill-buffer reflow)))))

(defun pdf-reflow-test-headings ()
  "Return the headings of the current reflow buffer as (LEVEL . TEXT)."
  (let ((pos (point-min)) result)
    (while (setq pos (text-property-not-all pos (point-max)
                                            'pdf-reflow-heading nil))
      (let ((end (or (next-single-property-change pos 'pdf-reflow-heading)
                     (point-max))))
        (push (cons (get-text-property pos 'pdf-reflow-heading)
                    (string-trim (buffer-substring-no-properties pos end)))
              result)
        (setq pos end)))
    (nreverse result)))

(defun pdf-reflow-test-page-of (string)
  "Return the page of the block containing STRING in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (should (search-forward string nil t))
    (pdf-reflow--block-page (match-beginning 0))))

(ert-deftest pdf-reflow-tree ()
  (let ((tree (pdf-reflow--tree
               '(((depth . 1) (kind . document))
                 ((depth . 2) (kind . heading-1))
                 ((depth . 3) (kind . span))
                 ((depth . 2) (kind . paragraph))
                 ((depth . 1) (kind . part))))))
    (should (equal '((document (heading-1 (span)) (paragraph)) (part))
                   (cl-labels ((kinds (node)
                                 (cons (pdf-reflow--kind node)
                                       (mapcar #'kinds (cdr node)))))
                     (mapcar #'kinds tree))))))

(ert-deftest pdf-reflow-paragraphs ()
  (let ((pdf-reflow-paragraph-line-ratio 0.8))
    (should (equal '("A long first line of text that goes on and the next \
line which is long, too."
                     "A new paragraph, its first line is long and its last \
line ends with a broken word."
                     "Short."
                     "A last paragraph.")
                   (pdf-reflow--paragraphs
                    "A long first line of text that goes on
and the next line which is long, too.

A new paragraph, its first line is long
and its last line ends with a bro-
ken word.
Short.
A last paragraph.")))))

(ert-deftest pdf-reflow-list-labels ()
  (should (equal '("1." "2.") (list (pdf-reflow--list-label 'decimal 0)
                                    (pdf-reflow--list-label 'decimal 1))))
  (should (equal "iv." (pdf-reflow--list-label 'lower-roman 3)))
  (should (equal "C." (pdf-reflow--list-label 'upper-alpha 2)))
  (should (equal "•" (pdf-reflow--list-label nil 0))))

(ert-deftest pdf-reflow-structure-full ()
  (skip-unless (pdf-info-structure-tree-p))
  (pdf-test-with-pdf "reflow.pdf"
    (let* ((full (pdf-info-structure nil t))
           (short (pdf-info-structure))
           (paragraph (cl-find-if (lambda (e)
                                    (string-prefix-p "This paragraph"
                                                     (alist-get 'text e)))
                                  full)))
      (should (= (length full) (length short)))
      ;; Without FULL the records are as before.
      (should-not (assq 'list-numbering (car short)))
      (should (cl-every (lambda (e) (<= (length (alist-get 'text e)) 120))
                        short))
      ;; With it, the whole text, and not read as PDFDocEncoding.
      (should paragraph)
      (should (string-suffix-p "café, naïve, Straße." (alist-get 'text paragraph)))
      (should (assq 'list-numbering paragraph))
      (should (equal '(none none none none)
                     (mapcar (lambda (e) (alist-get 'list-numbering e))
                             (cl-remove 'list-item full
                                        :key (lambda (e) (alist-get 'kind e))
                                        :test-not #'eq))))
      (should (equal '("•" "•" "1." "2.")
                     (mapcar (lambda (e) (alist-get 'text e))
                             (cl-remove 'list-label short
                                        :key (lambda (e) (alist-get 'kind e))
                                        :test-not #'eq))))
      ;; A figure adds no text to the paragraph it is in.
      (should (equal '("")
                     (cl-loop for (e next) on full
                              when (eq 'figure (alist-get 'kind next))
                              collect (alist-get 'text e)))))))

(ert-deftest pdf-reflow-tagged ()
  (skip-unless (pdf-info-structure-tree-p))
  (pdf-reflow-test-with-reflow "reflow.pdf"
    (should (eq major-mode 'pdf-reflow-mode))
    (should buffer-read-only)
    (should visual-line-mode)
    (goto-char (point-min))
    (should (search-forward "structure tree" nil t))
    (should (equal '((1 . "1 Reading order") (2 . "1.1 Lists")
                     (3 . "1.1.1 A table") (1 . "2 Second page"))
                   (pdf-reflow-test-headings)))
    (should (equal 'pdf-reflow-heading-2
                   (save-excursion
                     (goto-char (point-min))
                     (search-forward "1.1 Lists")
                     (get-text-property (match-beginning 0) 'face))))
    ;; Paragraphs are one line each, for `visual-line-mode' to wrap.
    (should (string-match-p
             "^This paragraph is long enough .* café, naïve, Straße\\.$"
             (buffer-string)))
    (should (string-match-p "^• The first point\\.\n• The second point\\.$"
                            (buffer-string)))
    (should (string-match-p "^1\\. Step one\\.\n2\\. Step two\\.$"
                            (buffer-string)))
    (should (string-match-p "^| alpha | 1     |$" (buffer-string)))
    (should (string-match-p "^\\[Figure: A grey placeholder rectangle\\]$"
                            (buffer-string)))
    ;; Every block knows its page.
    (should (= 1 (pdf-reflow-test-page-of "1.1 Lists")))
    (should (= 2 (pdf-reflow-test-page-of "Text on the second page.")))
    (should (= 1 (pdf-reflow-test-page-of "[Figure: A grey")))
    (pdf-reflow-goto-page 2)
    (should (looking-at "2 Second page"))
    (goto-char (point-min))
    (pdf-reflow-next-heading 2)
    (should (looking-at "1\\.1 Lists"))
    (pdf-reflow-next-block)
    (should (looking-at "A bulleted list"))
    (pdf-reflow-previous-block)
    (should (looking-at "1\\.1 Lists"))
    (should (equal "1 Reading order" (caar (pdf-reflow--imenu-index))))
    (when (fboundp 'outline-search-text-property)
      (should outline-minor-mode)
      (goto-char (point-min))
      (outline-next-heading)
      (should (looking-at "1 Reading order"))
      (should (= 1 (funcall outline-level))))
    (let ((text (buffer-string)))
      (revert-buffer)
      (should (equal text (buffer-string))))))

(ert-deftest pdf-reflow-tagged-captions ()
  (skip-unless (pdf-info-structure-tree-p))
  (pdf-reflow-test-with-reflow "a11y.pdf"
    (should (string-match-p "^Figure 1: A figure with good alternative text\\.$"
                            (buffer-string)))
    (should (string-match-p "^\\[Figure: example-image_a\\.png\\]$"
                            (buffer-string)))))

(ert-deftest pdf-reflow-untagged ()
  (pdf-reflow-test-with-reflow "a11y-untagged.pdf"
    (goto-char (point-min))
    (should (re-search-forward "not tagged.*guess" nil t))
    (should (search-forward "Page 1 of 1" nil t))
    (should (search-forward
             "This page has no structure tree and its body font is not embedded."
             nil t))
    (should (= 1 (pdf-reflow--block-page (match-beginning 0)))))
  (pdf-reflow-test-with-reflow "test.pdf"
    (should (= 6 (length (pdf-reflow-test-headings))))
    (pdf-reflow-goto-page 3)
    (should (looking-at "Page 3 of 6"))
    (should (= 2 (pdf-reflow-test-page-of "strikeout")))))

(ert-deftest pdf-reflow-text-edges ()
  (skip-unless (pdf-info-structure-tree-p))
  (pdf-test-with-pdf "reflow.pdf"
    (let* ((paragraph (cl-find-if (lambda (e)
                                    (string-prefix-p "This paragraph"
                                                     (alist-get 'text e)))
                                  (pdf-info-structure nil t)))
           (edges (pdf-a11y--text-edges paragraph (current-buffer))))
      ;; The whole paragraph, over several lines.
      (should (> (length edges) 1))
      (should (cl-every #'pdf-test-relative-edges-p edges)))))
