;; -*- lexical-binding: t -*-

;; Fixtures: a11y.pdf is a tagged PDF/UA-2 document built from
;; a11y.tex, a11y-noalt.pdf is derived from it (see a11y.tex),
;; a11y-untagged.pdf has no structure tree and a font that is not
;; embedded.

(require 'pdf-a11y)
(require 'ert)

(defun pdf-a11y-test-kinds (elements)
  (mapcar (lambda (e) (alist-get 'kind e)) elements))

(ert-deftest pdf-a11y-structure ()
  (skip-unless (pdf-info-structure-tree-p))
  (pdf-test-with-pdf "a11y.pdf"
    (let ((elements (pdf-info-structure)))
      (should elements)
      (should (equal '((depth . 1) (kind . document))
                     (list (assq 'depth (car elements))
                           (assq 'kind (car elements)))))
      (should (cl-every (lambda (e)
                          (let-alist e
                            (and (natnump .depth) (symbolp .kind)
                                 (memq .page '(1 2)) (stringp .text)
                                 (or (null .edges)
                                     (pdf-test-relative-edges-p .edges)))))
                        elements))
      (should (equal '("1 Introduction" "1.1 Bad alternative text"
                       "2 Second page")
                     (delq nil (mapcar (lambda (e)
                                         (let-alist e
                                           (and (pdf-a11y-heading-level .kind)
                                                .text)))
                                       elements))))
      (should (equal '("example-image_a.png" "example-image-b.pdf"
                       "A grey placeholder rectangle with the word Image")
                     (delq nil (mapcar (lambda (e)
                                         (let-alist e
                                           (and (eq .kind 'figure) .alt)))
                                       elements)))))))

(ert-deftest pdf-a11y-structure-untagged ()
  (skip-unless (pdf-info-structure-tree-p))
  (pdf-test-with-test-pdf
    (should-not (pdf-info-structure))))

(ert-deftest pdf-a11y-fonts ()
  (pdf-test-with-pdf "a11y-untagged.pdf"
    (let ((fonts (pdf-info-fonts)))
      (should (= 2 (length fonts)))
      (should (cl-every (lambda (f)
                          (let-alist f
                            (and (stringp .name) (symbolp .type))))
                        fonts))
      (let ((times (cl-find "Times-Roman" fonts
                            :key (lambda (f) (alist-get 'name f))
                            :test #'equal)))
        (should times)
        (should-not (alist-get 'embedded times))
        (should (eq 'type1 (alist-get 'type times))))))
  (pdf-test-with-pdf "a11y.pdf"
    (should (cl-every (lambda (f) (alist-get 'embedded f))
                      (pdf-info-fonts)))))

(ert-deftest pdf-a11y-xmp-metadata ()
  (pdf-test-with-pdf "a11y.pdf"
    (should (string-match-p "pdfuaid" (pdf-info-xmp-metadata))))
  (pdf-test-with-pdf "a11y-untagged.pdf"
    (should-not (pdf-info-xmp-metadata))))

(ert-deftest pdf-a11y-alt-text-problem ()
  (should (eq 'missing (pdf-a11y-alt-text-problem nil)))
  (should (eq 'empty (pdf-a11y-alt-text-problem "")))
  (should (eq 'empty (pdf-a11y-alt-text-problem "  \n")))
  (should (eq 'file-name (pdf-a11y-alt-text-problem "example-image-b.pdf")))
  (should (eq 'file-name (pdf-a11y-alt-text-problem "Plot.PNG")))
  (should (eq 'file-name (pdf-a11y-alt-text-problem "fig_3_results")))
  (should (eq 'file-name (pdf-a11y-alt-text-problem "figures/loop")))
  (should (eq 'generic (pdf-a11y-alt-text-problem "Image")))
  (should (eq 'generic (pdf-a11y-alt-text-problem "Figure 3.")))
  (should-not (pdf-a11y-alt-text-problem
               "A loop invariant diagram with three states"))
  (should-not (pdf-a11y-alt-text-problem "Logo of Clemson University")))

(ert-deftest pdf-a11y-heading-outline ()
  (let ((headings (pdf-a11y--headings
                   '(((kind . heading-2) (text . "Intro"))
                     ((kind . paragraph) (text . "Body"))
                     ((kind . heading-1) (text . "Start"))
                     ((kind . heading-3) (text . "Too deep"))
                     ((kind . heading-2) (text . " "))
                     ((kind . heading) (text . "Unnumbered"))))))
    (should (equal '(first-not-h1 nil skipped-level no-text nil)
                   (mapcar (lambda (h) (alist-get 'problem h)) headings)))))

(ert-deftest pdf-a11y-reading-order-entries ()
  (let ((entries (pdf-a11y--reading-order
                  '(((depth . 1) (kind . document) (page . 1))
                    ((depth . 2) (kind . paragraph) (page . 1))
                    ((depth . 3) (kind . span) (page . 1))
                    ((depth . 3) (kind . figure) (page . 1))
                    ((depth . 2) (kind . heading-1) (page . 2))
                    ((depth . 2) (kind . section) (page . 1))
                    ((depth . 3) (kind . caption) (page . 1))))))
    (should (equal '((0 . paragraph) (1 . figure) (0 . heading-1)
                     (0 . caption))
                   (mapcar (lambda (e) (cons (car e) (alist-get 'kind (cdr e))))
                           entries)))
    (should (equal '(nil nil nil 2)
                   (mapcar (lambda (e) (alist-get 'after-page (cdr e)))
                           entries)))))

(ert-deftest pdf-a11y-analyze-tagged ()
  (skip-unless (pdf-info-structure-tree-p))
  (pdf-test-with-pdf "a11y.pdf"
    (let-alist (pdf-a11y-analyze)
      (should (eq t .tagged))
      (should (= 2 .pages))
      (should (equal "en" .language))
      (should (equal "2" .ua-part))
      (should (equal "Accessibility Fixture" .title))
      (should-not .untagged-pages)
      (should-not .fonts)
      (should (equal '(file-name file-name nil)
                     (mapcar (lambda (f) (alist-get 'problem f)) .figures)))
      (should (equal '(heading-1 heading-2 heading-1)
                     (pdf-a11y-test-kinds .headings)))
      (should-not (cl-some (lambda (h) (alist-get 'problem h)) .headings))
      ;; The tagging code puts the figure float after the last page.
      (let ((late (cl-remove-if-not (lambda (e) (alist-get 'after-page (cdr e)))
                                    .reading-order)))
        (should (= 1 (length late)))
        (should (= 2 (alist-get 'after-page (cdar late))))))))

(ert-deftest pdf-a11y-analyze-missing-alt ()
  (skip-unless (pdf-info-structure-tree-p))
  (pdf-test-with-pdf "a11y-noalt.pdf"
    (let-alist (pdf-a11y-analyze)
      (should (equal '(empty missing nil)
                     (mapcar (lambda (f) (alist-get 'problem f)) .figures))))))

(ert-deftest pdf-a11y-analyze-untagged ()
  (pdf-test-with-pdf "a11y-untagged.pdf"
    (let-alist (pdf-a11y-analyze)
      (should-not (eq t .tagged))
      (should-not .language)
      (should-not .figures)
      (should (equal '("Times-Roman")
                     (mapcar (lambda (f) (alist-get 'name f)) .fonts))))))

(ert-deftest pdf-a11y-report ()
  (skip-unless (pdf-info-structure-tree-p))
  (pdf-test-with-pdf "a11y.pdf"
    (let ((report (pdf-a11y-report)))
      (unwind-protect
          (with-current-buffer report
            (should (eq major-mode 'pdf-a11y-report-mode))
            (goto-char (point-min))
            (should (search-forward "Figures (3)" nil t))
            (should (search-forward "[alt text is a file name]" nil t))
            (should (search-forward "Headings in reading order (3)" nil t))
            (should (search-forward "[read after page 2]" nil t))
            ;; Every figure, heading and reading order entry is a button.
            (let ((n 0) (button (next-button (point-min) t)))
              (while button
                (cl-incf n)
                (setq button (next-button (button-end button))))
              (should (= (+ 3 3 15) n))))
        (kill-buffer report))))
  (pdf-test-with-pdf "a11y-untagged.pdf"
    (let ((report (pdf-a11y-report)))
      (unwind-protect
          (with-current-buffer report
            (should (string-match-p "no, there is no structure tree"
                                    (buffer-string)))
            (should (string-match-p "Times-Roman" (buffer-string))))
        (kill-buffer report)))))

(ert-deftest pdf-a11y-text-edges ()
  (skip-unless (pdf-info-structure-tree-p))
  (pdf-test-with-pdf "a11y.pdf"
    (let* ((elements (pdf-info-structure))
           (heading (cl-find 'heading-2 elements
                             :key (lambda (e) (alist-get 'kind e))))
           (figure (cl-find 'figure elements
                            :key (lambda (e) (alist-get 'kind e))))
           (edges (pdf-a11y--text-edges heading (current-buffer))))
      (should edges)
      (should (cl-every #'pdf-test-relative-edges-p edges))
      ;; Figures are not located by their text.
      (should-not (pdf-a11y--text-edges figure (current-buffer))))))
