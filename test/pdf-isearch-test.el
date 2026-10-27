;; -*- lexical-binding: t -*-

(require 'pdf-isearch)
(require 'ert)

;; Tests for pdf-isearch.el, against the text of test.pdf:
;;
;; - page 1 holds the same paragraph twice; "stored" occurs four times,
;;   "stored in" four times as words (twice across a line break);
;; - "line" occurs on page 2 ("underline") and page 5 ("Outline") only;
;; - page 3 has "Links", "Internal link" and "External link";
;; - page 6 is empty.
;;
;; Rendering pages needs a window system.  Where the PDF has to be
;; shown in a window, page display is stubbed out and the image size
;; is fixed to 612x792 pixels (one pixel per PDF point), so that match
;; edges in "image space" are predictable.  Highlighting and scrolling
;; to a match (`pdf-isearch-hl-matches', `pdf-isearch-focus-match')
;; only draw, so the end-to-end tests record their arguments instead.

(defconst pdf-isearch-test--size '(612 . 792))

(defvar pdf-isearch-test--highlighted nil
  "Arguments of the calls to the stubbed `pdf-isearch-hl-matches'.")

(defvar pdf-isearch-test--focused nil
  "Arguments of the calls to the stubbed `pdf-isearch-focus-match'.")

(defvar pdf-isearch-test--visible '(0 0 612 792)
  "Visible part of the page reported by `pdf-util-image-displayed-edges'.
Defaults to the whole page, as with a window that fits the page.")

(defvar pdf-isearch-test--scrolled nil
  "The image scrolling commands called, most recent first.")

(defmacro pdf-isearch-test-with-displayed-pdf (&rest body)
  "Run BODY in test.pdf, shown on page 1 in the selected window."
  (declare (indent 0) (debug t))
  `(pdf-test-with-test-pdf-view
     (cl-letf (((symbol-function 'pdf-view-display-page) #'ignore)
               ((symbol-function 'pdf-view-image-size)
                (lambda (&rest _) pdf-isearch-test--size))
               ;; There is no image to measure or scroll in batch.
               ((symbol-function 'pdf-util-image-displayed-edges)
                (lambda (&rest _) pdf-isearch-test--visible))
               ((symbol-function 'image-scroll-up)
                (lambda (&rest _) (push 'up pdf-isearch-test--scrolled)))
               ((symbol-function 'image-scroll-down)
                (lambda (&rest _) (push 'down pdf-isearch-test--scrolled))))
       (setq pdf-isearch-test--scrolled nil)
       (switch-to-buffer (current-buffer))
       (delete-other-windows)
       (pdf-view-goto-page 1)
       ,@body)))

(defmacro pdf-isearch-test-with-isearch (&rest body)
  "Like `pdf-isearch-test-with-displayed-pdf', with drawing recorded."
  (declare (indent 0) (debug t))
  `(pdf-isearch-test-with-displayed-pdf
     ;; `pdf-isearch-active-mode' sets this, and only a real isearch
     ;; session resets it; keep it from leaking into later tests.
     (let ((overriding-terminal-local-map overriding-terminal-local-map))
     (cl-letf (((symbol-function 'pdf-isearch-hl-matches)
                (lambda (&rest args) (push args pdf-isearch-test--highlighted)))
               ((symbol-function 'pdf-isearch-focus-match)
                (lambda (&rest args) (push args pdf-isearch-test--focused))))
       (setq pdf-isearch-test--highlighted nil
             pdf-isearch-test--focused nil)
       (pdf-isearch-minor-mode 1)
       ,@body))))

(defmacro pdf-isearch-test-with-search-state (spec &rest body)
  "Bind the isearch variables of SPEC around BODY.
SPEC is a plist with :forward, :regexp, :case-fold and :word."
  (declare (indent 1) (debug t))
  `(let ((isearch-forward (plist-get ',spec :forward))
         (isearch-regexp (plist-get ',spec :regexp))
         (isearch-case-fold-search (plist-get ',spec :case-fold))
         (isearch-regexp-function (plist-get ',spec :word))
         (isearch-nonincremental t)
         (isearch-cmds nil)
         (isearch-string ""))
     ,@body))

(defun pdf-isearch-test--pixel-matches (matches)
  "Scale the edges of MATCHES from `pdf-info-search-*' to image space."
  (mapcar (lambda (m)
            (pdf-util-scale (alist-get 'edges m) pdf-isearch-test--size 'round))
          matches))

;;; Word search regexps

(ert-deftest pdf-isearch-word-search-regexp ()
  (should (equal (pdf-isearch-word-search-regexp "") ""))
  (should (equal (pdf-isearch-word-search-regexp "...") "\\W+"))
  (should (equal (pdf-isearch-word-search-regexp "foo bar")
                 "\\bfoo\\W+bar\\b"))
  ;; Any run of non-word characters separates words.
  (should (equal (pdf-isearch-word-search-regexp "foo, -bar")
                 "\\bfoo\\W+bar\\b"))
  ;; Leading and trailing punctuation become \W+ instead of \b.
  (should (equal (pdf-isearch-word-search-regexp " foo.") "\\W+foo\\W+"))
  ;; Lax: no word boundary at the ends, for incremental input.
  (should (equal (pdf-isearch-word-search-regexp "foo ba" t)
                 "foo\\W+ba")))

(ert-deftest pdf-isearch-word-search-regexp-hyphenation ()
  ;; With hyphenation characters, a hyphen and line break may occur
  ;; between any two letters; the characters are escaped for a
  ;; bracket expression.
  (should (equal (pdf-isearch-word-search-regexp "ab" nil "-")
                 "\\ba(?:[\\-]\\n)?b\\b"))
  (should (equal (pdf-isearch-word-search-regexp "ab" nil "^]")
                 "\\ba(?:[\\^\\]]\\n)?b\\b")))

;;; Search functions

(ert-deftest pdf-isearch-search-fun-default-dispatch ()
  (pdf-isearch-test-with-search-state ()
    (should (eq (pdf-isearch-search-fun) 'pdf-info-search-string)))
  (pdf-isearch-test-with-search-state (:regexp t)
    (should (functionp (pdf-isearch-search-fun)))
    (should-not (eq (pdf-isearch-search-fun) 'pdf-info-search-string)))
  (let ((pdf-isearch-search-fun-function
         (lambda () (lambda (&rest _) 'custom))))
    (should (eq (funcall (pdf-isearch-search-fun) "x" 1) 'custom))))

(ert-deftest pdf-isearch-search-fun-string-regexp-word ()
  (pdf-isearch-test-with-displayed-pdf
    (let ((count (lambda (string page)
                   (length (funcall (pdf-isearch-search-fun) string page)))))
      (pdf-isearch-test-with-search-state (:case-fold t)
        (should (= 4 (funcall count "stored" 1)))
        (should (= 0 (funcall count "s.ored" 1))))
      (pdf-isearch-test-with-search-state (:regexp t :case-fold t)
        (should (= 4 (funcall count "s.ored" 1)))
        (should (= 2 (funcall count "replace?ment" 1)))
        ;; isearch turns this error into "incomplete input".
        (should-error (funcall count "(stored" 1) :type 'invalid-regexp))
      ;; Word search ignores how the words are separated, including
      ;; line breaks.
      (pdf-isearch-test-with-search-state (:word t :case-fold t)
        (should (= 4 (funcall count "stored in" 1)))
        (should (= 2 (funcall count "file system" 1)))
        (should (= 0 (funcall count "store" 1)))))))

(ert-deftest pdf-isearch-search-fun-word-toggled ()
  ;; BUG: `pdf-isearch-search-fun-default' only treats
  ;; `isearch-regexp-function' (alias `isearch-word') eq to t as word
  ;; search.  Toggling word search inside isearch (M-s w) sets it to
  ;; the function `word-search-regexp' instead (and M-s _ to
  ;; `isearch-symbol-regexp'), so the PDF is then searched for the
  ;; literal string.  `pdf-isearch-occur' does handle a function value.
  :expected-result :failed
  (pdf-isearch-test-with-displayed-pdf
    (pdf-isearch-test-with-search-state (:word word-search-regexp :case-fold t)
      ;; As with (:word t): four matches, two of them across a line break.
      (should (= 4 (length (funcall (pdf-isearch-search-fun) "stored in" 1)))))))

(ert-deftest pdf-isearch-search-page-case-folding ()
  (pdf-isearch-test-with-displayed-pdf
    (pdf-view-goto-page 3)
    (pdf-isearch-test-with-search-state (:case-fold t)
      (should (= 3 (length (pdf-isearch-search-page "link")))))
    (pdf-isearch-test-with-search-state (:case-fold nil)
      (should (= 2 (length (pdf-isearch-search-page "link"))))
      (should (= 1 (length (pdf-isearch-search-page "Link")))))
    (pdf-isearch-test-with-search-state (:regexp t :case-fold nil)
      (should (= 0 (length (pdf-isearch-search-page "LINK")))))
    (pdf-isearch-test-with-search-state (:regexp t :case-fold t)
      (should (= 3 (length (pdf-isearch-search-page "LINK")))))))

(ert-deftest pdf-isearch-search-page-edges ()
  (pdf-isearch-test-with-displayed-pdf
    (pdf-isearch-test-with-search-state (:case-fold t)
      (let ((matches (pdf-isearch-search-page "stored")))
        ;; Edges in image space, one list of edges per match.
        (should (equal matches
                       (pdf-isearch-test--pixel-matches
                        (pdf-info-search-string "stored" 1))))
        (should (cl-typep matches '(list-of pdf-isearch-match)))
        ;; Sorted top to bottom, then left to right.
        (should (equal matches
                       (sort (copy-sequence matches)
                             (lambda (a b)
                               (let ((a (car a)) (b (car b)))
                                 (or (< (nth 1 a) (nth 1 b))
                                     (and (= (nth 1 a) (nth 1 b))
                                          (< (nth 0 a) (nth 0 b)))))))))
        ;; PAGE defaults to the current page.
        (should (equal (pdf-isearch-search-page "stored" 1) matches))
        (should-not (pdf-isearch-search-page "stored" 2))))))

(ert-deftest pdf-isearch-find-next-matching-page ()
  (pdf-test-with-test-pdf-view
    (pdf-isearch-test-with-search-state (:forward t :case-fold t)
      (should (= 2 (pdf-isearch-find-next-matching-page "line" 1)))
      (should (= 5 (pdf-isearch-find-next-matching-page "line" 2)))
      (should-not (pdf-isearch-find-next-matching-page "line" 5))
      ;; Found beyond the first, exponentially growing, batches.
      (should (= 5 (pdf-isearch-find-next-matching-page "subsection" 1)))
      (should (= 3 (pdf-isearch-find-next-matching-page "links" 1))))
    (pdf-isearch-test-with-search-state (:forward nil :case-fold t)
      (should (= 2 (pdf-isearch-find-next-matching-page "line" 5)))
      (should (= 5 (pdf-isearch-find-next-matching-page "line" 6)))
      (should-not (pdf-isearch-find-next-matching-page "line" 2))
      (should (= 1 (pdf-isearch-find-next-matching-page "stored" 6))))
    (pdf-isearch-test-with-search-state (:forward t :case-fold nil)
      (should-not (pdf-isearch-find-next-matching-page "links" 1)))))

;;; Choosing the next match

(defconst pdf-isearch-test--m1 '((10 10 20 20)))
(defconst pdf-isearch-test--m2 '((50 10 60 20)))
(defconst pdf-isearch-test--m3 '((10 40 20 50) (10 52 30 60)))

(ert-deftest pdf-isearch-closest-match ()
  (let ((matches (list pdf-isearch-test--m1 pdf-isearch-test--m2
                       pdf-isearch-test--m3)))
    ;; Forward: the next match on the same line, else on a later line.
    (should (eq (pdf-isearch-closest-match '((30 10 40 20)) matches t)
                pdf-isearch-test--m2))
    (should (eq (pdf-isearch-closest-match '((70 10 80 20)) matches t)
                pdf-isearch-test--m3))
    (should-not (pdf-isearch-closest-match '((0 70 10 80)) matches t))
    ;; Backward, with MATCHES in reverse order.
    (setq matches (reverse matches))
    (should (eq (pdf-isearch-closest-match '((30 10 40 20)) matches nil)
                pdf-isearch-test--m1))
    (should (eq (pdf-isearch-closest-match '((0 70 10 80)) matches nil)
                pdf-isearch-test--m3))
    (should-not (pdf-isearch-closest-match '((0 0 5 5)) matches nil)))
  (should-error (pdf-isearch-closest-match '((1 2 3)) nil)
                :type 'wrong-type-argument)
  (should-error (pdf-isearch-closest-match '((1 2 3 4)) '(((1 2))))
                :type 'wrong-type-argument))

(ert-deftest pdf-isearch-next-match-isearch ()
  (let ((matches (list pdf-isearch-test--m1 pdf-isearch-test--m2
                       pdf-isearch-test--m3))
        (pdf-isearch-batch-mode nil))
    ;; A new page starts at its first match.
    (should (eq (pdf-isearch-next-match 1 2 pdf-isearch-test--m2 matches t t)
                pdf-isearch-test--m1))
    ;; Repeating the same search steps through the matches in order.
    (should (eq (pdf-isearch-next-match 1 1 pdf-isearch-test--m1 matches t t)
                pdf-isearch-test--m2))
    (should (eq (pdf-isearch-next-match 1 1 pdf-isearch-test--m2 matches t t)
                pdf-isearch-test--m3))
    (should-not (pdf-isearch-next-match 1 1 pdf-isearch-test--m3 matches t t))
    ;; A changed search string continues from the last match.
    (should (eq (pdf-isearch-next-match 1 1 '((30 10 40 20)) matches nil t)
                pdf-isearch-test--m2))
    ;; Without a previous match, start at the top of the visible area.
    (cl-letf (((symbol-function 'pdf-util-image-displayed-edges)
               (lambda (&rest _) '(0 30 100 100))))
      (should (eq (pdf-isearch-next-match 1 1 nil matches t t)
                  pdf-isearch-test--m3))
      (should (eq (pdf-isearch-next-match 1 1 nil (reverse matches) t nil)
                  pdf-isearch-test--m3)))))

(ert-deftest pdf-isearch-next-match-batch ()
  (let ((matches (list pdf-isearch-test--m1 pdf-isearch-test--m2
                       pdf-isearch-test--m3))
        (pdf-isearch-batch-mode t))
    (cl-letf (((symbol-function 'pdf-util-image-displayed-edges)
               (lambda (&rest _) '(0 0 100 30))))
      ;; m2 is visible together with m1, so batch mode skips it.
      (should (eq (pdf-isearch-next-match 1 1 pdf-isearch-test--m1 matches t t)
                  pdf-isearch-test--m3))
      ;; Otherwise it behaves like normal isearch.
      (should (eq (pdf-isearch-next-match 1 2 pdf-isearch-test--m2 matches t t)
                  pdf-isearch-test--m1))
      (should (eq (pdf-isearch-next-match 1 1 '((30 10 40 20)) matches nil t)
                  pdf-isearch-test--m2)))))

(ert-deftest pdf-isearch-same-search-p ()
  (with-temp-buffer
    (let ((isearch-string "foo")
          (isearch-regexp nil)
          (isearch-case-fold-search t)
          (isearch-regexp-function nil))
      ;; No previous search counts as the same.
      (setq pdf-isearch-current-parameter nil)
      (should (pdf-isearch-same-search-p))
      (setq pdf-isearch-current-parameter (list "foo" nil t nil))
      (should (pdf-isearch-same-search-p))
      (let ((isearch-string "foob"))
        (should-not (pdf-isearch-same-search-p))
        (should (pdf-isearch-same-search-p t)))
      (let ((isearch-regexp t))
        (should-not (pdf-isearch-same-search-p t)))
      (let ((isearch-case-fold-search nil))
        (should-not (pdf-isearch-same-search-p t))))))

(ert-deftest pdf-isearch-empty-match-p ()
  (should-not (pdf-isearch-empty-match-p nil))
  (should-not (pdf-isearch-empty-match-p '(nil nil)))
  (should (pdf-isearch-empty-match-p '((((0 0 0 0))) (((0 0 0 0) (0 0 0 0))))))
  (should-not (pdf-isearch-empty-match-p '((((0 0 0 0))) (((0 0 1 0)))))))

;;; The isearch interface

(ert-deftest pdf-isearch-minor-mode-setup ()
  (pdf-isearch-test-with-displayed-pdf
    (pdf-isearch-minor-mode 1)
    (should (eq (funcall isearch-search-fun-function)
                'pdf-isearch-search-function))
    (should (eq isearch-push-state-function 'pdf-isearch-push-state-function))
    (should (eq isearch-wrap-function 'pdf-isearch-wrap-function))
    (should-not isearch-lazy-highlight)
    (should (memq 'pdf-isearch-mode-initialize isearch-mode-hook))
    (should (memq 'pdf-isearch-mode-cleanup isearch-mode-end-hook))
    (should (memq 'pdf-isearch-update isearch-update-post-hook))
    (should (eq (lookup-key pdf-isearch-minor-mode-map [remap occur])
                'pdf-occur))
    (pdf-isearch-minor-mode -1)
    (should-not (local-variable-p 'isearch-search-fun-function))
    (should-not (local-variable-p 'isearch-wrap-function))
    (should-not (memq 'pdf-isearch-mode-initialize isearch-mode-hook))
    (should-not (memq 'pdf-isearch-update isearch-update-post-hook))))

(ert-deftest pdf-isearch-minor-mode-requires-pdf-buffer ()
  (with-temp-buffer
    (should-error (pdf-isearch-minor-mode 1))))

(ert-deftest pdf-isearch-search-function-moves-to-matching-page ()
  (pdf-isearch-test-with-isearch
    (pdf-isearch-mode-initialize)
    (unwind-protect
        (pdf-isearch-test-with-search-state (:forward t :case-fold t)
          (let ((isearch-string "line"))
            ;; Nothing on page 1: go to page 2 and stop at "underline".
            (should (pdf-isearch-search-function "line"))
            (should (= (pdf-view-current-page) 2))
            (should (= pdf-isearch-current-page 2))
            (let ((match (car (pdf-isearch-test--pixel-matches
                               (pdf-info-search-string "line" 2)))))
              (should (equal pdf-isearch-current-match match))
              (should (equal pdf-isearch-current-matches (list (list match))))
              (should (equal (car pdf-isearch-test--focused) (list match)))
              (should (equal (car pdf-isearch-test--highlighted)
                             (list match (list (list match)) nil '(2)))))
            (should (equal pdf-isearch-current-parameter
                           '("line" nil t nil)))
            ;; Searching again moves on to the match on page 5.
            (should (pdf-isearch-search-function "line"))
            (should (= (pdf-view-current-page) 5))
            ;; No further match: fail and stay put.
            (should-not (pdf-isearch-search-function "line"))
            (should (= (pdf-view-current-page) 5))))
      (pdf-isearch-mode-cleanup))))

(ert-deftest pdf-isearch-search-function-steps-through-page ()
  (pdf-isearch-test-with-isearch
    (pdf-isearch-mode-initialize)
    (unwind-protect
        (pdf-isearch-test-with-search-state (:forward t :case-fold t)
          (let* ((isearch-string "stored")
                 (expected (pdf-isearch-test--pixel-matches
                            (pdf-info-search-string "stored" 1)))
                 seen)
            (dotimes (_ 4)
              (should (pdf-isearch-search-function "stored"))
              (push pdf-isearch-current-match seen))
            (should (equal (nreverse seen) expected))
            (should (= (pdf-view-current-page) 1))
            ;; There is no "stored" after page 1.
            (should-not (pdf-isearch-search-function "stored"))
            (should (= (pdf-view-current-page) 1))))
      (pdf-isearch-mode-cleanup))))

(ert-deftest pdf-isearch-search-function-backward ()
  (pdf-isearch-test-with-isearch
    (pdf-view-goto-page 6)
    (pdf-isearch-mode-initialize)
    (unwind-protect
        (pdf-isearch-test-with-search-state (:forward nil :case-fold t)
          (let ((isearch-string "line"))
            (should (pdf-isearch-search-function "line"))
            (should (= (pdf-view-current-page) 5))
            (should (pdf-isearch-search-function "line"))
            (should (= (pdf-view-current-page) 2))
            (should-not (pdf-isearch-search-function "line"))))
      (pdf-isearch-mode-cleanup))))

(ert-deftest pdf-isearch-search-function-backward-below-visible-area ()
  ;; A backward search that moves to an earlier page starts from the
  ;; bottom of that page, not of the visible part of it.
  (pdf-isearch-test-with-isearch
    (pdf-view-goto-page 6)
    (pdf-isearch-mode-initialize)
    (unwind-protect
        ;; "Outline" on page 5 is at y = 125..137.
        (let ((pdf-isearch-test--visible '(0 0 612 100)))
          (pdf-isearch-test-with-search-state (:forward nil :case-fold t)
            (let ((isearch-string "line"))
              (should (pdf-isearch-search-function "line"))
              (should (= (pdf-view-current-page) 5)))))
      (pdf-isearch-mode-cleanup))))

(ert-deftest pdf-isearch-search-function-narrowed-to-page ()
  (pdf-isearch-test-with-isearch
    (pdf-isearch-mode-initialize)
    (unwind-protect
        (pdf-isearch-test-with-search-state (:forward t :case-fold t)
          (let ((isearch-string "line")
                (pdf-isearch-narrow-to-page t))
            (should-not (pdf-isearch-search-function "line"))
            (should (= (pdf-view-current-page) 1))))
      (pdf-isearch-mode-cleanup))))

(ert-deftest pdf-isearch-search-function-filter ()
  (pdf-isearch-test-with-isearch
    (pdf-isearch-mode-initialize)
    (unwind-protect
        (pdf-isearch-test-with-search-state (:forward t :case-fold t)
          (let* ((isearch-string "stored")
                 (all (pdf-isearch-test--pixel-matches
                       (pdf-info-search-string "stored" 1)))
                 (pdf-isearch-filter-matches-function
                  (lambda (matches) (last matches))))
            (should (pdf-isearch-search-function "stored"))
            (should (equal pdf-isearch-current-match (car (last all))))
            (should (equal pdf-isearch-current-matches
                           (list (last all))))))
      (pdf-isearch-mode-cleanup))))

(ert-deftest pdf-isearch-search-function-empty-string ()
  (pdf-isearch-test-with-isearch
    (pdf-isearch-test-with-search-state (:forward t)
      (should-not (pdf-isearch-search-function ""))
      (should-not pdf-isearch-test--highlighted))))

(ert-deftest pdf-isearch-wrap-function ()
  (pdf-isearch-test-with-isearch
    (pdf-view-goto-page 4)
    (setq pdf-isearch-current-match pdf-isearch-test--m1)
    (let ((isearch-forward t))
      (pdf-isearch-wrap-function)
      (should (= (pdf-view-current-page) 1))
      ;; The top of the first page is shown.
      (should (equal pdf-isearch-test--scrolled '(down)))
      (should-not pdf-isearch-current-match))
    (let ((isearch-forward nil))
      (pdf-isearch-wrap-function)
      (should (= (pdf-view-current-page) 6))
      ;; The bottom of the last page is shown.
      (should (equal pdf-isearch-test--scrolled '(up down))))
    ;; Already on the page to wrap to: nothing to do.
    (let ((isearch-forward nil))
      (pdf-isearch-wrap-function)
      (should (equal pdf-isearch-test--scrolled '(up down))))
    (let ((isearch-forward t)
          (pdf-isearch-narrow-to-page t))
      (pdf-isearch-wrap-function)
      (should (= (pdf-view-current-page) 6)))))

(ert-deftest pdf-isearch-push-state-restores ()
  (pdf-isearch-test-with-isearch
    (pdf-view-goto-page 2)
    (setq pdf-isearch-current-page 2
          pdf-isearch-current-match pdf-isearch-test--m1
          pdf-isearch-current-matches (list (list pdf-isearch-test--m1))
          pdf-isearch-current-parameter '("x" nil t nil))
    (let ((restore (pdf-isearch-push-state-function)))
      (pdf-view-goto-page 5)
      (setq pdf-isearch-current-page 5
            pdf-isearch-current-match nil
            pdf-isearch-current-matches nil
            pdf-isearch-current-parameter '("xy" nil t nil))
      (funcall restore nil)
      (should (= (pdf-view-current-page) 2))
      (should (= pdf-isearch-current-page 2))
      (should (equal pdf-isearch-current-match pdf-isearch-test--m1))
      (should (equal pdf-isearch-current-parameter '("x" nil t nil)))
      ;; The restored match is highlighted again.
      (should (equal (caar pdf-isearch-test--highlighted)
                     pdf-isearch-test--m1)))))

(ert-deftest pdf-isearch-initialize-and-cleanup ()
  (pdf-isearch-test-with-isearch
    (pdf-view-goto-page 3)
    (setq pdf-isearch-current-match pdf-isearch-test--m1
          pdf-isearch-current-parameter '("x" nil t nil))
    (pdf-isearch-mode-initialize)
    (unwind-protect
        (progn
          (should pdf-isearch-active-mode)
          (should (eq isearch-mode-map pdf-isearch-active-mode-map))
          (should (= pdf-isearch-current-page 3))
          (should-not pdf-isearch-current-match)
          (should-not pdf-isearch-current-parameter))
      (pdf-isearch-mode-cleanup))
    (should-not pdf-isearch-active-mode)
    (should-not (local-variable-p 'isearch-mode-map))))

;;; Highlighting

(ert-deftest pdf-isearch-hl-matches-regions ()
  (pdf-isearch-test-with-displayed-pdf
    (let (calls)
      (cl-letf (((symbol-function 'pdf-info-renderpage-text-regions)
                 (lambda (&rest args) (push args calls)))
                ;; The colours are not under test, and looking them up
                ;; may need a frame, which batch Emacs does not have.
                ((symbol-function 'pdf-util-face-colors)
                 (lambda (&rest _) '("black" . "yellow"))))
        (pdf-isearch-hl-matches
         pdf-isearch-test--m2
         (list (list pdf-isearch-test--m1 pdf-isearch-test--m2
                     pdf-isearch-test--m3))))
      (should (= 1 (length calls)))
      (cl-destructuring-bind (page width single-colors _sel _style current lazy)
          (car calls)
        (should (= page 1))
        (should (= width (car pdf-isearch-test--size)))
        (should (eq single-colors t))
        ;; (FG BG EDGES...), with edges relative to the page.
        (should (equal (cddr current)
                       (pdf-util-scale-pixel-to-relative pdf-isearch-test--m2)))
        ;; Every other match is drawn in the lazy colours.
        (should (equal (cddr lazy)
                       (pdf-util-scale-pixel-to-relative
                        (append pdf-isearch-test--m1 pdf-isearch-test--m3))))))))

(ert-deftest pdf-isearch-hl-matches-several-pages ()
  (pdf-isearch-test-with-displayed-pdf
    (pdf-view-goto-page 2)
    (let (calls)
      (cl-letf (((symbol-function 'pdf-info-renderpage-text-regions)
                 (lambda (&rest args) (push args calls)))
                ;; The colours are not under test, and looking them up
                ;; may need a frame, which batch Emacs does not have.
                ((symbol-function 'pdf-util-face-colors)
                 (lambda (&rest _) '("black" . "yellow"))))
        (pdf-isearch-hl-matches
         pdf-isearch-test--m3
         (list (list pdf-isearch-test--m1) (list pdf-isearch-test--m3))
         nil '(1 2)))
      (setq calls (nreverse calls))
      (should (equal (mapcar #'car calls) '(1 2)))
      ;; The current match is only drawn on the current page.
      (should-not (cddr (nth 5 (nth 0 calls))))
      (should (equal (cddr (nth 6 (nth 0 calls)))
                     (pdf-util-scale-pixel-to-relative pdf-isearch-test--m1)))
      (should (equal (cddr (nth 5 (nth 1 calls)))
                     (pdf-util-scale-pixel-to-relative pdf-isearch-test--m3)))
      (should-not (cddr (nth 6 (nth 1 calls)))))))

(ert-deftest pdf-isearch-hl-matches-type-checks ()
  (should-error (pdf-isearch-hl-matches '((1 2 3)) nil)
                :type 'wrong-type-argument)
  (should-error (pdf-isearch-hl-matches nil '(((1 2 3 4)) nil))
                :type 'wrong-type-argument))

;;; Occur hand-off

(ert-deftest pdf-isearch-occur-hand-off ()
  (let (calls)
    (cl-letf (((symbol-function 'pdf-occur)
               (lambda (&rest args) (push args calls)))
              ((symbol-function 'isearch-message) #'ignore))
      (let ((isearch-string "foo bar")
            (isearch-case-fold-search t))
        (let ((isearch-regexp nil) (isearch-regexp-function nil))
          (pdf-isearch-occur))
        (let ((isearch-regexp t) (isearch-regexp-function nil))
          (pdf-isearch-occur))
        (let ((isearch-regexp nil) (isearch-regexp-function t))
          (pdf-isearch-occur))
        (let ((isearch-regexp nil)
              (isearch-regexp-function #'word-search-regexp))
          (pdf-isearch-occur))))
    (should (equal (nreverse calls)
                   `(("foo bar" nil)
                     ("foo bar" "foo bar")
                     (,(pdf-isearch-word-search-regexp
                        "foo bar" nil pdf-isearch-hyphenation-character)
                      ,(pdf-isearch-word-search-regexp
                        "foo bar" nil pdf-isearch-hyphenation-character))
                     (,(word-search-regexp "foo bar")
                      ,(word-search-regexp "foo bar")))))))

;;; Interactive isearch, driven by keyboard macros

(ert-deftest pdf-isearch-interactive-forward-and-wrap ()
  (pdf-isearch-test-with-isearch
    (execute-kbd-macro (kbd "C-s line RET"))
    (should (= (pdf-view-current-page) 2))
    (should-not pdf-isearch-active-mode)
    (pdf-view-goto-page 1)
    (execute-kbd-macro (kbd "C-s line C-s RET"))
    (should (= (pdf-view-current-page) 5))
    ;; Past the last match isearch fails once, then wraps around to the
    ;; first match of the document.
    (pdf-view-goto-page 1)
    (execute-kbd-macro (kbd "C-s line C-s C-s C-s RET"))
    (should (= (pdf-view-current-page) 2))))

(ert-deftest pdf-isearch-interactive-backward ()
  (pdf-isearch-test-with-isearch
    (pdf-view-goto-page 6)
    (execute-kbd-macro (kbd "C-r line RET"))
    (should (= (pdf-view-current-page) 5))
    (pdf-view-goto-page 6)
    (execute-kbd-macro (kbd "C-r line C-r RET"))
    (should (= (pdf-view-current-page) 2))))

(ert-deftest pdf-isearch-interactive-regexp ()
  (pdf-isearch-test-with-isearch
    (execute-kbd-macro (kbd "C-M-s under?line RET"))
    (should (= (pdf-view-current-page) 2))
    (should (equal pdf-isearch-current-parameter '("under?line" t t nil)))))

(ert-deftest pdf-isearch-interactive-quit-restores-page ()
  (pdf-isearch-test-with-isearch
    (pdf-view-goto-page 1)
    (condition-case nil
        (execute-kbd-macro (kbd "C-s line C-g"))
      (quit nil))
    (should (= (pdf-view-current-page) 1))))

(ert-deftest pdf-isearch-interactive-occur ()
  (pdf-isearch-test-with-isearch
    (let (calls)
      (cl-letf (((symbol-function 'pdf-occur)
                 (lambda (&rest args) (push args calls))))
        (execute-kbd-macro (kbd "C-s stored M-s o RET")))
      (should (equal calls '(("stored" nil)))))))

(provide 'pdf-isearch-test)

;;; pdf-isearch-test.el ends here
