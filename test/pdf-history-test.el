;; -*- lexical-binding: t -*-

(require 'pdf-history)
(require 'ert)

;; Tests for pdf-history.el.  The PDF is shown in the selected window
;; so that page changes go through `pdf-view-goto-page' as they do
;; interactively.  Rendering a page needs a window system, so
;; `pdf-view-display-page' is stubbed out; nothing here looks at the
;; image.

(defmacro pdf-history-test-with-history (&rest body)
  "Run BODY in test.pdf on page 1 with `pdf-history-minor-mode' on."
  (declare (indent 0) (debug t))
  `(pdf-test-with-test-pdf-view
     (cl-letf (((symbol-function 'pdf-view-display-page) #'ignore))
       (switch-to-buffer (current-buffer))
       (pdf-view-goto-page 1)
       (pdf-history-minor-mode 1)
       (unwind-protect
           (progn ,@body)
         (pdf-history-minor-mode -1)))))

(defun pdf-history-test-pages ()
  "Return the pages on the history stack, most recent first."
  (mapcar #'car pdf-history-stack))

(defun pdf-history-test-visit (&rest pages)
  "Turn to each of PAGES in order."
  (dolist (p pages)
    (pdf-view-goto-page p)))

;;; Recording

(ert-deftest pdf-history-mode-starts-with-current-page ()
  (pdf-history-test-with-history
    (should (equal (pdf-history-test-pages) '(1)))
    (should (= pdf-history-index 0))
    (should (memq #'pdf-history-before-change-page-hook
                  pdf-view-after-change-page-hook))))

(ert-deftest pdf-history-page-changes-push-items ()
  (pdf-history-test-with-history
    (pdf-history-test-visit 2 4 3)
    (should (equal (pdf-history-test-pages) '(3 4 2 1)))
    (should (= pdf-history-index 0))))

(ert-deftest pdf-history-push-is-idempotent-for-current-page ()
  (pdf-history-test-with-history
    (pdf-history-test-visit 2)
    (pdf-history-push)
    (pdf-history-push)
    ;; Going to the page already shown does not run the hook at all.
    (pdf-view-goto-page 2)
    (should (equal (pdf-history-test-pages) '(2 1)))))

(ert-deftest pdf-history-not-recorded-while-isearching ()
  (pdf-history-test-with-history
    (let ((pdf-isearch-active-mode t))
      (pdf-history-test-visit 2 3))
    (should (equal (pdf-history-test-pages) '(1)))))

(ert-deftest pdf-history-disabling-mode-stops-recording ()
  (pdf-history-test-with-history
    (pdf-history-test-visit 2 3)
    (pdf-history-minor-mode -1)
    (should-not (memq #'pdf-history-before-change-page-hook
                      pdf-view-after-change-page-hook))
    ;; Turning the mode off clears the stack down to the current page.
    (should (equal (pdf-history-test-pages) '(3)))
    (pdf-history-test-visit 5)
    (should (equal (pdf-history-test-pages) '(3)))))

(ert-deftest pdf-history-clear ()
  (pdf-history-test-with-history
    (pdf-history-test-visit 2 3 4)
    (pdf-history-backward 2)
    (should (= (pdf-view-current-page) 2))
    (pdf-history-clear)
    (should (equal (pdf-history-test-pages) '(2)))
    (should (= pdf-history-index 0))
    (should (pdf-history-beginning-of-history-p))
    (should (pdf-history-end-of-history-p))))

;;; Navigation

(ert-deftest pdf-history-backward-and-forward ()
  (pdf-history-test-with-history
    (pdf-history-test-visit 2 3)
    (pdf-history-backward 1)
    (should (= (pdf-view-current-page) 2))
    (should (= pdf-history-index 1))
    (pdf-history-backward 1)
    (should (= (pdf-view-current-page) 1))
    (should (pdf-history-end-of-history-p))
    (pdf-history-forward 1)
    (should (= (pdf-view-current-page) 2))
    (pdf-history-forward 1)
    (should (= (pdf-view-current-page) 3))
    (should (pdf-history-beginning-of-history-p))
    ;; Moving through the history does not add items.
    (should (equal (pdf-history-test-pages) '(3 2 1)))))

(ert-deftest pdf-history-errors-at-either-end ()
  (pdf-history-test-with-history
    (pdf-history-test-visit 2)
    (should-error (pdf-history-forward 1) :type 'error)
    (pdf-history-backward 1)
    (should-error (pdf-history-backward 1) :type 'error)
    (should (= (pdf-view-current-page) 1))
    ;; A count of zero is a no-op, even at an end.
    (should (= 0 (pdf-history-backward 0)))
    (should (= (pdf-view-current-page) 1))))

(ert-deftest pdf-history-counts-are-clamped ()
  (pdf-history-test-with-history
    (pdf-history-test-visit 2 3)
    ;; Three items; asking for 5 steps back stops at the oldest and
    ;; returns the 3 steps that could not be taken.
    (should (= 3 (pdf-history-backward 5)))
    (should (= (pdf-view-current-page) 1))
    (should (= pdf-history-index 2))
    (should (= -2 (pdf-history-forward 4)))
    (should (= (pdf-view-current-page) 3))
    (should (= pdf-history-index 0))))

(ert-deftest pdf-history-new-page-truncates-forward-items ()
  (pdf-history-test-with-history
    (pdf-history-test-visit 2 3 4)
    (pdf-history-backward 2)
    (should (= (pdf-view-current-page) 2))
    ;; Visiting a new page drops the items "ahead" of the current one,
    ;; like a browser.
    (pdf-history-test-visit 5)
    (should (equal (pdf-history-test-pages) '(5 2 1)))
    (should (= pdf-history-index 0))
    (should-error (pdf-history-forward 1))
    (pdf-history-backward 1)
    (should (= (pdf-view-current-page) 2))))

(ert-deftest pdf-history-goto ()
  (pdf-history-test-with-history
    (pdf-history-test-visit 2 3)
    (pdf-history-goto 2)
    (should (= (pdf-view-current-page) 1))
    (should (= pdf-history-index 2))
    (should-error (pdf-history-goto 3))
    (should-error (pdf-history-goto -1))
    (setq pdf-history-stack nil)
    (should-error (pdf-history-goto 0))))

(ert-deftest pdf-history-keymap ()
  (dolist (binding '(("B" . pdf-history-backward)
                     ("l" . pdf-history-backward)
                     ("N" . pdf-history-forward)
                     ("r" . pdf-history-forward)))
    (should (eq (lookup-key pdf-history-minor-mode-map (kbd (car binding)))
                (cdr binding)))))

(provide 'pdf-history-test)

;;; pdf-history-test.el ends here
