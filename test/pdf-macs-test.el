;; -*- lexical-binding: t -*-

(require 'pdf-macs)
(require 'pdf-view)
(require 'ert)

;; pdf-macs.el holds the accessors for per-window view state.  The rest
;; of the package reads them and also uses them as `setf' places
;; (e.g. `pdf-view-goto-page' does (setf (pdf-view-current-page window)
;; page)), so both directions are checked here.

(ert-deftest pdf-macs-accessors-read-window-properties ()
  (dolist (spec '((pdf-view-current-page . page)
                  (pdf-view-current-overlay . overlay)
                  (pdf-view-current-image . image)
                  (pdf-view-current-slice . slice)
                  (pdf-view-current-window-size . window-size)
                  (pdf-view-window-needs-redisplay . needs-redisplay)))
    (should (equal (macroexpand-1 (list (car spec) 'win))
                   `(image-mode-window-get ',(cdr spec) win)))
    ;; WINDOW is optional.
    (should (equal (macroexpand-1 (list (car spec)))
                   `(image-mode-window-get ',(cdr spec) nil)))))

(ert-deftest pdf-macs-accessors-are-setf-places ()
  (pdf-test-with-test-pdf-view
    (cl-letf (((symbol-function 'pdf-view-display-page) #'ignore))
      (switch-to-buffer (current-buffer))
      (let ((win (selected-window)))
        (setf (pdf-view-current-page win) 4)
        (should (= (image-mode-window-get 'page win) 4))
        (should (= (pdf-view-current-page) 4))
        (setf (pdf-view-current-slice win) '(0.1 0.2 0.5 0.6))
        (should (equal (pdf-view-current-slice win) '(0.1 0.2 0.5 0.6)))
        (setf (pdf-view-window-needs-redisplay win) t)
        (should (eq (image-mode-window-get 'needs-redisplay win) t))))))

(provide 'pdf-macs-test)

;;; pdf-macs-test.el ends here
