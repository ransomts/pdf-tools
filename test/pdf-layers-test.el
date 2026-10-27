;; -*- lexical-binding: t -*-

;; layers.pdf is written by layers.py, which describes its layers.

(require 'pdf-layers)
(require 'pdf-links)
(require 'ert)

(defmacro pdf-layers-test-with-pdf (&rest body)
  (declare (indent 0) (debug t))
  `(pdf-test-with-pdf "layers.pdf"
     (unless (derived-mode-p 'pdf-view-mode)
       (pdf-view-mode))
     ,@body))

(defun pdf-layers-test-visible (layers)
  "Return the titles of the visible LAYERS."
  (mapcar (lambda (l) (alist-get 'title l))
          (cl-remove-if-not (lambda (l) (alist-get 'visible l)) layers)))

(ert-deftest pdf-layers-feature ()
  (pdf-test-with-test-pdf
    (should (pdf-info-layers-p))))

(ert-deftest pdf-layers-info-layers ()
  (pdf-layers-test-with-pdf
    (should (equal (pdf-info-layers)
                   '(((depth . 1) (id . 1) (visible . t)
                      (radio-group) (title . "Base"))
                     ((depth . 1) (id . 2) (visible)
                      (radio-group) (title . "Answers"))
                     ((depth . 2) (id . 3) (visible . t)
                      (radio-group) (title . "Hints"))
                     ((depth . 1) (id) (visible)
                      (radio-group) (title . "Language"))
                     ((depth . 2) (id . 4) (visible . t)
                      (radio-group . 1) (title . "English"))
                     ((depth . 2) (id . 5) (visible)
                      (radio-group . 1) (title . "Deutsch")))))))

(ert-deftest pdf-layers-info-no-layers ()
  (pdf-test-with-test-pdf
    (should (null (pdf-info-layers)))))

(ert-deftest pdf-layers-info-setlayer ()
  (pdf-layers-test-with-pdf
    (should (equal (pdf-layers-test-visible (pdf-info-setlayer 2 t))
                   '("Base" "Answers" "Hints" "English")))
    (should (equal (pdf-layers-test-visible (pdf-info-setlayer 1 nil))
                   '("Answers" "Hints" "English")))
    ;; The state persists across commands.
    (should (equal (pdf-layers-test-visible (pdf-info-layers))
                   '("Answers" "Hints" "English")))
    (should-error (pdf-info-setlayer 6 t))
    (should-error (pdf-info-setlayer 0 t))))

(ert-deftest pdf-layers-info-radio-group ()
  (pdf-layers-test-with-pdf
    ;; Showing Deutsch hides English, its radio button group sibling.
    (should (equal (pdf-layers-test-visible (pdf-info-setlayer 5 t))
                   '("Base" "Hints" "Deutsch")))
    (should (equal (pdf-layers-test-visible (pdf-info-setlayer 4 t))
                   '("Base" "Hints" "English")))
    ;; Hiding one does not show another.
    (should (equal (pdf-layers-test-visible (pdf-info-setlayer 4 nil))
                   '("Base" "Hints")))))

(ert-deftest pdf-layers-info-renderpage ()
  (pdf-layers-test-with-pdf
    (let ((default (pdf-info-renderpage 1 220)))
      (pdf-info-setlayer 2 t)
      (let ((answers (pdf-info-renderpage 1 220)))
        (should-not (equal default answers))
        (pdf-info-setlayer 1 nil)
        (should-not (equal answers (pdf-info-renderpage 1 220))))
      (pdf-info-setlayer 1 t)
      (pdf-info-setlayer 2 nil)
      (should (equal default (pdf-info-renderpage 1 220))))))

(ert-deftest pdf-layers-info-close-resets ()
  (pdf-layers-test-with-pdf
    (let ((default (pdf-info-layers)))
      (pdf-info-setlayer 2 t)
      (should-not (equal default (pdf-info-layers)))
      (pdf-info-close)
      (should (equal default (pdf-info-layers))))))

(ert-deftest pdf-layers-info-ocg-state-link ()
  (pdf-layers-test-with-pdf
    (let ((links (pdf-info-pagelinks 1)))
      (should (= 1 (length links)))
      (let-alist (car links)
        (should (eq .type 'ocg-state))
        (should (equal .description "Toggle Answers"))
        (should (= .page 1))
        (should (pdf-test-relative-edges-p .edges))
        (should (equal (pdf-layers-test-visible
                        (pdf-info-ocgstate .page .index))
                       '("Base" "Answers" "Hints" "English")))
        (should (equal (pdf-layers-test-visible
                        (pdf-info-ocgstate .page .index))
                       '("Base" "Hints" "English")))
        (should-error (pdf-info-ocgstate .page (1+ .index)))))))

(ert-deftest pdf-layers-links-action-perform ()
  (pdf-layers-test-with-pdf
    (let ((link (car (pdf-cache-pagelinks 1))))
      (should (string-match-p "Toggle Answers"
                              (pdf-links-action-to-string link)))
      (pdf-cache-put-image 1 220 "stale")
      (pdf-links-action-perform link)
      (should (member "Answers" (pdf-layers-test-visible (pdf-info-layers))))
      ;; The rendered images are out of date.
      (should-not (pdf-cache-get-image 1 220)))))

(ert-deftest pdf-layers-list-buffer ()
  (pdf-layers-test-with-pdf
    (let ((list-buffer (pdf-layers t)))
      (unwind-protect
          (with-current-buffer list-buffer
            (should (derived-mode-p 'pdf-layers-list-mode))
            (should (equal (mapcar (lambda (e) (aref (cadr e) 0))
                                   (pdf-layers-list-entries))
                           '("[x]" "[ ]" "[x]" "" "(*)" "( )")))
            (should (equal (mapcar (lambda (e) (aref (cadr e) 1))
                                   (pdf-layers-list-entries))
                           '("Base" "Answers" "  Hints" "Language"
                             "  English" "  Deutsch")))
            (goto-char (point-min))
            (should (re-search-forward "Answers" nil t))
            (should (eq 2 (tabulated-list-get-id)))
            (pdf-layers-toggle (pdf-layers--layer-at-point
                                pdf-layers-document-buffer))
            ;; The list is updated and point stays on the layer.
            (should (eq 2 (tabulated-list-get-id)))
            (should (string-match-p "\\[x\\] +Answers"
                                    (buffer-substring
                                     (line-beginning-position)
                                     (line-end-position))))
            ;; Show Deutsch, which hides English.
            (goto-char (point-min))
            (re-search-forward "Deutsch")
            (call-interactively #'pdf-layers-toggle)
            (should (equal (pdf-layers-test-visible
                            (pdf-info-layers pdf-layers-document-buffer))
                           '("Base" "Answers" "Hints" "Deutsch")))
            ;; Labels cannot be toggled.
            (goto-char (point-min))
            (re-search-forward "Language")
            (should-error (call-interactively #'pdf-layers-toggle)
                          :type 'user-error))
        (kill-buffer list-buffer)))))

(ert-deftest pdf-layers-no-layers-message ()
  (pdf-test-with-test-pdf
    (unless (derived-mode-p 'pdf-view-mode)
      (pdf-view-mode))
    (let ((inhibit-message t))
      (should-not (pdf-layers))
      (with-current-buffer (messages-buffer)
        (goto-char (point-max))
        (forward-line -1)
        (should (looking-at-p ".*has no layers"))))))
