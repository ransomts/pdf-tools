;; -*- lexical-binding: t -*-

(require 'pdf-annot)
(require 'ert)

(ert-deftest pdf-annot-property-modifiable-p-shapes ()
  (let ((square '((type . square)))
        (line '((type . line)))
        (stamp '((type . stamp)))
        (text '((type . text))))
    (should (pdf-annot-property-modifiable-p square 'interior-color))
    (should (pdf-annot-property-modifiable-p square 'edges))
    (should (pdf-annot-property-modifiable-p square 'label))
    (should-not (pdf-annot-property-modifiable-p square 'icon))
    (should (pdf-annot-property-modifiable-p line 'line))
    (should-not (pdf-annot-property-modifiable-p line 'edges))
    (should-not (pdf-annot-property-modifiable-p line 'interior-color))
    (should (pdf-annot-property-modifiable-p stamp 'icon))
    (should-not (pdf-annot-property-modifiable-p stamp 'label))
    (should (pdf-annot-property-modifiable-p text 'icon))
    (should-not (pdf-annot-property-modifiable-p text 'interior-color))))

(ert-deftest pdf-annot-shape-properties ()
  (let ((pdf-annot-default-annotation-properties
         '((t (label . "Joe"))
           (square (color . "blue"))
           (stamp (icon . "Final"))))
        (pdf-annot-color-history nil))
    (should (equal (pdf-annot--shape-properties 'square)
                   '((color . "blue") (label . "Joe"))))
    (should (equal (pdf-annot--shape-properties
                    'square '((interior-color . "red")))
                   '((interior-color . "red") (color . "blue")
                     (label . "Joe"))))
    (should (equal (pdf-annot--shape-properties 'circle)
                   '((label . "Joe") (color . "#ff0000"))))
    ;; Stamps have no label.
    (should (equal (pdf-annot--shape-properties 'stamp)
                   '((icon . "Final"))))
    (should (equal (pdf-annot--shape-properties 'stamp '((icon . "Sold")))
                   '((icon . "Sold"))))))

(ert-deftest pdf-annot-region-rectangle ()
  (should (equal (pdf-annot--region-rectangle '(1 (0.5 0.6 0.1 0.2)))
                 '(0.1 0.2 0.5 0.6)))
  (should (equal (pdf-annot--region-rectangle
                  '(1 (0.1 0.1 0.2 0.2) (0.3 0.05 0.4 0.15)))
                 '(0.1 0.05 0.4 0.2))))

(ert-deftest pdf-annot-add-shape-annotations ()
  (skip-unless (and (pdf-info-shape-annotations-p)
                    (pdf-info-stamp-annotations-p)))
  (pdf-test-with-test-pdf
    (let ((pdf-annot-default-annotation-properties
           '((t (label . "Joe"))
             (stamp (icon . "Approved"))))
          (pdf-annot-color-history nil)
          (pdf-annot-activate-created-annotations nil))
      (cl-letf (((symbol-function 'pdf-util-assert-pdf-window) #'ignore)
                ;; The test buffer is not in pdf-view-mode.
                ((symbol-function 'pdf-util-pdf-buffer-p)
                 (lambda (&rest _) t)))
        (let ((square (pdf-annot-add-square-annotation
                       '(1 (0.5 0.5 0.25 0.25)) "blue"
                       '((interior-color . "yellow"))))
              (circle (pdf-annot-add-circle-annotation
                       '(1 (0.1 0.1 0.2 0.2))))
              (line (pdf-annot-add-line-annotation
                     '(1 (0.9 0.1 0.6 0.4)) "green"))
              (stamp (pdf-annot-add-stamp-annotation
                      '(1 (0.1 0.7 0.5 0.8)))))
          (should (buffer-modified-p))
          (should (eq 'square (pdf-annot-get-type square)))
          (should (equal '(0.25 0.25 0.5 0.5)
                         (mapcar (lambda (x) (/ (round (* x 1000)) 1000.0))
                                 (pdf-annot-get square 'edges))))
          (should (equal "#0000ff" (pdf-annot-get square 'color)))
          (should (equal "#ffff00" (pdf-annot-get square 'interior-color)))
          (should (equal "Joe" (pdf-annot-get square 'label)))
          (should (eq 'circle (pdf-annot-get-type circle)))
          (should (equal "#ff0000" (pdf-annot-get circle 'color)))
          (should (eq 'line (pdf-annot-get-type line)))
          (should (equal "#00ff00" (pdf-annot-get line 'color)))
          (should (< 0.59 (car (pdf-annot-get line 'edges)) 0.6))
          (should (eq 'stamp (pdf-annot-get-type stamp)))
          (should (equal "Approved" (pdf-annot-get stamp 'icon)))
          ;; Edit, move and delete them.
          (setq circle (pdf-annot-put circle 'interior-color "red"))
          (should (equal "#ff0000" (pdf-annot-get circle 'interior-color)))
          (setq stamp (pdf-annot-put stamp 'icon "Sold"))
          (should (equal "Sold" (pdf-annot-get stamp 'icon)))
          (setq square (pdf-annot-put square 'edges '(0.6 0.6 -1 -1)))
          (should (< 0.849 (nth 2 (pdf-annot-get square 'edges)) 0.851))
          (setq line (pdf-annot-put line 'line '(0.1 0.9 0.2 0.9)))
          (should (< 0.89 (nth 1 (pdf-annot-get line 'edges)) 0.9))
          (should-error (pdf-annot-put line 'edges '(0 0 1 1)))
          (should-error (pdf-annot-put stamp 'label "Joe"))
          (should-not (pdf-annot-delete circle))
          (should (equal '(line square stamp)
                         (sort (mapcar #'pdf-annot-get-type
                                       (pdf-annot-getannots
                                        nil '(square circle line stamp)))
                               #'string-lessp)))
          ;; Listed, unless the user says otherwise.
          (should (cl-subsetp '(square circle line stamp)
                              pdf-annot-list-listed-types))
          (should (cl-every (lambda (a)
                              (memq (pdf-annot-get-type a)
                                    pdf-annot-list-listed-types))
                            (pdf-annot-getannots
                             nil '(square circle line stamp))))
          (should (stringp (pdf-annot-print-property square
                                                     'interior-color))))))))

(ert-deftest pdf-annot-context-menu-shapes ()
  (let ((square '((type . square) (id . annot-1-0)))
        (stamp '((type . stamp) (id . annot-1-1))))
    (cl-letf (((symbol-function 'pdf-info-writable-annotations-p) (lambda (&rest _) t))
              ((symbol-function 'pdf-info-shape-annotations-p) (lambda (&rest _) t))
              ((symbol-function 'pdf-info-stamp-annotations-p) (lambda (&rest _) t)))
      (let ((menu (pdf-annot-create-context-menu square)))
        (should (lookup-key menu [change-interior-color]))
        (should (lookup-key menu [change-color]))
        (should-not (lookup-key menu [change-stamp-icon])))
      (let ((menu (pdf-annot-create-context-menu stamp)))
        (should (lookup-key menu [change-stamp-icon]))
        (should-not (lookup-key menu [change-color]))
        (should (lookup-key menu [change-stamp-icon icon-Sold]))
        (should-not (lookup-key menu [change-stamp-icon icon-Draft]))))))

(ert-deftest pdf-annot-property-modifiable-p-free-text-ink ()
  (let ((free-text '((type . free-text)))
        (ink '((type . ink)))
        (square '((type . square))))
    (dolist (property '(font-name font-size font-color color contents
                                  edges label))
      (should (pdf-annot-property-modifiable-p free-text property)))
    (should-not (pdf-annot-property-modifiable-p free-text 'icon))
    (should (pdf-annot-property-modifiable-p ink 'edges))
    (should (pdf-annot-property-modifiable-p ink 'color))
    (should-not (pdf-annot-property-modifiable-p ink 'font-size))
    ;; The width is only set when creating them.
    (should-not (pdf-annot-property-modifiable-p ink 'line-width))
    (should-not (pdf-annot-property-modifiable-p square 'font-color))))

(ert-deftest pdf-annot-free-text-ink-properties ()
  (let ((pdf-annot-default-annotation-properties
         '((t (label . "Joe"))
           (free-text (font-size . 14) (font-color . "blue"))
           (ink (color . "green") (line-width . 3))))
        (pdf-annot-color-history '("orange")))
    ;; Free text gets no background color by default.
    (should (equal (pdf-annot--shape-properties 'free-text)
                   '((font-size . 14) (font-color . "blue")
                     (label . "Joe"))))
    (should (equal (pdf-annot--shape-properties
                    'free-text '((font-color . "red")))
                   '((font-color . "red") (font-size . 14)
                     (label . "Joe"))))
    (should (equal (pdf-annot--shape-properties 'ink)
                   '((color . "green") (line-width . 3)
                     (label . "Joe"))))))

(ert-deftest pdf-annot-free-text-size ()
  (let ((size (pdf-annot--free-text-size "Hello" 10)))
    (should (< 30 (car size) 40))
    (should (< 12 (cdr size) 20)))
  (let ((one (pdf-annot--free-text-size "Hello" 10))
        (two (pdf-annot--free-text-size "Hi\nHello" 10)))
    (should (= (car one) (car two)))
    (should (> (cdr two) (cdr one)))))

(ert-deftest pdf-annot-assert-creatable ()
  (cl-letf (((symbol-function 'pdf-info-creatable-annotation-types)
             (lambda (&rest _) '(text highlight free-text))))
    (should-not (pdf-annot--assert-creatable 'free-text))
    (should (string-match-p
             "25\\.06"
             (cadr (should-error (pdf-annot--assert-creatable 'ink)))))))

(ert-deftest pdf-annot-add-free-text-ink-annotations ()
  (skip-unless (and (pdf-info-free-text-annotations-p)
                    (pdf-info-ink-annotations-p)))
  (pdf-test-with-test-pdf
    (let ((pdf-annot-default-annotation-properties
           '((t (label . "Joe"))
             (free-text (font-size . 14))
             (ink (color . "green") (line-width . 3))))
          (pdf-annot-color-history nil)
          (pdf-annot-activate-created-annotations nil))
      (cl-letf (((symbol-function 'pdf-util-assert-pdf-window) #'ignore)
                ;; The test buffer is not in pdf-view-mode.
                ((symbol-function 'pdf-util-pdf-buffer-p)
                 (lambda (&rest _) t)))
        (let ((text (pdf-annot-add-free-text-annotation
                     '(1 (0.5 0.2 0.1 0.1)) "Hello" "blue"))
              (ink (pdf-annot-add-ink-annotation
                    '(1 (0.2 0.5 0.3 0.6 0.4 0.5) (0.2 0.7)))))
          (should (buffer-modified-p))
          (should (eq 'free-text (pdf-annot-get-type text)))
          (should (equal "Hello" (pdf-annot-get text 'contents)))
          (should (equal '(0.1 0.1 0.5 0.2)
                         (mapcar (lambda (x) (/ (round (* x 1000)) 1000.0))
                                 (pdf-annot-get text 'edges))))
          (should (equal "#0000ff" (pdf-annot-get text 'font-color)))
          (should (= 14 (pdf-annot-get text 'font-size)))
          (should (null (pdf-annot-get text 'color)))
          (should (equal "Joe" (pdf-annot-get text 'label)))
          (should (eq 'ink (pdf-annot-get-type ink)))
          (should (equal "#00ff00" (pdf-annot-get ink 'color)))
          (should (< 0.19 (car (pdf-annot-get ink 'edges)) 0.2))
          (should (< 0.7 (nth 3 (pdf-annot-get ink 'edges)) 0.71))
          ;; Edit, move and delete them.
          (setq text (pdf-annot-put text 'contents "Hello again"))
          (should (equal "Hello again" (pdf-annot-get text 'contents)))
          (setq text (pdf-annot-put text 'font-size 20))
          (should (= 20 (pdf-annot-get text 'font-size)))
          (setq text (pdf-annot-put text 'font-color "red"))
          (should (equal "#ff0000" (pdf-annot-get text 'font-color)))
          (setq ink (pdf-annot-put ink 'edges '(0.5 0.5 -1 -1)))
          (should (< 0.499 (car (pdf-annot-get ink 'edges)) 0.501))
          (should-error (pdf-annot-put ink 'font-size 10))
          (should-error (pdf-annot-add-free-text-annotation
                         '(1 (0.1 0.1 0.2 0.2)) ""))
          (should-not (pdf-annot-delete ink))
          (should (equal '(free-text)
                         (mapcar #'pdf-annot-get-type
                                 (pdf-annot-getannots nil '(free-text ink)))))
          ;; Listed, unless the user says otherwise.
          (should (cl-subsetp '(free-text ink) pdf-annot-list-listed-types))
          (should (stringp (pdf-annot-print-property text 'font-color))))))))

(ert-deftest pdf-annot-add-free-text-ink-unsupported ()
  (skip-unless (and (pdf-info-writable-annotations-p)
                    (not (pdf-info-free-text-annotations-p))
                    (not (pdf-info-ink-annotations-p))))
  (pdf-test-with-test-pdf
    (cl-letf (((symbol-function 'pdf-util-assert-pdf-window) #'ignore))
      (should-error (pdf-annot-add-free-text-annotation
                     '(1 (0.1 0.1 0.2 0.2)) "Hello"))
      (should-error (pdf-annot-add-ink-annotation
                     '(1 (0.1 0.1 0.2 0.2)))))))

(ert-deftest pdf-annot-keys-free-text-ink ()
  (should (eq (and (pdf-info-free-text-annotations-p)
                   'pdf-annot-add-free-text-annotation)
              (lookup-key pdf-annot-minor-mode-map (kbd "C-c C-a f"))))
  (should (eq (and (pdf-info-ink-annotations-p)
                   'pdf-annot-add-ink-annotation)
              (lookup-key pdf-annot-minor-mode-map (kbd "C-c C-a i")))))

(ert-deftest pdf-annot-context-menu-free-text ()
  (let ((text '((type . free-text) (id . annot-1-0) (font-size . 12)))
        (ink '((type . ink) (id . annot-1-1))))
    (cl-letf (((symbol-function 'pdf-info-writable-annotations-p) (lambda (&rest _) t))
              ((symbol-function 'pdf-info-shape-annotations-p) (lambda (&rest _) t))
              ((symbol-function 'pdf-info-free-text-annotations-p) (lambda (&rest _) t)))
      (let ((menu (pdf-annot-create-context-menu text)))
        (should (lookup-key menu [change-font-color]))
        (should (lookup-key menu [change-font-size]))
        (should (equal "Change background color"
                       (nth 2 (assq 'change-color menu)))))
      (let ((menu (pdf-annot-create-context-menu ink)))
        (should-not (lookup-key menu [change-font-color]))
        (should (equal "Change color"
                       (nth 2 (assq 'change-color menu))))))))
