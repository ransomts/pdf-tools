;; -*- lexical-binding: t -*-

(ert-deftest pdf-info-open/close ()
  (pdf-test-with-test-pdf
   (should-not (pdf-info-open))
   (should (pdf-info-close)))
  (pdf-test-with-encrypted-pdf
   (should-error (pdf-info-open nil "Invalid Password"))
   (should-not (pdf-info-open nil "pdftool"))))

(ert-deftest pdf-info-metadata ()
  (pdf-test-with-test-pdf
    (should (cl-every (lambda (elt)
                        (and (consp elt)
                             (symbolp (car elt))))
                      (pdf-info-metadata)))))

(ert-deftest pdf-info-search-string ()
  (pdf-test-with-test-pdf
   (let (matches)
     (should (setq matches (pdf-info-search-string "PDF Tools")))
     (should (= 2 (length matches)))
     (should (cl-every (lambda (m)
                         (let-alist m
                           (and (stringp .text)
                                (cl-every 'pdf-test-relative-edges-p .edges)
                                (= 1 .page))))
                       matches)))))


(ert-deftest pdf-info-search-regexp ()
  (pdf-test-with-test-pdf
    (let (case-fold-search matches)
      (should (setq matches (pdf-info-search-regexp "PDF Tools")))
      (should (= 2 (length matches)))
      (should (cl-every (lambda (m)
                          (let-alist m
                            (and (stringp .text)
                                 (cl-every 'pdf-test-relative-edges-p .edges)
                                 (= 1 .page))))
                        matches)))))

(ert-deftest pdf-info-pagelinks ()
  (pdf-test-with-test-pdf
    (let ((links (pdf-info-pagelinks 3)))
      (should (= 2 (length links)))
      (should (cl-every 'pdf-test-relative-edges-p
                        (mapcar (apply-partially 'alist-get 'edges)
                                links)))
      (should (equal (mapcar (apply-partially 'alist-get 'type) links)
                     '(goto-dest uri)))
      (should (equal (mapcar (apply-partially 'alist-get 'uri)
                             links)
                     '(nil "http://www.gnu.org")))
      (should (equal (mapcar (apply-partially 'alist-get 'page)
                             links)
                     '(1 nil))))))

(ert-deftest pdf-info-number-of-pages ()
  (pdf-test-with-test-pdf
    (should (= 6 (pdf-info-number-of-pages)))))

(ert-deftest pdf-info-outline ()
  (pdf-test-with-test-pdf
   (let ((outline (pdf-info-outline)))
     (should (= 7 (length outline)))
     (should (equal (mapcar (apply-partially 'alist-get 'depth) outline)
                    '(1 1 1 1 1 2 3)))
     (should (cl-every (lambda (elt)
                         (eq (alist-get 'type elt)
                             'goto-dest))
                       outline)))))

(ert-deftest pdf-info-gettext ()
  (pdf-test-with-test-pdf
    (should (string-match "PDF Tools\\(?:.\\|\n\\)*in memory"
                          (pdf-info-gettext 1 '(0 0 1 1))))))

(ert-deftest pdf-info-getselection ()
  (pdf-test-with-test-pdf
    (should (consp (pdf-info-getselection 1 '(0 0 1 1))))
    (should (cl-every 'pdf-test-relative-edges-p
                      (pdf-info-getselection 1 '(0 0 1 1))))))

(ert-deftest pdf-info-textregions ()
  (pdf-test-with-test-pdf
    (should (consp (pdf-info-textregions 1)))))

(ert-deftest pdf-info-pagesize ()
  (pdf-test-with-test-pdf
    (should (cl-every (lambda (size)
                        (and (consp size)
                             (natnump (car size))
                             (natnump (cdr size))))
                      (list (pdf-info-pagesize 1))))))

(ert-deftest pdf-info-quit ()
  (pdf-test-with-test-pdf
    (pdf-info-quit)
    (let (pdf-info-restart-process-p)
      (should-error (pdf-info-open)))
    (let ((pdf-info-restart-process-p t))
      (pdf-info-open)
      (should (pdf-info-close)))))

(ert-deftest pdf-info-getannots ()
  (skip-unless (pdf-info-writable-annotations-p))
  (pdf-test-with-test-pdf
    (when (memq 'markup-annotations
                (pdf-info-features))
      (cl-labels ((alists-get (alists key)
                    (mapcar (lambda (alist)
                              (cdr (assoc key alist)))
                            alists)))
        (let (annots)
          (should (= 5 (length (setq annots (cl-remove-if
                                             (lambda (elt)
                                               (memq (cdr (assq 'type elt))
                                                     '(file link)))
                                             (pdf-info-getannots))))))
          (should (equal (alists-get annots 'page)
                         '(2 2 2 2 2)))
          (should (equal (sort (copy-sequence (alists-get annots 'type)) 'string-lessp)
                         (sort (copy-sequence
                                '(text strike-out highlight underline squiggly))
                               'string-lessp)))
          (should (equal (alists-get annots 'color)
                         '("#ff0000" "#ff0000" "#ff0000"
                           "#ff0000" "#ff0000")))
          (should (cl-every 'pdf-test-relative-edges-p
                            (alists-get annots 'edges))))))))

(ert-deftest pdf-info-getannot ()
  (skip-unless (pdf-info-writable-annotations-p))
  (pdf-test-with-test-pdf
    (let* ((text-annot (car
                        (cl-remove-if-not (lambda (elt)
                                            (eq (cdr (assq 'type elt)) 'text))
                                          (pdf-info-getannots))))
           (key (cdr (assq 'id text-annot))))
      (should (consp text-annot))
      (should (symbolp key))
      (should key)
      (should (equal (cl-sort text-annot 'string-lessp :key 'car)
                     (cl-sort (pdf-info-getannot key)
                              'string-lessp :key 'car))))))

(ert-deftest pdf-info-addannot ()
  (skip-unless (pdf-info-writable-annotations-p))
  (pdf-test-with-test-pdf
    (let (annot)
      (should (consp (setq annot
                           (pdf-info-addannot 1 '(0 0 1 1) 'text))))
      (should (eq 1 (cdr (assq 'page annot))))
      (should (eq 'text (cdr (assq 'type annot))))
      (should (equal "" (cdr (assq 'contents annot)))))))

(ert-deftest pdf-info-delannot ()
  (skip-unless (pdf-info-writable-annotations-p))
  (pdf-test-with-test-pdf
    (let ((nannots (length (pdf-info-getannots)))
          annots)
      (push (pdf-info-addannot 1 '(0 0 1 1) 'text)
            annots)
      (when (memq 'markup-annotations
                  (pdf-info-features))

         (push (pdf-info-addannot 1 '(0 0 1 1) 'squiggly nil nil '(0 0 1 1)) annots)
         (push (pdf-info-addannot 1 '(0 0 1 1) 'highlight 'word nil '(0 0 1 1)) annots)
         (push (pdf-info-addannot 1 '(0 0 1 1) 'underline 'line nil '(0 0 1 1)) annots)
        (push (pdf-info-addannot 1 '(0 0 1 1) 'strike-out 'glyph nil '(0 0 1 1)) annots))
      (dolist (a annots)
        (pdf-info-delannot (cdr (assq 'id a))))
      (should (= nannots (length (pdf-info-getannots)))))))

(ert-deftest pdf-info-mvannot ()
  (skip-unless (pdf-info-writable-annotations-p))
  (pdf-test-with-test-pdf
    (let ((edges '(0.25 0.25 1.0 1.0))
          (id (cdr (assq 'id (car (pdf-info-getannots))))))
      (pdf-info-mvannot id edges)
      (should (equal (cdr (assq 'edges (pdf-info-getannot id)))
                     edges)))))

(ert-deftest pdf-info-editannot ()
  (skip-unless (pdf-info-writable-annotations-p))
  (pdf-test-with-test-pdf
    (let ((color "#ffa500")
          (id (cdr (assq 'id (car (pdf-info-getannots))))))
      (should (and id (symbolp id)))
      (pdf-info-editannot id `((color . ,color)))
      (should (equal (cdr (assq 'color (pdf-info-getannot id)))
                     color)))))

(ert-deftest pdf-info-save ()
  (skip-unless (pdf-info-writable-annotations-p))
  (pdf-test-with-test-pdf
    (dolist (id (mapcar (lambda (a)
                          (cdr (assq 'id a)))
                        (pdf-info-getannots)))
      (pdf-info-delannot id))
    (let (tempfile)
      (unwind-protect
          (progn
            (setq tempfile (pdf-info-save))
            (should (file-exists-p tempfile))
            (should (= 0 (length (pdf-info-getannots nil tempfile)))))
        (when (file-exists-p tempfile)
          (delete-file tempfile))))))

(defun pdf-info-test--edges-equal (e1 e2)
  "Return non-nil, if edges E1 and E2 are equal up to rounding."
  (and (= (length e1) (length e2))
       (cl-every (lambda (x y) (< (abs (- x y)) 1e-4)) e1 e2)))

(ert-deftest pdf-info-creatable-annotation-types-shapes ()
  (skip-unless (pdf-info-markup-annotations-p))
  (let ((types (pdf-info-creatable-annotation-types)))
    (should (eq (and (memq 'square types) t)
                (pdf-info-shape-annotations-p)))
    (should (eq (and (memq 'circle types) t)
                (pdf-info-shape-annotations-p)))
    (should (eq (and (memq 'line types) t)
                (pdf-info-shape-annotations-p)))
    (should (eq (and (memq 'stamp types) t)
                (pdf-info-stamp-annotations-p)))))

(ert-deftest pdf-info-addannot-square-circle ()
  (skip-unless (pdf-info-shape-annotations-p))
  (pdf-test-with-test-pdf
    (dolist (type '(square circle))
      (let* ((edges '(0.25 0.25 0.5 0.5))
             (a (pdf-info-addannot 1 edges type))
             (id (cdr (assq 'id a))))
        (should (eq type (cdr (assq 'type a))))
        (should (pdf-info-test--edges-equal edges (cdr (assq 'edges a))))
        (should (null (cdr (assq 'interior-color a))))
        (setq a (pdf-info-editannot id '((color . "#ff0000")
                                         (interior-color . "#00ff00"))))
        (should (equal "#ff0000" (cdr (assq 'color a))))
        (should (equal "#00ff00" (cdr (assq 'interior-color a))))
        (should (equal "#00ff00"
                       (cdr (assq 'interior-color (pdf-info-getannot id)))))
        ;; nil removes the fill color.
        (setq a (pdf-info-editannot id '((interior-color))))
        (should (null (cdr (assq 'interior-color a))))
        (pdf-info-mvannot id '(0.5 0.5 0.75 0.75))
        (should (pdf-info-test--edges-equal
                 '(0.5 0.5 0.75 0.75)
                 (cdr (assq 'edges (pdf-info-getannot id)))))
        (should-error (pdf-info-editannot id '((icon . "Approved"))))
        (should-error (pdf-info-editannot id '((line 0 0 1 1)))))))
  (pdf-test-with-test-pdf
    (should-error (pdf-info-addannot 1 '(0 0 1 1) 'square nil nil
                                     '(0 0 1 1)))))

(ert-deftest pdf-info-addannot-line ()
  (skip-unless (pdf-info-shape-annotations-p))
  (pdf-test-with-test-pdf
    ;; Without points, the line is the diagonal of the edges.
    (let ((a (pdf-info-addannot 1 '(0.25 0.25 0.5 0.5) 'line)))
      (should (eq 'line (cdr (assq 'type a))))
      (should (pdf-info-test--edges-equal '(0.25 0.25 0.5 0.5)
                                          (cdr (assq 'edges a)))))
    ;; With points, the edges are their bounding box plus a margin.
    (let* ((a (pdf-info-addannot 1 '(0 0 1 1) 'line nil nil
                                 '(0.75 0.25 0.25 0.5)))
           (id (cdr (assq 'id a)))
           (edges (cdr (assq 'edges a))))
      (should (pdf-test-relative-edges-p edges))
      (should (< 0.24 (nth 0 edges) 0.25))
      (should (< 0.24 (nth 1 edges) 0.25))
      (should (< 0.75 (nth 2 edges) 0.76))
      (should (< 0.5 (nth 3 edges) 0.51))
      ;; Lines are moved by their points, not their edges.
      (should-error (pdf-info-mvannot id '(0 0 0.1 0.1)))
      (setq edges (cdr (assq 'edges (pdf-info-editannot
                                     id '((line 0.1 0.1 0.2 0.2)
                                          (color . "#0000ff"))))))
      (should (< 0.09 (nth 0 edges) 0.1))
      (should (< 0.2 (nth 3 edges) 0.21))
      (should (equal "#0000ff"
                     (cdr (assq 'color (pdf-info-getannot id))))))))

(ert-deftest pdf-info-addannot-stamp ()
  (skip-unless (pdf-info-stamp-annotations-p))
  (pdf-test-with-test-pdf
    (let* ((edges '(0.25 0.25 0.5 0.3))
           (a (pdf-info-addannot 1 edges 'stamp nil nil "Approved"))
           (id (cdr (assq 'id a))))
      (should (eq 'stamp (cdr (assq 'type a))))
      (should (pdf-info-test--edges-equal edges (cdr (assq 'edges a))))
      (should (equal "Approved" (cdr (assq 'icon a))))
      ;; Stamps are no markup annotations for epdfinfo.
      (should-not (assq 'label a))
      (should (equal "TopSecret"
                     (cdr (assq 'icon (pdf-info-editannot
                                       id '((icon . "TopSecret"))))))))
    ;; Draft is the default, but can not be read back.
    (should (null (cdr (assq 'icon (pdf-info-addannot 1 '(0 0 0.5 0.1)
                                                      'stamp)))))
    (should (pdf-info-addannot 1 '(0 0 0.5 0.1) 'stamp nil nil "Draft"))
    (should-error (pdf-info-addannot 1 '(0 0 0.5 0.1) 'stamp nil nil
                                     "NoSuchIcon"))
    (let ((id (cdr (assq 'id (pdf-info-addannot 1 '(0 0 0.5 0.1) 'stamp)))))
      (should-error (pdf-info-editannot id '((icon . "Draft"))))
      (should-error (pdf-info-editannot id '((label . "Joe")))))))

(ert-deftest pdf-info-addannot-image-stamp ()
  (skip-unless (pdf-info-image-stamps-p))
  (pdf-test-with-test-pdf
    (let* ((edges '(0.25 0.25 0.5 0.3))
           (image (expand-file-name "signature.png"))
           (a (pdf-info-addannot 1 edges 'stamp nil nil
                                (concat "image:" image)))
           (id (cdr (assq 'id a))))
      (should (eq 'stamp (cdr (assq 'type a))))
      (should (pdf-info-test--edges-equal edges (cdr (assq 'edges a))))
      ;; The image is not an icon, and stamps are no markup
      ;; annotations for epdfinfo.
      (should (null (cdr (assq 'icon a))))
      (should-not (assq 'label a))
      ;; The annotation can be moved and deleted like any stamp.
      (should (pdf-info-test--edges-equal
               '(0.3 0.3 0.55 0.35)
               (cdr (assq 'edges (pdf-info-mvannot id '(0.3 0.3 0.55 0.35))))))
      (should (null (pdf-info-delannot id))))
    ;; The image is checked before anything is added.
    (should-error (pdf-info-addannot 1 '(0 0 0.5 0.1) 'stamp nil nil
                                     "image:/no/such/image.png"))
    (should-error (pdf-info-addannot
                   1 '(0 0 0.5 0.1) 'stamp nil nil
                   (concat "image:" (expand-file-name "test.pdf"))))))

(ert-deftest pdf-info-save-image-stamp ()
  (skip-unless (pdf-info-image-stamps-p))
  (pdf-test-with-test-pdf
    (let ((image (expand-file-name "signature.png"))
          saved)
      (pdf-info-addannot 1 '(0.25 0.25 0.5 0.3) 'stamp nil nil
                         (concat "image:" image))
      ;; Moving the stamp keeps its image.
      (let ((id (cdr (assq 'id (car (pdf-info-getannots 1))))))
        (pdf-info-mvannot id '(0.3 0.3 0.55 0.35)))
      (unwind-protect
          (progn
            (setq saved (pdf-info-save))
            ;; The stamp and its image appearance are written to the
            ;; file and read back.
            (should (cl-some
                     (lambda (a)
                       (and (eq 'stamp (cdr (assq 'type a)))
                            (null (cdr (assq 'icon a)))))
                     (pdf-info-getannots 1 saved)))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally saved)
              (goto-char (point-min))
              ;; The appearance of the image survived the move.
              (should (search-forward "/AP" nil t))))
        (when (file-exists-p saved)
          (delete-file saved))))))

(ert-deftest pdf-info-save-shape-annotations ()
  (skip-unless (and (pdf-info-shape-annotations-p)
                    (pdf-info-stamp-annotations-p)))
  (pdf-test-with-test-pdf
    (dolist (id (mapcar (lambda (a)
                          (cdr (assq 'id a)))
                        (pdf-info-getannots)))
      (pdf-info-delannot id))
    (let ((square (pdf-info-addannot 1 '(0.1 0.1 0.2 0.2) 'square))
          (line (pdf-info-addannot 1 '(0 0 1 1) 'line nil nil
                                   '(0.1 0.5 0.9 0.5)))
          tempfile)
      (pdf-info-editannot (cdr (assq 'id square))
                          '((color . "#ff0000") (interior-color . "#ffff00")))
      (pdf-info-addannot 1 '(0.5 0.1 0.7 0.2) 'circle)
      (pdf-info-addannot 1 '(0.1 0.7 0.5 0.8) 'stamp nil nil "Final")
      (pdf-info-delannot (cdr (assq 'id line)))
      (unwind-protect
          (let (annots)
            (setq tempfile (pdf-info-save))
            (setq annots (pdf-info-getannots nil tempfile))
            (should (equal '(circle square stamp)
                           (sort (mapcar (lambda (a) (cdr (assq 'type a)))
                                         annots)
                                 #'string-lessp)))
            (let ((square (cl-find 'square annots
                                   :key (lambda (a) (cdr (assq 'type a))))))
              (should (equal "#ff0000" (cdr (assq 'color square))))
              (should (equal "#ffff00" (cdr (assq 'interior-color square))))
              (should (pdf-info-test--edges-equal '(0.1 0.1 0.2 0.2)
                                                  (cdr (assq 'edges square)))))
            (should (equal "Final"
                           (cdr (assq 'icon (cl-find 'stamp annots
                                                     :key (lambda (a) (cdr (assq 'type a)))))))))
        (when (and tempfile (file-exists-p tempfile))
          (pdf-info-close tempfile)
          (delete-file tempfile))))))

(ert-deftest pdf-info-creatable-annotation-types-free-text-ink ()
  (skip-unless (pdf-info-markup-annotations-p))
  (let ((types (pdf-info-creatable-annotation-types)))
    (should (eq (and (memq 'free-text types) t)
                (pdf-info-free-text-annotations-p)))
    (should (eq (and (memq 'ink types) t)
                (pdf-info-ink-annotations-p)))))

(ert-deftest pdf-info-addannot-free-text-unsupported ()
  (skip-unless (and (pdf-info-writable-annotations-p)
                    (not (pdf-info-free-text-annotations-p))))
  (pdf-test-with-test-pdf
    (should-error (pdf-info-addannot 1 '(0.1 0.1 0.5 0.2) 'free-text))))

(ert-deftest pdf-info-addannot-ink-unsupported ()
  (skip-unless (and (pdf-info-writable-annotations-p)
                    (not (pdf-info-ink-annotations-p))))
  (pdf-test-with-test-pdf
    (should-error (pdf-info-addannot 1 '(0 0 1 1) 'ink nil nil
                                     '(0.1 0.1 0.2 0.2)))))

(ert-deftest pdf-info-addannot-free-text ()
  (skip-unless (pdf-info-free-text-annotations-p))
  (pdf-test-with-test-pdf
    (let* ((edges '(0.1 0.1 0.5 0.2))
           (a (pdf-info-addannot 1 edges 'free-text))
           (id (cdr (assq 'id a))))
      (should (eq 'free-text (cdr (assq 'type a))))
      (should (pdf-info-test--edges-equal edges (cdr (assq 'edges a))))
      ;; The defaults: black Helvetica on a transparent background.
      (should (equal "Helvetica" (cdr (assq 'font-name a))))
      (should (= 12 (cdr (assq 'font-size a))))
      (should (equal "#000000" (cdr (assq 'font-color a))))
      (should (null (cdr (assq 'color a))))
      (setq a (pdf-info-editannot id '((contents . "Hello, wörld")
                                       (font-size . 20)
                                       (font-color . "#ff0000")
                                       (color . "#ffff00"))))
      (should (equal "Hello, wörld" (cdr (assq 'contents a))))
      (should (= 20 (cdr (assq 'font-size a))))
      (should (equal "#ff0000" (cdr (assq 'font-color a))))
      (should (equal "#ffff00" (cdr (assq 'color a))))
      ;; Changing the text keeps the font.
      (setq a (pdf-info-editannot id '((contents . "Hello again"))))
      (should (= 20 (cdr (assq 'font-size a))))
      (should (equal "Helvetica" (cdr (assq 'font-name a))))
      (setq a (pdf-info-editannot id '((font-name . "Times"))))
      (should (equal "Times" (cdr (assq 'font-name a))))
      (should (= 20 (cdr (assq 'font-size a))))
      (pdf-info-mvannot id '(0.2 0.3 -1 -1))
      (should (pdf-info-test--edges-equal
               '(0.2 0.3 0.6 0.4)
               (cdr (assq 'edges (pdf-info-getannot id)))))
      (should-error (pdf-info-editannot id '((font-size . 0))))
      (should-error (pdf-info-editannot id '((font-name . ""))))
      (should-error (pdf-info-editannot id '((icon . "Note")))))
    (let ((id (cdr (assq 'id (pdf-info-addannot 1 '(0 0 0.1 0.1) 'text)))))
      (should-error (pdf-info-editannot id '((font-size . 10))))
      (should-error (pdf-info-editannot id '((font-color . "#ff0000")))))
    (should-error (pdf-info-addannot 1 '(0 0 1 1) 'free-text nil nil
                                     "Text"))))

(ert-deftest pdf-info-addannot-ink ()
  (skip-unless (pdf-info-ink-annotations-p))
  (pdf-test-with-test-pdf
    (let* ((a (pdf-info-addannot 1 '(0 0 1 1) 'ink nil nil
                                 "2" '(0.25 0.5 0.5 0.25 0.75 0.5)
                                 '(0.25 0.75)))
           (id (cdr (assq 'id a)))
           (edges (cdr (assq 'edges a))))
      (should (eq 'ink (cdr (assq 'type a))))
      ;; The strokes, a dot included, plus the line width determine
      ;; the edges.
      (should (pdf-test-relative-edges-p edges))
      (should (< 0.24 (nth 0 edges) 0.25))
      (should (< 0.24 (nth 1 edges) 0.25))
      (should (< 0.75 (nth 2 edges) 0.76))
      (should (< 0.75 (nth 3 edges) 0.76))
      (should (equal "#0000ff"
                     (cdr (assq 'color (pdf-info-editannot
                                        id '((color . "#0000ff")))))))
      ;; Moving the strokes keeps their size.
      (pdf-info-mvannot id '(0.1 0.2 -1 -1))
      (let ((moved (cdr (assq 'edges (pdf-info-getannot id)))))
        (should (pdf-info-test--edges-equal
                 (list 0.1 0.2
                       (+ 0.1 (- (nth 2 edges) (nth 0 edges)))
                       (+ 0.2 (- (nth 3 edges) (nth 1 edges))))
                 moved))))
    ;; Without a width.
    (should (pdf-info-addannot 1 '(0 0 1 1) 'ink nil nil '(0.1 0.1 0.2 0.2)))
    (should-error (pdf-info-addannot 1 '(0 0 1 1) 'ink))
    (should-error (pdf-info-addannot 1 '(0 0 1 1) 'ink nil nil "2"))
    (should-error (pdf-info-addannot 1 '(0 0 1 1) 'ink nil nil "0"
                                     '(0.1 0.1 0.2 0.2)))
    (should-error (pdf-info-addannot 1 '(0 0 1 1) 'ink nil nil
                                     '(0.1 0.1 0.2)))
    (should-error (pdf-info-addannot 1 '(0 0 1 1) 'ink nil nil
                                     '(0.1 1.5)))))

(ert-deftest pdf-info-save-free-text-ink-annotations ()
  (skip-unless (and (pdf-info-free-text-annotations-p)
                    (pdf-info-ink-annotations-p)))
  (pdf-test-with-test-pdf
    (dolist (id (mapcar (lambda (a)
                          (cdr (assq 'id a)))
                        (pdf-info-getannots)))
      (pdf-info-delannot id))
    (let ((text (pdf-info-addannot 1 '(0.1 0.1 0.5 0.2) 'free-text))
          (ink (pdf-info-addannot 1 '(0 0 1 1) 'ink nil nil
                                  "3" '(0.1 0.5 0.3 0.6)))
          tempfile)
      (pdf-info-editannot (cdr (assq 'id text))
                          '((contents . "Saved text") (font-size . 16)
                            (font-color . "#0000ff")))
      (pdf-info-editannot (cdr (assq 'id ink)) '((color . "#ff0000")))
      (unwind-protect
          (let (annots)
            (setq tempfile (pdf-info-save))
            (setq annots (pdf-info-getannots nil tempfile))
            (should (equal '(free-text ink)
                           (sort (mapcar (lambda (a) (cdr (assq 'type a)))
                                         annots)
                                 #'string-lessp)))
            (let ((text (cl-find 'free-text annots
                                 :key (lambda (a) (cdr (assq 'type a)))))
                  (ink (cl-find 'ink annots
                                :key (lambda (a) (cdr (assq 'type a))))))
              (should (equal "Saved text" (cdr (assq 'contents text))))
              (should (= 16 (cdr (assq 'font-size text))))
              (should (equal "#0000ff" (cdr (assq 'font-color text))))
              (should (pdf-info-test--edges-equal '(0.1 0.1 0.5 0.2)
                                                  (cdr (assq 'edges text))))
              (should (equal "#ff0000" (cdr (assq 'color ink))))
              (should (< 0.09 (nth 0 (cdr (assq 'edges ink))) 0.1))))
        (when (and tempfile (file-exists-p tempfile))
          (pdf-info-close tempfile)
          (delete-file tempfile))))))

(ert-deftest pdf-info-check-epdfinfo ()
  (should (progn (pdf-info-check-epdfinfo) t))
  (should-error
   (let (pdf-info-epdfinfo-program)
     (pdf-info-check-epdfinfo)))
  (should-error
   (let ((pdf-info-epdfinfo-program 42))
     (pdf-info-check-epdfinfo)))
  (should-error
   (let ((pdf-info-epdfinfo-program
          (make-temp-name "pdf-info")))
     (pdf-info-check-epdfinfo))))

;; FIXME: Write me.
;; (ert-deftest pdf-info-getattachment-from-annot ()
;;   (pdf-test-with-test-pdf
;;     ))

;; (ert-deftest pdf-info-getattachments ()
;;   (pdf-test-with-test-pdf
;;     ))

;; (ert-deftest pdf-info-synctex-forward-search ()
;;   (pdf-test-with-test-pdf
;;     ))

;; (ert-deftest pdf-info-synctex-backward-search ()
;;   (pdf-test-with-test-pdf
;;     ))

;; (ert-deftest pdf-info-renderpage ()
;;   (pdf-test-with-test-pdf
;;     ))

;; (ert-deftest pdf-info-renderpage-text-regions ()
;;   (pdf-test-with-test-pdf
;;     ))

;; (ert-deftest pdf-info-renderpage-highlight ()
;;   (pdf-test-with-test-pdf
;;     ))

;; (ert-deftest pdf-info-boundingbox ()
;;   (pdf-test-with-test-pdf
;;     ))

(ert-deftest pdf-info-query-runs-no-timers-while-waiting ()
  "A synchronous query may not hand control to a timer.

The wait is reachable from a mode-line construct, which Emacs evaluates
while it is displaying, so a timer running here runs arbitrary Lisp in the
middle of a redisplay."
  (pdf-test-with-test-pdf
    (let (fired)
      (run-at-time 0 nil (lambda () (setq fired t)))
      ;; Long enough for a timer due now to have had its chance.
      (should (pdf-info-renderpage 1 1400))
      (should-not fired)
      ;; And it is only held off, not lost.
      (sit-for 0.1)
      (should fired))))
