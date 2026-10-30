;; -*- lexical-binding: t -*-

(require 'pdf-xfdf)
(require 'pdf-info)
(require 'cl-lib)
(require 'xml)
(require 'ert)

;; Tests for pdf-xfdf.el.  They work on temporary XFDF files and
;; change only the copy of a document that the server holds, never
;; the fixtures themselves.

(defun pdf-xfdf-test--approx (edges)
  "Return EDGES rounded to three decimals."
  (mapcar (lambda (x) (/ (round (* x 1000)) 1000.0)) edges))

(defun pdf-xfdf-test--edges-close-p (a b)
  "Return non-nil if the edges A and B differ by at most 0.02 each."
  (and (= (length a) (length b))
       (cl-every (lambda (pair)
                   (< (abs (- (car pair) (cdr pair))) 0.02))
                 (cl-mapcar #'cons a b))))

(defun pdf-xfdf-test--parse (file)
  "Return the parsed contents of the XFDF file FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (xml-parse-region)))

(defun pdf-xfdf-test--annots (tree)
  "Return the annotation elements of the parsed XFDF TREE."
  (let* ((root (car tree))
         (annots (and (eq 'xfdf (xml-node-name root))
                      (cl-find-if (lambda (node)
                                    (and (consp node)
                                         (eq 'annots (xml-node-name node))))
                                  (xml-node-children root)))))
    (cl-remove-if-not #'consp (xml-node-children annots))))

(defun pdf-xfdf-test--attr (element name)
  "Return the attribute NAME of the XFDF ELEMENT."
  (cdr (assq name (xml-node-attributes element))))

(defun pdf-xfdf-test--element (elements name)
  "Return the first of ELEMENTS named NAME, a string."
  (cl-find (intern name) elements :key #'xml-node-name))

(ert-deftest pdf-xfdf-export ()
  (skip-unless (pdf-info-writable-annotations-p))
  (pdf-test-with-test-pdf
    (let* ((dir (make-temp-file "pdf-xfdf" t))
           (target (expand-file-name "test.xfdf" dir))
           (highlight (pdf-info-addannot
                      1 '(0 0 1 1) 'highlight 'word nil '(0 0 1 1)))
           (note (pdf-info-addannot 1 '(0.1 0.1 0.3 0.3) 'text))
           tree elements)
      (should (eq 'highlight (cdr (assq 'type highlight))))
      (unwind-protect
          (progn
            (pdf-info-editannot
             (cdr (assq 'id note))
             '((contents . "A <note>")
               (label . "Tim")
               (color . "#ffa500")
               (popup 0.5 0.5 0.8 0.7)
               (popup-is-open . t)))
            (pdf-xfdf-export (buffer-file-name) target)
            (setq tree (pdf-xfdf-test--parse target)
                  elements (pdf-xfdf-test--annots tree))
            ;; The two new annotations and the five annotations of
            ;; test.pdf (four markup, one text note) are written;
            ;; its links and its file attachment are not.
            (should (= 7 (length elements)))
            (let ((element (pdf-xfdf-test--element elements "text"))
                  (tokens 0))
              (should element)
              (should (equal "0" (pdf-xfdf-test--attr element 'page)))
              (should (equal "Tim" (pdf-xfdf-test--attr element 'title)))
              (should (equal "#ffa500"
                             (pdf-xfdf-test--attr element 'color)))
              ;; The XML parser replaced the entities.
              (should (equal "A <note>"
                             (pdf-xfdf-test--attr element 'contents)))
              (should (eq 'popup
                         (xml-node-name
                          (car (xml-node-children element)))))
              (should (equal "yes"
                             (pdf-xfdf-test--attr
                              (car (xml-node-children element)) 'open))))
            (let ((element (pdf-xfdf-test--element elements "highlight")))
              (should element)
              (should (equal "0" (pdf-xfdf-test--attr element 'page)))
              ;; Quad points, eight per marked-up region.
              (setq tokens (length
                           (split-string
                            (pdf-xfdf-test--attr element 'coords) "," t)))
              (should (> tokens 0))
              (should (= 0 (% tokens 8))))
            ;; The document is named in an f element.
            (should (cl-some
                     (lambda (node)
                       (and (consp node) (eq 'f (xml-node-name node))))
                     (xml-node-children (car tree)))))
        (delete-directory dir t)))))

(ert-deftest pdf-xfdf-import ()
  (skip-unless (pdf-info-writable-annotations-p))
  (pdf-test-with-test-pdf
    (let* ((dir (make-temp-file "pdf-xfdf" t))
           (source (expand-file-name "in.xfdf" dir))
           (size (pdf-info-pagesize 1))
           (w (car size))
           (h (cdr size))
           imported note highlight)
      (unwind-protect
          (progn
            (with-temp-file source
              (insert
               "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
               "<xfdf xml:space=\"preserve\" xmlns=\"http://ns.adobe.com/xfdf/\">\n"
               " <annots>\n"
               (format "  <text page=\"0\" rect=\"%.6f,%.6f,%.6f,%.6f\" color=\"#ffa500\" title=\"Tim\" contents=\"A note\" open=\"no\"/>\n"
                       (* 0.1 w) (* (- 1 0.3) h) (* 0.3 w) (* (- 1 0.1) h))
               (format "  <highlight page=\"0\" rect=\"%.6f,%.6f,%.6f,%.6f\" color=\"#ff0000\" title=\"Tim\" coords=\"%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f\"><popup page=\"0\" rect=\"0,0,10,10\" open=\"no\"/></highlight>\n"
                       0 0 w h
                       (* 0.05 w) (* 0.95 h) (* 0.95 w) (* 0.95 h)
                       (* 0.05 w) (* 0.05 h) (* 0.95 w) (* 0.05 h))
               " </annots>\n"
               "</xfdf>\n"))
            (setq imported (pdf-xfdf-import (buffer-file-name) source))
            (should (= 2 (length imported)))
            (setq note (cl-find 'text imported
                                :key (lambda (a) (cdr (assq 'type a))))
                  highlight (cl-find 'highlight imported
                                     :key (lambda (a)
                                            (cdr (assq 'type a)))))
            ;; The note.
            (should (equal "A note" (cdr (assq 'contents note))))
            (should (equal "Tim" (cdr (assq 'label note))))
            (should (equal "#ffa500" (cdr (assq 'color note))))
            (should (equal '(0.1 0.1 0.3 0.3)
                           (mapcar (lambda (x) (/ (round (* x 10)) 10.0))
                                   (cdr (assq 'edges note)))))
            ;; The highlight, on the text its quad points select:
            ;; the server snaps the region to the words in it, so
            ;; only check that it marks up text.
            (should (equal "Tim" (cdr (assq 'label highlight))))
            (should (cdr (assq 'markup-edges highlight)))
            (should (cdr (assq 'popup-edges highlight))))
        (delete-directory dir t)))))

(ert-deftest pdf-xfdf-round-trip ()
  (skip-unless (pdf-info-writable-annotations-p))
  (let* ((dir (make-temp-file "pdf-xfdf" t))
         (target (expand-file-name "round.xfdf" dir))
         (copy (expand-file-name "test.pdf" dir)))
    ;; Start the server: the previous test quit it, and
    ;; `pdf-view-mode' needs it.
    (pdf-info-quit)
    (pdf-info-process-assert-running t)
    (unwind-protect
        (progn
          (copy-file "test.pdf" copy)
          (with-current-buffer (find-file-noselect copy)
            (unwind-protect
                (let ((note (progn
                              (pdf-view-mode)
                              (when (eq image-mode-winprops-alist t)
                                (setq image-mode-winprops-alist nil))
                              (pdf-info-addannot
                               1 '(0.1 0.1 0.3 0.3) 'text)))
                      (highlight (pdf-info-addannot
                                  1 '(0 0 1 1) 'highlight
                                  'word nil '(0 0 1 1)))
                      before after)
                  (pdf-info-editannot
                   (cdr (assq 'id note))
                   '((contents . "Feedback")
                     (label . "Tim")
                     (color . "#ffa500")))
                  (pdf-info-editannot
                   (cdr (assq 'id highlight))
                   '((label . "Tim")))
                  (setq before (pdf-info-getannots nil copy))
                  ;; Write every annotation: mine and test.pdf's.
                  (pdf-xfdf-export copy target)
                  ;; Remove them all and read them back.
                  (dolist (a (pdf-info-getannots nil copy))
                    (pdf-info-delannot (cdr (assq 'id a))))
                  (setq after (pdf-xfdf-import copy target))
                  ;; The links and the file attachment are not
                  ;; written, everything else comes back.
                  (should (= 7 (length after)))
                  (let ((note (cl-find 'text after
                                        :key (lambda (a)
                                               (cdr (assq 'type a)))))
                        (highlight (cl-find 'highlight after
                                            :key (lambda (a)
                                                   (cdr (assq 'type a)))))
                        (old-note (cl-find 'text before
                                           :key (lambda (a)
                                                  (cdr (assq 'type a)))))
                        (old-highlight (cl-find 'highlight before
                                                 :key (lambda (a)
                                                        (cdr (assq 'type a))))))
                    ;; What survives a round trip.
                    (should (equal (cdr (assq 'contents old-note))
                                   (cdr (assq 'contents note))))
                    (should (equal (cdr (assq 'label old-note))
                                   (cdr (assq 'label note))))
                    (should (equal (cdr (assq 'color old-note))
                                   (cdr (assq 'color note))))
                    (should (equal (pdf-xfdf-test--approx
                                    (cdr (assq 'edges old-note)))
                                   (pdf-xfdf-test--approx
                                    (cdr (assq 'edges note)))))
                    (should (equal (cdr (assq 'label old-highlight))
                                   (cdr (assq 'label highlight))))
                    ;; The markup annotation covers the same text
                    ;; again; the import snaps the quad points by
                    ;; glyphs, so the regions may be grouped and
                    ;; sized slightly differently than by words.
                    (should (pdf-xfdf-test--edges-close-p
                             (apply #'pdf-util-edges-union
                                    (cdr (assq 'markup-edges old-highlight)))
                             (apply #'pdf-util-edges-union
                                    (cdr (assq 'markup-edges highlight)))))))
              (kill-buffer))))
      (delete-directory dir t))))

(ert-deftest pdf-xfdf-import-skip ()
  (pdf-test-with-test-pdf
    (let* ((dir (make-temp-file "pdf-xfdf" t))
           (source (expand-file-name "skip.xfdf" dir)))
      (unwind-protect
          (progn
            (with-temp-file source
              (insert
               "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
               "<xfdf xml:space=\"preserve\" xmlns=\"http://ns.adobe.com/xfdf/\">\n"
               " <annots>\n"
               ;; An annotation with a type this epdfinfo cannot
               ;; create, and one with an unknown element name.
               "  <ink page=\"0\" rect=\"0,0,10,10\"/>\n"
               "  <nosuchtype page=\"0\" rect=\"0,0,10,10\"/>\n"
               " </annots>\n"
               "</xfdf>\n"))
            ;; Both are skipped, with a message rather than an error.
            (should (null (pdf-xfdf-import (buffer-file-name) source))))
        (delete-directory dir t)))))
