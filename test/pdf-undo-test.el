;; -*- lexical-binding: t -*-

(require 'pdf-undo)
(require 'pdf-pages)
(require 'pdf-annot)
(require 'ert)

;; Tests for pdf-undo.el.  The page operation half needs the qpdf
;; program; the other half works on a temporary copy of a fixture, like
;; the tests of `pdf-pages'.

(defvar pdf-undo-test-file nil
  "The temporary copy of the fixture visited by the current test.")

(defmacro pdf-undo-test-with-copy (fixture &rest body)
  "Run BODY in a buffer visiting a temporary copy of FIXTURE."
  (declare (indent 1) (debug t))
  (let ((dir (make-symbol "dir")))
    `(let* ((,dir (make-temp-file "pdf-undo-test" t))
            (pdf-undo-test-file (expand-file-name ,fixture ,dir))
            (pdf-pages-confirm nil)
            (pdf-undo--alist nil)
            (pdf-undo-backup-directory (expand-file-name "backups" ,dir)))
       (copy-file ,fixture pdf-undo-test-file)
       (unwind-protect
           (pdf-test-with-pdf pdf-undo-test-file
             (unless (derived-mode-p 'pdf-view-mode)
               (pdf-view-mode))
             (when (eq image-mode-winprops-alist t)
               (setq image-mode-winprops-alist nil))
             ,@body)
         (delete-directory ,dir t)))))

(defun pdf-undo-test-file-contents (file)
  "Return the bytes of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defmacro pdf-undo-test-with-answer (answer &rest body)
  "Run BODY with `y-or-n-p' answering ANSWER."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) ,answer)))
     ,@body))

(defun pdf-undo-test-annot ()
  "Return an annotation of the fixture on page 2."
  (or (car (pdf-annot-getannots 2))
      (error "The fixture has no annotation on page 2")))

;; The keys belong to this module, but the maps are in other ones.

(ert-deftest pdf-undo-test-keys ()
  (dolist (key '("C-/" "C-_"))
    (should (eq #'pdf-undo (lookup-key pdf-view-mode-map (kbd key)))))
  (should (eq #'pdf-undo
              (lookup-key pdf-pages-minor-mode-map
                          (vconcat pdf-pages-minor-mode-map-prefix (kbd "u"))))))

(ert-deftest pdf-undo-test-nothing-to-undo ()
  (pdf-undo-test-with-copy "test.pdf"
    (should (string-match-p
             "Nothing to undo"
             (cadr (should-error (pdf-undo) :type 'user-error))))))

;; A buffer with unsaved changes is undone by going back to the file on
;; disk, which loses the changes and therefore asks first.

(ert-deftest pdf-undo-test-discard-unsaved-changes ()
  (pdf-undo-test-with-copy "test.pdf"
    (let* ((annot (pdf-undo-test-annot))
           (id (alist-get 'id annot))
           (contents (alist-get 'contents annot))
           (before (pdf-undo-test-file-contents pdf-undo-test-file)))
      (pdf-annot-put annot 'contents "an unsaved change")
      (should (buffer-modified-p))
      (pdf-undo-test-with-answer t
        (pdf-undo))
      (should-not (buffer-modified-p))
      ;; The file never held the change, and the server sees the old
      ;; annotation again.
      (should (equal (pdf-undo-test-file-contents pdf-undo-test-file) before))
      (should (equal (alist-get 'contents
                                (cl-find id (pdf-annot-getannots 2)
                                         :key (lambda (a) (alist-get 'id a))
                                         :test #'equal))
                     contents))
      ;; Nothing is left to undo either.
      (should-error (pdf-undo) :type 'user-error))))

(ert-deftest pdf-undo-test-discard-asks ()
  (pdf-undo-test-with-copy "test.pdf"
    (let ((annot (pdf-undo-test-annot)))
      (pdf-annot-put annot 'contents "an unsaved change")
      (pdf-undo-test-with-answer nil
        (should-error (pdf-undo) :type 'user-error))
      (should (buffer-modified-p))
      (should (equal (alist-get 'contents
                                (cl-find (alist-get 'id annot)
                                         (pdf-annot-getannots 2)
                                         :key (lambda (a) (alist-get 'id a))
                                         :test #'equal))
                     "an unsaved change")))))

;; The page operations and the unsaved changes fit together because a
;; page operation refuses to run while the buffer is modified: the
;; newer change always comes first.

(ert-deftest pdf-undo-test-page-operation-then-unsaved-changes ()
  (skip-unless (executable-find pdf-pages-qpdf-program))
  (pdf-undo-test-with-copy "test.pdf"
    (let ((before (pdf-undo-test-file-contents pdf-undo-test-file))
          (file (file-truename pdf-undo-test-file)))
      (pdf-pages-rotate '(1) 90)
      (should (equal (length (cdr (assoc file pdf-undo--alist))) 1))
      (should-not (buffer-modified-p))
      (let ((rotated (pdf-undo-test-file-contents pdf-undo-test-file)))
        ;; An annotation changed on top of the rotated file.
        (pdf-annot-put (pdf-undo-test-annot) 'contents "an unsaved change")
        (should (buffer-modified-p))
        (pdf-undo-test-with-answer t
          (pdf-undo))
        (should-not (buffer-modified-p))
        ;; Only the annotation is gone.
        (should (equal (pdf-undo-test-file-contents pdf-undo-test-file)
                       rotated))
        (pdf-undo)
        (should (equal (pdf-undo-test-file-contents pdf-undo-test-file)
                       before))
        (should-not (cdr (assoc file pdf-undo--alist)))
        (should-error (pdf-undo) :type 'user-error)))))
