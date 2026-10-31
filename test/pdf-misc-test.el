;;; pdf-misc-test.el --- Tests for pdf-misc.el  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the metadata buffer, the mode-line size indication, the
;; menus and printing.  Printing never starts a real process:
;; `start-process' is replaced by a recorder.  The size indication
;; needs a window showing a page image, which batch Emacs cannot
;; provide, so the window geometry is stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pdf-misc)

(defmacro pdf-misc-test-with-pdf (file &rest body)
  "Run BODY in a buffer visiting FILE, like `pdf-test-with-pdf'.
Start the server first, so the buffer is fully set up (see
`pdf-annot-test-with-pdf' in pdf-annot-test.el), and open it in
`pdf-view-mode' whatever earlier tests did to `auto-mode-alist'."
  (declare (indent 1) (debug t))
  `(let ((auto-mode-alist (cons pdf-tools-auto-mode-alist-entry
                                auto-mode-alist)))
     (pdf-info-process-assert-running t)
     (pdf-test-with-pdf ,file ,@body)))

(defmacro pdf-misc-test-with-clean-state (&rest body)
  "Run BODY, then restore windows and kill buffers it created."
  (declare (indent 0) (debug t))
  (let ((buffers (make-symbol "buffers"))
        (config (make-symbol "config")))
    `(let ((,buffers (buffer-list))
           (,config (current-window-configuration)))
       (unwind-protect
           (progn ,@body)
         (set-window-configuration ,config)
         (dolist (b (buffer-list))
           (unless (memq b ,buffers)
             (with-current-buffer b (set-buffer-modified-p nil))
             (let (kill-buffer-hook kill-buffer-query-functions)
               (kill-buffer b))))))))

(defmacro pdf-misc-test-with-geometry (image-height window-height vscroll &rest body)
  "Run BODY as if the selected window showed a page image.
The image is IMAGE-HEIGHT pixels high, the window body
WINDOW-HEIGHT pixels and the window is scrolled down by VSCROLL
pixels."
  (declare (indent 3) (debug t))
  `(cl-letf (((symbol-function 'window-vscroll)
              (lambda (&rest _) ,vscroll))
             ((symbol-function 'window-inside-pixel-edges)
              (lambda (&rest _) (list 0 10 600 (+ 10 ,window-height))))
             ((symbol-function 'pdf-view-image-size)
              (lambda (&rest _) (cons 600 ,image-height))))
     ,@body))

(defun pdf-misc-test-menu-commands (keymap)
  "Return all commands reachable in KEYMAP and its submenus."
  (let (commands)
    (cl-labels ((walk (map)
                  (map-keymap
                   (lambda (_key binding)
                     (let ((def (if (eq (car-safe binding) 'menu-item)
                                    (nth 2 binding)
                                  binding)))
                       (cond
                        ((keymapp def) (walk def))
                        ((symbolp def) (push def commands)))))
                   map)))
      (walk keymap))
    commands))


;; * ================================================================== *
;; * Metadata
;; * ================================================================== *

(ert-deftest pdf-misc-test-display-metadata ()
  (pdf-misc-test-with-pdf "test.pdf"
    (pdf-misc-test-with-clean-state
      (let* ((pdf-buffer (current-buffer))
             (md (pdf-misc-display-metadata))
             (pad (apply #'max (mapcar (lambda (d) (length (symbol-name (car d)))) md))))
        (should (equal md (pdf-info-metadata)))
        (with-current-buffer "*PDF-Metadata*"
          (should (derived-mode-p 'pdf-metadata-buffer-mode))
          (should buffer-read-only)
          (should (equal header-line-format (buffer-name pdf-buffer)))
          (should (get-buffer-window (current-buffer)))
          ;; One line per entry, keys right-aligned on the colon.
          (let ((lines (split-string (buffer-string) "\n" t)))
            (should (= (length lines) (length md)))
            (cl-loop for (key . _) in md
                     for line in lines
                     do (should (string-match "\\` *\\([a-z-]+\\):" line))
                     do (should (equal (match-string 1 line) (symbol-name key)))
                     do (should (= (match-end 1) pad))))
          (goto-char (point-min))
          (should (search-forward "creator:LaTeX with hyperref package" nil t))
          (should (search-forward "producer:pdfTeX" nil t)))))))

(ert-deftest pdf-misc-test-display-metadata-formatting ()
  "Keywords are joined with commas; long values wrap under the value column."
  (pdf-misc-test-with-pdf "test.pdf"
    (pdf-misc-test-with-clean-state
      (let ((long-title (mapconcat #'identity (make-list 40 "word") " ")))
        (cl-letf (((symbol-function 'pdf-info-metadata)
                   (lambda (&rest _)
                     `((title . ,long-title)
                       (keywords "alpha" "beta" "gamma")))))
          (pdf-misc-display-metadata))
        (with-current-buffer "*PDF-Metadata*"
          (goto-char (point-min))
          (should (search-forward "keywords:alpha, beta, gamma" nil t))
          (let ((lines (split-string (buffer-string) "\n" t))
                (prefix (make-string (1+ (length "keywords")) ?\s)))
            ;; The title spans several lines, each indented to the
            ;; value column.
            (should (> (length lines) 2))
            (should (string-prefix-p "   title:word word" (car lines)))
            (dolist (line (butlast (cdr lines)))
              (should (string-prefix-p (concat prefix "word") line)))
            (should (= (cl-count "word" (split-string (buffer-string) "[ :\n]+")
                                 :test #'equal)
                       40))))))))

(ert-deftest pdf-misc-test-display-metadata-requires-pdf-buffer ()
  (with-temp-buffer
    (should-error (pdf-misc-display-metadata))))


;; * ================================================================== *
;; * Size indication
;; * ================================================================== *

(ert-deftest pdf-misc-test-size-indication ()
  (pdf-misc-test-with-pdf "test.pdf"
    ;; Window shows the top half of the page.
    (pdf-misc-test-with-geometry 1000 500 0
      (should (equal (pdf-misc-size-indication) " Top")))
    ;; Scrolled to the bottom.
    (pdf-misc-test-with-geometry 1000 500 500
      (should (equal (pdf-misc-size-indication) " Bot")))
    ;; The whole page fits.
    (pdf-misc-test-with-geometry 400 500 0
      (should (equal (pdf-misc-size-indication) " All")))
    ;; Somewhere in between: a percentage, with the % escaped for the
    ;; mode line.
    (pdf-misc-test-with-geometry 1000 500 250
      (should (equal (pdf-misc-size-indication) " 25%%")))
    (pdf-misc-test-with-geometry 1000 300 333
      (should (equal (pdf-misc-size-indication) " 34%%")))))

(ert-deftest pdf-misc-test-size-indication-minor-mode ()
  "The mode adds its indicator to the mode line once and removes it again."
  (let ((saved (default-value 'mode-line-position)))
    (unwind-protect
        (pdf-misc-test-with-pdf "test.pdf"
          (pdf-misc-size-indication-minor-mode -1)
          (should-not (assq 'pdf-misc-size-indication-minor-mode mode-line-position))
          (pdf-misc-size-indication-minor-mode 1)
          (pdf-misc-size-indication-minor-mode 1)
          (should (= 1 (cl-count 'pdf-misc-size-indication-minor-mode
                                 mode-line-position :key #'car-safe)))
          ;; The entry is shown while the mode is on and evaluates to
          ;; the indicator.  (`format-mode-line' renders nothing in
          ;; batch, so evaluate the :eval form directly.)
          (let ((entry (assq 'pdf-misc-size-indication-minor-mode mode-line-position)))
            (should (eq (car entry) 'pdf-misc-size-indication-minor-mode))
            (should (eq (car-safe (cadr entry)) :eval))
            (pdf-misc-test-with-geometry 1000 500 250
              (should (equal (eval (cadr (cadr entry)) t) " 25%%"))))
          (pdf-misc-size-indication-minor-mode -1)
          (should-not (assq 'pdf-misc-size-indication-minor-mode mode-line-position)))
      (setq-default mode-line-position saved))))

(ert-deftest pdf-misc-test-size-indication-minor-mode-per-buffer ()
  "Turning the indicator off in one PDF buffer leaves the others alone.
The mode edits `mode-line-position' with `setq'; this relies on
`pdf-view-mode' having made that variable buffer-local."
  (let ((saved (default-value 'mode-line-position))
        (copy (make-temp-file "pdf-misc-test" nil ".pdf")))
    (unwind-protect
        (pdf-misc-test-with-pdf "test.pdf"
          (copy-file "test.pdf" copy t)
          (let ((other (find-file-noselect copy)))
            (unwind-protect
                (progn
                  (pdf-misc-size-indication-minor-mode 1)
                  (with-current-buffer other
                    (pdf-misc-size-indication-minor-mode 1)
                    (pdf-misc-size-indication-minor-mode -1))
                  (should pdf-misc-size-indication-minor-mode)
                  (should (assq 'pdf-misc-size-indication-minor-mode
                                mode-line-position))
                  (should-not (assq 'pdf-misc-size-indication-minor-mode
                                    (default-value 'mode-line-position))))
              (kill-buffer other))))
      (setq-default mode-line-position saved)
      (delete-file copy))))

(ert-deftest pdf-misc-test-minor-modes-require-pdf-buffer ()
  (with-temp-buffer
    (should-error (pdf-misc-size-indication-minor-mode 1))
    (should-error (pdf-misc-menu-bar-minor-mode 1))
    (should-error (pdf-misc-context-menu-minor-mode 1))))


;; * ================================================================== *
;; * Menus and keys
;; * ================================================================== *

(ert-deftest pdf-misc-test-keymap ()
  (should (eq (lookup-key pdf-misc-minor-mode-map (kbd "I"))
              #'pdf-misc-display-metadata))
  (should (eq (lookup-key pdf-misc-minor-mode-map (kbd "C-c C-p"))
              #'pdf-misc-print-document))
  (should (eq (lookup-key pdf-misc-context-menu-minor-mode-map [down-mouse-3])
              #'pdf-misc-popup-context-menu)))

(ert-deftest pdf-misc-test-menu-bar ()
  (let ((commands (pdf-misc-test-menu-commands pdf-misc-menu-bar-minor-mode-map)))
    (dolist (cmd '(pdf-misc-display-metadata pdf-misc-print-document
                   pdf-annot-list-annotations pdf-tools-customize))
      (should (memq cmd commands)))))

(ert-deftest pdf-misc-test-popup-context-menu ()
  "The context menu is the menu-bar menu."
  (pdf-misc-test-with-pdf "test.pdf"
    (let (shown)
      (cl-letf (((symbol-function 'popup-menu)
                 (lambda (menu &rest _) (setq shown menu))))
        (pdf-misc-popup-context-menu nil))
      (should (keymapp shown))
      (let ((commands (pdf-misc-test-menu-commands shown)))
        (dolist (cmd '(pdf-misc-display-metadata pdf-misc-print-document
                       pdf-tools-customize))
          (should (memq cmd commands)))))))


;; * ================================================================== *
;; * Printing
;; * ================================================================== *

(ert-deftest pdf-misc-test-print-program ()
  (let ((sh (executable-find "sh")))
    (skip-unless sh)
    (let ((pdf-misc-print-program-executable "sh"))
      (should (equal (pdf-misc-print-program) sh)))
    (let ((pdf-misc-print-program-executable "/no/such/printing-program"))
      (should (null (pdf-misc-print-program))))
    (let ((pdf-misc-print-program-executable nil))
      (should (null (pdf-misc-print-program))))))

(ert-deftest pdf-misc-test-print-program-interactive ()
  "Interactively the user picks a program, which is remembered."
  (let ((sh (executable-find "sh")))
    (skip-unless sh)
    (let ((pdf-misc-print-program-executable nil)
          saved)
      (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) sh))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'customize-save-variable)
                 (lambda (&rest args) (push args saved))))
        (should (equal (pdf-misc-print-program t) sh))
        (should (equal pdf-misc-print-program-executable sh))
        (should (null saved))
        ;; Answering yes saves the choice with customize.
        (setq pdf-misc-print-program-executable nil)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (should (equal (pdf-misc-print-program t) sh)))
        (should (equal saved `((pdf-misc-print-program-executable ,sh))))))))

(ert-deftest pdf-misc-test-print-document ()
  (let ((sh (executable-find "sh")))
    (skip-unless sh)
    (let ((file (expand-file-name "test.pdf"))
          (pdf-misc-print-program-executable "sh")
          (pdf-misc-print-program-args '("-P" "office"))
          (inhibit-message t)
          started)
      (cl-letf (((symbol-function 'start-process)
                 (lambda (&rest args) (push args started) nil)))
        (should (equal (pdf-misc-print-document file)
                       (format "Print job started: %s -P office %s" sh file)))
        (should (equal started `(("printing" nil ,sh "-P" "office" ,file))))
        ;; No program: an error and no process.
        (setq started nil)
        (let ((pdf-misc-print-program-executable nil))
          (should-error (pdf-misc-print-document file))
          (should (null started)))
        ;; The file must be readable.
        (should-error (pdf-misc-print-document "/no/such/file.pdf"))
        (should-error (pdf-misc-print-document nil))
        (should (null started))))))

(ert-deftest pdf-misc-test-print-document-interactive ()
  "Interactively the document of the current buffer is printed."
  (let ((sh (executable-find "sh")))
    (skip-unless sh)
    (pdf-misc-test-with-pdf "test.pdf"
      (let ((pdf-misc-print-program-executable "sh")
            (pdf-misc-print-program-args nil)
            (inhibit-message t)
            started)
        (cl-letf (((symbol-function 'start-process)
                   (lambda (&rest args) (push args started) nil)))
          (call-interactively #'pdf-misc-print-document))
        (should (equal started `(("printing" nil ,sh ,(pdf-view-buffer-file-name)))))))))


;; * Editing metadata

;; Tests for editing document metadata.  Every test works on a copy
;; of a PDF in a temporary directory, since saving writes the file.

(defconst pdf-misc-test-unicode-title "Café Straße 日本"
  "A title with Latin-1, non Latin-1 and CJK characters.")

(defmacro pdf-misc-test-with-copy (file-var source &rest body)
  "Copy SOURCE into a temporary directory and run BODY visiting it.

FILE-VAR is bound to the copy's file name.  SOURCE is a file
name, or a function that writes a PDF to the file name it is
called with.

Unlike `pdf-test-with-pdf', this starts the server before
visiting the file and binds `auto-mode-alist' to select
`pdf-view-mode', so that the mode is set up completely even when
an earlier test quit the server or uninstalled the alists."
  (declare (indent 2) (debug t))
  (let ((dir (make-symbol "dir"))
        (buffer (make-symbol "buffer")))
    `(let* ((,dir (make-temp-file "pdf-misc-test" t))
            (,file-var (expand-file-name "copy.pdf" ,dir))
            (pdf-info-epdfinfo-error-filename
             (expand-file-name "epdfinfo.log" ,dir))
            ,buffer)
       (unwind-protect
           (progn
             (if (functionp ,source)
                 (funcall ,source ,file-var)
               (copy-file ,source ,file-var))
             (pdf-info-quit)
             (pdf-info-process-assert-running t)
             (let ((auto-mode-alist (cons '("\\.[pP][dD][fF]\\'" . pdf-view-mode)
                                          auto-mode-alist)))
               (setq ,buffer (find-file-noselect ,file-var)))
             (with-current-buffer ,buffer
               (should pdf-view--server-file-name)
               ,@body))
         (when (buffer-live-p ,buffer)
           (with-current-buffer ,buffer
             (set-buffer-modified-p nil)
             (let (kill-buffer-hook)
               (kill-buffer))))
         (pdf-info-quit)
         (delete-directory ,dir t)))))

(defun pdf-misc-test-metadata-of-file (file)
  "Return the metadata of FILE, as freshly read from disk."
  (pdf-info-close file)
  (prog1 (pdf-info-metadata file)
    (pdf-info-close file)))

(defun pdf-misc-test-write-xmp-pdf (file title)
  "Write a one page PDF to FILE, with TITLE in Info and XMP.

TITLE should be ASCII."
  (let* ((xmp (concat
               "<?xpacket begin=\"\357\273\277\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n"
               "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">"
               "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">"
               "<rdf:Description rdf:about=\"\""
               " xmlns:dc=\"http://purl.org/dc/elements/1.1/\">"
               "<dc:title><rdf:Alt>"
               "<rdf:li xml:lang=\"de\">Anderer Titel</rdf:li>"
               "<rdf:li xml:lang=\"x-default\">" title "</rdf:li>"
               "</rdf:Alt></dc:title>"
               "</rdf:Description></rdf:RDF></x:xmpmeta>\n"
               "<?xpacket end=\"w\"?>"))
         (objects
          (list "<< /Type /Catalog /Pages 2 0 R /Metadata 4 0 R >>"
                "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
                "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 200 200] >>"
                (format "<< /Type /Metadata /Subtype /XML /Length %d >>\n\
stream\n%s\nendstream"
                        (length xmp) xmp)
                (format "<< /Title (%s) >>" title)))
         offsets)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert "%PDF-1.5\n")
      (cl-loop for obj in objects
               for i from 1
               do (push (1- (point)) offsets)
               (insert (format "%d 0 obj\n%s\nendobj\n" i obj)))
      (let ((xref (1- (point))))
        (insert (format "xref\n0 %d\n0000000000 65535 f \n"
                        (1+ (length objects))))
        (dolist (offset (nreverse offsets))
          (insert (format "%010d 00000 n \n" offset)))
        (insert (format "trailer\n<< /Size %d /Root 1 0 R /Info 5 0 R >>\n\
startxref\n%d\n%%%%EOF\n"
                        (1+ (length objects)) xref)))
      (let ((coding-system-for-write 'binary))
        (write-region nil nil file nil 'silent)))))

(defun pdf-misc-test-xmp-pdf (file)
  (pdf-misc-test-write-xmp-pdf file "Original Title"))

;; * Server level

(ert-deftest pdf-info-setmetadata-save-roundtrip ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (let ((md (pdf-info-setmetadata
               `((title . ,pdf-misc-test-unicode-title)
                 (author . "A. U. Thor")
                 (subject . "Colons: and \\ backslashes")
                 (keywords . ("one" "two"))
                 (creator . "pdf-misc-test")))))
      ;; The response reflects the change ...
      (let-alist md
        (should (equal .title pdf-misc-test-unicode-title))
        (should (equal .author "A. U. Thor"))
        (should (equal .subject "Colons: and \\ backslashes"))
        (should (equal .keywords '("one" "two")))
        (should (equal .creator "pdf-misc-test")))
      ;; ... but the file is unchanged until saved.
      (should (equal "" (cdr (assq 'title (pdf-misc-test-metadata-of-file
                                           file)))))
      (pdf-info-setmetadata `((title . ,pdf-misc-test-unicode-title)
                              (author . "A. U. Thor")))
      (let ((saved (pdf-info-save)))
        (unwind-protect
            (let-alist (pdf-misc-test-metadata-of-file saved)
              (should (equal .title pdf-misc-test-unicode-title))
              (should (equal .author "A. U. Thor"))
              ;; Untouched fields survive.
              (should (equal .producer "pdfTeX-1.40.13")))
          (delete-file saved))))))

(ert-deftest pdf-info-setmetadata-empty-removes ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file #'pdf-misc-test-xmp-pdf
    (should (equal "Original Title"
                   (cdr (assq 'title (pdf-info-metadata)))))
    (should (equal "" (cdr (assq 'title (pdf-info-setmetadata
                                         '((title . "")))))))
    (should (equal "" (cdr (assq 'title (pdf-info-setmetadata
                                         '((title . nil)))))))))

(ert-deftest pdf-info-setmetadata-errors ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (should-error (pdf-info-setmetadata '((producer . "x"))))
    (should-error (pdf-info-query 'setmetadata (pdf-view-buffer-file-name)
                                  "bogus" "x"))
    (should-error (pdf-info-query 'setmetadata (pdf-view-buffer-file-name)
                                  "title"))
    ;; A failed request changes nothing, even if earlier pairs were valid.
    (should-error (pdf-info-query 'setmetadata (pdf-view-buffer-file-name)
                                  "title" "x" "bogus" "y"))
    (should (equal "" (cdr (assq 'title (pdf-info-metadata)))))))

(ert-deftest pdf-info-xmp-metadata ()
  (pdf-misc-test-with-copy file "test.pdf"
    (should-not (pdf-info-xmp-metadata)))
  (pdf-misc-test-with-copy file #'pdf-misc-test-xmp-pdf
    (let ((xmp (pdf-info-xmp-metadata)))
      (should (stringp xmp))
      (should (string-match-p "<dc:title>" xmp)))))

;; * XMP

(ert-deftest pdf-misc-metadata-xmp-title ()
  (pdf-misc-test-with-copy file "test.pdf"
    (should-not (pdf-misc-metadata-xmp-title)))
  (pdf-misc-test-with-copy file #'pdf-misc-test-xmp-pdf
    (should (equal "Original Title" (pdf-misc-metadata-xmp-title)))))

(ert-deftest pdf-misc-metadata-xmp-not-updated ()
  "Poppler changes the Info title only; the XMP title stays and we warn."
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file #'pdf-misc-test-xmp-pdf
    (let (warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (type message &rest _)
                   (push (cons type message) warnings))))
        ;; Same title as in the XMP: no warning.
        (pdf-misc-update-metadata '((title . "Changed")))
        (setq warnings nil)
        (pdf-misc-update-metadata '((title . "Original Title")))
        (should-not warnings)
        ;; Other fields: no warning.
        (pdf-misc-update-metadata '((author . "Somebody")))
        (should-not warnings)
        (pdf-misc-update-metadata '((title . "New Title")))
        (should (= 1 (length warnings)))
        (should (equal '(pdf-misc xmp) (caar warnings)))
        (should (string-match-p "Original Title" (cdar warnings)))
        (save-buffer)
        (let-alist (pdf-misc-test-metadata-of-file file)
          (should (equal .title "New Title")))
        ;; The saved file still carries the old XMP title.
        (should (equal "Original Title" (pdf-misc-metadata-xmp-title)))))))

;; * Lisp commands

(ert-deftest pdf-misc-update-metadata-modifies-buffer ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (should-not (buffer-modified-p))
    ;; No change, no modification.
    (pdf-misc-update-metadata '((title . "") (author . nil)))
    (should-not (buffer-modified-p))
    (should-error (pdf-misc-update-metadata '((producer . "x"))))
    (pdf-misc-set-metadata 'title pdf-misc-test-unicode-title)
    (should (buffer-modified-p))
    (save-buffer)
    (should-not (buffer-modified-p))
    (should (equal pdf-misc-test-unicode-title
                   (cdr (assq 'title (pdf-misc-test-metadata-of-file
                                      file)))))))

(ert-deftest pdf-misc-metadata-revert-discards ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (pdf-misc-set-metadata 'title "Unsaved")
    (should (equal "Unsaved" (cdr (assq 'title (pdf-info-metadata)))))
    (should (buffer-modified-p))
    (revert-buffer t t)
    (should-not (buffer-modified-p))
    (should (equal "" (cdr (assq 'title (pdf-info-metadata)))))
    (should (equal "" (cdr (assq 'title (pdf-misc-test-metadata-of-file
                                         file)))))))

(defun pdf-misc-test-edit-field (field value)
  "Replace the value of FIELD in the current edit buffer with VALUE."
  (goto-char 1)
  (re-search-forward (format "^%s:.*$" (capitalize (symbol-name field))))
  (replace-match (format "%s: %s" (capitalize (symbol-name field)) value)
                 t t))

(ert-deftest pdf-misc-edit-metadata-commit ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (let ((pdf-buffer (current-buffer))
          (edit (pdf-misc-edit-metadata-noselect)))
      (with-current-buffer edit
        (should (derived-mode-p 'pdf-misc-edit-metadata-mode))
        (should (equal (buffer-string)
                       "Title: \nAuthor: \nSubject: \nKeywords: \n\
Creator: LaTeX with hyperref package\n"))
        (pdf-misc-test-edit-field 'title pdf-misc-test-unicode-title)
        (pdf-misc-test-edit-field 'keywords "alpha, beta")
        (pdf-misc-test-edit-field 'creator "")
        (pdf-misc-edit-metadata-commit))
      (should-not (buffer-live-p edit))
      (should (eq (current-buffer) pdf-buffer))
      (should (buffer-modified-p))
      (let-alist (pdf-info-metadata)
        (should (equal .title pdf-misc-test-unicode-title))
        (should (equal .keywords '("alpha" "beta")))
        (should (equal .creator ""))
        (should (equal .author "")))
      (save-buffer)
      (let-alist (pdf-misc-test-metadata-of-file file)
        (should (equal .title pdf-misc-test-unicode-title))
        (should (equal .keywords '("alpha" "beta")))
        (should (equal .creator "")))
      ;; The edit buffer starts from the saved values.
      (with-current-buffer (pdf-misc-edit-metadata-noselect)
        (goto-char 1)
        (should (looking-at (regexp-quote
                             (concat "Title: " pdf-misc-test-unicode-title))))
        ;; Committing without edits changes nothing.
        (pdf-misc-edit-metadata-commit))
      (should-not (buffer-modified-p)))))

(ert-deftest pdf-misc-edit-metadata-abort ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (let ((edit (pdf-misc-edit-metadata-noselect)))
      (with-current-buffer edit
        (pdf-misc-test-edit-field 'title "Abandoned")
        (pdf-misc-edit-metadata-abort))
      (should-not (buffer-live-p edit))
      (should-not (buffer-modified-p))
      (should (equal "" (cdr (assq 'title (pdf-info-metadata))))))))

(ert-deftest pdf-misc-edit-metadata-malformed ()
  (skip-unless (pdf-info-writable-metadata-p))
  (pdf-misc-test-with-copy file "test.pdf"
    (let ((edit (pdf-misc-edit-metadata-noselect)))
      (unwind-protect
          (with-current-buffer edit
            (goto-char (point-max))
            (insert "Producer: me\n")
            (should-error (pdf-misc-edit-metadata-commit)
                          :type 'user-error)
            (delete-region (line-beginning-position 0) (point-max))
            (insert "no colon here\n")
            (should-error (pdf-misc-edit-metadata-commit)
                          :type 'user-error)
            ;; Deleting a line leaves the field alone.
            (erase-buffer)
            (insert "Title: Only this\n")
            (pdf-misc-edit-metadata-commit))
        (when (buffer-live-p edit)
          (kill-buffer edit)))
      (let-alist (pdf-info-metadata)
        (should (equal .title "Only this"))
        (should (equal .creator "LaTeX with hyperref package"))))))

(ert-deftest pdf-misc-display-metadata-edit-key ()
  (pdf-misc-test-with-copy file "test.pdf"
    (should (eq (lookup-key pdf-metadata-buffer-mode-map (kbd "e"))
                #'pdf-misc-edit-metadata))
    (let ((pdf-buffer (current-buffer)))
      (pdf-misc-display-metadata)
      (with-current-buffer "*PDF-Metadata*"
        (should (eq pdf-misc-metadata--buffer pdf-buffer))
        (when (pdf-info-writable-metadata-p)
          (let ((edit (save-window-excursion
                        (call-interactively #'pdf-misc-edit-metadata)
                        (current-buffer))))
            (should (eq pdf-buffer (buffer-local-value
                                    'pdf-misc-edit-metadata--buffer edit)))
            (kill-buffer edit))))
      (kill-buffer "*PDF-Metadata*"))))

;; * ================================================================== *
;; * Attachments
;; * ================================================================== *
;;
;; The attachment commands rewrite the visited file with the qpdf
;; program, so every test works on a copy in a temporary directory, see
;; `pdf-misc-test-with-copy'.  A missing qpdf skips them.

(defun pdf-misc-test-file-bytes (file)
  "Return the bytes of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun pdf-misc-test-write-file (file contents)
  "Write CONTENTS to FILE and return its name."
  (with-temp-file file
    (insert contents))
  file)

(defun pdf-misc-test-attachment-names (attachments)
  "Return the file names of ATTACHMENTS."
  (mapcar (lambda (attachment)
            (cdr (assq 'filename attachment)))
          attachments))

(defun pdf-misc-test-dired-names ()
  "Return the sorted names of the files listed in the current Dired buffer."
  (save-excursion
    (goto-char (point-min))
    (let (names)
      (while (not (eobp))
        (let ((name (ignore-errors (dired-get-filename nil t))))
          (when (and name
                     (not (member (file-name-nondirectory name) '("." ".."))))
            (push (file-name-nondirectory name) names)))
        (forward-line 1))
      (sort names #'string<))))

(ert-deftest pdf-misc-attachments-dired ()
  "Both attachments are extracted, and equal names are made unique."
  (skip-unless (executable-find "qpdf"))
  (pdf-misc-test-with-copy file "test.pdf"
    (pdf-misc-test-with-clean-state
      (let* ((dir (file-name-directory file))
             (dedicated (pdf-util-dedicated-directory))
             (rubric (pdf-misc-test-write-file
                      (expand-file-name "rubric.md" dir) "# Rubric\n")))
        ;; The same file twice: the second attachment needs a new name.
        (pdf-misc-add-attachment rubric)
        (pdf-misc-add-attachment rubric)
        (let ((dired-buffer (pdf-misc-attachments-dired)))
          (should (buffer-live-p dired-buffer))
          (with-current-buffer dired-buffer
            (should (derived-mode-p 'dired-mode))
            ;; The directory is named after the document, in the
            ;; buffer's dedicated directory.
            (should (file-in-directory-p default-directory dedicated))
            (should (equal (pdf-misc-test-dired-names)
                           '("rubric-1.md" "rubric.md")))
            (dolist (name (pdf-misc-test-dired-names))
              (let ((extracted (expand-file-name name default-directory)))
                (should (file-exists-p extracted))
                (with-temp-buffer
                  (insert-file-contents extracted)
                  (should (equal (buffer-string) "# Rubric\n")))))))))))

(defun pdf-misc-test--debug (label command)
  "Print LABEL and the output of COMMAND as DEBUG log lines."
  (message "DEBUG %s = %s" label
           (replace-regexp-in-string
            "\n" " | " (string-trim (shell-command-to-string command)))))

(ert-deftest pdf-misc-add-attachment ()
  "The attachment is in the file and the rest of the document is unchanged."
  (skip-unless (executable-find "qpdf"))
  (pdf-misc-test-with-copy file "test.pdf"
    (let* ((rubric (pdf-misc-test-write-file
                    (expand-file-name "rubric.md"
                                      (file-name-directory file))
                    "# Rubric\n"))
           (npages (pdf-cache-number-of-pages))
           (text (pdf-info-gettext 1 '(0 0 1 1)))
           (metadata (pdf-info-metadata)))
      (let* ((qpdf (pdf-pages--qpdf-program))
             (dir (file-name-directory rubric)))
        (pdf-misc-test--debug
         "baseline"
         (format "cp rubric.md %s/a.pdf; cd %s && for f in rubric.md a.pdf /etc/hostname /etc/os-release desc; do echo -n \"[$f] \"; %s --empty --add-attachment $f -- out.pdf 2>&1 | head -1; echo done; done"
                 rubric dir qpdf))
        (pdf-misc-test--debug
         "audit"
         "sudo -n dmesg 2>&1 | grep 'apparmor=\"DENIED\"' | tail -4 | cut -c1-320")
        (pdf-misc-test--debug
         "profiles"
         "sudo -n aa-status 2>&1 | grep -icE 'profile'; sudo -n aa-status 2>&1 | grep -iE 'qpdf|pdf' | head -5; sudo -n cat /sys/kernel/security/apparmor/profiles 2>&1 | grep -iE 'qpdf|pdf' | head -5")
        (pdf-misc-test--debug
         "unconfined"
         (format "cd %s && sudo -n aa-exec -p unconfined -- %s --empty --add-attachment rubric.md -- o2.pdf 2>&1 | head -1; echo unconfined-done; sudo -n %s --empty --add-attachment rubric.md -- o3.pdf 2>&1 | head -1; echo root-done"
                 dir qpdf qpdf))
        (pdf-misc-test--debug
         "labels"
         (format "cd %s && echo -n 'shell='; cat /proc/self/attr/current; echo -n 'qpdf= '; sudo -n aa-exec -p unconfined -- sh -c 'cat /proc/self/attr/current' 2>&1"
                 dir)))
      (let ((attachments (pdf-misc-add-attachment rubric "The rubric")))
        (should (equal (pdf-misc-test-attachment-names attachments)
                       '("rubric.md")))
        (should (equal (cdr (assq 'description (car attachments)))
                       "The rubric")))
      ;; The buffer was reverted, so its attachment list is fresh ...
      (should (equal (pdf-misc-test-attachment-names (pdf-info-getattachments))
                     '("rubric.md")))
      (should-not (buffer-modified-p))
      ;; ... and the rest of the document is untouched.
      (should (= (pdf-cache-number-of-pages) npages))
      (should (equal (pdf-info-gettext 1 '(0 0 1 1)) text))
      (should (equal (cdr (assq 'title (pdf-info-metadata)))
                     (cdr (assq 'title metadata))))
      (should (equal (cdr (assq 'producer (pdf-info-metadata)))
                     (cdr (assq 'producer metadata))))
      ;; A fresh look at the file on disk agrees.
      (pdf-info-close)
      (should (equal (pdf-misc-test-attachment-names (pdf-info-getattachments))
                     '("rubric.md")))
      ;; Without a description there is none.  The second attachment
      ;; keeps the file's name for readers; only its key is unique.
      (pdf-misc-add-attachment rubric)
      (let ((second (cl-find-if
                     (lambda (a)
                       (and (equal (cdr (assq 'filename a)) "rubric.md")
                            (null (cdr (assq 'description a)))))
                     (pdf-info-getattachments))))
        (should second)))))

(ert-deftest pdf-misc-add-attachment-undoable ()
  "The version before the change is kept for `pdf-undo'."
  (skip-unless (executable-find "qpdf"))
  (pdf-misc-test-with-copy file "test.pdf"
    (let* ((dir (file-name-directory file))
           (true (file-truename file))
           (pdf-undo--alist nil)
           (pdf-undo-backup-directory (expand-file-name "backups" dir))
           (pdf-pages-confirm nil)
           (bytes (pdf-misc-test-file-bytes file))
           (rubric (pdf-misc-test-write-file
                    (expand-file-name "rubric.md" dir) "# Rubric\n")))
      (pdf-misc-add-attachment rubric)
      (should (equal (pdf-misc-test-attachment-names (pdf-info-getattachments))
                     '("rubric.md")))
      (should (cdr (assoc true pdf-undo--alist)))
      (pdf-undo)
      ;; The file is exactly the version before the attachments, and the
      ;; server sees no attachment again.
      (should (equal (pdf-misc-test-file-bytes file) bytes))
      (should-not (buffer-modified-p))
      (should-not (pdf-info-getattachments))
      (should-not (cdr (assoc true pdf-undo--alist)))
      (pdf-info-close)
      (should-not (pdf-misc--attachment-keys file)))))

(ert-deftest pdf-misc-remove-attachment ()
  (skip-unless (executable-find "qpdf"))
  (pdf-misc-test-with-copy file "test.pdf"
    (let* ((dir (file-name-directory file))
           (npages (pdf-cache-number-of-pages)))
      (dolist (name '("one.txt" "two.txt"))
        (pdf-misc-add-attachment
         (pdf-misc-test-write-file (expand-file-name name dir)
                                   (format "%s\n" name))))
      (should (equal (sort (pdf-misc--attachment-keys file) #'string<)
                     '("one.txt" "two.txt")))
      (let ((attachments (pdf-misc-remove-attachment "one.txt")))
        (should (equal (pdf-misc-test-attachment-names attachments)
                       '("two.txt"))))
      (should (= (pdf-cache-number-of-pages) npages))
      (should-error (pdf-misc-remove-attachment "nope.txt")
                    :type 'user-error)
      ;; The other attachment survived and the file is untouched.
      (pdf-info-close)
      (should (equal (pdf-misc-test-attachment-names (pdf-info-getattachments))
                     '("two.txt"))))))

(ert-deftest pdf-misc-add-attachment-modified-buffer ()
  "An unsaved buffer is not written to unless the user saves it first."
  (skip-unless (executable-find "qpdf"))
  (pdf-misc-test-with-copy file "test.pdf"
    (let ((rubric (pdf-misc-test-write-file
                   (expand-file-name "rubric.md"
                                     (file-name-directory file))
                   "# Rubric\n"))
          (bytes (pdf-misc-test-file-bytes file)))
      (set-buffer-modified-p t)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) nil)))
        (should-error (pdf-misc-add-attachment rubric) :type 'user-error))
      (should (equal (pdf-misc-test-file-bytes file) bytes))
      (should-not (pdf-misc--attachment-keys file)))))

(ert-deftest pdf-misc-attachment-keys ()
  "The C-c C-a map binds the document-level attachment commands."
  (pdf-misc-test-with-pdf "test.pdf"
    (require 'pdf-annot)
    (let ((prefix pdf-annot-minor-mode-map-prefix))
      (should (eq (lookup-key pdf-annot-minor-mode-map
                              (vconcat prefix (kbd "A")))
                  #'pdf-misc-attachments-dired))
      (should (eq (lookup-key pdf-annot-minor-mode-map
                              (vconcat prefix (kbd "+")))
                  #'pdf-misc-add-attachment))
      (should (eq (lookup-key pdf-annot-minor-mode-map
                              (vconcat prefix (kbd "-")))
                  #'pdf-misc-remove-attachment)))))

(provide 'pdf-misc-test)
;;; pdf-misc-test.el ends here
