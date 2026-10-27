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

(provide 'pdf-misc-test)
;;; pdf-misc-test.el ends here
