;;; pdf-annot-test.el --- Tests for pdf-annot.el  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for annotation handling: the property accessors and
;; modifiability rules, `pdf-annot-put', creating and deleting
;; annotations, the batching of `pdf-annot-modified-functions', the
;; edit-contents buffer, the annotation list buffer, hotspots, context
;; menus, attachments and saving annotations to a file.
;;
;; test.pdf contains, on page 2, one text annotation (annot-2-0,
;; contents "insert") and four markup annotations (strike-out,
;; highlight, underline, squiggly; annot-2-1 ... annot-2-4), two link
;; annotations on page 3 and a file attachment annotation on page 4.
;;
;; Tests run in batch without a window system, so nothing here may
;; display a PDF page.  Changes to annotations stay in the epdfinfo
;; server unless a test saves, and the tests that save work on a
;; temporary copy of test.pdf.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pdf-annot)

;; * ================================================================== *
;; * Helpers
;; * ================================================================== *

(defmacro pdf-annot-test-with-pdf (file &rest body)
  "Run BODY in a buffer visiting FILE, like `pdf-test-with-pdf'.

`pdf-test-with-pdf' visits the file before it restarts the server,
which the previous test has quit.  Opening the file then fails
half-way through `pdf-view-mode' (\"File mode specification
error: The epdfinfo server quit\"), leaving a buffer without its
minor modes and without `pdf-view--write-contents-function', so
that `save-buffer' writes the unchanged file.  Start the server
first so the buffer is set up as it would be interactively.

Also make sure FILE opens in `pdf-view-mode' even if an earlier
test (pdf-loader-install/uninstall-alists) removed PDF Tools from
`auto-mode-alist'."
  (declare (indent 1) (debug t))
  `(let ((auto-mode-alist (cons pdf-tools-auto-mode-alist-entry
                                auto-mode-alist)))
     (pdf-info-process-assert-running t)
     (pdf-test-with-pdf ,file ,@body)))

(defmacro pdf-annot-test-with-clean-state (&rest body)
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

(defmacro pdf-annot-test-recording-modifications (var &rest body)
  "Record calls of `pdf-annot-modified-functions' in VAR while running BODY.

Each call pushes a plist (:inserted IDS :changed IDS :deleted IDS)
onto VAR, most recent call first."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var nil))
     (add-hook 'pdf-annot-modified-functions
               (lambda (fn)
                 (push (list :inserted (pdf-annot-test-ids (funcall fn :inserted))
                             :changed (pdf-annot-test-ids (funcall fn :changed))
                             :deleted (pdf-annot-test-ids (funcall fn :deleted)))
                       ,var))
               nil t)
     ,@body))

(defmacro pdf-annot-test-with-temp-copy (var &rest body)
  "Bind VAR to a temporary copy of test.pdf while running BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (make-temp-file "pdf-annot-test" nil ".pdf")))
     (unwind-protect
         (progn
           (copy-file "test.pdf" ,var t)
           ,@body)
       (when (file-exists-p ,var)
         (delete-file ,var)))))

(defun pdf-annot-test-ids (annotations)
  "Return the sorted ids of ANNOTATIONS."
  (sort (mapcar #'pdf-annot-get-id annotations)
        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

(defun pdf-annot-test-fake (&rest alist)
  "Return a fake annotation of the current buffer made of ALIST.
ALIST is given as alternating keys and values."
  (let (result)
    (while alist
      (push (cons (pop alist) (pop alist)) result))
    (pdf-annot-create (nreverse result))))

(defun pdf-annot-test-text-annot ()
  "Return the text annotation of test.pdf."
  (car (pdf-annot-getannots 2 'text)))

(defun pdf-annot-test-highlight-annot ()
  "Return the highlight annotation of test.pdf."
  (car (pdf-annot-getannots 2 'highlight)))

(defun pdf-annot-test-list-annotations ()
  "Run `pdf-annot-list-annotations' without its context window.
Batch Emacs before 28 has a frame too small to split for it, and
the tests are about the list."
  (cl-letf (((symbol-function 'tablist-display-context-window) #'ignore))
    (pdf-annot-list-annotations)))

(defun pdf-annot-test-list-rows ()
  "Return the ids of the rows of the current list buffer, in order."
  (save-excursion
    (goto-char (point-min))
    (let (ids)
      (while (not (eobp))
        (when (tabulated-list-get-id)
          (push (tabulated-list-get-id) ids))
        (forward-line))
      (nreverse ids))))

(defun pdf-annot-test-color-key (color)
  "Return the key of COLOR in the color submenu."
  (vector (intern (format "color-%s" color))))

(defun pdf-annot-test-approx= (a b)
  "Return non-nil if numbers A and B differ by less than 1e-3."
  (< (abs (- a b)) 1e-3))


;; * ================================================================== *
;; * Accessors and predicates
;; * ================================================================== *

(ert-deftest pdf-annot-test-create-and-get ()
  "An annotation is an alist carrying its buffer; get falls back to DEFAULT."
  (with-temp-buffer
    (let ((a (pdf-annot-create '((id . x) (type . text) (page . 1)
                                 (contents . "") (label))))
          (other (generate-new-buffer " *pdf-annot-other*")))
      (unwind-protect
          (progn
            (should (eq (pdf-annot-get-buffer a) (current-buffer)))
            (should (eq (pdf-annot-get-id a) 'x))
            (should (eq (pdf-annot-get-type a) 'text))
            (should (equal (pdf-annot-get a 'contents) ""))
            (should (equal (pdf-annot-get a 'label "none") "none"))
            (should (equal (pdf-annot-get a 'missing 42) 42))
            (should (eq (pdf-annot-get-buffer
                         (pdf-annot-create '((id . y)) other))
                        other)))
        (kill-buffer other)))))

(ert-deftest pdf-annot-test-type-predicates ()
  (with-temp-buffer
    (let ((text (pdf-annot-test-fake 'type 'text))
          (highlight (pdf-annot-test-fake 'type 'highlight))
          (file (pdf-annot-test-fake 'type 'file))
          (widget (pdf-annot-test-fake 'type 'widget)))
      (should (pdf-annot-text-annotation-p text))
      (should-not (pdf-annot-text-annotation-p highlight))
      (should (pdf-annot-markup-annotation-p text))
      (should (pdf-annot-markup-annotation-p highlight))
      (should-not (pdf-annot-markup-annotation-p widget))
      (should (pdf-annot-has-attachment-p file))
      (should-not (pdf-annot-has-attachment-p text)))))

(ert-deftest pdf-annot-test-property-modifiable-p ()
  "Common properties are editable everywhere; others depend on the type."
  (with-temp-buffer
    (let ((text (pdf-annot-test-fake 'type 'text))
          (highlight (pdf-annot-test-fake 'type 'highlight))
          (widget (pdf-annot-test-fake 'type 'widget)))
      (dolist (a (list text highlight widget))
        (dolist (p '(edges color flags contents))
          (should (pdf-annot-property-modifiable-p a p)))
        (dolist (p '(id page type modified buffer))
          (should-not (pdf-annot-property-modifiable-p a p))))
      ;; Markup-only properties.
      (dolist (p '(label opacity popup popup-is-open))
        (should (pdf-annot-property-modifiable-p text p))
        (should (pdf-annot-property-modifiable-p highlight p))
        (should-not (pdf-annot-property-modifiable-p widget p)))
      ;; Text-only properties.
      (dolist (p '(icon is-open))
        (should (pdf-annot-property-modifiable-p text p))
        (should-not (pdf-annot-property-modifiable-p highlight p))
        (should-not (pdf-annot-property-modifiable-p widget p))))))

(ert-deftest pdf-annot-test-equal ()
  "Annotations are equal when buffer and id match, whatever else differs."
  (let ((b1 (generate-new-buffer " *pdf-annot-1*"))
        (b2 (generate-new-buffer " *pdf-annot-2*")))
    (unwind-protect
        (let ((a (pdf-annot-create '((id . annot-1-0) (color . "red")) b1))
              (a-changed (pdf-annot-create '((id . annot-1-0) (color . "blue")) b1))
              (other-id (pdf-annot-create '((id . annot-1-1)) b1))
              (other-buffer (pdf-annot-create '((id . annot-1-0)) b2)))
          (should (pdf-annot-equal a a-changed))
          (should-not (pdf-annot-equal a other-id))
          (should-not (pdf-annot-equal a other-buffer)))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest pdf-annot-test-display-edges ()
  "Markup edges, when present, are what gets displayed."
  (with-temp-buffer
    (should (equal (pdf-annot-get-display-edges
                    (pdf-annot-test-fake 'edges '(0 0 1 1)))
                   '((0 0 1 1))))
    (should (equal (pdf-annot-get-display-edges
                    (pdf-annot-test-fake 'edges '(0 0 1 1)
                                         'markup-edges '((0.1 0.1 0.2 0.2)
                                                         (0.3 0.3 0.4 0.4))))
                   '((0.1 0.1 0.2 0.2) (0.3 0.3 0.4 0.4))))))

(ert-deftest pdf-annot-test-merge-alists ()
  "The first occurrence of a key wins and order is kept."
  (should (equal (pdf-annot-merge-alists
                  '((icon . "Note"))
                  nil
                  '((color . "red") (icon . "Key"))
                  '((label . "me") (color . "blue")))
                 '((icon . "Note") (color . "red") (label . "me"))))
  (should (null (pdf-annot-merge-alists nil nil))))

(ert-deftest pdf-annot-test-compare-annotations ()
  "Annotations sort by page, then top edge, then left edge."
  (with-temp-buffer
    (let* ((p1-low (pdf-annot-test-fake 'id 'a 'page 1 'edges '(0.1 0.5 0.2 0.6)))
           (p1-high-right (pdf-annot-test-fake 'id 'b 'page 1 'edges '(0.5 0.1 0.6 0.2)))
           (p1-high-left (pdf-annot-test-fake 'id 'c 'page 1 'edges '(0.2 0.1 0.3 0.2)))
           (p2-top (pdf-annot-test-fake 'id 'd 'page 2 'edges '(0.0 0.0 0.1 0.1)))
           (markup (pdf-annot-test-fake 'id 'e 'page 1 'edges '(0.0 0.0 0.0 0.0)
                                        'markup-edges '((0.0 0.3 0.1 0.4)))))
      (should (pdf-annot-compare-annotations p1-low p2-top))
      (should-not (pdf-annot-compare-annotations p2-top p1-low))
      (should (pdf-annot-compare-annotations p1-high-left p1-high-right))
      (should-not (pdf-annot-compare-annotations p1-high-right p1-high-left))
      (should (pdf-annot-compare-annotations p1-high-right p1-low))
      (should (equal (mapcar #'pdf-annot-get-id
                             (sort (list p2-top p1-low markup p1-high-right p1-high-left)
                                   #'pdf-annot-compare-annotations))
                     ;; markup sorts by its markup edges (top 0.3), not
                     ;; by its edges (top 0.0).
                     '(c b e a d))))))

(ert-deftest pdf-annot-test-property-completions ()
  (should (equal (pdf-annot-property-completions 'icon)
                 pdf-annot-standard-text-icons))
  ;; A copy, so callers may modify it.
  (should-not (eq (pdf-annot-property-completions 'icon)
                  pdf-annot-standard-text-icons))
  (should (null (pdf-annot-property-completions 'label))))


;; * ================================================================== *
;; * Printing
;; * ================================================================== *

(ert-deftest pdf-annot-test-print-property ()
  (with-temp-buffer
    (let ((a (pdf-annot-test-fake 'color "#ff0000" 'opacity 0.5
                                  'modified nil 'subject nil
                                  'label "Joe" 'page 3 'contents nil)))
      (should (equal (pdf-annot-print-property a 'color) "#ff0000"))
      (should (equal (get-text-property 0 'face (pdf-annot-print-property a 'color))
                     '(:background "#ff0000")))
      (should (equal (pdf-annot-print-property a 'opacity) "50%"))
      (should (equal (pdf-annot-print-property a 'modified) "No date"))
      (should (equal (pdf-annot-print-property a 'created) "No date"))
      (should (equal (pdf-annot-print-property a 'subject) "No subject"))
      (should (equal (pdf-annot-print-property a 'label) "Joe"))
      (should (equal (pdf-annot-print-property a 'page) "3"))
      (should (equal (pdf-annot-print-property a 'contents) "")))
    (let ((a (pdf-annot-test-fake 'modified '(0 0) 'color nil)))
      (should (equal (pdf-annot-print-property a 'modified)
                     (current-time-string '(0 0))))
      (should (equal (pdf-annot-print-property a 'color) ""))
      ;; Missing opacity means fully opaque.
      (should (equal (pdf-annot-print-property a 'opacity) "100%")))))

(ert-deftest pdf-annot-test-print-annotation ()
  "The default printer is header, newline, contents; hooks take precedence."
  (with-temp-buffer
    (let ((a (pdf-annot-test-fake 'type 'text 'label "Joe" 'subject "Subj"
                                  'modified nil 'contents "body text"))
          (pdf-annot-print-annotation-functions nil))
      (let ((printed (pdf-annot-print-annotation a)))
        (should (equal (substring-no-properties printed)
                       "Subj;Joe;No date\nbody text"))
        (should (eq (get-text-property 0 'face printed) 'header-line))
        ;; The display property makes the face show up in tooltips.
        (should (get-text-property 0 'display printed)))
      (let ((pdf-annot-print-annotation-functions
             (list (lambda (_a) nil)
                   (lambda (a) (concat "custom:" (pdf-annot-get a 'label))))))
        (should (equal (pdf-annot-print-annotation a) "custom:Joe"))))))

(ert-deftest pdf-annot-test-latex-predicate ()
  "LaTeX printing is only attempted for contents that look like LaTeX."
  (should (funcall pdf-annot-latex-string-predicate "$x^2$"))
  (should (funcall pdf-annot-latex-string-predicate "  \n\\frac{1}{2}"))
  (should-not (funcall pdf-annot-latex-string-predicate "costs $5"))
  (should-not (funcall pdf-annot-latex-string-predicate nil))
  (with-temp-buffer
    (should-not (pdf-annot-print-annotation-latex-maybe
                 (pdf-annot-test-fake 'contents "plain text")))))


;; * ================================================================== *
;; * Reading annotations from a document
;; * ================================================================== *

(ert-deftest pdf-annot-test-getannots ()
  (pdf-annot-test-with-pdf "test.pdf"
    (let ((all (pdf-annot-getannots)))
      (should (cl-every (lambda (a) (eq (pdf-annot-get-buffer a) (current-buffer)))
                        all))
      (should (equal (pdf-annot-test-ids (pdf-annot-getannots 2))
                     '(annot-2-0 annot-2-1 annot-2-2 annot-2-3 annot-2-4)))
      ;; TYPES may be a symbol or a list.
      (should (equal (pdf-annot-test-ids (pdf-annot-getannots nil 'link))
                     '(annot-3-0 annot-3-1)))
      (should (equal (pdf-annot-test-ids (pdf-annot-getannots nil '(text file)))
                     '(annot-2-0 annot-4-0)))
      (should (null (pdf-annot-getannots 1)))
      ;; BUFFER argument, called from another buffer.
      (let ((pdf-buffer (current-buffer)))
        (with-temp-buffer
          (should (= (length (pdf-annot-getannots nil nil pdf-buffer))
                     (length all)))
          (should-error (pdf-annot-getannots)))))))

(ert-deftest pdf-annot-test-getannot ()
  "An id from `pdf-annot-getannots' fetches the same annotation."
  (pdf-annot-test-with-pdf "test.pdf"
    (let* ((listed (pdf-annot-test-text-annot))
           (pdf-buffer (current-buffer))
           (a (with-temp-buffer
                (pdf-annot-getannot (pdf-annot-get-id listed) pdf-buffer))))
      (should (eq (pdf-annot-get-buffer a) pdf-buffer))
      (should (pdf-annot-equal a listed))
      (should (eq (pdf-annot-get-type a) 'text))
      (should (equal (pdf-annot-get a 'contents) "insert"))
      (should (equal (pdf-annot-get a 'page) 2)))))


;; * ================================================================== *
;; * Modification hooks
;; * ================================================================== *

(ert-deftest pdf-annot-test-run-modified-hooks-arguments ()
  (pdf-annot-test-with-pdf "test.pdf"
    (let ((a (pdf-annot-test-fake 'id 'x 'page 1)))
      (should-error (pdf-annot-run-modified-hooks :frobnicate a))
      (should-error (pdf-annot-run-modified-hooks nil a)))))

(ert-deftest pdf-annot-test-run-modified-hooks-batching ()
  "Inhibited modifications are collected and reported in a single call."
  (pdf-annot-test-with-pdf "test.pdf"
    (let* ((inserted (pdf-annot-test-fake 'id 'ins 'page 1 'v 'old))
           (inserted-then-changed (pdf-annot-test-fake 'id 'ins 'page 1 'v 'new))
           (changed (pdf-annot-test-fake 'id 'chg 'page 1))
           (deleted (pdf-annot-test-fake 'id 'del 'page 2))
           closures)
      (add-hook 'pdf-annot-modified-functions
                (lambda (fn) (push fn closures)) nil t)
      (let ((pdf-annot-inhibit-modification-hooks t))
        (pdf-annot-run-modified-hooks :insert inserted)
        (pdf-annot-run-modified-hooks :change inserted-then-changed)
        (pdf-annot-run-modified-hooks :change changed)
        (pdf-annot-run-modified-hooks :change changed)
        (pdf-annot-run-modified-hooks :delete deleted))
      (should (null closures))
      (should pdf-annot-delayed-modified-annotations)
      (pdf-annot-run-modified-hooks)
      (should (= 1 (length closures)))
      (let ((fn (car closures)))
        ;; An annotation inserted and then changed is reported as
        ;; inserted in its changed state.
        (should (equal (funcall fn :inserted) (list inserted-then-changed)))
        ;; Duplicates are dropped.
        (should (equal (pdf-annot-test-ids (funcall fn :changed)) '(chg ins)))
        (should (equal (funcall fn :deleted) (list deleted)))
        (should (equal (pdf-annot-test-ids (funcall fn t)) '(chg del ins)))
        (should (null (funcall fn nil)))
        ;; Results are copies.
        (should-not (eq (funcall fn :changed) (funcall fn :changed))))
      (should (null pdf-annot-delayed-modified-annotations))
      ;; Nothing pending, nothing to report.
      (pdf-annot-run-modified-hooks)
      (should (= 1 (length closures))))))

(ert-deftest pdf-annot-test-modified-closure-rejects-unknown-argument ()
  "Per the docstring of `pdf-annot-modified-functions', calling the
closure with an argument other than :inserted, :deleted, :changed,
t or nil signals an error."
  (pdf-annot-test-with-pdf "test.pdf"
    (let (closure)
      (add-hook 'pdf-annot-modified-functions
                (lambda (fn) (setq closure fn)) nil t)
      (pdf-annot-run-modified-hooks :change (pdf-annot-test-fake 'id 'x 'page 1))
      (should (functionp closure))
      (should-error (funcall closure :insert)))))

(ert-deftest pdf-annot-test-atomic-modifications ()
  "Several changes inside the macro run the hooks once, even on error."
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-recording-modifications calls
      (pdf-annot-with-atomic-modifications
        (pdf-annot-put (pdf-annot-test-text-annot) 'color "#00ff00")
        (pdf-annot-put (pdf-annot-test-highlight-annot) 'color "#00ff00")
        (should (null calls)))
      (should (= 1 (length calls)))
      (should (equal (plist-get (car calls) :changed) '(annot-2-0 annot-2-2)))
      (setq calls nil)
      (should-error
       (pdf-annot-with-atomic-modifications
         (pdf-annot-put (pdf-annot-test-text-annot) 'color "#0000ff")
         (error "Boom")))
      (should (= 1 (length calls)))
      (should (equal (plist-get (car calls) :changed) '(annot-2-0))))))


;; * ================================================================== *
;; * Changing, adding and deleting annotations
;; * ================================================================== *

(ert-deftest pdf-annot-test-put ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-recording-modifications calls
      (set-buffer-modified-p nil)
      (let ((result (pdf-annot-put (pdf-annot-test-text-annot) 'color "#00ff00")))
        ;; The returned annotation reflects the change ...
        (should (equal (pdf-annot-get result 'color) "#00ff00"))
        (should (eq (pdf-annot-get-buffer result) (current-buffer)))
        ;; ... and so does the document.
        (should (equal (pdf-annot-get (pdf-annot-test-text-annot) 'color) "#00ff00"))
        (should (buffer-modified-p))
        (should (equal calls '((:inserted nil :changed (annot-2-0) :deleted nil)))))
      (pdf-annot-put (pdf-annot-test-text-annot) 'contents "new contents")
      (should (equal (pdf-annot-get (pdf-annot-test-text-annot) 'contents)
                     "new contents"))
      (pdf-annot-put (pdf-annot-test-text-annot) 'icon "Note")
      (should (equal (pdf-annot-get (pdf-annot-test-text-annot) 'icon) "Note"))
      (pdf-annot-put (pdf-annot-test-highlight-annot) 'label "Someone")
      (should (equal (pdf-annot-get (pdf-annot-test-highlight-annot) 'label)
                     "Someone")))))

(ert-deftest pdf-annot-test-put-unchanged-value ()
  "Putting the current value changes nothing and runs no hooks."
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-recording-modifications calls
      (set-buffer-modified-p nil)
      (let ((a (pdf-annot-test-text-annot)))
        (should (pdf-annot-equal (pdf-annot-put a 'color (pdf-annot-get a 'color)) a))
        ;; Even a read-only property may be "set" to its current value.
        (pdf-annot-put a 'page 2))
      (should-not (buffer-modified-p))
      (should (null calls)))))

(ert-deftest pdf-annot-test-put-read-only-property ()
  (pdf-annot-test-with-pdf "test.pdf"
    (should-error (pdf-annot-put (pdf-annot-test-text-annot) 'page 3))
    (should-error (pdf-annot-put (pdf-annot-test-highlight-annot) 'icon "Note"))
    (should (equal (pdf-annot-get (pdf-annot-test-text-annot) 'page) 2))
    (should-not (pdf-annot-get (pdf-annot-test-highlight-annot) 'icon))))

(ert-deftest pdf-annot-test-put-uses-fresh-annotation ()
  "A stale annotation object is refreshed before comparing values."
  (pdf-annot-test-with-pdf "test.pdf"
    (let ((stale (pdf-annot-test-text-annot)))
      (should (equal (pdf-annot-get stale 'color) "#ff0000"))
      (pdf-annot-put (pdf-annot-test-text-annot) 'color "#00ff00")
      ;; STALE still says #ff0000, but the document does not, so this
      ;; must change the color back.
      (pdf-annot-put stale 'color "#ff0000")
      (should (equal (pdf-annot-get (pdf-annot-test-text-annot) 'color) "#ff0000")))))

(ert-deftest pdf-annot-test-delete ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-recording-modifications calls
      (set-buffer-modified-p nil)
      (let ((a (pdf-annot-test-highlight-annot))
            (count (length (pdf-annot-getannots))))
        (should (null (pdf-annot-delete a)))
        (should (= (1- count) (length (pdf-annot-getannots))))
        (should-not (pdf-annot-getannots nil 'highlight))
        (should (buffer-modified-p))
        (should (equal calls '((:inserted nil :changed nil :deleted (annot-2-2)))))
        ;; The annotation is gone; using it again is a user error.
        (should-error (pdf-annot-delete a) :type 'user-error)
        (should-error (pdf-annot-put a 'color "#000000") :type 'user-error)))))

(ert-deftest pdf-annot-test-add-text-annotation-on-page ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-recording-modifications calls
      (set-buffer-modified-p nil)
      (let ((a (pdf-annot-add-annotation 'text '(0.1 0.2 0.3 0.4)
                                         '((contents . "hello")
                                           (color . "#0000ff")
                                           (icon . "Key"))
                                         1)))
        (should (eq (pdf-annot-get-buffer a) (current-buffer)))
        (should (eq (pdf-annot-get-type a) 'text))
        (should (equal (pdf-annot-get a 'page) 1))
        (should (equal (pdf-annot-get a 'contents) "hello"))
        (should (equal (pdf-annot-get a 'color) "#0000ff"))
        (should (equal (pdf-annot-get a 'icon) "Key"))
        (should (pdf-annot-test-approx= (nth 0 (pdf-annot-get a 'edges)) 0.1))
        (should (pdf-annot-test-approx= (nth 1 (pdf-annot-get a 'edges)) 0.2))
        (should (equal (pdf-annot-test-ids (pdf-annot-getannots 1))
                       (list (pdf-annot-get-id a))))
        (should (buffer-modified-p))
        (should (equal calls `((:inserted (,(pdf-annot-get-id a))
                                :changed nil :deleted nil))))))))

(ert-deftest pdf-annot-test-add-markup-annotation-on-page ()
  "Markup annotations cover the text selected inside EDGES."
  (skip-unless (pdf-info-markup-annotations-p))
  (pdf-annot-test-with-pdf "test.pdf"
    (dolist (type '(highlight underline squiggly strike-out))
      (let ((a (pdf-annot-add-annotation type '((0.1 0.1 0.9 0.3))
                                         `((label . ,(symbol-name type)))
                                         1)))
        (should (eq (pdf-annot-get-type a) type))
        (should (equal (pdf-annot-get a 'label) (symbol-name type)))
        (should (pdf-annot-get a 'markup-edges))
        (should (cl-every #'pdf-test-relative-edges-p
                          (pdf-annot-get a 'markup-edges)))))
    (should (= 4 (length (pdf-annot-getannots 1))))))

(ert-deftest pdf-annot-test-add-annotation-invalid-arguments ()
  (pdf-annot-test-with-pdf "test.pdf"
    (let ((count (length (pdf-annot-getannots))))
      (should-error (pdf-annot-add-annotation 'link '(0.1 0.1 0.2 0.2) nil 1))
      ;; Text annotations take a single edges list.
      (should-error (pdf-annot-add-annotation
                     'text '((0.1 0.1 0.2 0.2) (0.3 0.3 0.4 0.4)) nil 1))
      ;; Without PAGE the selected window must show the document.
      (should-error (pdf-annot-add-annotation 'text '(0.1 0.1 0.2 0.2)))
      (should (= count (length (pdf-annot-getannots)))))))

(ert-deftest pdf-annot-test-add-annotation-rolls-back-on-bad-property ()
  "If setting PROPERTY-ALIST fails, the new annotation is deleted again."
  (pdf-annot-test-with-pdf "test.pdf"
    (let ((count (length (pdf-annot-getannots))))
      (should-error (pdf-annot-add-annotation 'text '(0.1 0.1 0.2 0.2)
                                              '((no-such-property . 1))
                                              1))
      (should (= count (length (pdf-annot-getannots)))))))

(ert-deftest pdf-annot-test-add-annotation-activates-when-asked ()
  (pdf-annot-test-with-pdf "test.pdf"
    (let* ((activated nil)
           (pdf-annot-activate-handler-functions
            (list (lambda (a) (push a activated) t))))
      (let ((pdf-annot-activate-created-annotations nil))
        (pdf-annot-add-annotation 'text '(0.1 0.1 0.2 0.2) nil 1)
        (should (null activated)))
      (let* ((pdf-annot-activate-created-annotations t)
             (a (pdf-annot-add-annotation 'text '(0.5 0.5 0.6 0.6) nil 1)))
        (should (= 1 (length activated)))
        (should (pdf-annot-equal (car activated) a))))))

(ert-deftest pdf-annot-test-activate-annotation ()
  "Handler functions run first; the default handler is the fallback."
  (pdf-annot-test-with-pdf "test.pdf"
    (let* ((text (pdf-annot-test-text-annot))
           (file (car (pdf-annot-getannots nil 'file)))
           (log nil))
      (cl-letf (((symbol-function 'pdf-annot-edit-contents)
                 (lambda (a) (push (cons 'edit (pdf-annot-get-id a)) log) 'edited))
                ((symbol-function 'pdf-annot-pop-to-attachment)
                 (lambda (a) (push (cons 'attachment (pdf-annot-get-id a)) log) 'popped)))
        (let ((pdf-annot-activate-handler-functions
               (list (lambda (_a) nil)
                     (lambda (a) (push (cons 'handler (pdf-annot-get-id a)) log) 'handled))))
          (should (eq (pdf-annot-activate-annotation text) 'handled))
          (should (equal log '((handler . annot-2-0)))))
        (setq log nil)
        (let ((pdf-annot-activate-handler-functions nil))
          (should (eq (pdf-annot-activate-annotation text) 'edited))
          (should (eq (pdf-annot-activate-annotation file) 'popped))
          (should (equal log '((attachment . annot-4-0) (edit . annot-2-0)))))))))

(ert-deftest pdf-annot-test-save-and-reread ()
  "Added, changed and deleted annotations survive saving the file."
  (skip-unless (pdf-info-markup-annotations-p))
  (pdf-annot-test-with-temp-copy file
    (pdf-annot-test-with-pdf file
      (pdf-annot-add-annotation 'text '(0.1 0.1 0.2 0.2)
                                '((contents . "saved note") (label . "Tester"))
                                1)
      (pdf-annot-put (pdf-annot-test-text-annot) 'contents "edited")
      (pdf-annot-delete (pdf-annot-test-highlight-annot))
      (should (buffer-modified-p))
      (save-buffer)
      (should-not (buffer-modified-p)))
    (pdf-annot-test-with-pdf file
      (let ((page1 (pdf-annot-getannots 1)))
        (should (= 1 (length page1)))
        (should (eq (pdf-annot-get-type (car page1)) 'text))
        (should (equal (pdf-annot-get (car page1) 'contents) "saved note"))
        (should (equal (pdf-annot-get (car page1) 'label) "Tester")))
      (should (equal (mapcar (lambda (a) (pdf-annot-get a 'contents))
                             (pdf-annot-getannots 2 'text))
                     '("edited")))
      (should-not (pdf-annot-getannots nil 'highlight))))
  ;; The fixture itself is untouched.
  (pdf-annot-test-with-pdf "test.pdf"
    (should (pdf-annot-getannots nil 'highlight))
    (should-not (pdf-annot-getannots 1))))


;; * ================================================================== *
;; * Positions and sizes in image coordinates
;; * ================================================================== *

(ert-deftest pdf-annot-test-image-position-and-size ()
  (with-temp-buffer
    (let ((a (pdf-annot-test-fake 'edges '(0.1 0.2 0.4 0.6))))
      (let ((pos (pdf-annot-image-position a '(1000 . 2000))))
        (should (pdf-annot-test-approx= (car pos) 100))
        (should (pdf-annot-test-approx= (cdr pos) 400)))
      (let ((size (pdf-annot-image-size a '(1000 . 2000))))
        (should (pdf-annot-test-approx= (car size) 300))
        (should (pdf-annot-test-approx= (cdr size) 800))))))

(ert-deftest pdf-annot-test-image-set-position ()
  (pdf-annot-test-with-pdf "test.pdf"
    (let* ((a (pdf-annot-add-annotation 'text '(0.1 0.1 0.2 0.2) nil 1))
           (id (pdf-annot-get-id a)))
      (pdf-annot-image-set-position a 300 500 '(1000 . 1000))
      (let ((edges (pdf-annot-get (pdf-annot-getannot id) 'edges)))
        (should (pdf-annot-test-approx= (nth 0 edges) 0.3))
        (should (pdf-annot-test-approx= (nth 1 edges) 0.5)))
      ;; Positions beyond the page are clamped so the annotation stays
      ;; on it.
      (pdf-annot-image-set-position (pdf-annot-getannot id) 5000 -10 '(1000 . 1000))
      (let ((edges (pdf-annot-get (pdf-annot-getannot id) 'edges)))
        (should (<= (nth 2 edges) 1.001))
        (should (> (nth 0 edges) 0.5))
        (should (pdf-annot-test-approx= (nth 1 edges) 0))))))


;; * ================================================================== *
;; * Hotspots and menus
;; * ================================================================== *

(ert-deftest pdf-annot-test-create-hotspots ()
  (pdf-annot-test-with-pdf "test.pdf"
    (let* ((text (pdf-annot-test-text-annot))
           (highlight (pdf-annot-test-highlight-annot))
           (text-spots (pdf-annot-create-hotspots text '(1000 . 1000)))
           (highlight-spots (pdf-annot-create-hotspots highlight '(1000 . 1000))))
      (should (= 1 (length text-spots)))
      (let* ((spot (car text-spots))
             (edges (pdf-util-scale (pdf-annot-get text 'edges) '(1000 . 1000) 'round)))
        (should (equal (car spot)
                       `(rect . ((,(nth 0 edges) . ,(nth 1 edges))
                                 . (,(nth 2 edges) . ,(nth 3 edges))))))
        (should (eq (nth 1 spot) 'annot-2-0))
        (should (string-match-p "insert"
                                (plist-get (nth 2 spot) 'help-echo))))
      ;; Markup annotations use their markup edges.
      (should (= (length highlight-spots)
                 (length (pdf-annot-get highlight 'markup-edges))))
      (let ((edges (pdf-util-scale (car (pdf-annot-get highlight 'markup-edges))
                                   '(1000 . 1000) 'round)))
        (should (equal (car (car highlight-spots))
                       `(rect . ((,(nth 0 edges) . ,(nth 1 edges))
                                 . (,(nth 2 edges) . ,(nth 3 edges)))))))
      ;; Clicking activates; only text and file annotations can be dragged.
      (should (commandp (lookup-key (current-local-map) [annot-2-0 mouse-1])))
      (should (commandp (lookup-key (current-local-map) [annot-2-0 down-mouse-1])))
      (should (commandp (lookup-key (current-local-map) [annot-2-2 mouse-1])))
      (should-not (commandp (lookup-key (current-local-map) [annot-2-2 down-mouse-1]))))))

(ert-deftest pdf-annot-test-hotspot-function-skips-links ()
  (pdf-annot-test-with-pdf "test.pdf"
    (should (null (pdf-annot-hotspot-function 3 '(1000 . 1000))))
    (let ((spots (pdf-annot-hotspot-function 2 '(1000 . 1000))))
      (should (equal (sort (delete-dups (mapcar #'cadr spots))
                           (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                     '(annot-2-0 annot-2-1 annot-2-2 annot-2-3 annot-2-4))))))

(ert-deftest pdf-annot-test-minor-mode-hotspots ()
  "The minor mode registers its hotspot function and removes it again."
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-minor-mode 1)
    (should (memq 'pdf-annot-hotspot-function (pdf-view-sorted-hotspot-functions)))
    (should (memq #'pdf-annot-attachment-delete-base-directory
                  pdf-info-close-document-hook))
    (pdf-annot-minor-mode -1)
    (should-not (memq 'pdf-annot-hotspot-function (pdf-view-sorted-hotspot-functions)))
    (should-not (memq #'pdf-annot-attachment-delete-base-directory
                      pdf-info-close-document-hook))
    (pdf-annot-minor-mode 1)
    (should (memq 'pdf-annot-hotspot-function (pdf-view-sorted-hotspot-functions)))))

(ert-deftest pdf-annot-test-context-menu ()
  "Text annotations get an icon submenu; every annotation a color one."
  (pdf-annot-test-with-pdf "test.pdf"
    (let ((text-menu (pdf-annot-create-context-menu (pdf-annot-test-text-annot)))
          (highlight-menu (pdf-annot-create-context-menu (pdf-annot-test-highlight-annot))))
      (dolist (menu (list text-menu highlight-menu))
        (should (keymapp menu))
        (should (lookup-key menu [change-color]))
        (should (lookup-key menu [activate-annotation]))
        (should (lookup-key menu [goto-annotation]))
        (when (pdf-info-writable-annotations-p)
          (should (lookup-key menu [delete-annotation]))))
      (should (lookup-key text-menu [change-text-icon]))
      (should-not (lookup-key highlight-menu [change-text-icon])))))

(ert-deftest pdf-annot-test-context-menu-delete ()
  (skip-unless (pdf-info-writable-annotations-p))
  (pdf-annot-test-with-pdf "test.pdf"
    (let ((menu (pdf-annot-create-context-menu (pdf-annot-test-highlight-annot)))
          (inhibit-message t))
      (funcall (lookup-key menu [delete-annotation]))
      (should-not (pdf-annot-getannots nil 'highlight)))))

(ert-deftest pdf-annot-test-icon-submenu ()
  (pdf-annot-test-with-pdf "test.pdf"
    (let ((menu (pdf-annot-create-icon-submenu (pdf-annot-test-text-annot))))
      (dolist (icon pdf-annot-standard-text-icons)
        (should (commandp (lookup-key menu (vector (intern (format "icon-%s" icon)))))))
      (funcall (lookup-key menu [icon-Comment]))
      (should (equal (pdf-annot-get (pdf-annot-test-text-annot) 'icon) "Comment")))))

(ert-deftest pdf-annot-test-color-submenu ()
  (pdf-annot-test-with-pdf "test.pdf"
    (let* ((pdf-annot-color-history '("#123456" "#654321"))
           (menu (pdf-annot-create-color-submenu (pdf-annot-test-text-annot))))
      (should (lookup-key menu [color-chooser]))
      (should (commandp (lookup-key menu (pdf-annot-test-color-key "#123456"))))
      (should (commandp (lookup-key menu (pdf-annot-test-color-key "#654321"))))
      (funcall (lookup-key menu (pdf-annot-test-color-key "#654321")))
      (should (equal (pdf-annot-get (pdf-annot-test-text-annot) 'color) "#654321")))
    ;; A long history is cut down to 12 entries.
    (let* ((pdf-annot-color-history
            (cl-loop for i from 1 to 20 collect (format "#0000%02d" i)))
           (menu (pdf-annot-create-color-submenu (pdf-annot-test-text-annot))))
      (should (= 12 (cl-count-if (lambda (b)
                                   (and (consp b)
                                        (string-prefix-p "color-#" (symbol-name (car b)))))
                                 (cdr menu)))))))

(ert-deftest pdf-annot-test-color-submenu-keeps-recent-colors ()
  "The color submenu offers the most recently used colors."
  ;; BUG: `pdf-annot-create-color-submenu' trims the history with
  ;; (butlast (reverse pdf-annot-color-history) N).  The history is
  ;; most-recent-first, so the reversed list is oldest-first and
  ;; `butlast' drops the N *most recent* colors.  With 13 colors in the
  ;; history, the color used last is missing from the menu while the
  ;; oldest one is offered.
  :expected-result :failed
  (pdf-annot-test-with-pdf "test.pdf"
    (let* ((pdf-annot-color-history
            (cl-loop for i from 1 to 13 collect (format "#0000%02d" i)))
           (menu (pdf-annot-create-color-submenu (pdf-annot-test-text-annot))))
      ;; "#000001" is the most recent color.
      (should (lookup-key menu (pdf-annot-test-color-key "#000001")))
      (should-not (lookup-key menu (pdf-annot-test-color-key "#000013"))))))


;; * ================================================================== *
;; * Attachments
;; * ================================================================== *

(ert-deftest pdf-annot-test-attachments ()
  (pdf-annot-test-with-pdf "test.pdf"
    (unwind-protect
        (let* ((file-annot (car (pdf-annot-getannots nil 'file)))
               (att (pdf-annot-get-attachment file-annot)))
          (should (equal (cdr (assq 'filename att)) "test.tex"))
          (should-error (pdf-annot-get-attachment (pdf-annot-test-text-annot)))
          (should (equal (substring-no-properties
                          (pdf-annot-print-annotation-header file-annot))
                         "File attachment `test.tex' of unknown size"))
          ;; Saving copies the data to a stable, unique file name.
          (let* ((saved (pdf-annot-get-attachment file-annot t))
                 (name (pdf-annot-attachment-save saved)))
            (unwind-protect
                (progn
                  (should (file-exists-p name))
                  (should (equal (file-name-nondirectory name) "test.tex"))
                  (should (file-in-directory-p
                           name (pdf-annot-attachment-base-directory)))
                  (with-temp-buffer
                    (insert-file-contents name)
                    (should (string-match-p "documentclass" (buffer-string))))
                  (should (equal (pdf-annot-attachment-save saved) name))
                  ;; Another attachment with the same file name gets a
                  ;; different one.
                  (let ((other (pdf-annot-attachment-unique-filename
                                '((id . other-id) (filename . "test.tex")))))
                    (should-not (equal other name))
                    (should (equal (file-name-nondirectory other) "test-1.tex"))))
              (delete-file (cdr (assq 'file saved)))))
          ;; Without the data file, there is nothing to save.
          (should-error (pdf-annot-attachment-save att)))
      (pdf-annot-attachment-delete-base-directory))
    (should (null pdf-annot--attachment-file-alist))))

(ert-deftest pdf-annot-test-attachment-dired ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (unwind-protect
          (let ((dired-buffer (pdf-annot-attachment-dired)))
            (should (buffer-live-p dired-buffer))
            (with-current-buffer dired-buffer
              (should (derived-mode-p 'dired-mode))
              (should (file-exists-p (expand-file-name "test.tex" default-directory)))))
        (pdf-annot-attachment-delete-base-directory)))))


;; * ================================================================== *
;; * Editing contents
;; * ================================================================== *

(ert-deftest pdf-annot-test-edit-contents-buffer ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (let* ((text (pdf-annot-test-text-annot))
             (edit (pdf-annot-edit-contents-noselect text)))
        (with-current-buffer edit
          (should (equal (buffer-string) "insert"))
          (should-not (buffer-modified-p))
          (should pdf-annot-edit-contents-minor-mode)
          (should (derived-mode-p 'org-mode))
          (should (stringp header-line-format))
          (should (pdf-annot-equal pdf-annot-edit-contents--annotation text)))
        ;; The edit buffer is reused for the next annotation.
        (pdf-annot-put (pdf-annot-test-highlight-annot) 'contents "$x^2$")
        (should (eq edit (pdf-annot-edit-contents-noselect
                          (pdf-annot-test-highlight-annot))))
        (with-current-buffer edit
          (should (equal (buffer-string) "$x^2$"))
          ;; LaTeX contents get a LaTeX mode.
          (should (derived-mode-p 'latex-mode))
          (should pdf-annot-edit-contents-minor-mode))))))

(ert-deftest pdf-annot-test-edit-contents-commit ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (pdf-annot-test-recording-modifications calls
        (set-buffer-modified-p nil)
        (let ((pdf-buffer (current-buffer)))
          (with-current-buffer (pdf-annot-edit-contents-noselect
                                (pdf-annot-test-text-annot))
            (erase-buffer)
            (insert "first line\nsecond line")
            (pdf-annot-edit-contents-commit)
            (should-not (buffer-modified-p)))
          (should (buffer-modified-p pdf-buffer)))
        (should (equal (pdf-annot-get (pdf-annot-test-text-annot) 'contents)
                       "first line\nsecond line"))
        (should (equal (plist-get (car calls) :changed) '(annot-2-0)))
        ;; Committing without changes does nothing.
        (setq calls nil)
        (set-buffer-modified-p nil)
        (with-current-buffer (pdf-annot-edit-contents-noselect
                              (pdf-annot-test-text-annot))
          (pdf-annot-edit-contents-commit))
        (should (null calls))
        (should-not (buffer-modified-p))))))

(ert-deftest pdf-annot-test-edit-contents-abort ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (set-buffer-modified-p nil)
      (with-current-buffer (pdf-annot-edit-contents-noselect
                            (pdf-annot-test-text-annot))
        (insert "discarded ")
        (pdf-annot-edit-contents-abort)
        (should-not (buffer-modified-p)))
      (should (equal (pdf-annot-get (pdf-annot-test-text-annot) 'contents) "insert"))
      (should-not (buffer-modified-p)))))

(ert-deftest pdf-annot-test-edit-contents-switch-annotation ()
  "Switching to another annotation offers to save pending edits."
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (let ((highlight-id (pdf-annot-get-id (pdf-annot-test-highlight-annot))))
        ;; Answer yes: the pending edit is written to the first annotation.
        (with-current-buffer (pdf-annot-edit-contents-noselect
                              (pdf-annot-test-text-annot))
          (erase-buffer)
          (insert "kept"))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (with-current-buffer (pdf-annot-edit-contents-noselect
                                (pdf-annot-getannot highlight-id))
            (should (equal (buffer-string) ""))))
        (should (equal (pdf-annot-get (pdf-annot-test-text-annot) 'contents) "kept"))
        ;; Answer no: the pending edit is dropped.
        (with-current-buffer (pdf-annot-edit-contents-noselect
                              (pdf-annot-getannot highlight-id))
          (insert "dropped"))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (with-current-buffer (pdf-annot-edit-contents-noselect
                                (pdf-annot-test-text-annot))
            (should (equal (buffer-string) "kept"))))
        (should (equal (pdf-annot-get (pdf-annot-getannot highlight-id) 'contents) ""))))))

(ert-deftest pdf-annot-test-edit-contents-displays-buffer ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (pdf-annot-edit-contents (pdf-annot-test-text-annot))
      (should pdf-annot-edit-contents-minor-mode)
      (should (eq (window-buffer (selected-window)) (current-buffer)))
      (should (equal (buffer-string) "insert")))))


;; * ================================================================== *
;; * The annotation list
;; * ================================================================== *

(ert-deftest pdf-annot-test-list-annotations ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (let ((pdf-buffer (current-buffer)))
        (pdf-annot-test-list-annotations)
        (should (buffer-live-p pdf-annot-list-buffer))
        (should (equal (buffer-name pdf-annot-list-buffer) "*test's annots*"))
        (should (memq #'pdf-annot-list-update pdf-annot-modified-functions))
        (with-current-buffer pdf-annot-list-buffer
          (should (derived-mode-p 'pdf-annot-list-mode))
          (should (eq pdf-annot-list-document-buffer pdf-buffer))
          ;; Rows are in document order and links are not listed.
          (should (equal (pdf-annot-test-list-rows)
                         '(annot-2-0 annot-2-1 annot-2-2 annot-2-3 annot-2-4 annot-4-0)))
          (goto-char (point-min))
          (should (search-forward "PDF Tools" nil t)))))))

(ert-deftest pdf-annot-test-list-columns ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (pdf-annot-test-list-annotations)
      (with-current-buffer pdf-annot-list-buffer
        (should (equal (mapcar #'car tabulated-list-format)
                       (mapcar (lambda (f)
                                 (pcase (car f)
                                   ('page "Pg.")
                                   ('date "Date")
                                   (field (capitalize (symbol-name field)))))
                               pdf-annot-list-format)))))))

(ert-deftest pdf-annot-test-list-follows-modifications ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (pdf-annot-test-list-annotations)
      (cl-flet ((listed ()
                  (with-current-buffer pdf-annot-list-buffer
                    (pdf-annot-test-list-rows))))
        (should (memq 'annot-2-2 (listed)))
        (pdf-annot-delete (pdf-annot-test-highlight-annot))
        (should-not (memq 'annot-2-2 (listed)))
        (let ((a (pdf-annot-add-annotation 'text '(0.1 0.1 0.2 0.2) nil 1)))
          ;; The new annotation is on page 1 and therefore listed first.
          (should (eq (car (listed)) (pdf-annot-get-id a))))))))

(ert-deftest pdf-annot-test-list-entries ()
  "Entries honour `pdf-annot-list-listed-types' and come sorted."
  (pdf-annot-test-with-pdf "test.pdf"
    (let ((pdf-buffer (current-buffer)))
      (with-temp-buffer
        (setq pdf-annot-list-document-buffer pdf-buffer)
        (let ((pdf-annot-list-listed-types '(text file)))
          (should (equal (mapcar #'car (pdf-annot-list-entries))
                         '(annot-2-0 annot-4-0))))
        (let ((pdf-annot-list-listed-types '(link highlight)))
          (should (equal (mapcar #'car (pdf-annot-list-entries))
                         '(annot-2-2 annot-3-0 annot-3-1))))
        (setq pdf-annot-list-document-buffer nil)
        (should-error (pdf-annot-list-entries))))))

(ert-deftest pdf-annot-test-list-entry-format ()
  (pdf-annot-test-with-pdf "test.pdf"
    (let* ((pdf-annot-list-format '((page . 3) (type . 10) (label . 24)
                                    (date . 24) (contents . 8)))
           (a (pdf-annot-put (pdf-annot-test-text-annot)
                  'contents "line one\nline two"))
           (entry (pdf-annot-list-create-entry a))
           (columns (cadr entry)))
      (should (eq (car entry) 'annot-2-0))
      (should (= 5 (length columns)))
      (should (equal (aref columns 0) "2"))
      (should (equal (substring-no-properties (aref columns 1)) "text"))
      (should (equal (aref columns 2) "PDF Tools"))
      (should (equal (get-text-property 0 'date (aref columns 3))
                     (pdf-annot-get a 'modified)))
      ;; Newlines are flattened and the contents cut to the width.
      (should (equal (aref columns 4) "line one")))))

(ert-deftest pdf-annot-test-list-type-highlight ()
  "The type column shows the annotation color with a readable foreground."
  (with-temp-buffer
    (let ((format-type (lambda (color)
                         (funcall (pdf-annot--make-entry-formatter
                                   (pdf-annot-test-fake 'type 'text 'color color))
                                  '(type . 10)))))
      (let ((pdf-annot-list-highlight-type t))
        (should (equal (get-text-property 0 'face (funcall format-type "#ffff00"))
                       '(:background "#ffff00" :foreground "black")))
        (should (equal (get-text-property 0 'face (funcall format-type "#000080"))
                       '(:background "#000080" :foreground "white")))
        (should-not (get-text-property 0 'face (funcall format-type nil))))
      (let ((pdf-annot-list-highlight-type nil))
        (should-not (get-text-property 0 'face (funcall format-type "#ffff00")))))))

(ert-deftest pdf-annot-test-list-sorting ()
  "The page and date columns sort numerically and chronologically."
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (pdf-annot-test-list-annotations)
      (with-current-buffer pdf-annot-list-buffer
        (let* ((entries (pdf-annot-list-entries))
               (page-sorter (nth 2 (aref tabulated-list-format 0)))
               (date-sorter (nth 2 (aref tabulated-list-format 3)))
               (file-entry (assq 'annot-4-0 entries))
               (text-entry (assq 'annot-2-0 entries)))
          (should (functionp page-sorter))
          (should (functionp date-sorter))
          (should (funcall page-sorter text-entry file-entry))
          (should-not (funcall page-sorter file-entry text-entry))
          ;; The file annotation (Feb 1 2015) is older than the text
          ;; annotation (Feb 10 2015).
          (should (funcall date-sorter file-entry text-entry))
          (should-not (funcall date-sorter text-entry file-entry))
          ;; Pages compare as numbers, not strings.
          (should (funcall page-sorter
                           (list 'x (vector "9" "" "" ""))
                           (list 'y (vector "10" "" "" "")))))))))

(ert-deftest pdf-annot-test-list-filters-links ()
  "Link annotations are hidden by the list's default filter."
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (let ((pdf-annot-list-listed-types '(text link)))
        (pdf-annot-test-list-annotations)
        (with-current-buffer pdf-annot-list-buffer
          (should (equal tablist-current-filter '(not (== "Type" "link"))))
          (goto-char (point-min))
          (let (visible hidden)
            (while (not (eobp))
              (let ((id (tabulated-list-get-id)))
                (when id
                  (if (invisible-p (point)) (push id hidden) (push id visible))))
              (forward-line))
            (should (equal visible '(annot-2-0)))
            (should (equal (sort hidden (lambda (a b) (string< (symbol-name a)
                                                                (symbol-name b))))
                           '(annot-3-0 annot-3-1)))))))))

(ert-deftest pdf-annot-test-list-operation-delete ()
  "Deleting from the list removes the annotations in one modification."
  (pdf-annot-test-with-pdf "test.pdf"
    ;; The ids are known to the server once the annotations were listed.
    (pdf-annot-getannots)
    (pdf-annot-test-recording-modifications calls
      (let ((pdf-buffer (current-buffer)))
        (with-temp-buffer
          (setq pdf-annot-list-document-buffer pdf-buffer)
          (should (memq 'delete (pdf-annot-list-operation-function
                                 'supported-operations)))
          (should (memq 'find-entry (pdf-annot-list-operation-function
                                     'supported-operations)))
          (pdf-annot-list-operation-function 'delete '(annot-2-1 annot-2-3))))
      (should (equal (pdf-annot-test-ids (pdf-annot-getannots 2))
                     '(annot-2-0 annot-2-2 annot-2-4)))
      (should (= 1 (length calls)))
      (should (equal (plist-get (car calls) :deleted) '(annot-2-1 annot-2-3))))))

(ert-deftest pdf-annot-test-list-context-function ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (let ((pdf-buffer (current-buffer)))
        (pdf-annot-getannots)
        (pdf-annot-list-context-function 'annot-2-0 pdf-buffer)
        (with-current-buffer "*Contents*"
          (should buffer-read-only)
          (should (string-match-p "\ninsert\\'" (buffer-string))))
        (pdf-annot-list-context-function nil pdf-buffer)
        (with-current-buffer "*Contents*"
          (should (equal (buffer-string) "")))))))

(ert-deftest pdf-annot-test-list-goto-annotation ()
  (pdf-annot-test-with-pdf "test.pdf"
    (pdf-annot-test-with-clean-state
      (let ((a (car (pdf-annot-getannots 2 'underline))))
        ;; See `pdf-annot-test-list-annotations'.
        (cl-letf (((symbol-function 'tablist-display-context-window)
                   #'ignore))
          (pdf-annot-list-goto-annotation a))
        (with-current-buffer pdf-annot-list-buffer
          (should (eq (tabulated-list-get-id
                       (window-point (get-buffer-window (current-buffer))))
                      'annot-2-3)))))))

(ert-deftest pdf-annot-test-list-follow-mode-requires-list-buffer ()
  (with-temp-buffer
    (should-error (pdf-annot-list-follow-minor-mode 1))))

(provide 'pdf-annot-test)
;;; pdf-annot-test.el ends here
