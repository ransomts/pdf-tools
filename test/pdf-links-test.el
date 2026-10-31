;; -*- lexical-binding: t -*-

(require 'pdf-links)
(require 'ert)

;; Tests for pdf-links.el.  Page 3 of test.pdf ("Links") has two links:
;; an internal one to section 1 on page 1, and a uri link to
;; http://www.gnu.org.  No other page has links.
;;
;; Rendering pages and showing tooltips need a window system.  The
;; macro below shows the PDF in the selected window with page display
;; stubbed out, a fixed image size, and the "you are here" tooltip
;; arrow recorded instead of shown.

(defvar pdf-links-test--arrows nil
  "Arguments passed to the stubbed `pdf-util-tooltip-arrow'.")

(defconst pdf-links-test--image-size '(612 . 792)
  "Image size reported by the stubbed `pdf-view-image-size'.")

(defmacro pdf-links-test-with-displayed-pdf (&rest body)
  "Run BODY in test.pdf, shown on page 1 in the selected window."
  (declare (indent 0) (debug t))
  `(pdf-test-with-test-pdf-view
     (cl-letf (((symbol-function 'pdf-view-display-page) #'ignore)
               ((symbol-function 'pdf-view-image-size)
                (lambda (&rest _) pdf-links-test--image-size))
               ((symbol-function 'pdf-util-tooltip-arrow)
                (lambda (top &rest _) (push top pdf-links-test--arrows))))
       (setq pdf-links-test--arrows nil)
       (switch-to-buffer (current-buffer))
       (delete-other-windows)
       (pdf-view-goto-page 1)
       ,@body)))

(defun pdf-links-test--page-3-links ()
  "Return the (internal uri) links of page 3 of the current PDF."
  (let ((links (pdf-info-pagelinks 3)))
    (list (cl-find 'goto-dest links :key (lambda (l) (alist-get 'type l)))
          (cl-find 'uri links :key (lambda (l) (alist-get 'type l))))))

;;; pdf-links-action-to-string

(ert-deftest pdf-links-action-to-string-goto-dest ()
  (should (equal (pdf-links-action-to-string
                  '((type . goto-dest) (title . "") (page . 3) (top . 0.5)))
                 "Goto page 3"))
  (should (equal (pdf-links-action-to-string
                  '((type . goto-dest) (title . "Intro") (page . 3)))
                 "Goto page 3 (Intro)"))
  (should (equal (pdf-links-action-to-string
                  '((type . goto-dest) (title . "") (page . 0)))
                 "Destination not found")))

(ert-deftest pdf-links-action-to-string-goto-remote ()
  (let ((file (expand-file-name "test.pdf")))
    (should (equal (pdf-links-action-to-string
                    `((type . goto-remote) (title . "") (filename . ,file)
                      (page . 2)))
                   (format "Goto p.2 of file '%s'" file)))
    (should (equal (pdf-links-action-to-string
                    `((type . goto-remote) (title . "Other") (filename . ,file)
                      (page . 0)))
                   (format "Goto file '%s' (Other)" file))))
  (should (equal (pdf-links-action-to-string
                  '((type . goto-remote) (title . "")
                    (filename . "/nonexistent/x.pdf") (page . 1)))
                 "Link to nonexistent file '/nonexistent/x.pdf'"))
  (should (equal (pdf-links-action-to-string
                  '((type . goto-remote) (title . "") (page . 1)))
                 "Link to nonexistent file 'nil'")))

(ert-deftest pdf-links-action-to-string-uri-and-unknown ()
  (should (equal (pdf-links-action-to-string
                  '((type . uri) (title . "") (uri . "https://gnu.org")))
                 "Link to uri 'https://gnu.org'"))
  (should (equal (pdf-links-action-to-string
                  '((type . uri) (title . "") (uri . "")))
                 "Link to empty uri"))
  (should (equal (pdf-links-action-to-string
                  '((type . uri) (title . "Home") (uri . nil)))
                 "Link to empty uri (Home)"))
  (should (equal (pdf-links-action-to-string '((type . launch) (title . "")))
                 "Unrecognized link type: launch")))

(ert-deftest pdf-links-action-to-string-real-links ()
  (pdf-test-with-test-pdf-view
    (should-not (pdf-info-pagelinks 1))
    (cl-destructuring-bind (internal uri) (pdf-links-test--page-3-links)
      (should (equal (pdf-links-action-to-string internal) "Goto page 1"))
      (should (equal (pdf-links-action-to-string uri)
                     "Link to uri 'http://www.gnu.org'")))))

;;; pdf-links-action-perform

(ert-deftest pdf-links-action-perform-goto-dest ()
  (pdf-links-test-with-displayed-pdf
    (pdf-view-goto-page 3)
    (let ((internal (car (pdf-links-test--page-3-links))))
      (should-not (pdf-links-action-perform internal))
      (should (= (pdf-view-current-page) 1))
      ;; The arrow points at the destination's top edge.
      (should (equal pdf-links-test--arrows
                     (list (alist-get 'top internal)))))))

(ert-deftest pdf-links-action-perform-goto-dest-without-top ()
  (pdf-links-test-with-displayed-pdf
    (pdf-links-action-perform '((type . goto-dest) (title . "") (page . 5)))
    (should (= (pdf-view-current-page) 5))
    (should-not pdf-links-test--arrows)))

(ert-deftest pdf-links-action-perform-goto-dest-nowhere ()
  (pdf-links-test-with-displayed-pdf
    (should-error (pdf-links-action-perform
                   '((type . goto-dest) (title . "") (page . 0))))
    (should (= (pdf-view-current-page) 1))))

(ert-deftest pdf-links-action-perform-goto-remote ()
  (let ((remote (make-temp-file "pdf-links-test" nil ".pdf"))
        remote-buffer)
    (unwind-protect
        (pdf-links-test-with-displayed-pdf
          (copy-file "test.pdf" remote t)
          (let ((origin (current-buffer)))
            (pdf-links-action-perform
             `((type . goto-remote) (title . "") (filename . ,remote)
               (page . 4) (top . 0.25)))
            (setq remote-buffer (find-buffer-visiting remote))
            (should (buffer-live-p remote-buffer))
            (let ((win (get-buffer-window remote-buffer)))
              (should win)
              (with-selected-window win
                (should (= (pdf-view-current-page) 4))))
            (should (equal pdf-links-test--arrows '(0.25)))
            ;; The document holding the link stays where it was.
            (should (eq (current-buffer) origin))
            (should (= (pdf-view-current-page) 1))))
      (when (buffer-live-p remote-buffer)
        (with-current-buffer remote-buffer
          (set-buffer-modified-p nil))
        (kill-buffer remote-buffer))
      (delete-other-windows)
      (delete-file remote))))

(ert-deftest pdf-links-action-perform-goto-remote-missing-file ()
  (pdf-links-test-with-displayed-pdf
    (should-error (pdf-links-action-perform
                   '((type . goto-remote) (title . "")
                     (filename . "/nonexistent/x.pdf") (page . 1))))
    (should (= (pdf-view-current-page) 1))))

(ert-deftest pdf-links-action-perform-uri ()
  (pdf-test-with-test-pdf-view
    (let* ((visited nil)
           (pdf-links-browse-uri-function (lambda (uri) (push uri visited))))
      (pdf-links-action-perform (cadr (pdf-links-test--page-3-links)))
      (should (equal visited '("http://www.gnu.org"))))))

(ert-deftest pdf-links-action-perform-unknown-type ()
  (should-error (pdf-links-action-perform '((type . launch) (title . "")))))

(ert-deftest pdf-links-browse-uri-default ()
  (let (opened)
    (cl-letf (((symbol-function 'org-link-open-from-string)
               (lambda (s &rest _) (push s opened))))
      (pdf-links-browse-uri-default "http://www.gnu.org"))
    (should (equal opened '("[[http://www.gnu.org]]"))))
  (should-error (pdf-links-browse-uri-default nil)
                :type 'wrong-type-argument))

;;; Hotspots and the minor mode

(ert-deftest pdf-links-hotspots-function ()
  (pdf-links-test-with-displayed-pdf
    ;; `local-set-key' writes into the current local map; give the
    ;; buffer its own so that `pdf-view-mode-map' is left alone.
    (use-local-map (make-composed-keymap nil (current-local-map)))
    (should-not (pdf-links-hotspots-function 1 '(612 . 792)))
    (let* ((links (pdf-info-pagelinks 3))
           (hotspots (pdf-links-hotspots-function 3 '(612 . 792))))
      (should (= (length hotspots) 2))
      (cl-loop
       for link in links
       for spot in hotspots
       for i from 1
       do
       (let ((id (intern (format "link-3-%d" i)))
             (e (pdf-util-scale (alist-get 'edges link) '(612 . 792) 'round)))
         ;; (AREA ID PLIST), in document order.
         (should (equal (car spot)
                        `(rect . ((,(nth 0 e) . ,(nth 1 e))
                                  . (,(nth 2 e) . ,(nth 3 e))))))
         (should (eq (nth 1 spot) id))
         (should (eq (plist-get (nth 2 spot) 'pointer) 'hand))
         (should (equal (plist-get (nth 2 spot) 'help-echo)
                        (pdf-links-action-to-string link)))
         (should (commandp (lookup-key (current-local-map)
                                       (vector id 'mouse-1))))))
      ;; Clicking the internal link follows it.
      (pdf-view-goto-page 3)
      (funcall (lookup-key (current-local-map) [link-3-1 mouse-1]))
      (should (= (pdf-view-current-page) 1)))))

(ert-deftest pdf-links-minor-mode-registers-hotspots ()
  (pdf-links-test-with-displayed-pdf
    (pdf-links-minor-mode -1)
    (should-not (rassq 'pdf-links-hotspots-function
                       pdf-view--hotspot-functions))
    (pdf-links-minor-mode 1)
    (should (equal (rassq 'pdf-links-hotspots-function
                          pdf-view--hotspot-functions)
                   '(0 . pdf-links-hotspots-function)))
    (pdf-links-minor-mode -1)
    (should-not (rassq 'pdf-links-hotspots-function
                       pdf-view--hotspot-functions))))

;;; Reading a link with keys

(ert-deftest pdf-links-create-keys ()
  (should-not (pdf-links-read-link-action--create-keys 0))
  (should (equal (pdf-links-read-link-action--create-keys 1) '((?A))))
  (should (equal (pdf-links-read-link-action--create-keys 3)
                 '((?A) (?B) (?C))))
  (dolist (n '(5 25 26 27 100 676 677 1000))
    (let ((keys (pdf-links-read-link-action--create-keys n)))
      (should (= (length keys) n))
      ;; Unique, equally long, and made of upper-case letters only, so
      ;; that no key is a prefix of another.
      (should (= (length (cl-remove-duplicates keys :test #'equal)) n))
      (should (= 1 (length (cl-remove-duplicates (mapcar #'length keys)))))
      (should (cl-every (lambda (k) (cl-every (lambda (c) (<= ?A c ?Z)) k))
                        keys))))
  (should (equal (car (last (pdf-links-read-link-action--create-keys 27)))
                 '(?B ?A))))

(ert-deftest pdf-links-create-keys-shortest ()
  (should (equal (mapcar #'length
                         (pdf-links-read-link-action--create-keys 26))
                 (make-list 26 1))))

(ert-deftest pdf-links-read-chars ()
  (let* ((events nil)
         (alist '(((?A ?A) . first) ((?A ?B) . second) ((?B ?A) . third))))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt chars &rest _)
                 (let ((ch (pop events)))
                   (should (memq ch chars))
                   ch))))
      ;; Two keystrokes pick an entry; case does not matter.
      (setq events (list ?a ?b))
      (should (eq (pdf-links-read-link-action--read-chars "Link: " alist)
                  'second))
      ;; A unique first letter already decides.
      (setq events (list ?B))
      (should (eq (pdf-links-read-link-action--read-chars
                   "Link: " '(((?A) . one) ((?B) . two)))
                  'two)))))

(ert-deftest pdf-links-read-link-action-no-links ()
  (pdf-links-test-with-displayed-pdf
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (&rest _) (error "Read a key for no links"))))
      (should (equal (cadr (should-error (pdf-links-read-link-action "Link: ")))
                     "No links on displayed pages")))))

;;; Isearch restricted to links

(ert-deftest pdf-links-isearch-filter-matches ()
  (pdf-links-test-with-displayed-pdf
    (pdf-view-goto-page 3)
    (let* ((size pdf-links-test--image-size)
           (find (lambda (string)
                   (mapcar (lambda (m)
                             (pdf-util-scale (alist-get 'edges m) size 'round))
                           (pdf-info-search-string string 3))))
           (in-link (funcall find "gnu.org"))
           (not-link (funcall find "External")))
      (should (= 1 (length in-link)))
      (should (= 1 (length not-link)))
      (should (equal (pdf-links-isearch-link-filter-matches
                      (append not-link in-link))
                     in-link)))))

(provide 'pdf-links-test)

;;; pdf-links-test.el ends here
