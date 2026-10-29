;; -*- lexical-binding: t -*-

;; Tests for pdf-present.el.  The decks are built from
;; present-*.tex (see the comment at the top of each):
;;
;;   present-plain.pdf          5 slides, no notes
;;   present-second-screen.pdf  5 double-width pages, notes on the right
;;   present-notes-pages.pdf    5 slides and 4 notes pages (2 4 6 9)
;;
;; Slide 2 has two overlay steps, and slide 3 has no note, so the
;; slides are pages 1 3 5 7 8 of present-notes-pages.pdf.
;;
;; Frames cannot be made in batch mode and images cannot be shown, so
;; the tests replace the display layer: frame setup splits the batch
;; frame into windows, and `pdf-view-goto-page' only records the page.
;; Everything else (detection, maps, navigation, slices, timer, quit)
;; is the real code.

(require 'ert)
(require 'cl-lib)
(require 'pdf-view)
(require 'pdf-present)

(defmacro pdf-present-test-with-deck (pdf &rest body)
  "Run BODY in a `pdf-view-mode' buffer of PDF."
  (declare (indent 1) (debug t))
  `(pdf-test-with-pdf ,pdf
     (pdf-view-mode)
     ,@body))

(defun pdf-present-test--fake-goto-page (page &optional window)
  "Record PAGE as the page of WINDOW without displaying anything."
  (setf (pdf-view-current-page window) page)
  nil)

(defun pdf-present-test--fake-setup (session _monitor)
  "Give SESSION windows in the batch frame instead of new frames."
  ;; Split the widest windows, so that even the 10 columns of the
  ;; batch frame before Emacs 28 leave each window two.
  (let* ((audience (split-window (selected-window) nil 'below))
         (current (split-window audience nil 'below))
         (next (split-window current nil 'right))
         (notes (split-window current nil 'right))
         (info (split-window next nil 'right)))
    (set-window-buffer info (pdf-present--text-buffer "*pdf-present-info*"))
    (setf (pdf-present--session-audience-window session) audience
          (pdf-present--session-current-window session) current
          (pdf-present--session-next-window session) next
          (pdf-present--session-notes-window session) notes
          (pdf-present--session-info-window session) info)))

(defun pdf-present-test--fake-teardown (session)
  "Delete the windows `pdf-present-test--fake-setup' made for SESSION."
  (dolist (window (list (pdf-present--session-audience-window session)
                        (pdf-present--session-current-window session)
                        (pdf-present--session-next-window session)
                        (pdf-present--session-notes-window session)
                        (pdf-present--session-info-window session)))
    (when (window-live-p window)
      (delete-window window)))
  (dolist (name pdf-present--buffer-names)
    (when (get-buffer name)
      (kill-buffer name))))

(defmacro pdf-present-test-presenting (pdf &rest body)
  "Start `pdf-present' on PDF with the display layer replaced, run BODY.
The presentation is ended afterwards if BODY did not end it."
  (declare (indent 1) (debug t))
  `(pdf-present-test-with-deck ,pdf
     (cl-letf (((symbol-function 'pdf-view-goto-page)
                #'pdf-present-test--fake-goto-page)
               ;; Batch Emacs cannot display page images.
               ((symbol-function 'pdf-view-display-page) #'ignore)
               ((symbol-function 'pdf-present--setup-display)
                #'pdf-present-test--fake-setup)
               ((symbol-function 'pdf-present--teardown-display)
                #'pdf-present-test--fake-teardown))
       (let ((pdf-present-notes-layout 'auto)
             (pdf-present-start-hook nil)
             (pdf-present-quit-hook nil)
             ;; Batch Emacs is never idle, and the timer would render
             ;; real pages; `pdf-present-prefetch-timer' tests it by
             ;; hand instead.
             (pdf-present-prefetch-delay nil)
             ;; Before Emacs 28 the batch frame is 10x9, too small
             ;; for the fake layout with the default minimum sizes.
             (window-min-height 1)
             (window-min-width 1))
         (unwind-protect
             (progn ,@body)
           (pdf-present-quit))))))

(defun pdf-present-test--shown (window)
  "Return what WINDOW shows: (PAGE SLICE) or a buffer name."
  (if (derived-mode-p 'pdf-view-mode)
      (if (eq (window-buffer window) (current-buffer))
          (list (pdf-view-current-page window)
                (pdf-view-current-slice window))
        (buffer-name (window-buffer window)))
    (error "Not in the PDF buffer")))

(defun pdf-present-test--state ()
  "Return what the presentation windows show, as a plist."
  (let ((s pdf-present--session))
    (list :audience (pdf-present-test--shown
                     (pdf-present--session-audience-window s))
          :current (pdf-present-test--shown
                    (pdf-present--session-current-window s))
          :next (pdf-present-test--shown
                 (pdf-present--session-next-window s))
          :notes (pdf-present-test--shown
                  (pdf-present--session-notes-window s)))))


;; * ================================================================== *
;; * Layout detection
;; * ================================================================== *

(ert-deftest pdf-present-second-screen-size ()
  (let ((pdf-present-second-screen-ratio 2.4))
    ;; 16:9 and 4:3 slides with notes on the second screen.
    (should (pdf-present--second-screen-size-p '(907 . 255)))
    (should (pdf-present--second-screen-size-p '(683 . 256)))
    ;; Single 16:9, 4:3 and 2.35:1 slides, A4 portrait.
    (should-not (pdf-present--second-screen-size-p '(454 . 255)))
    (should-not (pdf-present--second-screen-size-p '(341 . 256)))
    (should-not (pdf-present--second-screen-size-p '(600 . 255)))
    (should-not (pdf-present--second-screen-size-p '(595 . 842)))
    (should-not (pdf-present--second-screen-size-p '(10 . 0)))))

(ert-deftest pdf-present-detect-second-screen ()
  (pdf-present-test-with-deck "present-second-screen.pdf"
    (let ((pdf-present-notes-layout 'auto))
      (should (equal (pdf-present--detect (current-buffer))
                     '(second-screen))))))

(ert-deftest pdf-present-detect-notes-pages ()
  (pdf-present-test-with-deck "present-notes-pages.pdf"
    (let ((pdf-present-notes-layout 'auto))
      (should (equal (pdf-present--detect (current-buffer))
                     '(notes-pages 2 4 6 9))))))

(ert-deftest pdf-present-detect-plain ()
  (pdf-present-test-with-deck "present-plain.pdf"
    (let ((pdf-present-notes-layout 'auto))
      (should (equal (pdf-present--detect (current-buffer)) '(none))))))

(ert-deftest pdf-present-detect-forced-layout ()
  "A layout set by the user wins over what the PDF looks like."
  (pdf-present-test-with-deck "present-second-screen.pdf"
    (let ((pdf-present-notes-layout 'none))
      (should (equal (pdf-present--detect (current-buffer)) '(none)))))
  (pdf-present-test-with-deck "present-plain.pdf"
    (let ((pdf-present-notes-layout 'notes-pages))
      ;; Asked for notes pages, but there are none.
      (should (equal (pdf-present--detect (current-buffer))
                     '(notes-pages))))))

(ert-deftest pdf-present-notes-pages-rule ()
  "A notes page never follows a notes page."
  (should (equal (pdf-present--notes-pages 6 (lambda (_page _prev) t))
                 '(2 4 6)))
  (should (equal (pdf-present--notes-pages 6 (lambda (_page _prev) nil))
                 nil))
  (should (equal (pdf-present--notes-pages 1 (lambda (_page _prev) t))
                 nil))
  (should (equal (pdf-present--notes-pages
                  5 (lambda (page _prev) (memq page '(3 4 5))))
                 '(3 5))))


;; * ================================================================== *
;; * Slide and notes maps
;; * ================================================================== *

(ert-deftest pdf-present-page-map ()
  (should (equal (pdf-present--page-map 'none 3)
                 [(1) (2) (3)]))
  (should (equal (pdf-present--page-map 'second-screen 3)
                 [(1 . 1) (2 . 2) (3 . 3)]))
  (should (equal (pdf-present--page-map 'notes-pages 9 '(2 4 6 9))
                 [(1 . 2) (3 . 4) (5 . 6) (7) (8 . 9)]))
  ;; Notes pages without a slide of their own are dropped.
  (should (equal (pdf-present--page-map 'notes-pages 3 '(2 3))
                 [(1 . 2)])))

(ert-deftest pdf-present-session-maps ()
  (let ((pdf-present-notes-layout 'auto))
    (pdf-present-test-with-deck "present-notes-pages.pdf"
      (let ((s (pdf-present--make-session (current-buffer))))
        (should (eq (pdf-present--session-layout s) 'notes-pages))
        (should (= (pdf-present--count s) 5))
        (should (equal (mapcar (lambda (i) (pdf-present--slide-page s i))
                               '(1 2 3 4 5))
                       '(1 3 5 7 8)))
        (should (equal (mapcar (lambda (i) (pdf-present--notes-page s i))
                               '(1 2 3 4 5))
                       '(2 4 6 nil 9)))
        ;; A notes page belongs to its slide.
        (should (= (pdf-present--index-of-page s 6) 3))
        (should (= (pdf-present--index-of-page s 7) 4))
        ;; Labels are frame numbers here, so they are worth showing.
        (should (equal (aref (pdf-present--session-labels s) 4) "2"))))
    (pdf-present-test-with-deck "present-second-screen.pdf"
      (let ((s (pdf-present--make-session (current-buffer))))
        (should (eq (pdf-present--session-layout s) 'second-screen))
        (should (= (pdf-present--count s) 5))
        (should (equal (pdf-present--notes-page s 4) 4))
        ;; pgfpages scrambles the labels; they are not shown.
        (should-not (pdf-present--session-labels s))))
    (pdf-present-test-with-deck "present-plain.pdf"
      (let ((s (pdf-present--make-session (current-buffer))))
        (should (eq (pdf-present--session-layout s) 'none))
        (should (= (pdf-present--count s) 5))
        (should-not (pdf-present--notes-page s 1))))))

(ert-deftest pdf-present-slices ()
  (should (equal (pdf-present--slice 'second-screen 'slide) '(0 0 0.5 1)))
  (should (equal (pdf-present--slice 'second-screen 'notes) '(0.5 0 0.5 1)))
  (should-not (pdf-present--slice 'notes-pages 'slide))
  (should-not (pdf-present--slice 'notes-pages 'notes))
  (should-not (pdf-present--slice 'none 'slide)))


;; * ================================================================== *
;; * Navigation and sync
;; * ================================================================== *

(ert-deftest pdf-present-navigation-bounds ()
  (pdf-present-test-presenting "present-plain.pdf"
    (pdf-present)
    (let ((s pdf-present--session))
      (should (= (pdf-present--session-index s) 1))
      (pdf-present-previous-slide)
      (should (= (pdf-present--session-index s) 1))
      (pdf-present-next-slide)
      (pdf-present-next-slide)
      (should (= (pdf-present--session-index s) 3))
      (pdf-present-next-slide 10)
      (should (= (pdf-present--session-index s) 5))
      (pdf-present-next-slide)
      (should (= (pdf-present--session-index s) 5))
      (pdf-present-goto-slide 0)
      (should (= (pdf-present--session-index s) 1))
      (pdf-present-goto-slide 99)
      (should (= (pdf-present--session-index s) 5))
      (pdf-present-first-slide)
      (should (= (pdf-present--session-index s) 1))
      (pdf-present-last-slide)
      (should (= (pdf-present--session-index s) 5))
      (pdf-present-previous-slide 2)
      (should (= (pdf-present--session-index s) 3)))))

(ert-deftest pdf-present-sync-second-screen ()
  (pdf-present-test-presenting "present-second-screen.pdf"
    (pdf-present)
    (should (equal (pdf-present-test--state)
                   '(:audience (1 (0 0 0.5 1)) :current (1 (0 0 0.5 1))
                     :next (2 (0 0 0.5 1)) :notes (1 (0.5 0 0.5 1)))))
    (pdf-present-next-slide)
    (should (equal (pdf-present-test--state)
                   '(:audience (2 (0 0 0.5 1)) :current (2 (0 0 0.5 1))
                     :next (3 (0 0 0.5 1)) :notes (2 (0.5 0 0.5 1)))))
    (pdf-present-last-slide)
    (should (equal (pdf-present-test--state)
                   '(:audience (5 (0 0 0.5 1)) :current (5 (0 0 0.5 1))
                     :next " *pdf-present-empty*"
                     :notes (5 (0.5 0 0.5 1)))))))

(ert-deftest pdf-present-sync-notes-pages ()
  (pdf-present-test-presenting "present-notes-pages.pdf"
    (pdf-present)
    (should (equal (pdf-present-test--state)
                   '(:audience (1 nil) :current (1 nil)
                     :next (3 nil) :notes (2 nil))))
    (pdf-present-goto-slide 4)
    ;; Slide 4 (page 7) has no notes page.
    (should (equal (pdf-present-test--state)
                   '(:audience (7 nil) :current (7 nil)
                     :next (8 nil) :notes " *pdf-present-empty*")))
    (pdf-present-next-slide)
    (should (equal (pdf-present-test--state)
                   '(:audience (8 nil) :current (8 nil)
                     :next " *pdf-present-empty*" :notes (9 nil))))))

(ert-deftest pdf-present-blank-screen ()
  (pdf-present-test-presenting "present-second-screen.pdf"
    (pdf-present)
    (pdf-present-next-slide)
    (pdf-present-toggle-black)
    (let ((state (pdf-present-test--state)))
      (should (equal (plist-get state :audience) " *pdf-present-black*"))
      ;; The presenter still sees the slide.
      (should (equal (plist-get state :current) '(2 (0 0 0.5 1)))))
    (should (string-match-p "BLACK SCREEN"
                            (with-current-buffer "*pdf-present-info*"
                              (buffer-string))))
    (pdf-present-toggle-white)
    (should (equal (plist-get (pdf-present-test--state) :audience)
                   " *pdf-present-white*"))
    (pdf-present-toggle-white)
    (should (equal (plist-get (pdf-present-test--state) :audience)
                   '(2 (0 0 0.5 1))))
    ;; Moving on ends a black screen, as in PowerPoint.
    (pdf-present-toggle-black)
    (pdf-present-next-slide)
    (should (equal (plist-get (pdf-present-test--state) :audience)
                   '(3 (0 0 0.5 1))))))

(ert-deftest pdf-present-keys ()
  "The clicker and PowerPoint keys run the presentation commands."
  (with-temp-buffer
    (pdf-present-mode 1)
    (dolist (binding '(("<next>" . pdf-present-next-slide)
                       ("<prior>" . pdf-present-previous-slide)
                       ("<right>" . pdf-present-next-slide)
                       ("<left>" . pdf-present-previous-slide)
                       ("n" . pdf-present-next-slide)
                       ("p" . pdf-present-previous-slide)
                       ("SPC" . pdf-present-next-slide)
                       ("<home>" . pdf-present-first-slide)
                       ("<end>" . pdf-present-last-slide)
                       ("g" . pdf-present-goto-slide)
                       ("b" . pdf-present-toggle-black)
                       ("." . pdf-present-toggle-black)
                       ("w" . pdf-present-toggle-white)
                       ("t" . pdf-present-toggle-timer)
                       ("r" . pdf-present-reset-timer)
                       ("q" . pdf-present-quit)))
      (should (eq (key-binding (kbd (car binding))) (cdr binding))))))

(ert-deftest pdf-present-starts-at-current-page ()
  (pdf-present-test-presenting "present-notes-pages.pdf"
    ;; Page 6 is the notes page of slide 3.
    (setf (pdf-view-current-page t) 6)
    (pdf-present)
    (should (= (pdf-present--session-index pdf-present--session) 3))))


;; * ================================================================== *
;; * Timer
;; * ================================================================== *

(ert-deftest pdf-present-timer ()
  (let ((s (pdf-present--session-create)))
    (should (= (pdf-present--elapsed s 100) 0))
    (pdf-present--timer-start s 100)
    (should (= (pdf-present--elapsed s 130) 30))
    (pdf-present--timer-pause s 160)
    (should (= (pdf-present--elapsed s 500) 60))
    ;; Pausing twice changes nothing.
    (pdf-present--timer-pause s 600)
    (should (= (pdf-present--elapsed s 700) 60))
    (pdf-present--timer-start s 1000)
    (should (= (pdf-present--elapsed s 1030) 90))
    ;; Starting a running timer changes nothing.
    (pdf-present--timer-start s 1030)
    (should (= (pdf-present--elapsed s 1040) 100))
    ;; Reset keeps it running.
    (pdf-present--timer-reset s 1100)
    (should (= (pdf-present--elapsed s 1110) 10))
    ;; Reset while paused stays paused at zero.
    (pdf-present--timer-pause s 1120)
    (pdf-present--timer-reset s 1200)
    (should (= (pdf-present--elapsed s 1300) 0))
    (should-not (pdf-present--session-running-since s))))

(ert-deftest pdf-present-format-duration ()
  (should (equal (pdf-present--format-duration 0) "0:00:00"))
  (should (equal (pdf-present--format-duration 59.9) "0:00:59"))
  (should (equal (pdf-present--format-duration 3725) "1:02:05"))
  (should (equal (pdf-present--format-duration 36000) "10:00:00")))

(ert-deftest pdf-present-info-text ()
  (let ((s (pdf-present--session-create
            :map [(1) (3) (5)] :labels ["1" "2" "2" "3" "3"] :index 2
            :elapsed 61.0)))
    (let ((text (pdf-present--info-text s 1000)))
      (should (string-match-p "Slide 2 / 3" text))
      (should (string-match-p "(page 2)" text))
      (should (string-match-p "Elapsed 0:01:01" text))
      (should (string-match-p "paused" text)))))


;; * ================================================================== *
;; * Prefetching
;; * ================================================================== *

(ert-deftest pdf-present-slide-pages ()
  (pdf-present-test-presenting "present-notes-pages.pdf"
    (pdf-present)
    (let ((s pdf-present--session))
      ;; On slide 2 (page 3), the audience and current windows show
      ;; its page, the notes window its notes page (4), and the next
      ;; window the page of slide 3 (5).
      (should (equal (pdf-present--slide-pages s 2)
                     (list (cons (pdf-present--session-next-window s) 5)
                           (cons (pdf-present--session-notes-window s) 4)
                           (cons (pdf-present--session-audience-window s) 3)
                           (cons (pdf-present--session-current-window s) 3))))
      ;; Slide 4 (page 7) has no notes page.
      (should (equal (pdf-present--slide-pages s 4)
                     (list (cons (pdf-present--session-next-window s) 8)
                           (cons (pdf-present--session-audience-window s) 7)
                           (cons (pdf-present--session-current-window s) 7))))
      ;; On the last slide, the next window shows nothing.
      (should (equal (pdf-present--slide-pages s 5)
                     (list (cons (pdf-present--session-notes-window s) 9)
                           (cons (pdf-present--session-audience-window s) 8)
                           (cons (pdf-present--session-current-window s) 8)))))))

(ert-deftest pdf-present-prefetch-chain ()
  "One slide is prefetched per idle timer, chained up to
`pdf-present-prefetch-count' slides beyond the current one."
  (pdf-present-test-presenting "present-plain.pdf"
    (pdf-present)
    (let ((s pdf-present--session))
      ;; With a count of one, only the next slide is rendered.
      (let ((pdf-present-prefetch-delay 0.5)
            (pdf-present-prefetch-count 1))
        (pdf-present--prefetch s 2)
        (should-not (pdf-present--session-prefetch s)))
      ;; Beyond the count of slides from slide 1, nothing is
      ;; rendered and nothing is chained.
      (let ((pdf-present-prefetch-delay 0.5)
            (pdf-present-prefetch-count 3))
        (pdf-present--prefetch s 5)
        (should-not (pdf-present--session-prefetch s))
        ;; Rendering slide 2 of 5 chains slide 3; quitting at the
        ;; end of the test cancels it.
        (pdf-present--prefetch s 2)
        (should (memq (pdf-present--session-prefetch s)
                      timer-idle-list))))))

(ert-deftest pdf-present-prefetch-timer ()
  (pdf-present-test-presenting "present-plain.pdf"
    (pdf-present)
    ;; The presenting macro turns prefetching off: no timer is made.
    (should-not (pdf-present--session-prefetch pdf-present--session))
    (let ((pdf-present-prefetch-delay 0.5))
      (pdf-present--schedule-prefetch pdf-present--session)
      (let ((timer (pdf-present--session-prefetch pdf-present--session)))
        (should (timerp timer))
        (should (memq timer timer-idle-list))
        ;; Moving to another slide replaces the timer.
        (pdf-present-next-slide)
        (should-not (memq timer timer-idle-list))
        (should (memq (pdf-present--session-prefetch pdf-present--session)
                      timer-idle-list))
        ;; Quitting cancels it.
        (pdf-present-quit)
        (should-not (memq timer timer-idle-list))))))

(ert-deftest pdf-present-prefetch-after-resize ()
  "A window manager that resizes the windows makes the prefetched
pages stale; `pdf-present--on-resize' prefetches them again."
  (pdf-present-test-presenting "present-plain.pdf"
    (pdf-present)
    (let ((pdf-present-prefetch-delay 0.5))
      (pdf-present--on-resize (selected-frame))
      (should (timerp (pdf-present--session-prefetch pdf-present--session))))))



;; * ================================================================== *
;; * Monitors
;; * ================================================================== *

(ert-deftest pdf-present-choose-monitor ()
  (let* ((laptop '((name . "eDP-1") (geometry 0 0 1920 1080)))
         (projector '((name . "HDMI-1") (geometry 1920 0 1280 720)))
         (tv '((name . "DP-2") (geometry 3200 0 3840 2160)))
         (monitors (list laptop projector tv)))
    ;; Default: the first monitor the presenter is not on.
    (should (eq (pdf-present--choose-monitor monitors laptop) projector))
    (should (eq (pdf-present--choose-monitor monitors projector) laptop))
    ;; By position and by name.
    (should (eq (pdf-present--choose-monitor monitors laptop 2) tv))
    (should (eq (pdf-present--choose-monitor monitors laptop "DP-2") tv))
    ;; Unknown choices fall back to the default.
    (should (eq (pdf-present--choose-monitor monitors laptop 7) projector))
    (should (eq (pdf-present--choose-monitor monitors laptop "VGA") projector))
    ;; One monitor: the presenter's.
    (should (eq (pdf-present--choose-monitor (list laptop) laptop) laptop))))

(ert-deftest pdf-present-audience-rectangle ()
  (let ((laptop '((name . "eDP-1") (geometry 0 0 1920 1080)
                  (workarea 0 40 1920 1040)))
        (projector '((name . "HDMI-1") (geometry 1920 0 1280 720)
                     (workarea 1920 0 1280 720))))
    ;; Two monitors: the whole monitor, panels or not.
    (should (equal (pdf-present--audience-rectangle projector nil)
                   '(1920 0 1280 720)))
    ;; One monitor: a quarter of the workarea, not of the monitor.
    (should (equal (pdf-present--audience-rectangle laptop t)
                   '(0 40 960 520)))))


;; * ================================================================== *
;; * Quitting
;; * ================================================================== *

(ert-deftest pdf-present-quit-restores-state ()
  (pdf-present-test-presenting "present-second-screen.pdf"
    (let ((window (selected-window))
          (config (current-window-configuration))
          (buffer (current-buffer)))
      (set-window-buffer window buffer)
      (setq pdf-view-display-size 'fit-width)
      (setf (pdf-view-current-slice window) '(0.1 0.1 0.8 0.8))
      (pdf-view-goto-page 3 window)
      (setq config (current-window-configuration))
      (pdf-present)
      (should pdf-present--session)
      (should pdf-present-mode)
      (should (eq pdf-view-display-size 'fit-page))
      (let ((ticker (pdf-present--session-ticker pdf-present--session)))
        (should (memq ticker timer-list))
        (pdf-present-next-slide)
        (pdf-present-toggle-black)
        (pdf-present-quit)
        (should-not (memq ticker timer-list)))
      (should-not pdf-present--session)
      (should-not pdf-present-mode)
      (should (eq pdf-view-display-size 'fit-width))
      (should (eq (selected-window) window))
      (should (eq (window-buffer window) buffer))
      (should (= (length (window-list)) 1))
      (should (compare-window-configurations
               config (current-window-configuration)))
      (should (= (pdf-view-current-page window) 3))
      (should (equal (pdf-view-current-slice window) '(0.1 0.1 0.8 0.8)))
      (should-not (memq #'pdf-present-quit kill-buffer-hook))
      (dolist (name pdf-present--buffer-names)
        (should-not (get-buffer name)))
      ;; Quitting again is harmless.
      (pdf-present-quit))))

(ert-deftest pdf-present-quit-on-kill-buffer ()
  (pdf-present-test-presenting "present-plain.pdf"
    (pdf-present)
    (should pdf-present--session)
    (run-hooks 'kill-buffer-hook)
    (should-not pdf-present--session)))
