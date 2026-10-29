;;; pdf-present.el --- Present slides with a presenter view. -*- lexical-binding: t -*-

;; Copyright (C) 2026  Tim Ransom

;; Author: Tim Ransom
;; Keywords: files, multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `pdf-present' turns the PDF in the current buffer into a slide
;; show with two windows on the desktop:
;;
;; - the audience window shows the current slide alone, covering
;;   another monitor (the projector);
;;
;; - the presenter window shows the current slide, the next slide,
;;   the speaker notes, the slide counter, the elapsed time and
;;   the clock.
;;
;; Both are plain windows rather than fullscreen frames: switching
;; a frame to fullscreen is slow, and Wayland compositors such as
;; COSMIC ignore it outright.  Each window is made in its final
;; size, so the slides render once, and while the presenter holds
;; on a slide the coming ones are rendered in advance (see
;; `pdf-present-prefetch-delay' and `pdf-present-prefetch-count').  Where the window manager honors
;; positions, the windows are put on their monitors; a compositor
;; that places windows itself — the tiling one in Pop!_OS, say —
;; can be told to float them on their outputs with its window
;; rules for "PDF Audience" and "PDF Presenter".
;;
;; Both windows use the same `pdf-view-mode' buffer; every window
;; shows its own page (and slice) of it, so pdf-tools does all the
;; rendering.  The windows always show the same slide: all
;; navigation goes through one session object, which then updates
;; every window.
;;
;; Speaker notes come from the PDF itself.  Three layouts are known (see
;; `pdf-present-notes-layout'):
;;
;; - `second-screen': Beamer's \setbeameroption{show notes on second
;;   screen=right}.  Each page is twice as wide as a slide; the slide is
;;   the left half and its notes the right half.  The audience sees the
;;   left half only.
;;
;; - `notes-pages': Beamer's \setbeameroption{show notes}.  A notes page
;;   follows each slide that has a \note.  The audience never sees the
;;   notes pages; they are shown in the presenter's notes window.
;;
;; - `none': a PDF without notes.
;;
;; In both frames: n/p (also PageDown/PageUp, the arrow keys, SPC and
;; DEL, which is what presentation clickers send) move by one slide, </>
;; (Home/End) go to the first/last slide, g asks for a slide number, b
;; and w black or white out the audience screen, d sets the talk's
;; planned length, t pauses the timer, r resets it, and q ends the
;; presentation and restores everything.

(require 'pdf-view)
(require 'pdf-info)
(require 'pdf-cache)
(require 'pdf-util)
(require 'cl-lib)
(require 'subr-x)

(defvar pdf-view-roll-minor-mode)
(declare-function pdf-view-roll-minor-mode "pdf-roll")

;;; Code:



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-present nil
  "Presenting PDF slides with an audience and a presenter view."
  :group 'pdf-tools)

(defcustom pdf-present-notes-layout 'auto
  "Where the speaker notes are in the PDF.

`auto' decides from the PDF: pages much wider than tall (see
`pdf-present-second-screen-ratio') are `second-screen', a PDF
with Beamer notes pages is `notes-pages', anything else `none'.

`second-screen' is Beamer's \\setbeameroption{show notes on
second screen=right}: each page holds the slide on its left half
and the notes on its right half.

`notes-pages' is Beamer's \\setbeameroption{show notes}: a notes
page follows every slide that has notes.  A notes page is
recognised by the miniature of the preceding slide that Beamer's
default note template puts on it.

`none' shows every page as a slide and no notes."
  :group 'pdf-present
  :type '(choice (const :tag "Detect from the PDF" auto)
                 (const :tag "Notes on the right half (Beamer second screen)"
                        second-screen)
                 (const :tag "Separate notes pages (Beamer show notes)"
                        notes-pages)
                 (const :tag "No notes" none)))

(defcustom pdf-present-second-screen-ratio 2.4
  "Smallest width/height ratio of a page holding a slide and its notes.

A 16:9 slide with notes on the second screen is 3.56 times as wide
as it is tall, a 4:3 one 2.67; single slides reach at most 2.35
\(Beamer's aspectratio=235).  Used when `pdf-present-notes-layout'
is `auto'."
  :group 'pdf-present
  :type 'number)

(defcustom pdf-present-audience-monitor nil
  "The monitor that shows the audience frame.

nil means the first monitor that the presenter frame is not on.
An integer is a position in `display-monitor-attributes-list'
\(counting from 0); a string is a monitor name such as \"HDMI-1\".
With a prefix argument, `pdf-present' asks.

Window managers that let a program place its windows put the
frame on this monitor; Wayland compositors place it themselves and
keep only its size."
  :group 'pdf-present
  :type '(choice (const :tag "Another monitor than the presenter's" nil)
                 (integer :tag "Monitor number")
                 (string :tag "Monitor name")))

(defcustom pdf-present-clock-format "%H:%M"
  "Format of the wall clock in the presenter view.
See `format-time-string'."
  :group 'pdf-present
  :type 'string)

(defcustom pdf-present-prefetch-delay 0.5
  "Seconds of idle time before the next slide is rendered.
While a slide is held, the pages that the next one will put on
screen are rendered into the cache, so that moving on does not
wait for them.  nil turns the prefetching off."
  :group 'pdf-present
  :type '(choice (number :tag "Seconds of idle time")
                 (const :tag "Off" nil)))

(defcustom pdf-present-prefetch-count 3
  "How many slides beyond the current one to render in advance.
While the presenter holds on a slide, the coming slides have their
pages rendered into the cache, one slide every
`pdf-present-prefetch-delay' seconds of idle time.  A slide needs
up to four images and the cache holds `pdf-cache-image-limit' of
them, so a count much beyond a quarter of that thrashes it;
smaller counts mean one slide."
  :group 'pdf-present
  :type 'natnum)

(defcustom pdf-present-talk-duration nil
  "Planned length of the talk, in minutes.
When set, the presenter view shows how much of it is left, in the
`pdf-present-alert' face once little is left (see
`pdf-present-talk-warning') and as overtime once it is over.  The
`d' key sets it while presenting."
  :group 'pdf-present
  :type '(choice (const :tag "No planned duration" nil)
                 (number :tag "Minutes")))

(defcustom pdf-present-talk-warning 2
  "Minutes left of the talk at which to warn the presenter.
The time left is shown in the `pdf-present-alert' face from here
on down, and when the talk is over."
  :group 'pdf-present
  :type 'number)

(defcustom pdf-present-start-hook nil
  "Hook run after a presentation has started.
The PDF buffer is current."
  :group 'pdf-present
  :type 'hook)

(defcustom pdf-present-quit-hook nil
  "Hook run after a presentation has ended and the state is restored.
The PDF buffer is current."
  :group 'pdf-present
  :type 'hook)

(defface pdf-present-info
  '((t (:height 1.6)))
  "Face of the slide counter and the timers in the presenter view."
  :group 'pdf-present)

(defface pdf-present-alert
  '((t (:inherit (pdf-present-info warning) :weight bold)))
  "Face of the black/white screen and paused-timer indicators."
  :group 'pdf-present)

(defface pdf-present-label
  '((t (:inherit shadow)))
  "Face of the window labels in the presenter view."
  :group 'pdf-present)



;; * ================================================================== *
;; * Session
;; * ================================================================== *

(cl-defstruct (pdf-present--session
               (:constructor pdf-present--session-create)
               (:copier nil))
  "State of a running presentation."
  buffer
  ;; One of `second-screen', `notes-pages' or `none'.
  layout
  ;; Vector of (SLIDE-PAGE . NOTES-PAGE-OR-NIL), one per slide.
  map
  ;; Page labels, or nil when they are not shown.
  labels
  ;; Current slide, counting from 1.
  (index 1)
  ;; nil, `black' or `white'.
  blank
  ;; Seconds accumulated while the timer ran, before RUNNING-SINCE.
  (elapsed 0.0)
  ;; `float-time' at which the timer was last started, or nil if paused.
  running-since
  ;; Planned length of the talk in minutes, or nil for none.
  duration
  ;; The timer that updates the clock.
  ticker
  ;; The idle timer rendering coming slides in advance, or nil.
  prefetch
  audience-frame presenter-frame
  audience-window current-window next-window notes-window info-window
  ;; Where `pdf-present' was called, and what to restore on quit.
  origin-frame origin-window saved
  ;; Non-nil while quitting.
  quitting)

(defvar pdf-present--session nil
  "The running presentation, or nil.")

(defun pdf-present--count (session)
  "Return the number of slides in SESSION."
  (length (pdf-present--session-map session)))

(defun pdf-present--slide-page (session &optional index)
  "Return the PDF page of slide INDEX in SESSION.
INDEX defaults to the current slide."
  (car (aref (pdf-present--session-map session)
             (1- (or index (pdf-present--session-index session))))))

(defun pdf-present--notes-page (session &optional index)
  "Return the PDF page with the notes of slide INDEX, or nil.
INDEX defaults to the current slide of SESSION."
  (cdr (aref (pdf-present--session-map session)
             (1- (or index (pdf-present--session-index session))))))

(defun pdf-present--next-index (session &optional index)
  "Return the index of the slide after INDEX in SESSION, or nil.
INDEX defaults to the current slide."
  (let ((next (1+ (or index (pdf-present--session-index session)))))
    (and (<= next (pdf-present--count session)) next)))

(defun pdf-present--index-of-page (session page)
  "Return the index of the slide that PAGE belongs to in SESSION.
A notes page belongs to its slide; a page before the first slide
to the first."
  (let ((map (pdf-present--session-map session))
        (found 1))
    (dotimes (i (length map))
      (let ((entry (aref map i)))
        (when (or (<= (car entry) page)
                  (eql (cdr entry) page))
          (setq found (1+ i)))))
    found))

(defun pdf-present--slice (layout role)
  "Return the slice for ROLE (`slide' or `notes') in LAYOUT.
The slice is a list (X Y WIDTH HEIGHT) for `pdf-view-set-slice',
or nil for the whole page."
  (when (eq layout 'second-screen)
    (if (eq role 'notes)
        (list 0.5 0 0.5 1)
      (list 0 0 0.5 1))))



;; * ================================================================== *
;; * Notes layouts
;; * ================================================================== *

(defun pdf-present--second-screen-size-p (size)
  "Return non-nil if page SIZE (WIDTH . HEIGHT) holds slide and notes."
  (and (> (cdr size) 0)
       (>= (/ (float (car size)) (cdr size))
           pdf-present-second-screen-ratio)))

(defconst pdf-present--miniature-scale 0.75
  "Text this much smaller than on the slide marks a slide miniature.")

(defun pdf-present--first-line (page &optional buffer)
  "Return the first line of text on PAGE of BUFFER, or nil.
The line is cut to at most 40 characters at a word boundary."
  (let* ((text (pdf-info-gettext page '(0 0 1 1) 'line buffer))
         (line (cl-find-if (lambda (l) (>= (length l) 3))
                           (mapcar #'string-trim (split-string text "\n")))))
    (when line
      (if (<= (length line) 40)
          line
        (let ((cut (substring line 0 40)))
          (if (string-match "\\`\\(.*[^ ]\\) " cut)
              (match-string 1 cut)
            cut))))))

(defun pdf-present--match-heights (string page &optional buffer)
  "Return the heights of the matches of STRING on PAGE of BUFFER."
  (let ((case-fold-search nil))
    (mapcar (lambda (match)
              (let ((edges (car (alist-get 'edges match))))
                (- (nth 3 edges) (nth 1 edges))))
            (pdf-info-search-string string page buffer))))

(defun pdf-present--miniature-p (page previous &optional buffer)
  "Return non-nil if PAGE of BUFFER shows a miniature of page PREVIOUS.

That is what a Beamer notes page looks like: the first line of
PREVIOUS reappears on PAGE in much smaller type."
  (let ((line (pdf-present--first-line previous buffer)))
    (when line
      (let ((ref (pdf-present--match-heights line previous buffer))
            (here (pdf-present--match-heights line page buffer)))
        (and ref here
             (< (apply #'min here)
                (* pdf-present--miniature-scale (apply #'max ref))))))))

(defun pdf-present--notes-pages (npages note-page-p)
  "Return the list of notes pages among pages 1 to NPAGES.

NOTE-PAGE-P is called with a page and the page before it, and
says whether the page is a notes page for it.  A notes page
always follows a slide: the first page, and a page right after a
notes page, are slides."
  (let (notes)
    (dotimes (i (max 0 (1- npages)))
      (let ((page (+ i 2)))
        (when (and (not (memql (1- page) notes))
                   (funcall note-page-p page (1- page)))
          (push page notes))))
    (nreverse notes)))

(defun pdf-present--page-map (layout npages &optional notes-pages)
  "Return the slide map for a PDF of NPAGES pages in LAYOUT.

The map is a vector of (SLIDE-PAGE . NOTES-PAGE) conses, one per
slide, where NOTES-PAGE is nil for a slide without notes.
NOTES-PAGES lists the notes pages, for the `notes-pages' layout."
  (let (map)
    (dotimes (i npages)
      (let ((page (1+ i)))
        (cond
         ((eq layout 'second-screen)
          (push (cons page page) map))
         ((and (eq layout 'notes-pages) (memql page notes-pages))
          (when (and map (null (cdar map)))
            (setcdr (car map) page)))
         (t
          (push (cons page nil) map)))))
    (vconcat (nreverse map))))

(defun pdf-present--detect (buffer &optional layout)
  "Return (LAYOUT . NOTES-PAGES) for the PDF in BUFFER.
LAYOUT defaults to `pdf-present-notes-layout'; when that is
`auto', it is decided from the PDF."
  (with-current-buffer buffer
    (let ((layout (or layout pdf-present-notes-layout))
          (npages (pdf-cache-number-of-pages)))
      (when (and (eq layout 'auto)
                 (pdf-present--second-screen-size-p (pdf-cache-pagesize 1)))
        (setq layout 'second-screen))
      (if (memq layout '(auto notes-pages))
          (let ((notes (pdf-present--notes-pages
                        npages
                        (lambda (page previous)
                          (pdf-present--miniature-p page previous buffer)))))
            (cons (if (or notes (eq layout 'notes-pages)) 'notes-pages 'none)
                  notes))
        (cons layout nil)))))

(defun pdf-present--labels (buffer layout)
  "Return the page labels of BUFFER worth showing in LAYOUT, or nil.

Labels that only repeat the page numbers are not worth showing.
Neither are those of a `second-screen' PDF: pgfpages, which
Beamer uses for it, assigns them to the wrong pages."
  (unless (eq layout 'second-screen)
    (let ((labels (ignore-errors (pdf-info-pagelabels buffer)))
          (page 0))
      (when (cl-some (lambda (label)
                       (not (equal label (number-to-string (cl-incf page)))))
                     labels)
        (vconcat labels)))))

(defun pdf-present--make-session (buffer &optional layout)
  "Return a new presentation session for the PDF in BUFFER.
LAYOUT overrides `pdf-present-notes-layout'."
  (with-current-buffer buffer
    (let* ((detected (pdf-present--detect buffer layout))
           (layout (car detected)))
      (pdf-present--session-create
       :buffer buffer
       :layout layout
       :map (pdf-present--page-map layout (pdf-cache-number-of-pages)
                                   (cdr detected))
       :labels (pdf-present--labels buffer layout)
       :duration pdf-present-talk-duration))))



;; * ================================================================== *
;; * Timer
;; * ================================================================== *

(defun pdf-present--elapsed (session &optional now)
  "Return the seconds the timer of SESSION has run until NOW."
  (+ (pdf-present--session-elapsed session)
     (let ((since (pdf-present--session-running-since session)))
       (if since (max 0 (- (or now (float-time)) since)) 0))))

(defun pdf-present--timer-start (session &optional now)
  "Start or resume the timer of SESSION at NOW."
  (unless (pdf-present--session-running-since session)
    (setf (pdf-present--session-running-since session)
          (or now (float-time)))))

(defun pdf-present--timer-pause (session &optional now)
  "Pause the timer of SESSION at NOW."
  (when (pdf-present--session-running-since session)
    (setf (pdf-present--session-elapsed session)
          (pdf-present--elapsed session now))
    (setf (pdf-present--session-running-since session) nil)))

(defun pdf-present--timer-reset (session &optional now)
  "Set the timer of SESSION back to zero at NOW, keeping it running or not."
  (setf (pdf-present--session-elapsed session) 0.0)
  (when (pdf-present--session-running-since session)
    (setf (pdf-present--session-running-since session)
          (or now (float-time)))))

(defun pdf-present--format-duration (seconds)
  "Format SECONDS as H:MM:SS."
  (let ((s (floor seconds)))
    (format "%d:%02d:%02d" (/ s 3600) (% (/ s 60) 60) (% s 60))))

(defun pdf-present--left (session &optional now)
  "Return the seconds left of SESSION's talk duration, or nil.
NOW is as in `pdf-present--elapsed'; a negative result is
overtime."
  (when-let* ((minutes (pdf-present--session-duration session)))
    (- (* minutes 60) (pdf-present--elapsed session now))))



;; * ================================================================== *
;; * Monitors
;; * ================================================================== *

(defun pdf-present--monitor-label (monitor)
  "Return a short description of MONITOR for completion."
  (let ((geometry (alist-get 'geometry monitor)))
    (format "%s (%dx%d+%d+%d)"
            (or (alist-get 'name monitor) "monitor")
            (nth 2 geometry) (nth 3 geometry)
            (nth 0 geometry) (nth 1 geometry))))

(defun pdf-present--choose-monitor (monitors presenter &optional spec)
  "Return the audience monitor among MONITORS.

MONITORS is a list like `display-monitor-attributes-list' returns,
PRESENTER the attributes of the monitor the presenter frame is on.
SPEC is like `pdf-present-audience-monitor'.  When nothing else
fits, the result is PRESENTER."
  (let ((presenter-geometry (alist-get 'geometry presenter)))
    (or (cond
         ((integerp spec) (nth spec monitors))
         ((stringp spec)
          (cl-find spec monitors
                   :key (lambda (m) (alist-get 'name m)) :test #'equal)))
        (cl-find-if-not (lambda (m)
                          (equal (alist-get 'geometry m) presenter-geometry))
                        monitors)
        presenter)))

(defun pdf-present--read-monitor ()
  "Ask for the audience monitor and return its position."
  (let* ((monitors (display-monitor-attributes-list))
         (labels (mapcar #'pdf-present--monitor-label monitors))
         (choice (completing-read "Audience monitor: " labels nil t nil nil
                                  (car labels))))
    (cl-position choice labels :test #'equal)))



;; * ================================================================== *
;; * Display
;; * ================================================================== *

(defun pdf-present--text-buffer (name)
  "Return the presentation's text buffer NAME, creating it if needed."
  (or (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (setq buffer-read-only t)
        (setq-local cursor-type nil)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local mode-line-format nil)
        (setq-local truncate-lines nil)
        (setq-local truncate-partial-width-windows nil)
        (setq-local word-wrap t)
        (pdf-present-mode 1)
        (current-buffer))))

(defun pdf-present--set-text (buffer text)
  "Replace the contents of BUFFER with TEXT."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))))

(defun pdf-present--blank-buffer (color)
  "Return the buffer that blanks the audience screen in COLOR."
  (let ((name (format " *pdf-present-%s*" color)))
    (or (get-buffer name)
        (with-current-buffer (pdf-present--text-buffer name)
          (face-remap-add-relative 'default :background (symbol-name color))
          (face-remap-add-relative 'fringe :background (symbol-name color))
          (current-buffer)))))

(defconst pdf-present--buffer-names
  '(" *pdf-present-black*" " *pdf-present-white*"
    "*pdf-present-info*" " *pdf-present-empty*")
  "Names of the presentation's helper buffers.")

(defun pdf-present--show-page (window page slice)
  "Show PAGE of the presentation's PDF in WINDOW, cut to SLICE.
This is the only place that puts a PDF page on the screen."
  (let ((buffer (pdf-present--session-buffer pdf-present--session)))
    (unless (eq (window-buffer window) buffer)
      (set-window-buffer window buffer t))
    ;; Select WINDOW: when its winprops are new, the hook that sets them
    ;; up (`pdf-view-new-window-function') files the window's overlay
    ;; under the selected window.
    (with-selected-window window
      (image-mode-winprops window)
      (unless (equal (pdf-view-current-slice window) slice)
        (setf (pdf-view-current-slice window) slice))
      (pdf-view-goto-page page window))))

(defun pdf-present--show-buffer (window buffer)
  "Show the helper BUFFER in WINDOW."
  (unless (eq (window-buffer window) buffer)
    (set-window-buffer window buffer t)))

(defun pdf-present--info-text (session &optional now)
  "Return the text of the presenter's info window for SESSION at NOW."
  (let* ((index (pdf-present--session-index session))
         (count (pdf-present--count session))
         (labels (pdf-present--session-labels session))
         (page (pdf-present--slide-page session))
         (label (and labels (aref labels (1- page))))
         (running (pdf-present--session-running-since session))
         (blank (pdf-present--session-blank session)))
    (concat
     (propertize (format "Slide %d / %d" index count) 'face 'pdf-present-info)
     (if label
         (propertize (format "   (page %s)" label) 'face 'pdf-present-info)
       "")
     "\n"
     (propertize (format "Elapsed %s"
                         (pdf-present--format-duration
                          (pdf-present--elapsed session now)))
                 'face 'pdf-present-info)
     (if running "" (propertize "   paused" 'face 'pdf-present-alert))
     "\n"
     (when-let* ((left (pdf-present--left session now)))
       (concat
        (if (>= left 0)
            (propertize
             (format "Left    %s" (pdf-present--format-duration left))
             'face (if (<= left (* 60 pdf-present-talk-warning))
                       'pdf-present-alert 'pdf-present-info))
          (propertize
           (format "Over    %s" (pdf-present--format-duration (- left)))
           'face 'pdf-present-alert))
        "\n"))
     (propertize (format "Time    %s"
                         (format-time-string pdf-present-clock-format
                                             (and now (seconds-to-time now))))
                 'face 'pdf-present-info)
     "\n"
     (if blank
         (propertize (format "%s SCREEN\n" (upcase (symbol-name blank)))
                     'face 'pdf-present-alert)
       "\n")
     "\n"
     (propertize (concat "n/p next/previous   g go to   b/w black/white\n"
                         "d duration   t pause timer   r reset timer   q quit")
                 'face 'pdf-present-label))))

(defun pdf-present--update-info (session)
  "Update the presenter's info window of SESSION."
  (let ((window (pdf-present--session-info-window session)))
    (when (window-live-p window)
      (pdf-present--set-text (window-buffer window)
                             (pdf-present--info-text session)))))

(defun pdf-present--refresh (session)
  "Show the current state of SESSION in every presentation window."
  (let* ((layout (pdf-present--session-layout session))
         (slide (pdf-present--slide-page session))
         (slide-slice (pdf-present--slice layout 'slide))
         (next (pdf-present--next-index session))
         (notes (pdf-present--notes-page session))
         (blank (pdf-present--session-blank session))
         (empty (pdf-present--text-buffer " *pdf-present-empty*")))
    (when-let* ((window (pdf-present--session-audience-window session)))
      (if blank
          (pdf-present--show-buffer window (pdf-present--blank-buffer blank))
        (pdf-present--show-page window slide slide-slice)))
    (when-let* ((window (pdf-present--session-current-window session)))
      (pdf-present--show-page window slide slide-slice))
    (when-let* ((window (pdf-present--session-next-window session)))
      (if next
          (pdf-present--show-page
           window (pdf-present--slide-page session next) slide-slice)
        (pdf-present--show-buffer window empty)))
    (when-let* ((window (pdf-present--session-notes-window session)))
      (if notes
          (pdf-present--show-page window notes
                                  (pdf-present--slice layout 'notes))
        (pdf-present--show-buffer window empty)))
    (pdf-present--update-info session)
    ;; While the slide is held, render the next one in the background.
    (pdf-present--schedule-prefetch session)))

(defun pdf-present--decorate (window &optional label)
  "Remove the mode line of WINDOW; give it header LABEL if non-nil."
  (set-window-parameter window 'mode-line-format 'none)
  (set-window-parameter window 'header-line-format
                        (if label (propertize label 'face 'pdf-present-label)
                          'none)))

(defun pdf-present--make-frame (name rectangle &optional parameters)
  "Make an undecorated frame NAME exactly covering RECTANGLE.
RECTANGLE is a list (X Y WIDTH HEIGHT) of pixels, such as the
`geometry' or `workarea' of a monitor in
`display-monitor-attributes-list'.  PARAMETERS are more frame
parameters for `make-frame'.

The frame never goes fullscreen: switching a frame to fullscreen
is slow, and Wayland compositors such as COSMIC ignore it.  It is
born in its final size instead, with no menu, tool or tab bars,
fringes, borders or scroll bars, so that it covers RECTANGLE
exactly.  Window managers that honor positions put it there; those
that place windows themselves (all Wayland compositors) keep only
its size, and a tiling one tiles it."
  (let ((frame-resize-pixelwise t))
    (make-frame
     (append parameters
             `((name . ,name)
               (left . ,(nth 0 rectangle))
               (top . ,(nth 1 rectangle))
               (user-position . t)
               (undecorated . t)
               (menu-bar-lines . 0)
               (tool-bar-lines . 0)
               (tab-bar-lines . 0)
               (vertical-scroll-bars . nil)
               (horizontal-scroll-bars . nil)
               (left-fringe . 0)
               (right-fringe . 0)
               (internal-border-width . 0)
               (width . (text-pixels . ,(nth 2 rectangle)))
               (height . (text-pixels . ,(nth 3 rectangle))))))))

(defun pdf-present--audience-rectangle (monitor one-monitor)
  "Return the audience frame's rectangle (X Y WIDTH HEIGHT).
MONITOR is the audience monitor's attributes, as
`display-monitor-attributes-list' returns them.  With ONE-MONITOR
non-nil, the presentation shares the monitor with the presenter,
and the audience frame takes a quarter of its workarea instead of
the whole monitor."
  (if one-monitor
      (let ((workarea (alist-get 'workarea monitor)))
        (list (nth 0 workarea) (nth 1 workarea)
              (/ (nth 2 workarea) 2) (/ (nth 3 workarea) 2)))
    (alist-get 'geometry monitor)))

(defun pdf-present--setup-display (session monitor-spec)
  "Make the frames and windows of SESSION.
MONITOR-SPEC is like `pdf-present-audience-monitor'."
  (let* ((presenter-monitor (frame-monitor-attributes))
         (audience-monitor (pdf-present--choose-monitor
                            (display-monitor-attributes-list)
                            presenter-monitor monitor-spec))
         (one-monitor (equal (alist-get 'geometry audience-monitor)
                             (alist-get 'geometry presenter-monitor))))
    (when one-monitor
      (message "Only one monitor: the audience window takes a quarter of it"))
    ;; Make and split everything before any page is displayed, so
    ;; that none renders twice while the windows settle: pages are
    ;; displayed by `pdf-present--refresh', which `pdf-present'
    ;; calls after this, when every window has its final size.
    (let* ((pdf-view-inhibit-redisplay t)
           (presenter (pdf-present--make-frame
                       "PDF Presenter"
                       (alist-get 'workarea presenter-monitor)))
           (audience (pdf-present--make-frame
                      "PDF Audience"
                      (pdf-present--audience-rectangle
                       audience-monitor one-monitor)
                      `((minibuffer . ,(minibuffer-window presenter))
                        (background-color . "black")
                        (unsplittable . t)))))
      (setf (pdf-present--session-presenter-frame session) presenter
            (pdf-present--session-audience-frame session) audience)
      ;; Audience: one window, nothing but the slide.
      (let ((window (frame-root-window audience)))
        (pdf-present--decorate window)
        (setf (pdf-present--session-audience-window session) window))
      ;; Presenter: current | notes over next | info, or without notes
      ;; current | next over info.
      (let* ((current (frame-root-window presenter))
             (info (split-window current
                                 (floor (* 0.62 (window-total-height current)))
                                 'below))
             (notes-p (not (eq (pdf-present--session-layout session) 'none)))
             (right (split-window current
                                  (when notes-p
                                    (floor (* 0.5 (window-total-width current))))
                                  'right))
             (next (if notes-p
                       (split-window info nil 'left)
                     right)))
        (pdf-present--decorate current " Current slide")
        (pdf-present--decorate next " Next slide")
        (pdf-present--decorate info)
        (when notes-p (pdf-present--decorate right " Notes"))
        (set-window-buffer info (pdf-present--text-buffer "*pdf-present-info*"))
        (setf (pdf-present--session-current-window session) current
              (pdf-present--session-next-window session) next
              (pdf-present--session-notes-window session) (and notes-p right)
              (pdf-present--session-info-window session) info)
        (select-window current))
      (select-frame-set-input-focus presenter))))

(defun pdf-present--teardown-display (session)
  "Delete the frames of SESSION and kill the helper buffers."
  (dolist (frame (list (pdf-present--session-audience-frame session)
                       (pdf-present--session-presenter-frame session)))
    (when (frame-live-p frame)
      (delete-frame frame t)))
  (dolist (name pdf-present--buffer-names)
    (when (get-buffer name)
      (kill-buffer name))))



;; * ================================================================== *
;; * Prefetching
;; * ================================================================== *

(defun pdf-present--slide-pages (session index)
  "Return the (WINDOW . PAGE) pairs shown on slide INDEX of SESSION.
The audience and current windows show the slide's page, the notes
window its notes page, and the next window the page of the slide
after it; pairs without a page or a window are left out."
  (let* ((slide (pdf-present--slide-page session index))
         (pairs (list (cons (pdf-present--session-audience-window session)
                            slide)
                     (cons (pdf-present--session-current-window session)
                           slide))))
    ;; The notes window shows the slide's notes page.
    (when-let* ((window (pdf-present--session-notes-window session))
                (notes (pdf-present--notes-page session index)))
      (push (cons window notes) pairs))
    ;; The next window shows the slide after this one.
    (when-let* ((window (pdf-present--session-next-window session))
                (after (pdf-present--next-index session index)))
      (push (cons window (pdf-present--slide-page session after)) pairs))
    pairs))

(defun pdf-present--prefetch (session index)
  "Render the pages slide INDEX of SESSION will show into the cache.
Each page is rendered as its window would show it, so that moving
to the slide finds it there and only has to display it.  One slide
is rendered per call; while Emacs stays idle, another follows every
`pdf-present-prefetch-delay' seconds, until
`pdf-present-prefetch-count' slides beyond the current one are
cached."
  (setf (pdf-present--session-prefetch session) nil)
  (let* ((current (pdf-present--session-index session))
         (horizon (+ current (max 1 (or pdf-present-prefetch-count 1))))
         (after (pdf-present--next-index session index))
         (buffer (pdf-present--session-buffer session)))
    ;; The presenter may have moved since this was scheduled; the
    ;; pages are stale then, and a fresh prefetch is on its way.
    (when (and (> index current)
               (<= index horizon)
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (dolist (pair (pdf-present--slide-pages session index))
          (when (window-live-p (car pair))
            ;; `pdf-view-create-page' renders the page, or finds it in
            ;; the cache, without displaying anything.
            (ignore-errors
              (pdf-view-create-page (cdr pair) (car pair)))))))
    ;; Chain to the following slide.
    (when (and after
               pdf-present-prefetch-delay
               (> after current)
               (<= after horizon))
      (setf (pdf-present--session-prefetch session)
            (run-with-idle-timer pdf-present-prefetch-delay nil
                                 #'pdf-present--prefetch session after)))))

(defun pdf-present--schedule-prefetch (session)
  "Render SESSION's coming slides when Emacs is idle.
`pdf-present-prefetch-delay' says when each slide is rendered,
and nil turns the prefetching off."
  (when (timerp (pdf-present--session-prefetch session))
    (cancel-timer (pdf-present--session-prefetch session)))
  (let ((next (and pdf-present-prefetch-delay
                  (pdf-present--next-index session))))
    (setf (pdf-present--session-prefetch session)
          (and next
               (run-with-idle-timer pdf-present-prefetch-delay nil
                                    #'pdf-present--prefetch session next)))))



;; * ================================================================== *
;; * Saving and restoring
;; * ================================================================== *

(defun pdf-present--save-state (session)
  "Record in SESSION what the presentation changes in its buffer."
  (with-current-buffer (pdf-present--session-buffer session)
    (setf (pdf-present--session-origin-frame session) (selected-frame)
          (pdf-present--session-origin-window session) (selected-window)
          (pdf-present--session-saved session)
          `((display-size . ,pdf-view-display-size)
            (roll . ,(bound-and-true-p pdf-view-roll-minor-mode))
            (windows . ,(mapcar (lambda (window)
                                  (list window
                                        (pdf-view-current-page window)
                                        (pdf-view-current-slice window)))
                                (get-buffer-window-list nil nil t)))))))

(defun pdf-present--prepare-buffer (session)
  "Set up the PDF buffer of SESSION for presenting."
  (with-current-buffer (pdf-present--session-buffer session)
    (when (bound-and-true-p pdf-view-roll-minor-mode)
      (pdf-view-roll-minor-mode -1))
    (setq pdf-view-display-size 'fit-page)
    (add-hook 'kill-buffer-hook #'pdf-present-quit nil t)
    (pdf-present-mode 1)))

(defun pdf-present--restore-buffer (session)
  "Restore the PDF buffer of SESSION and its windows."
  (let ((buffer (pdf-present--session-buffer session))
        (saved (pdf-present--session-saved session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (remove-hook 'kill-buffer-hook #'pdf-present-quit t)
        (pdf-present-mode -1)
        (setq pdf-view-display-size (alist-get 'display-size saved))
        ;; Forget the presentation windows, then put the original
        ;; windows back on their page and slice.  Doing them last also
        ;; makes them the template for windows opened later.
        (image-mode-winprops t t)
        (pcase-dolist (`(,window ,page ,slice) (alist-get 'windows saved))
          (when (and (window-live-p window)
                     (eq (window-buffer window) buffer))
            (setf (pdf-view-current-slice window) slice)
            (pdf-present--goto-page-safely page window)))
        (when (and (alist-get 'roll saved)
                   (fboundp 'pdf-view-roll-minor-mode))
          (pdf-view-roll-minor-mode 1))))))

(defun pdf-present--goto-page-safely (page window)
  "Show PAGE in WINDOW with `pdf-view-goto-page', ignoring display errors."
  (condition-case err
      (pdf-view-goto-page page window)
    (error (message "pdf-present: %s" (error-message-string err)))))



;; * ================================================================== *
;; * Hooks
;; * ================================================================== *

(defun pdf-present--tick ()
  "Update the clock and timer in the presenter view."
  (when pdf-present--session
    (pdf-present--update-info pdf-present--session)))

(defun pdf-present--on-delete-frame (frame)
  "End the presentation when one of its frames, FRAME, is deleted."
  (let ((session pdf-present--session))
    (when (and session
               (not (pdf-present--session-quitting session))
               (memq frame (list (pdf-present--session-audience-frame session)
                                 (pdf-present--session-presenter-frame
                                  session))))
      (pdf-present-quit))))

(defun pdf-present--on-resize (_frame)
  "Fit the slides again after a presentation window changed size."
  (let ((session pdf-present--session))
    (when (and session
               (not (pdf-present--session-quitting session))
               (buffer-live-p (pdf-present--session-buffer session)))
      (with-current-buffer (pdf-present--session-buffer session)
        (pdf-view-maybe-redisplay-resized-windows))
      ;; The window manager (a tiling compositor, say) may resize
      ;; the presentation windows at any time; whatever was
      ;; prefetched for the old sizes is stale now.
      (pdf-present--schedule-prefetch session))))



;; * ================================================================== *
;; * Commands
;; * ================================================================== *

(defvar pdf-present-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (key '("n" "SPC" "RET" "<next>" "<right>" "<down>"))
      (define-key map (kbd key) #'pdf-present-next-slide))
    (dolist (key '("p" "DEL" "S-SPC" "<prior>" "<left>" "<up>"))
      (define-key map (kbd key) #'pdf-present-previous-slide))
    (dolist (key '("<" "<home>" "M-<"))
      (define-key map (kbd key) #'pdf-present-first-slide))
    (dolist (key '(">" "<end>" "M->"))
      (define-key map (kbd key) #'pdf-present-last-slide))
    (dolist (key '("g" "M-g g" "M-g M-g"))
      (define-key map (kbd key) #'pdf-present-goto-slide))
    (dolist (key '("b" "."))
      (define-key map (kbd key) #'pdf-present-toggle-black))
    (dolist (key '("w" ","))
      (define-key map (kbd key) #'pdf-present-toggle-white))
    (define-key map (kbd "t") #'pdf-present-toggle-timer)
    (define-key map (kbd "r") #'pdf-present-reset-timer)
    (define-key map (kbd "d") #'pdf-present-set-duration)
    (define-key map (kbd "q") #'pdf-present-quit)
    map)
  "Keymap of the presentation's buffers.")

(define-minor-mode pdf-present-mode
  "Keys of a running `pdf-present' presentation.

This mode is turned on in the presented buffer and the helper
buffers by `pdf-present', and off again by `pdf-present-quit'.

\\{pdf-present-mode-map}"
  :lighter " Present"
  :keymap pdf-present-mode-map
  :group 'pdf-present)

(defun pdf-present--session-or-error ()
  "Return the running presentation, or signal an error."
  (or pdf-present--session
      (user-error "No presentation is running; start one with M-x pdf-present")))

(defun pdf-present-goto-slide (index)
  "Show slide INDEX in both frames.
INDEX counts slides, not PDF pages; it is kept in range.
Showing a slide ends a black or white screen."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number (format "Slide (1-%d): "
                                (pdf-present--count
                                 (pdf-present--session-or-error)))))))
  (let ((session (pdf-present--session-or-error)))
    (setf (pdf-present--session-index session)
          (max 1 (min index (pdf-present--count session))))
    (setf (pdf-present--session-blank session) nil)
    (pdf-present--refresh session)))

(defun pdf-present-next-slide (&optional n)
  "Show the Nth next slide (default 1)."
  (interactive "p")
  (pdf-present-goto-slide
   (+ (pdf-present--session-index (pdf-present--session-or-error))
      (or n 1))))

(defun pdf-present-previous-slide (&optional n)
  "Show the Nth previous slide (default 1)."
  (interactive "p")
  (pdf-present-next-slide (- (or n 1))))

(defun pdf-present-first-slide ()
  "Show the first slide."
  (interactive)
  (pdf-present-goto-slide 1))

(defun pdf-present-last-slide ()
  "Show the last slide."
  (interactive)
  (pdf-present-goto-slide
   (pdf-present--count (pdf-present--session-or-error))))

(defun pdf-present--toggle-blank (color)
  "Blank the audience screen in COLOR, or show the slide again."
  (let ((session (pdf-present--session-or-error)))
    (setf (pdf-present--session-blank session)
          (unless (eq (pdf-present--session-blank session) color) color))
    (pdf-present--refresh session)))

(defun pdf-present-toggle-black ()
  "Black out the audience screen, or show the slide again."
  (interactive)
  (pdf-present--toggle-blank 'black))

(defun pdf-present-toggle-white ()
  "White out the audience screen, or show the slide again."
  (interactive)
  (pdf-present--toggle-blank 'white))

(defun pdf-present-toggle-timer ()
  "Pause or resume the elapsed-time timer."
  (interactive)
  (let ((session (pdf-present--session-or-error)))
    (if (pdf-present--session-running-since session)
        (pdf-present--timer-pause session)
      (pdf-present--timer-start session))
    (pdf-present--update-info session)))

(defun pdf-present-reset-timer ()
  "Set the elapsed-time timer back to zero."
  (interactive)
  (let ((session (pdf-present--session-or-error)))
    (pdf-present--timer-reset session)
    (pdf-present--update-info session)))

(defun pdf-present-set-duration (minutes)
  "Set the planned length of the talk to MINUTES.
Zero or less clears it.  Without a prefix argument, asks; the
initial value is `pdf-present-talk-duration'."
  (interactive
   (let ((session (pdf-present--session-or-error)))
     (list (read-number "Talk duration in minutes (0 clears): "
                        (or (pdf-present--session-duration session)
                            pdf-present-talk-duration 0)))))
  (let ((session (pdf-present--session-or-error)))
    (setf (pdf-present--session-duration session)
          (and (> minutes 0) minutes))
    (pdf-present--update-info session)))

;;;###autoload
(defun pdf-present (&optional choose-monitor)
  "Present the PDF in the current buffer.

Opens an audience window with the current slide alone, covering
another monitor (see `pdf-present-audience-monitor'), and a
presenter window with the current and next slides, the speaker
notes, the slide counter, the elapsed time and the clock.  Both
are plain windows, made in their final size, rather than
fullscreen frames.  With prefix argument CHOOSE-MONITOR, ask which
monitor the audience sees.  The presentation starts at the slide
of the page shown in the selected window.

Where the notes are is set by `pdf-present-notes-layout'.

Keys, in both frames:
\\<pdf-present-mode-map>
\\[pdf-present-next-slide], \\[pdf-present-previous-slide]	next, previous slide (also PageDown/PageUp, arrows)
\\[pdf-present-first-slide], \\[pdf-present-last-slide]	first, last slide
\\[pdf-present-goto-slide]	go to a slide by number
\\[pdf-present-toggle-black], \\[pdf-present-toggle-white]	black or white audience screen (again to undo)
\\[pdf-present-toggle-timer]	pause or resume the timer
\\[pdf-present-reset-timer]	reset the timer
\\[pdf-present-set-duration]	set the talk's planned length (see `pdf-present-talk-duration')
\\[pdf-present-quit]	end the presentation

\\[pdf-present-quit] deletes both frames and puts the buffer and its
windows back as they were."
  (interactive "P")
  (pdf-util-assert-pdf-buffer)
  (when pdf-present--session
    (pdf-present-quit))
  (let* ((monitor (if choose-monitor
                      (pdf-present--read-monitor)
                    pdf-present-audience-monitor))
         (buffer (current-buffer))
         (page (or (pdf-view-current-page) 1))
         (session (pdf-present--make-session buffer)))
    (setf (pdf-present--session-index session)
          (pdf-present--index-of-page session page))
    (pdf-present--save-state session)
    (setq pdf-present--session session)
    (condition-case err
        (progn
          (pdf-present--prepare-buffer session)
          (pdf-present--setup-display session monitor)
          (add-hook 'delete-frame-functions #'pdf-present--on-delete-frame)
          (add-hook 'window-size-change-functions #'pdf-present--on-resize)
          (pdf-present--timer-start session)
          (setf (pdf-present--session-ticker session)
                (run-at-time 1 1 #'pdf-present--tick))
          (pdf-present--refresh session)
          (with-current-buffer buffer
            (run-hooks 'pdf-present-start-hook))
          (message "Presenting %d slides (notes: %s); q quits"
                   (pdf-present--count session)
                   (pdf-present--session-layout session)))
      (error
       (pdf-present-quit)
       (signal (car err) (cdr err))))))

(defun pdf-present-quit ()
  "End the presentation and restore the buffer, windows and frames."
  (interactive)
  (let ((session pdf-present--session))
    (when (and session (not (pdf-present--session-quitting session)))
      (setf (pdf-present--session-quitting session) t)
      (unwind-protect
          (progn
            (when (timerp (pdf-present--session-ticker session))
              (cancel-timer (pdf-present--session-ticker session)))
            (when (timerp (pdf-present--session-prefetch session))
              (cancel-timer (pdf-present--session-prefetch session)))
            (remove-hook 'delete-frame-functions #'pdf-present--on-delete-frame)
            (remove-hook 'window-size-change-functions #'pdf-present--on-resize)
            (pdf-present--teardown-display session)
            (let ((frame (pdf-present--session-origin-frame session))
                  (window (pdf-present--session-origin-window session)))
              (when (frame-live-p frame)
                (select-frame-set-input-focus frame))
              (when (window-live-p window)
                (select-window window)))
            (pdf-present--restore-buffer session))
        (setq pdf-present--session nil))
      (let ((buffer (pdf-present--session-buffer session)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (run-hooks 'pdf-present-quit-hook)))))))

(provide 'pdf-present)

;;; pdf-present.el ends here
