;;; pdf-thumbs.el --- A grid of page thumbnails for PDF buffers. -*- lexical-binding: t -*-

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
;; `pdf-thumbs' shows small images of a PDF buffer's pages in a grid,
;; in a buffer of its own, each labelled with its page label.  The
;; grid reflows when its window changes width.  Moving around the
;; grid and pressing RET or clicking a thumbnail shows that page in
;; the PDF window; the PDF's current page is highlighted.
;;
;; Pages can be marked, as files in Dired, and then deleted,
;; extracted, moved or rotated with the commands of pdf-pages.el.
;; Without marks, these work on the page at point.
;;
;; Thumbnails are rendered by the epdfinfo server, one request at a
;; time and asynchronously, so a long document opens at once and
;; Emacs stays responsive.  The thumbnails in view come first, the
;; others follow while Emacs is idle, see `pdf-thumbs-prefetch-delay'.
;; They use the rendering options of the document, so they follow
;; `pdf-view-midnight-minor-mode' and the like.
;;
;; On a text terminal, and in batch mode, the grid shows the labels
;; without images.

(require 'pdf-view)
(require 'pdf-info)
(require 'pdf-cache)
(require 'pdf-util)
(require 'pdf-pages)
(require 'cl-lib)
(require 'subr-x)

;;; Code:



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-thumbs nil
  "A grid of page thumbnails."
  :group 'pdf-tools)

(defcustom pdf-thumbs-size 120
  "The default width of thumbnails, in pixels.

\\<pdf-thumbs-mode-map>\\[pdf-thumbs-enlarge] and \
\\[pdf-thumbs-shrink] change it in a grid buffer."
  :type 'integer)

(defcustom pdf-thumbs-size-factor 1.25
  "The factor by which `pdf-thumbs-enlarge' changes the thumbnail width."
  :type 'number)

(defcustom pdf-thumbs-columns nil
  "The number of thumbnails in a row.

If nil, fit as many as the window is wide."
  :type '(choice (const :tag "Fit to the window" nil)
                 (integer :tag "Fixed number")))

(defcustom pdf-thumbs-prefetch-delay 0.3
  "Idle time in seconds before thumbnails not in view are rendered.

Thumbnails in view are always rendered right away."
  :type 'number)

(defcustom pdf-thumbs-display-buffer-action
  '(nil (inhibit-same-window . t))
  "The action used to display the grid buffer, see `display-buffer'."
  :type display-buffer--action-custom-type)

(defcustom pdf-thumbs-minor-mode-map-prefix (kbd "T")
  "The key of `pdf-thumbs' in `pdf-thumbs-minor-mode-map'.

Setting this after the package was loaded has no effect."
  :type 'key-sequence)

(defface pdf-thumbs-current
  '((((background dark)) :background "#4f86c6" :weight bold)
    (t :background "#3f7fd0" :weight bold))
  "Face of the PDF window's current page.

Its background frames the thumbnail.")

(defface pdf-thumbs-marked
  '((((background dark)) :background "#d08a1a" :foreground "black")
    (t :background "#f0a030" :foreground "black"))
  "Face of marked pages.

Its background frames the thumbnail.")

(defface pdf-thumbs-frame
  '((((background dark)) :background "gray30")
    (t :background "gray86"))
  "Face of the frame around thumbnails.

Thumbnails not rendered yet are filled with its background.")

(defface pdf-thumbs-label
  '((t :inherit shadow))
  "Face of page labels.")

(defface pdf-thumbs-current-label
  '((((background dark)) :foreground "#78a8e8" :weight bold)
    (t :foreground "#1f5fb0" :weight bold))
  "Face of the label of the PDF window's current page.")

(defface pdf-thumbs-marked-label
  '((((background dark)) :foreground "#f0a030" :weight bold)
    (t :foreground "#c06800" :weight bold))
  "Face of the labels of marked pages.")



;; * ================================================================== *
;; * Internal variables
;; * ================================================================== *

(defconst pdf-thumbs--margin 4
  "Width of the frame around a thumbnail, in pixels.")

(defconst pdf-thumbs--gap 12
  "Horizontal space between thumbnails, in pixels.")

(defvar-local pdf-thumbs-pdf-buffer nil
  "The PDF buffer of this grid buffer.")

(defvar-local pdf-thumbs-pdf-window nil
  "The window last used to show pages of `pdf-thumbs-pdf-buffer'.")

(defvar-local pdf-thumbs--buffer nil
  "In a PDF buffer, its grid buffer.")
;; Keep the grid connected if the PDF buffer's mode is run again.
(put 'pdf-thumbs--buffer 'permanent-local t)

(defvar-local pdf-thumbs--size nil
  "The thumbnail width of this grid.")

(defvar-local pdf-thumbs--npages 0
  "The number of pages of the PDF.")

(defvar-local pdf-thumbs--labels nil
  "Vector of the page labels of the PDF.")

(defvar-local pdf-thumbs--aspect 1.414
  "Height divided by width of the first page, for placeholders.")

(defvar-local pdf-thumbs--current nil
  "The page highlighted as current.")

(defvar-local pdf-thumbs--marks nil
  "Hash table whose keys are the marked pages.")

(defvar-local pdf-thumbs--images nil
  "Hash table of rendered pages, mapping a page to (WIDTH . DATA).

WIDTH is the thumbnail width the image was rendered for, DATA
the PNG data.")

(defvar-local pdf-thumbs--columns 1
  "The number of thumbnails in a row of the current layout.")

(defvar-local pdf-thumbs--graphic nil
  "Non-nil if the current layout shows images.")

(defvar-local pdf-thumbs--cells nil
  "Vector of the buffer positions of the pages' thumbnails.

Element 0 is unused.")

(defvar-local pdf-thumbs--label-cells nil
  "Vector of the buffer positions of the pages' labels, or nil.

Element 0 is unused.  There are no separate labels without images.")

(defvar-local pdf-thumbs--queue nil
  "Pages waiting to be rendered, in order.")

(defvar-local pdf-thumbs--in-flight nil
  "The page being rendered by the server, or nil.")

(defvar-local pdf-thumbs--generation 0
  "Incremented whenever pending renderings become obsolete.")

(defvar-local pdf-thumbs--timer nil
  "The idle timer that renders thumbnails not in view.")



;; * ================================================================== *
;; * Creating the grid
;; * ================================================================== *

(defun pdf-thumbs-buffer-name (&optional pdf-buffer)
  "Return the name of the grid buffer of PDF-BUFFER."
  (format "*Thumbs %s*" (buffer-name (or pdf-buffer (current-buffer)))))

;;;###autoload
(defun pdf-thumbs (&optional buffer)
  "Show a grid of thumbnails of the pages of the PDF BUFFER.

BUFFER defaults to the current buffer.  Select the grid and move
to the current page.  See `pdf-thumbs-mode' for the commands."
  (interactive)
  (let* ((pdf-buffer (or buffer (current-buffer)))
         (pdf-window (and (eq pdf-buffer (window-buffer))
                          (selected-window)))
         (grid (pdf-thumbs-noselect pdf-buffer))
         (win (display-buffer grid pdf-thumbs-display-buffer-action)))
    (with-current-buffer grid
      (when (window-live-p pdf-window)
        (setq pdf-thumbs-pdf-window pdf-window))
      (pdf-thumbs--layout-if-needed win)
      (pdf-thumbs--set-current (pdf-thumbs--pdf-current-page))
      (set-window-point win (pdf-thumbs--cell pdf-thumbs--current))
      (pdf-thumbs--schedule))
    (select-window win)
    grid))

(defun pdf-thumbs-noselect (&optional buffer)
  "Return the grid buffer of the PDF BUFFER, creating it if needed."
  (with-current-buffer (or buffer (current-buffer))
    (pdf-util-assert-pdf-buffer)
    (if (buffer-live-p pdf-thumbs--buffer)
        pdf-thumbs--buffer
      (let ((pdf-buffer (current-buffer))
            (grid (get-buffer-create (pdf-thumbs-buffer-name))))
        (setq pdf-thumbs--buffer grid)
        (add-hook 'pdf-view-after-change-page-hook
                  #'pdf-thumbs--pdf-page-changed nil t)
        (add-hook 'after-revert-hook #'pdf-thumbs--pdf-reverted nil t)
        (add-hook 'kill-buffer-hook #'pdf-thumbs--pdf-killed nil t)
        (dolist (hook '(pdf-view-midnight-minor-mode-hook
                        pdf-view-themed-minor-mode-hook
                        pdf-view-printer-minor-mode-hook))
          (add-hook hook #'pdf-thumbs--pdf-colors-changed nil t))
        (with-current-buffer grid
          (pdf-thumbs-mode)
          (setq pdf-thumbs-pdf-buffer pdf-buffer
                pdf-thumbs--size pdf-thumbs-size
                pdf-thumbs--marks (make-hash-table)
                pdf-thumbs--images (make-hash-table))
          (setq-local other-window-scroll-buffer pdf-buffer)
          (pdf-thumbs--read-document)
          (pdf-thumbs--layout (pdf-thumbs--columns-for nil)))
        grid))))

(defun pdf-thumbs--read-document ()
  "Read the page count and labels of the PDF."
  (let* ((pdf pdf-thumbs-pdf-buffer)
         (npages (pdf-info-number-of-pages pdf))
         (labels (ignore-errors (pdf-info-pagelabels pdf)))
         (size (pdf-info-pagesize 1 pdf)))
    (setq pdf-thumbs--npages npages
          pdf-thumbs--labels
          (apply #'vector nil
                 (cl-loop for page from 1 to npages
                          for label = (pop labels)
                          collect (if (or (null label)
                                          (string-empty-p label)
                                          (equal label (number-to-string page)))
                                      (number-to-string page)
                                    (format "%s (%d)" label page))))
          pdf-thumbs--aspect (if (and (consp size) (> (car size) 0))
                                 (/ (float (cdr size)) (car size))
                               1.414))))

(defun pdf-thumbs--pdf-current-page ()
  "Return the current page of the PDF window, or 1."
  (let ((win (pdf-thumbs--pdf-window t)))
    (max 1 (min (or (and win
                         (with-current-buffer pdf-thumbs-pdf-buffer
                           (pdf-view-current-page win)))
                    1)
                (max 1 pdf-thumbs--npages)))))



;; * ================================================================== *
;; * Layout
;; * ================================================================== *

(defun pdf-thumbs--graphic-p (&optional window)
  "Return non-nil if WINDOW's frame can show the thumbnails."
  (and (display-images-p (and window (window-frame window)))
       (image-type-available-p (pdf-view-image-type))))

(defun pdf-thumbs--cell-width ()
  "Return the width of a cell: pixels with images, else columns."
  (if pdf-thumbs--graphic
      (+ pdf-thumbs--size (* 2 pdf-thumbs--margin))
    (+ 4 (cl-loop for i from 1 to pdf-thumbs--npages
                  maximize (string-width (aref pdf-thumbs--labels i))))))

(defun pdf-thumbs--columns-for (window)
  "Return the number of columns that fit into WINDOW.

WINDOW nil means the grid's window, or the selected one."
  (setq window (or window
                   (get-buffer-window (current-buffer) t)
                   (selected-window)))
  (setq pdf-thumbs--graphic (pdf-thumbs--graphic-p window))
  (or pdf-thumbs-columns
      (max 1 (if pdf-thumbs--graphic
                 (/ (- (window-body-width window t) pdf-thumbs--gap 2)
                    (+ (pdf-thumbs--cell-width) pdf-thumbs--gap))
               (/ (1- (window-body-width window))
                  (1+ (pdf-thumbs--cell-width)))))))

(defun pdf-thumbs--layout-if-needed (&optional window)
  "Lay out the grid again if WINDOW fits a different number of columns."
  (let ((columns (pdf-thumbs--columns-for window)))
    (unless (and (= columns pdf-thumbs--columns)
                 (eq pdf-thumbs--graphic
                     (and pdf-thumbs--label-cells t)))
      (pdf-thumbs--layout columns))))

(defun pdf-thumbs--layout (columns)
  "Fill the buffer with a grid of COLUMNS thumbnails per row.

Keep point at the page it was on."
  (let ((page (pdf-thumbs-page-at-point))
        (inhibit-read-only t)
        (graphic pdf-thumbs--graphic)
        (gap pdf-thumbs--gap)
        (cells (make-vector (1+ pdf-thumbs--npages) nil))
        (label-cells (and pdf-thumbs--graphic
                          (make-vector (1+ pdf-thumbs--npages) nil))))
    (setq pdf-thumbs--columns columns
          pdf-thumbs--cells cells
          pdf-thumbs--label-cells label-cells)
    (erase-buffer)
    (cl-loop
     for first from 1 to pdf-thumbs--npages by columns
     for last = (min pdf-thumbs--npages (+ first columns -1))
     do
     (when graphic
       (insert (propertize " " 'display `(space :width (,gap)))))
     (cl-loop for page from first to last do
              (aset cells page (point))
              (insert (propertize " " 'pdf-thumbs-page page
                                  'mouse-face 'highlight
                                  'help-echo #'pdf-thumbs--help-echo))
              (insert (if graphic
                          (propertize " " 'display `(space :width (,gap)))
                        " ")))
     (insert "\n")
     (when graphic
       (cl-loop for page from first to last do
                (aset label-cells page (point))
                (insert (propertize "  " 'pdf-thumbs-page page)))
       (insert (propertize "\n" 'line-spacing 0.8))))
    (dotimes (i pdf-thumbs--npages)
      (pdf-thumbs--update-cell (1+ i)))
    (goto-char (pdf-thumbs--cell page))))

(defun pdf-thumbs--help-echo (_window _object pos)
  "Return the tooltip of the thumbnail at POS."
  (let ((page (get-text-property pos 'pdf-thumbs-page)))
    (and page (format "Page %s, mouse-1: show it" page))))

(defun pdf-thumbs--cell (page)
  "Return the buffer position of PAGE's thumbnail."
  (or (and pdf-thumbs--cells
           (<= 1 page pdf-thumbs--npages)
           (aref pdf-thumbs--cells page))
      (point-min)))

(defun pdf-thumbs--image (page)
  "Return the image of PAGE, or nil if it is not rendered."
  (let ((entry (gethash page pdf-thumbs--images)))
    (when (cdr entry)
      (pdf-view-create-image (cdr entry)
        :width pdf-thumbs--size
        :margin pdf-thumbs--margin
        :ascent 100))))

(defun pdf-thumbs--label (page)
  "Return PAGE's label for display."
  (let ((label (aref pdf-thumbs--labels page)))
    (if (gethash page pdf-thumbs--marks)
        (concat "* " label)
      label)))

(defun pdf-thumbs--update-cell (page)
  "Redraw PAGE's thumbnail and label."
  (let* ((inhibit-read-only t)
         (pos (aref pdf-thumbs--cells page))
         (marked (gethash page pdf-thumbs--marks))
         (current (eql page pdf-thumbs--current))
         (faces (delq nil (list (and marked 'pdf-thumbs-marked)
                                (and current 'pdf-thumbs-current))))
         (label (pdf-thumbs--label page)))
    (if (not pdf-thumbs--graphic)
        (let ((width (pdf-thumbs--cell-width)))
          (put-text-property
           pos (1+ pos) 'display
           (propertize
            (concat "[" (truncate-string-to-width
                         (let ((pad (/ (- width 2 (string-width label)) 2)))
                           (concat (make-string (max 0 pad) ?\s) label))
                         (- width 2) nil ?\s)
                    "]")
            'face faces)))
      (let ((image (pdf-thumbs--image page))
            (width (+ pdf-thumbs--size (* 2 pdf-thumbs--margin))))
        (add-text-properties
         pos (1+ pos)
         (list 'display
               (or image
                   `(space :width (,width)
                           :height (,(+ (round (* pdf-thumbs--size
                                                  pdf-thumbs--aspect))
                                        (* 2 pdf-thumbs--margin)))))
               'face (or faces 'pdf-thumbs-frame)))
        ;; The label, centered below the thumbnail.
        (let* ((lpos (aref pdf-thumbs--label-cells page))
               (column (mod (1- page) pdf-thumbs--columns))
               (left (+ pdf-thumbs--gap
                        (* column (+ width pdf-thumbs--gap))))
               (label (truncate-string-to-width
                       label (max 1 (/ width (max 1 (frame-char-width)))))))
          (put-text-property
           lpos (1+ lpos) 'display
           `(space :align-to
                   (,(max 0 (+ left (/ (- width (pdf-thumbs--string-pixel-width
                                                  label))
                                       2))))))
          (put-text-property
           (1+ lpos) (+ 2 lpos) 'display
           (propertize label 'face
                       (cond (current 'pdf-thumbs-current-label)
                             (marked 'pdf-thumbs-marked-label)
                             (t 'pdf-thumbs-label)))))))))

(defun pdf-thumbs--string-pixel-width (string)
  "Return the width of STRING in pixels, roughly."
  (if (fboundp 'string-pixel-width)
      (string-pixel-width string)
    (* (string-width string) (frame-char-width))))



;; * ================================================================== *
;; * Rendering
;; * ================================================================== *

(defun pdf-thumbs--rendered-p (page)
  "Return non-nil if PAGE is rendered at the current size."
  (eql (car (gethash page pdf-thumbs--images)) pdf-thumbs--size))

(defun pdf-thumbs--visible-pages ()
  "Return the pages in view in any window of the grid, roughly."
  (let (pages)
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (let* ((start (or (get-text-property (window-start win) 'pdf-thumbs-page)
                        (save-excursion
                          (goto-char (window-start win))
                          (pdf-thumbs-page-at-point))))
             (first (1+ (* (/ (1- start) pdf-thumbs--columns)
                           pdf-thumbs--columns)))
             (row-height (if pdf-thumbs--graphic
                             (+ (round (* pdf-thumbs--size pdf-thumbs--aspect))
                                (* 2 pdf-thumbs--margin)
                                (* 2 (frame-char-height (window-frame win))))
                           1))
             (rows (1+ (/ (window-body-height win pdf-thumbs--graphic)
                          (max 1 row-height)))))
        (cl-loop for page from first
                 to (min pdf-thumbs--npages
                         (+ first (* rows pdf-thumbs--columns) -1))
                 do (push page pages))))
    (sort (delete-dups pages) #'<)))

(defun pdf-thumbs--schedule (&optional all)
  "Render the thumbnails in view that are missing.

With ALL non-nil, queue the others as well, nearest first."
  (when (and pdf-thumbs--graphic
             (buffer-live-p pdf-thumbs-pdf-buffer))
    (let* ((visible (cl-remove-if #'pdf-thumbs--rendered-p
                                  (pdf-thumbs--visible-pages)))
           (rest (when all
                   (let ((center (or (car visible)
                                     (pdf-thumbs-page-at-point))))
                     (sort (cl-loop for page from 1 to pdf-thumbs--npages
                                    unless (or (memq page visible)
                                               (pdf-thumbs--rendered-p page))
                                    collect page)
                           (lambda (a b)
                             (< (abs (- a center)) (abs (- b center)))))))))
      (setq pdf-thumbs--queue (nconc visible rest))
      (pdf-thumbs--render-next))))

(defun pdf-thumbs--render-next ()
  "Ask the server for the next thumbnail in the queue."
  (unless pdf-thumbs--in-flight
    (while (and pdf-thumbs--queue
                (pdf-thumbs--rendered-p (car pdf-thumbs--queue)))
      (pop pdf-thumbs--queue))
    (let ((page (pop pdf-thumbs--queue)))
      (when (and page (buffer-live-p pdf-thumbs-pdf-buffer))
        (let* ((grid (current-buffer))
               (generation pdf-thumbs--generation)
               (size pdf-thumbs--size)
               (pdf-info-asynchronous
                (lambda (status data)
                  (when (buffer-live-p grid)
                    (with-current-buffer grid
                      (when (= generation pdf-thumbs--generation)
                        (setq pdf-thumbs--in-flight nil)
                        (if status
                            ;; Do not retry a page that fails.
                            (puthash page (cons size nil) pdf-thumbs--images)
                          (puthash page (cons size data) pdf-thumbs--images)
                          (pdf-thumbs--update-cell page))
                        ;; Pages out of view wait while the user works,
                        ;; and `pdf-thumbs--timer' picks them up again.
                        (cond
                         ((and pdf-thumbs--queue
                               (or (current-idle-time)
                                   (memq (car pdf-thumbs--queue)
                                         (pdf-thumbs--visible-pages))))
                          (pdf-thumbs--render-next))
                         ((and (null pdf-thumbs--queue) (current-idle-time))
                          (pdf-thumbs--schedule t)))))))))
          (setq pdf-thumbs--in-flight page)
          (condition-case err
              (pdf-info-renderpage page size pdf-thumbs-pdf-buffer)
            (error
             (setq pdf-thumbs--in-flight nil)
             (message "Cannot render thumbnail of page %d: %s"
                      page (error-message-string err)))))))))

(defun pdf-thumbs--idle-render (grid)
  "Render the remaining thumbnails of GRID, while Emacs is idle."
  (when (buffer-live-p grid)
    (with-current-buffer grid
      (unless pdf-thumbs--in-flight
        (pdf-thumbs--schedule t)))))

(defun pdf-thumbs--invalidate (&optional keep-images)
  "Forget pending renderings, and the images unless KEEP-IMAGES."
  (cl-incf pdf-thumbs--generation)
  (setq pdf-thumbs--queue nil
        pdf-thumbs--in-flight nil)
  (unless keep-images
    (clrhash pdf-thumbs--images)))

(defun pdf-thumbs--window-scrolled (win _start)
  "Render the thumbnails that scrolled into view in WIN."
  (with-current-buffer (window-buffer win)
    (when (derived-mode-p 'pdf-thumbs-mode)
      ;; The window start is not final yet, render after redisplay.
      (run-at-time 0 nil #'pdf-thumbs--schedule-in (current-buffer)))))

(defun pdf-thumbs--schedule-in (grid)
  "Render the thumbnails in view of GRID."
  (when (buffer-live-p grid)
    (with-current-buffer grid
      (pdf-thumbs--schedule))))

(defun pdf-thumbs--window-size-changed (frame)
  "Reflow the grids shown on FRAME."
  (dolist (win (window-list frame 'no-minibuf))
    (with-current-buffer (window-buffer win)
      (when (and (derived-mode-p 'pdf-thumbs-mode)
                 (buffer-live-p pdf-thumbs-pdf-buffer))
        (let ((point-page (pdf-thumbs-page-at-point)))
          (pdf-thumbs--layout-if-needed win)
          (set-window-point win (pdf-thumbs--cell point-page))
          (pdf-thumbs--schedule))))))



;; * ================================================================== *
;; * Following the PDF buffer
;; * ================================================================== *

(defun pdf-thumbs--pdf-window (&optional if-visible-p)
  "Return the window showing the PDF buffer.

Display it if there is none, unless IF-VISIBLE-P is non-nil."
  (let ((buffer pdf-thumbs-pdf-buffer))
    (unless (buffer-live-p buffer)
      (user-error "The PDF buffer was killed"))
    (setq pdf-thumbs-pdf-window
          (if (and (window-live-p pdf-thumbs-pdf-window)
                   (eq (window-buffer pdf-thumbs-pdf-window) buffer))
              pdf-thumbs-pdf-window
            (or (get-buffer-window buffer)
                (get-buffer-window buffer t)
                (and (null if-visible-p)
                     (display-buffer buffer
                                     '(nil (inhibit-same-window . t)))))))))

(defun pdf-thumbs--set-current (page)
  "Highlight PAGE as the current one."
  (let ((old pdf-thumbs--current))
    (setq pdf-thumbs--current page)
    (when (and old (<= 1 old pdf-thumbs--npages))
      (pdf-thumbs--update-cell old))
    (when (and page (<= 1 page pdf-thumbs--npages))
      (pdf-thumbs--update-cell page))))

(defun pdf-thumbs--pdf-page-changed ()
  "Follow a page change in the PDF buffer's window."
  (let ((grid pdf-thumbs--buffer)
        (win (selected-window))
        (page (pdf-view-current-page)))
    (when (and (buffer-live-p grid)
               (eq (window-buffer win) (current-buffer))
               page)
      (with-current-buffer grid
        (setq pdf-thumbs-pdf-window win)
        (unless (eql page pdf-thumbs--current)
          (pdf-thumbs--set-current page)
          (dolist (gwin (get-buffer-window-list grid nil t))
            (unless (eq gwin (selected-window))
              (set-window-point gwin (pdf-thumbs--cell page))))
          (pdf-thumbs--schedule))))))

(defun pdf-thumbs--pdf-reverted ()
  "Show the reverted PDF in the grid."
  (when (buffer-live-p pdf-thumbs--buffer)
    (with-current-buffer pdf-thumbs--buffer
      (pdf-thumbs-refresh))))

(defun pdf-thumbs--pdf-colors-changed ()
  "Render the grid again with the PDF's new colors."
  (when (buffer-live-p pdf-thumbs--buffer)
    (with-current-buffer pdf-thumbs--buffer
      (pdf-thumbs--invalidate t)
      ;; Keep showing the old images until the new ones arrive.
      (maphash (lambda (page entry)
                 (puthash page (cons nil (cdr entry)) pdf-thumbs--images))
               pdf-thumbs--images)
      (pdf-thumbs--schedule))))

(defun pdf-thumbs--pdf-killed ()
  "Close the grid of the PDF buffer that is being killed."
  (let ((grid pdf-thumbs--buffer))
    (when (buffer-live-p grid)
      (dolist (win (get-buffer-window-list grid nil t))
        (when (window-parameter win 'quit-restore)
          (ignore-errors (quit-window nil win))))
      (kill-buffer grid))))

(dolist (fn '(pdf-thumbs--pdf-page-changed
              pdf-thumbs--pdf-reverted
              pdf-thumbs--pdf-colors-changed
              pdf-thumbs--pdf-killed))
  (put fn 'permanent-local-hook t))

(defun pdf-thumbs--kill-hook ()
  "Detach the grid that is being killed from its PDF buffer."
  (pdf-thumbs--invalidate)
  (when (timerp pdf-thumbs--timer)
    (cancel-timer pdf-thumbs--timer))
  (let ((grid (current-buffer)))
    (when (buffer-live-p pdf-thumbs-pdf-buffer)
      (with-current-buffer pdf-thumbs-pdf-buffer
        (when (eq pdf-thumbs--buffer grid)
          (setq pdf-thumbs--buffer nil)
          (remove-hook 'pdf-view-after-change-page-hook
                       #'pdf-thumbs--pdf-page-changed t)
          (remove-hook 'after-revert-hook #'pdf-thumbs--pdf-reverted t)
          (remove-hook 'kill-buffer-hook #'pdf-thumbs--pdf-killed t)
          (dolist (hook '(pdf-view-midnight-minor-mode-hook
                          pdf-view-themed-minor-mode-hook
                          pdf-view-printer-minor-mode-hook))
            (remove-hook hook #'pdf-thumbs--pdf-colors-changed t)))))))



;; * ================================================================== *
;; * Commands
;; * ================================================================== *

(defun pdf-thumbs-page-at-point ()
  "Return the page of the thumbnail at or before point, or 1."
  (or (get-text-property (point) 'pdf-thumbs-page)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'pdf-thumbs-page))
      (let ((pos (previous-single-property-change (point) 'pdf-thumbs-page)))
        (and pos (> pos (point-min))
             (get-text-property (1- pos) 'pdf-thumbs-page)))
      1))

(defun pdf-thumbs-goto-thumbnail (page)
  "Move point to the thumbnail of PAGE."
  (interactive
   (list (read-number "Go to thumbnail of page: "
                      (pdf-thumbs-page-at-point))))
  (goto-char (pdf-thumbs--cell (max 1 (min page pdf-thumbs--npages)))))

(defun pdf-thumbs--move (delta)
  "Move point DELTA thumbnails forward, within the document."
  (let ((page (pdf-thumbs-page-at-point)))
    (when (and (> delta 0) (= page pdf-thumbs--npages))
      (user-error "Last page"))
    (when (and (< delta 0) (= page 1))
      (user-error "First page"))
    (pdf-thumbs-goto-thumbnail (+ page delta))
    (pdf-thumbs--schedule)))

(defun pdf-thumbs-next (&optional n)
  "Move to the Nth next thumbnail."
  (interactive "p")
  (pdf-thumbs--move (or n 1)))

(defun pdf-thumbs-previous (&optional n)
  "Move to the Nth previous thumbnail."
  (interactive "p")
  (pdf-thumbs--move (- (or n 1))))

(defun pdf-thumbs-next-row (&optional n)
  "Move N rows down."
  (interactive "p")
  (let ((page (pdf-thumbs-page-at-point)))
    (if (and (> page (- pdf-thumbs--npages pdf-thumbs--columns))
             (= page pdf-thumbs--npages))
        (user-error "Last page")
      (pdf-thumbs--move (min (* (or n 1) pdf-thumbs--columns)
                             (- pdf-thumbs--npages page))))))

(defun pdf-thumbs-previous-row (&optional n)
  "Move N rows up."
  (interactive "p")
  (let ((page (pdf-thumbs-page-at-point)))
    (if (= page 1)
        (user-error "First page")
      (pdf-thumbs--move (- (min (* (or n 1) pdf-thumbs--columns)
                                (1- page)))))))

(defun pdf-thumbs-first ()
  "Move to the first thumbnail."
  (interactive)
  (pdf-thumbs-goto-thumbnail 1)
  (pdf-thumbs--schedule))

(defun pdf-thumbs-last ()
  "Move to the last thumbnail."
  (interactive)
  (pdf-thumbs-goto-thumbnail pdf-thumbs--npages)
  (pdf-thumbs--schedule))

(defun pdf-thumbs-current ()
  "Move to the thumbnail of the PDF window's current page."
  (interactive)
  (pdf-thumbs-goto-thumbnail (pdf-thumbs--pdf-current-page)))

(defun pdf-thumbs-display-page (&optional page)
  "Show PAGE, by default the one at point, in the PDF window.

Keep the grid selected."
  (interactive)
  (let ((page (or page (pdf-thumbs-page-at-point))))
    (with-selected-window (pdf-thumbs--pdf-window)
      (pdf-view-goto-page page))
    (pdf-thumbs--set-current page)))

(defun pdf-thumbs-show-page (&optional page)
  "Show PAGE, by default the one at point, and select the PDF window.

The grid stays in its window."
  (interactive)
  (let ((page (or page (pdf-thumbs-page-at-point))))
    (pdf-thumbs-display-page page)
    (select-window (pdf-thumbs--pdf-window))))

(defun pdf-thumbs-mouse-display-page (event)
  "Show the page whose thumbnail was clicked, see EVENT."
  (interactive "@e")
  (let* ((pos (posn-point (event-start event)))
         (page (and pos (get-text-property pos 'pdf-thumbs-page))))
    (when page
      (pdf-thumbs-goto-thumbnail page)
      (pdf-thumbs-display-page page))))

(defun pdf-thumbs-select-pdf-window ()
  "Select the PDF window."
  (interactive)
  (select-window (pdf-thumbs--pdf-window)))

(defun pdf-thumbs-refresh ()
  "Read the PDF again and render all thumbnails anew.

Marks are dropped, since page numbers may have changed."
  (interactive)
  (pdf-thumbs--invalidate)
  (clrhash pdf-thumbs--marks)
  (if (not (buffer-live-p pdf-thumbs-pdf-buffer))
      (kill-buffer)
    (pdf-thumbs--read-document)
    (setq pdf-thumbs--current nil)
    (let* ((win (get-buffer-window (current-buffer) t))
           (current (pdf-thumbs--pdf-current-page)))
      (pdf-thumbs--layout (pdf-thumbs--columns-for win))
      (pdf-thumbs--set-current current)
      (goto-char (pdf-thumbs--cell current))
      (when win (set-window-point win (point))))
    (pdf-thumbs--schedule)))

(defun pdf-thumbs--resize (size)
  "Change the thumbnail width to SIZE pixels."
  (setq size (max 32 (min 1200 (round size))))
  (unless (= size pdf-thumbs--size)
    (pdf-thumbs--invalidate t)
    (setq pdf-thumbs--size size)
    (pdf-thumbs--layout (pdf-thumbs--columns-for nil))
    (let ((win (get-buffer-window (current-buffer) t)))
      (when win (set-window-point win (point))))
    (pdf-thumbs--schedule))
  (message "Thumbnail width %d pixels" size))

(defun pdf-thumbs-enlarge (&optional n)
  "Make the thumbnails larger, N times by `pdf-thumbs-size-factor'."
  (interactive "p")
  (pdf-thumbs--resize (* pdf-thumbs--size
                         (expt pdf-thumbs-size-factor (or n 1)))))

(defun pdf-thumbs-shrink (&optional n)
  "Make the thumbnails smaller, N times by `pdf-thumbs-size-factor'."
  (interactive "p")
  (pdf-thumbs-enlarge (- (or n 1))))

(defun pdf-thumbs-reset-size ()
  "Make the thumbnails `pdf-thumbs-size' wide."
  (interactive)
  (pdf-thumbs--resize pdf-thumbs-size))

(defun pdf-thumbs-quit ()
  "Quit the grid window and kill the grid buffer."
  (interactive)
  (quit-window t))



;; * ================================================================== *
;; * Marks
;; * ================================================================== *

(defun pdf-thumbs-marked-pages ()
  "Return the marked pages in ascending order."
  (sort (hash-table-keys pdf-thumbs--marks) #'<))

(defun pdf-thumbs--set-mark (page mark)
  "Mark PAGE if MARK is non-nil, else unmark it."
  (if mark
      (puthash page t pdf-thumbs--marks)
    (remhash page pdf-thumbs--marks))
  (pdf-thumbs--update-cell page))

(defun pdf-thumbs--mark-and-move (mark n)
  "Set MARK on N pages from point on and move past them.

A negative N works backwards from the previous page, as
`dired-unmark-backward' does."
  (let ((n (or n 1)))
    (if (< n 0)
        (dotimes (_ (- n))
          (let ((page (pdf-thumbs-page-at-point)))
            (when (> page 1)
              (pdf-thumbs-goto-thumbnail (1- page))
              (pdf-thumbs--set-mark (1- page) mark))))
      (dotimes (_ n)
        (let ((page (pdf-thumbs-page-at-point)))
          (pdf-thumbs--set-mark page mark)
          (when (< page pdf-thumbs--npages)
            (pdf-thumbs-goto-thumbnail (1+ page)))))))
  (pdf-thumbs--schedule))

(defun pdf-thumbs-mark (&optional n)
  "Mark the page at point, or N pages, and move on."
  (interactive "p")
  (pdf-thumbs--mark-and-move t n))

(defun pdf-thumbs-unmark (&optional n)
  "Unmark the page at point, or N pages, and move on."
  (interactive "p")
  (pdf-thumbs--mark-and-move nil n))

(defun pdf-thumbs-unmark-backward (&optional n)
  "Move back and unmark the previous page, or N pages."
  (interactive "p")
  (pdf-thumbs--mark-and-move nil (- (or n 1))))

(defun pdf-thumbs-unmark-all ()
  "Unmark all pages."
  (interactive)
  (let ((pages (pdf-thumbs-marked-pages)))
    (clrhash pdf-thumbs--marks)
    (mapc #'pdf-thumbs--update-cell pages)
    (message "Unmarked %d page%s" (length pages)
             (if (= (length pages) 1) "" "s"))))

(defun pdf-thumbs-toggle-marks ()
  "Mark the unmarked pages and unmark the marked ones."
  (interactive)
  (dotimes (i pdf-thumbs--npages)
    (pdf-thumbs--set-mark (1+ i) (not (gethash (1+ i) pdf-thumbs--marks)))))



;; * ================================================================== *
;; * Page operations
;; * ================================================================== *

(defun pdf-thumbs-target-pages ()
  "Return the marked pages, or else the page at point."
  (or (pdf-thumbs-marked-pages)
      (list (pdf-thumbs-page-at-point))))

(defun pdf-thumbs--in-pdf (fn &rest args)
  "Call FN with ARGS in the PDF buffer and return its value.

Select the PDF window for the call, if there is one: the
pdf-pages commands show their result in the selected window.
They revert the PDF buffer afterwards, which refreshes the grid."
  (let ((win (pdf-thumbs--pdf-window t)))
    (if win
        (with-selected-window win
          (apply fn args))
      (with-current-buffer pdf-thumbs-pdf-buffer
        (apply fn args)))))

(defun pdf-thumbs-delete (pages)
  "Delete PAGES, the marked pages or the one at point.

See `pdf-pages-delete'."
  (interactive (list (pdf-thumbs-target-pages)))
  (pdf-thumbs--in-pdf #'pdf-pages-delete pages))

(defun pdf-thumbs-move (pages position)
  "Move PAGES, the marked pages or the one at point, to POSITION.

See `pdf-pages-move'."
  (interactive
   (let ((pages (pdf-thumbs-target-pages)))
     (list pages (pdf-thumbs--in-pdf #'pdf-pages-read-position pages))))
  (pdf-thumbs--in-pdf #'pdf-pages-move pages position))

(defun pdf-thumbs-rotate (pages angle)
  "Rotate PAGES, the marked pages or the one at point, by ANGLE.

Interactively, rotate clockwise by 90 degrees, and with a prefix
argument read the angle.  See `pdf-pages-rotate'."
  (interactive
   (list (pdf-thumbs-target-pages)
         (if current-prefix-arg (pdf-pages-read-angle) 90)))
  (pdf-thumbs--in-pdf #'pdf-pages-rotate pages angle))

(defun pdf-thumbs-rotate-counterclockwise (pages)
  "Rotate PAGES, the marked pages or the one at point, counterclockwise."
  (interactive (list (pdf-thumbs-target-pages)))
  (pdf-thumbs-rotate pages -90))

(defun pdf-thumbs-extract (pages file)
  "Write PAGES, the marked pages or the one at point, to FILE.

Display the new file.  See `pdf-pages-extract'."
  (interactive
   (let ((pages (pdf-thumbs-target-pages)))
     (list pages (pdf-thumbs--in-pdf #'pdf-pages-read-extract-file pages))))
  (let ((buffer (pdf-thumbs--in-pdf #'pdf-pages-extract pages file)))
    (when (called-interactively-p 'any)
      (display-buffer buffer '(nil (inhibit-same-window . t))))
    buffer))

(defun pdf-thumbs-undo ()
  "Undo the last change to the PDF file, see `pdf-undo'."
  (interactive)
  (pdf-thumbs--in-pdf #'pdf-undo))



;; * ================================================================== *
;; * Modes
;; * ================================================================== *

(defvar pdf-thumbs-mode-map
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap special-mode-map)
    (define-key kmap [right] #'pdf-thumbs-next)
    (define-key kmap (kbd "f") #'pdf-thumbs-next)
    (define-key kmap (kbd "C-f") #'pdf-thumbs-next)
    (define-key kmap [left] #'pdf-thumbs-previous)
    (define-key kmap (kbd "b") #'pdf-thumbs-previous)
    (define-key kmap (kbd "C-b") #'pdf-thumbs-previous)
    (define-key kmap [down] #'pdf-thumbs-next-row)
    (define-key kmap (kbd "n") #'pdf-thumbs-next-row)
    (define-key kmap (kbd "C-n") #'pdf-thumbs-next-row)
    (define-key kmap [up] #'pdf-thumbs-previous-row)
    (define-key kmap (kbd "p") #'pdf-thumbs-previous-row)
    (define-key kmap (kbd "C-p") #'pdf-thumbs-previous-row)
    (define-key kmap (kbd "<") #'pdf-thumbs-first)
    (define-key kmap (kbd ">") #'pdf-thumbs-last)
    (define-key kmap [remap beginning-of-buffer] #'pdf-thumbs-first)
    (define-key kmap [remap end-of-buffer] #'pdf-thumbs-last)
    (define-key kmap (kbd ".") #'pdf-thumbs-current)
    (define-key kmap (kbd "M-g g") #'pdf-thumbs-goto-thumbnail)
    (define-key kmap (kbd "RET") #'pdf-thumbs-show-page)
    (define-key kmap (kbd "SPC") #'pdf-thumbs-display-page)
    (define-key kmap (kbd "C-o") #'pdf-thumbs-display-page)
    (define-key kmap [mouse-1] #'pdf-thumbs-mouse-display-page)
    (define-key kmap [mouse-2] #'pdf-thumbs-mouse-display-page)
    (define-key kmap (kbd "o") #'pdf-thumbs-select-pdf-window)
    (define-key kmap (kbd "g") #'pdf-thumbs-refresh)
    (define-key kmap (kbd "q") #'pdf-thumbs-quit)
    (define-key kmap (kbd "+") #'pdf-thumbs-enlarge)
    (define-key kmap (kbd "=") #'pdf-thumbs-enlarge)
    (define-key kmap (kbd "-") #'pdf-thumbs-shrink)
    (define-key kmap (kbd "0") #'pdf-thumbs-reset-size)
    (define-key kmap (kbd "m") #'pdf-thumbs-mark)
    (define-key kmap (kbd "u") #'pdf-thumbs-unmark)
    (define-key kmap (kbd "DEL") #'pdf-thumbs-unmark-backward)
    (define-key kmap (kbd "U") #'pdf-thumbs-unmark-all)
    (define-key kmap (kbd "t") #'pdf-thumbs-toggle-marks)
    (define-key kmap (kbd "D") #'pdf-thumbs-delete)
    (define-key kmap (kbd "X") #'pdf-thumbs-extract)
    (define-key kmap (kbd "M") #'pdf-thumbs-move)
    (define-key kmap (kbd "R") #'pdf-thumbs-rotate)
    (define-key kmap (kbd "L") #'pdf-thumbs-rotate-counterclockwise)
    (define-key kmap [remap undo] #'pdf-thumbs-undo)
    kmap)
  "Keymap used in `pdf-thumbs-mode'.")

(easy-menu-define pdf-thumbs-mode-menu pdf-thumbs-mode-map
  "Menu for the thumbnail grid."
  '("Thumbs"
    ["Show Page" pdf-thumbs-show-page]
    ["Current Page" pdf-thumbs-current]
    "--"
    ["Mark Page" pdf-thumbs-mark]
    ["Unmark Page" pdf-thumbs-unmark]
    ["Unmark All" pdf-thumbs-unmark-all]
    ["Toggle Marks" pdf-thumbs-toggle-marks]
    "--"
    ["Delete Page(s)" pdf-thumbs-delete]
    ["Extract Page(s) to File..." pdf-thumbs-extract]
    ["Move Page(s)..." pdf-thumbs-move]
    ["Rotate Page(s) Clockwise" pdf-thumbs-rotate]
    ["Rotate Page(s) Counterclockwise" pdf-thumbs-rotate-counterclockwise]
    ["Undo Last Change" pdf-thumbs-undo]
    "--"
    ["Larger Thumbnails" pdf-thumbs-enlarge]
    ["Smaller Thumbnails" pdf-thumbs-shrink]
    ["Refresh" pdf-thumbs-refresh]
    ["Quit" pdf-thumbs-quit]))

(define-derived-mode pdf-thumbs-mode special-mode "PDF Thumbs"
  "View the pages of a PDF as a grid of thumbnails.

Move between thumbnails with the arrow keys, \\[pdf-thumbs-next-row], \
\\[pdf-thumbs-previous-row], \\[pdf-thumbs-next] and \\[pdf-thumbs-previous].
\\[pdf-thumbs-show-page] shows the page at point in the PDF window
and selects it, \\[pdf-thumbs-display-page] shows it and stays here.
The PDF window's current page is highlighted.

\\[pdf-thumbs-mark] marks pages, \\[pdf-thumbs-unmark] and \
\\[pdf-thumbs-unmark-all] unmark them.  The page operations
\\[pdf-thumbs-delete] (delete), \\[pdf-thumbs-extract] (extract), \
\\[pdf-thumbs-move] (move), \\[pdf-thumbs-rotate] and \
\\[pdf-thumbs-rotate-counterclockwise] (rotate)
work on the marked pages, or else on the page at point; see
`pdf-pages-minor-mode'.

\\[pdf-thumbs-enlarge] and \\[pdf-thumbs-shrink] change the size of \
the thumbnails, \\[pdf-thumbs-refresh] renders them anew.

\\{pdf-thumbs-mode-map}"
  :group 'pdf-thumbs
  (setq buffer-read-only t
        truncate-lines t
        cursor-type 'hollow)
  (buffer-disable-undo)
  (setq-local line-spacing 0)
  (add-hook 'window-scroll-functions #'pdf-thumbs--window-scrolled nil t)
  (add-hook 'window-size-change-functions #'pdf-thumbs--window-size-changed)
  (add-hook 'kill-buffer-hook #'pdf-thumbs--kill-hook nil t)
  (let ((grid (current-buffer)))
    (setq pdf-thumbs--timer
          (run-with-idle-timer (or pdf-thumbs-prefetch-delay 0.3) t
                               #'pdf-thumbs--idle-render grid))))

(defvar pdf-thumbs-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap pdf-thumbs-minor-mode-map-prefix #'pdf-thumbs)
    kmap)
  "Keymap used for `pdf-thumbs-minor-mode'.")

;;;###autoload
(define-minor-mode pdf-thumbs-minor-mode
  "Show the pages of the PDF as a grid of thumbnails.

\\{pdf-thumbs-minor-mode-map}"
  :group 'pdf-thumbs
  (pdf-util-assert-pdf-buffer))

(provide 'pdf-thumbs)

;;; pdf-thumbs.el ends here
