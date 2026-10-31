;;; pdf-xfdf.el --- Export and import annotations as XFDF. -*- lexical-binding: t -*-

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
;; Export the annotations of a PDF as XFDF, and import them again,
;; in the format that Adobe's XFDF specification defines and other
;; PDF editors, e.g. Okular and Foxit, read and write.  Use it to
;; move annotations between copies of a document: grade one copy of
;; an exam with `pdf-annot' and send the XFDF file along with the
;; blank original, rather than a modified PDF.
;;
;; `pdf-xfdf-export' writes the annotations of the visited PDF to an
;; XFDF file; `pdf-xfdf-import' reads them back into the visited
;; PDF.  Both work on files as well as buffers.
;;
;; What survives a round trip: the type, page, rectangle, color,
;; opacity, contents, label (the author), subject, the marked-up
;; text regions of markup annotations, the icon of text and stamp
;; annotations, the interior color of squares and circles, and the
;; popup rectangle and state.  What does not: an ink annotation is
;; not exported, for poppler cannot read its strokes back; the
;; vertices of a line are approximated by the corners of its
;; rectangle; an image stamp exports its rectangle, but not its
;; image, which XFDF cannot carry.

;;; Code:

(require 'pdf-view)
(require 'pdf-info)
(require 'pdf-util)
(require 'cl-lib)
(require 'xml)
(require 'subr-x)


;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-xfdf nil
  "Export and import PDF annotations as XFDF."
  :group 'pdf-tools)

(defcustom pdf-xfdf-export-types
  '(text highlight underline squiggly strike-out
         square circle line stamp free-text)
  "The annotation types `pdf-xfdf-export' writes.

Types missing from this list are left out of the XFDF file; ink
annotations cannot be exported at all, see `pdf-xfdf-export'."
  :type '(set (const text) (const highlight) (const underline)
              (const squiggly) (const strike-out) (const square)
              (const circle) (const line) (const stamp)
              (const free-text)))

(defcustom pdf-xfdf-author nil
  "The label, i.e. the author, given to imported annotations.

If nil, the author of the XFDF file, which the exporter of the
annotations wrote, is kept."
  :type '(choice (const :tag "Keep the author of the XFDF file" nil)
                 (string :tag "Author")))


;; * ================================================================== *
;; * XFDF elements
;; * ================================================================== *

(defconst pdf-xfdf--element-names
  '((text . "text")
    (highlight . "highlight")
    (underline . "underline")
    (squiggly . "squiggly")
    (strike-out . "strikeout")
    (square . "square")
    (circle . "circle")
    (line . "line")
    (stamp . "stamp")
    (free-text . "freetext"))
  "Annotation types and their XFDF element names.")

(defun pdf-xfdf--type-to-element (type)
  "Return the XFDF element name of the annotation TYPE."
  (cdr (assq type pdf-xfdf--element-names)))

(defun pdf-xfdf--element-to-type (element)
  "Return the annotation type of the XFDF element name ELEMENT."
  (car (cl-rassoc element pdf-xfdf--element-names :test #'equal)))

(defconst pdf-xfdf--flag-names
  '((1 . "invisible") (2 . "hidden") (4 . "print") (8 . "nozoom")
    (16 . "norotate") (32 . "noview") (64 . "readonly")
    (128 . "locked") (256 . "togglenoview")
    (512 . "lockedcontents"))
  "The bits of the annotation flags and their XFDF names.")

(defun pdf-xfdf--flags-string (flags)
  "Return the XFDF flags of the annotation flags number FLAGS."
  (mapconcat #'cdr
             (cl-remove-if-not
              (lambda (flag) (/= 0 (logand flags (car flag))))
              pdf-xfdf--flag-names)
             ","))

(defun pdf-xfdf--flags-number (string)
  "Return the annotation flags number of the XFDF flags STRING."
  (cl-reduce
   (lambda (sum name)
     (let ((flag (cl-rassoc name pdf-xfdf--flag-names :test #'equal)))
       (if flag (+ sum (car flag)) sum)))
   (split-string string "," t "[ \t\n]+")
   :initial-value 0))

(defun pdf-xfdf--format-date (time)
  "Return TIME, an Emacs time value or nil, as a PDF date."
  (when time
    (let ((date (format-time-string "D:%Y%m%d%H%M%S%z" time)))
      (if (string-match "\\`\\(.*[+-][0-9][0-9]\\)\\([0-9][0-9]\\)\\'" date)
          (concat (match-string 1 date) "'" (match-string 2 date))
        date))))

(defun pdf-xfdf--escape (text)
  "Return TEXT with the XML special characters escaped."
  (when text
    (let ((s (replace-regexp-in-string "&" "&amp;" text t t)))
      (dolist (pair '(("<" . "&lt;") (">" . "&gt;")
                     ("\"" . "&quot;") ("'" . "&apos;")))
        (setq s (replace-regexp-in-string (car pair) (cdr pair) s t t)))
      s)))

(defun pdf-xfdf--unescape (text)
  "Return TEXT with the XML entities replaced by their characters."
  (when text
    (let ((s (replace-regexp-in-string "&lt;" "<" text t t)))
      (dolist (pair '(("&gt;" . ">") ("&quot;" . "\"")
                     ("&apos;" . "'") ("&amp;" . "&")))
        (setq s (replace-regexp-in-string (car pair) (cdr pair) s t t)))
      s)))

(defun pdf-xfdf--number-list (string)
  "Return the numbers of the comma-separated XFDF string STRING."
  (mapcar #'string-to-number (split-string string "," t "[ \t\n]+")))

(defun pdf-xfdf--numbers-string (numbers)
  "Return NUMBERS as a comma-separated XFDF string."
  (mapconcat (lambda (n) (format "%.6f" n)) numbers ","))

(defun pdf-xfdf--partition (list n)
  "Return the elements of LIST in groups of N, the last one short."
  (let (groups)
    (while list
      (let ((size (min n (length list))))
        (push (cl-subseq list 0 size) groups)
        (setq list (nthcdr size list))))
    (nreverse groups)))

;; Both directions use rectangles and points in PDF user space,
;; i.e. in points with the origin in the lower left corner, while
;; the Lisp API of this package speaks in relative edges, whose Y
;; grows downwards.

(defun pdf-xfdf--page-size (size)
  "Return SIZE, as `pdf-info-pagesize' gives it, as a cons (W . H)."
  (cons (float (car size)) (float (cdr size))))

(defun pdf-xfdf--edges-to-rect (edges size)
  "Return the rectangle of relative EDGES on a page of SIZE.

The result is a list of the coordinates of the lower left and the
upper right corner, in user space."
  (pcase-let ((`(,w . ,h) (pdf-xfdf--page-size size)))
    (pdf-util-with-edges (edges)
      (list (* edges-left w) (* (- 1 edges-bot) h)
            (* edges-right w) (* (- 1 edges-top) h)))))

(defun pdf-xfdf--rect-to-edges (rect size)
  "Return the relative edges of the rectangle RECT on a page of SIZE."
  (pcase-let ((`(,w . ,h) (pdf-xfdf--page-size size))
              (`(,llx ,lly ,urx ,ury) rect))
    (list (/ llx w) (- 1 (/ ury h))
          (/ urx w) (- 1 (/ lly h)))))

(defun pdf-xfdf--region-to-quad (edges size)
  "Return the quad points of relative EDGES on a page of SIZE.

The result is a list of the corners of the region, upper left,
upper right, lower left, lower right, in the order in which XFDF
writes the quad points of a markup annotation."
  (pcase-let ((`(,w . ,h) (pdf-xfdf--page-size size)))
    (pdf-util-with-edges (edges)
      (list (* edges-left w) (* (- 1 edges-bot) h)
            (* edges-right w) (* (- 1 edges-bot) h)
            (* edges-left w) (* (- 1 edges-top) h)
            (* edges-right w) (* (- 1 edges-top) h)))))

(defun pdf-xfdf--quad-to-region (quad size)
  "Return the relative edges of the quad points QUAD on a page of SIZE."
  (pcase-let ((`(,w . ,h) (pdf-xfdf--page-size size))
              (`(,x1 ,y1 ,x2 ,y2 ,x3 ,y3 ,x4 ,y4) quad))
    (list (/ (min x1 x3) w) (- 1 (/ (max y1 y2) h))
          (/ (max x2 x4) w) (- 1 (/ (min y3 y4) h)))))

(defun pdf-xfdf--point-to-relative (point size)
  "Return the user space POINT on a page of SIZE as relative edges."
  (pcase-let ((`(,w . ,h) (pdf-xfdf--page-size size))
              (`(,x ,y) point))
    (list (/ x w) (- 1 (/ y h)))))


;; * ================================================================== *
;; * Export
;; * ================================================================== *

(defun pdf-xfdf--annot-attributes (annot size)
  "Return the XFDF attributes of ANNOT on a page of SIZE."
  (let* ((attributes
          `((page . ,(number-to-string (1- (alist-get 'page annot))))
            (rect . ,(pdf-xfdf--numbers-string
                      (pdf-xfdf--edges-to-rect
                       (alist-get 'edges annot) size)))))
         (push-when
          (lambda (name value)
            (when (and value (not (equal "" value)))
              (push (cons name value) attributes)))))
    (funcall push-when 'color (alist-get 'color annot))
    (funcall push-when 'opacity
             (and (alist-get 'opacity annot)
                  (format "%f" (alist-get 'opacity annot))))
    (funcall push-when 'title (alist-get 'label annot))
    (funcall push-when 'subject (alist-get 'subject annot))
    (funcall push-when 'date
             (pdf-xfdf--format-date (alist-get 'modified annot)))
    (funcall push-when 'flags
             (and (/= 0 (or (alist-get 'flags annot) 0))
                  (pdf-xfdf--flags-string (alist-get 'flags annot))))
    (funcall push-when 'contents (alist-get 'contents annot))
    (pcase (alist-get 'type annot)
      ((or 'highlight 'underline 'squiggly 'strike-out)
       (funcall push-when 'coords
                (and (alist-get 'markup-edges annot)
                     (pdf-xfdf--numbers-string
                      (apply #'append
                             (mapcar
                              (lambda (edges)
                                (pdf-xfdf--region-to-quad edges size))
                              (alist-get 'markup-edges annot)))))))
      ('text
       (funcall push-when 'icon (alist-get 'icon annot))
       (funcall push-when 'open
                (if (alist-get 'is-open annot) "yes" "no")))
      ('stamp
       (funcall push-when 'icon (alist-get 'icon annot)))
      ((or 'square 'circle)
       (funcall push-when 'interior-color
                (alist-get 'interior-color annot)))
      ('line
       ;; Poppler cannot read the vertices of a line back; use the
       ;; corners of its rectangle, which is where `pdf-annot'
       ;; draws a new line by default.
       (let ((rect (pdf-xfdf--edges-to-rect
                    (alist-get 'edges annot) size)))
         (funcall push-when 'start
                  (pdf-xfdf--numbers-string (nthcdr 2 rect)))
         (funcall push-when 'end
                  (pdf-xfdf--numbers-string (cl-subseq rect 0 2)))))
      (_ nil))
    (nreverse attributes)))

(defun pdf-xfdf--annot-children (annot size)
  "Return the XFDF children of ANNOT on a page of SIZE, as strings.

This is the popup of a markup annotation, and nothing else; the
contents are an attribute."
  (and (alist-get 'popup-edges annot)
       (list
        (format "<popup page=\"%d\" rect=\"%s\" open=\"%s\"/>"
                (1- (alist-get 'page annot))
                (pdf-xfdf--numbers-string
                 (pdf-xfdf--edges-to-rect
                  (alist-get 'popup-edges annot) size))
                (if (alist-get 'popup-is-open annot) "yes" "no")))))

(defun pdf-xfdf--write-annot (annot size)
  "Return the XFDF source of ANNOT on a page of SIZE."
  (let ((element (pdf-xfdf--type-to-element (alist-get 'type annot))))
    (unless element
      (error "Cannot export a %s annotation" (alist-get 'type annot)))
    (format "<%s%s>%s</%s>"
            element
            (mapconcat (lambda (attr)
                         (format " %s=\"%s\""
                                 (symbol-name (car attr))
                                 (pdf-xfdf--escape (cdr attr))))
                       (pdf-xfdf--annot-attributes annot size)
                       "")
            (mapconcat #'identity
                       (pdf-xfdf--annot-children annot size)
                       "")
            element)))

;;;###autoload
(defun pdf-xfdf-export (file &optional target)
  "Write the annotations of the PDF FILE to TARGET in XFDF.

FILE may be a filename or a buffer, as in `pdf-info-getannots';
interactively, it is the visited file of the current buffer, and
TARGET is read in the minibuffer, defaulting to FILE with the
suffix .xfdf.

Only the types in `pdf-xfdf-export-types' are written; ink
annotations cannot be exported at all, because poppler cannot
read their strokes back.  The annotations carry no reference to
FILE, but its basename is written in an f element, so that other
editors find the document again.

Return TARGET."
  (interactive
   (progn
     (pdf-util-assert-pdf-buffer)
     (let* ((file (buffer-file-name))
            (default (and file
                          (concat (file-name-sans-extension file)
                                  ".xfdf"))))
       (list file
             (read-file-name "Write XFDF annotations to: "
                             (and default (file-name-directory default))
                             default nil
                             (and default (file-name-nondirectory default)))))))
  (let ((target (expand-file-name target))
        (annotations (pdf-info-getannots nil file))
        (types pdf-xfdf-export-types)
        written
        (skipped 0))
    (dolist (annot annotations)
      (cond
       ((not (memq (alist-get 'type annot) types))
        (cl-incf skipped))
       ((not (pdf-xfdf--type-to-element (alist-get 'type annot)))
        (cl-incf skipped))
       (t
        (let ((size (pdf-info-pagesize (alist-get 'page annot) file)))
          (push (pdf-xfdf--write-annot annot size) written)))))
    (with-temp-file target
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (insert "<xfdf xml:space=\"preserve\" \
xmlns=\"http://ns.adobe.com/xfdf/\">\n <annots>\n")
      (dolist (source (reverse written))
        (insert "  " source "\n"))
      (insert " </annots>\n")
      (when (and (stringp file) (file-name-nondirectory file))
        (insert " <f href=\"" (file-name-nondirectory file) "\"/>\n"))
      (insert "</xfdf>\n"))
    (message "Wrote %d annotation%s%s to %s"
             (length written)
             (if (= 1 (length written)) "" "s")
             (if (> skipped 0)
                 (format " (skipped %d)" skipped)
               "")
             target)
    target))


;; * ================================================================== *
;; * Import
;; * ================================================================== *

(defun pdf-xfdf--popup (element size)
  "Return the popup of the XFDF ELEMENT on a page of SIZE.

The result is a cons (EDGES . OPEN), or nil, if ELEMENT has no
popup."
  (let ((popup (cl-find-if (lambda (child)
                              (and (consp child)
                                   (eq 'popup (xml-node-name child))))
                            (xml-node-children element))))
    (when popup
      (let* ((attrs (xml-node-attributes popup))
             (rect (cdr (assq 'rect attrs))))
        (when rect
          (cons (pdf-xfdf--rect-to-edges
                 (pdf-xfdf--number-list rect) size)
                (equal "yes" (cdr (assq 'open attrs)))))))))

(defun pdf-xfdf--element-annot (element file)
  "Create the annotation of the XFDF ELEMENT in FILE.

Return the new annotation."
  (let* ((attrs (xml-node-attributes element))
         (attr (lambda (name) (cdr (assq name attrs))))
         (type (pdf-xfdf--element-to-type
                (symbol-name (xml-node-name element))))
         (page (1+ (string-to-number (funcall attr 'page))))
         (size (pdf-info-pagesize page file))
         (rect (and (funcall attr 'rect)
                    (pdf-xfdf--number-list (funcall attr 'rect))))
         (edges (and rect (pdf-xfdf--rect-to-edges rect size)))
         (contents (pdf-xfdf--unescape (funcall attr 'contents)))
         (popup (pdf-xfdf--popup element size))
         extra-args annot)
    (unless type
      (error "Unknown XFDF element: %s" (xml-node-name element)))
    (unless (and rect (= 4 (length rect)))
      (error "Element %s has no rectangle"
             (symbol-name (xml-node-name element))))
    (setq extra-args
          (pcase type
            ((or 'highlight 'underline 'squiggly 'strike-out)
             (mapcar (lambda (quad)
                       (pdf-xfdf--quad-to-region quad size))
                     (pdf-xfdf--partition
                      (pdf-xfdf--number-list (funcall attr 'coords)) 8)))
            ('line
             (let* ((start (and (funcall attr 'start)
                                (pdf-xfdf--point-to-relative
                                 (pdf-xfdf--number-list
                                  (funcall attr 'start)) size)))
                    (end (and (funcall attr 'end)
                              (pdf-xfdf--point-to-relative
                               (pdf-xfdf--number-list
                                (funcall attr 'end)) size))))
               (and start end (list (append start end)))))
            ('stamp
             (and (funcall attr 'icon)
                  (list (funcall attr 'icon))))
            (_ nil)))
    (setq annot
          (apply #'pdf-info-addannot
                 page
                 (pcase type
                   ;; The server derives the rectangle of these
                   ;; from their points and regions.
                   ((or 'highlight 'underline 'squiggly 'strike-out)
                    (apply #'pdf-util-edges-union extra-args))
                   ('line
                    (let ((points (car extra-args)))
                      (list (min (nth 0 points) (nth 2 points))
                            (min (nth 1 points) (nth 3 points))
                            (max (nth 0 points) (nth 2 points))
                            (max (nth 1 points) (nth 3 points)))))
                   (_ edges))
                 type 0 file extra-args))
    (let ((edits
           (cl-remove-if
            (lambda (edit) (null (cdr edit)))
            `((color . ,(funcall attr 'color))
              (contents . ,contents)
              (label . ,(or pdf-xfdf-author (funcall attr 'title)))
              (subject . ,(funcall attr 'subject))
              (opacity . ,(and (funcall attr 'opacity)
                               (string-to-number
                                (funcall attr 'opacity))))
              (flags . ,(and (funcall attr 'flags)
                             (/= 0 (string-to-number
                                    (funcall attr 'flags)))
                             (pdf-xfdf--flags-number
                              (funcall attr 'flags))))
              (icon . ,(and (memq type '(text stamp))
                            (funcall attr 'icon)))
              (is-open . ,(and (eq type 'text)
                               (equal "yes" (funcall attr 'open))))
              (popup . ,(car popup))
              (popup-is-open . ,(cdr popup))))))
      (when edits
        (setq annot (pdf-info-editannot (alist-get 'id annot)
                                         edits file))))
    annot))

;;;###autoload
(defun pdf-xfdf-import (file &optional source)
  "Create the annotations of the XFDF file SOURCE in the PDF FILE.

FILE may be a filename or a buffer, as in `pdf-info-addannot';
interactively, it is the visited file of the current buffer, and
SOURCE is read in the minibuffer, defaulting to FILE with the
suffix .xfdf.  Annotations of types that this epdfinfo cannot
create are skipped with a message.

Return the list of new annotations."
  (interactive
   (progn
     (pdf-util-assert-pdf-buffer)
     (let* ((file (buffer-file-name))
            (default (and file
                          (concat (file-name-sans-extension file)
                                  ".xfdf"))))
       (list file
             (read-file-name "Import XFDF annotations from: "
                             (and default (file-name-directory default))
                             default nil
                             (and default (file-name-nondirectory default)))))))
  (let* ((source (expand-file-name source))
         (tree (with-temp-buffer
                 (insert-file-contents source)
                 (xml-parse-region)))
         (root (cl-find-if (lambda (node)
                             (and (consp node)
                                  (eq 'xfdf (xml-node-name node))))
                           tree))
         (annots (and root
                      (cl-find-if (lambda (node)
                                    (and (consp node)
                                         (eq 'annots (xml-node-name node))))
                                  (xml-node-children root))))
         imported
         (skipped 0))
    (unless root
      (error "Not an XFDF file: %s" (abbreviate-file-name source)))
    (dolist (element (and annots (xml-node-children annots)))
      (when (consp element)
        (let ((type (pdf-xfdf--element-to-type
                     (symbol-name (xml-node-name element)))))
          (if (and type (memq type (pdf-info-creatable-annotation-types)))
              (push (pdf-xfdf--element-annot element file) imported)
            (message "Skipping a %s annotation, which this epdfinfo \
cannot create" (symbol-name (xml-node-name element)))
            (cl-incf skipped)))))
    (message "Imported %d annotation%s%s from %s"
             (length imported)
             (if (= 1 (length imported)) "" "s")
             (if (> skipped 0)
                 (format " (skipped %d)" skipped)
               "")
             source)
    (nreverse imported)))

(provide 'pdf-xfdf)

;;; pdf-xfdf.el ends here
