;;; pdf-reflow.el --- Read a PDF's text reflowed in reading order. -*- lexical-binding: t -*-

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
;; `pdf-reflow' shows the text of a PDF in a text buffer, reflowed in
;; reading order, so that it can be read in a narrow window, at any
;; font size, with a screen reader and with the usual Emacs commands
;; for text.
;;
;; For a tagged PDF the text comes from the structure tree, in its
;; order: headings are shown as headings (they fold with TAB and are
;; listed by `imenu'), paragraphs are wrapped by `visual-line-mode',
;; lists are indented with their labels, tables are shown as rows of
;; cells and figures as their alternative text.  For a PDF that is
;; not tagged the text of each page is shown in the order poppler
;; reads it, joined into paragraphs, which is a guess.
;;
;; Every block of text knows its page: RET (or mouse-2) shows it in
;; the PDF and highlights it, like the entries of `pdf-a11y-report'.
;; Calling `pdf-reflow' in the PDF shows the block at its current
;; page.

(require 'pdf-a11y)
(require 'pdf-info)
(require 'pdf-util)
(require 'pdf-view)
(require 'outline)
(require 'imenu)
(require 'let-alist)
(require 'cl-lib)
(require 'subr-x)

;;; Code:



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-reflow nil
  "Read a PDF's text reflowed in reading order."
  :group 'pdf-tools)

(defcustom pdf-reflow-paragraph-line-ratio 0.8
  "When a line ends a paragraph of a PDF that is not tagged.

A line shorter than this fraction of the longest line on its page
ends a paragraph.  Only used for PDFs without a structure tree."
  :group 'pdf-reflow
  :type 'number)

(defcustom pdf-reflow-list-indent 2
  "Columns by which a nested list is indented."
  :group 'pdf-reflow
  :type 'integer)

(defface pdf-reflow-heading-1
  '((t (:inherit outline-1 :weight bold :height 1.5)))
  "Face for level 1 headings in reflowed text."
  :group 'pdf-reflow)

(defface pdf-reflow-heading-2
  '((t (:inherit outline-2 :weight bold :height 1.3)))
  "Face for level 2 headings in reflowed text."
  :group 'pdf-reflow)

(defface pdf-reflow-heading-3
  '((t (:inherit outline-3 :weight bold :height 1.15)))
  "Face for level 3 headings in reflowed text."
  :group 'pdf-reflow)

(defface pdf-reflow-heading-4
  '((t (:inherit outline-4 :weight bold :height 1.05)))
  "Face for level 4 headings in reflowed text."
  :group 'pdf-reflow)

(defface pdf-reflow-heading-5
  '((t (:inherit outline-5 :weight bold)))
  "Face for level 5 headings in reflowed text."
  :group 'pdf-reflow)

(defface pdf-reflow-heading-6
  '((t (:inherit outline-6 :weight bold)))
  "Face for level 6 headings in reflowed text."
  :group 'pdf-reflow)

(defface pdf-reflow-figure
  '((t (:inherit font-lock-doc-face :slant italic)))
  "Face for figures and other illustrations in reflowed text."
  :group 'pdf-reflow)

(defface pdf-reflow-table
  '((t (:inherit fixed-pitch)))
  "Face for tables in reflowed text."
  :group 'pdf-reflow)

(defface pdf-reflow-page
  '((t (:inherit shadow :weight bold)))
  "Face for page separators in reflowed text."
  :group 'pdf-reflow)

(defface pdf-reflow-note
  '((t (:inherit shadow)))
  "Face for the note at the top of reflowed text."
  :group 'pdf-reflow)



;; * ================================================================== *
;; * Reading the structure tree
;; * ================================================================== *

(defconst pdf-reflow--inline-kinds
  '(span quote note reference bibentry code link annot ruby warichu)
  "Element kinds whose text is read as part of the enclosing block.")

(defun pdf-reflow--tree (elements)
  "Return the flattened structure tree ELEMENTS as a tree.

ELEMENTS is a list as returned by `pdf-info-structure'.  The
result is a list of the top-level nodes, where each node is a
cons (ELEMENT . CHILDREN) and CHILDREN is a list of nodes."
  (let* ((root (list nil))
         (stack (list (cons 0 root))))
    (dolist (elt elements)
      (let ((depth (alist-get 'depth elt))
            (node (list elt)))
        (while (>= (caar stack) depth)
          (pop stack))
        (push node (cdr (cdar stack)))
        (push (cons depth node) stack)))
    (cl-labels ((fix (node)
                  (setcdr node (nreverse (cdr node)))
                  (mapc #'fix (cdr node))
                  node))
      (cdr (fix root)))))

(defun pdf-reflow--kind (node)
  "Return the kind of NODE's element."
  (alist-get 'kind (car node)))

(defun pdf-reflow--text (node)
  "Return the text of NODE's element, trimmed."
  (let-alist (car node)
    (string-trim (or .actual-text .text ""))))

(defun pdf-reflow--block-child-p (node)
  "Return non-nil if NODE is read as a block of its own."
  (let ((kind (pdf-reflow--kind node)))
    (not (or (memq kind pdf-reflow--inline-kinds)
             (memq kind pdf-a11y--illustration-kinds)))))

(defun pdf-reflow--illustrations (nodes)
  "Return the illustrations in NODES and their subtrees."
  (cl-loop for node in nodes
           if (memq (pdf-reflow--kind node) pdf-a11y--illustration-kinds)
           collect node
           else append (pdf-reflow--illustrations (cdr node))))



;; * ================================================================== *
;; * Inserting blocks
;; * ================================================================== *

(defvar pdf-reflow--tight nil
  "Non-nil while inserting blocks that are not separated by a blank line.")

(defvar pdf-reflow--indent 0
  "The indentation of the blocks being inserted, in columns.")

(defvar pdf-reflow--in-list nil
  "Non-nil while inserting the items of a list.")

(defvar pdf-reflow--label nil
  "A label (of a list item, say) to put before the next block.")

(defvar-local pdf-reflow--body-start nil
  "Where the reflowed text starts, after the note at the top.")

(defun pdf-reflow--separate ()
  "Insert a blank line before the next block, if needed."
  (unless (or pdf-reflow--tight
              (<= (point) pdf-reflow--body-start))
    (insert "\n")))

(defun pdf-reflow--insert-block (text element &rest properties)
  "Insert TEXT as a block that shows ELEMENT.

Put the pending label, see `pdf-reflow--label', before it and
indent it by `pdf-reflow--indent'.  PROPERTIES are more text
properties for the block."
  (let* ((label (and pdf-reflow--label (concat pdf-reflow--label " ")))
         (indent (make-string pdf-reflow--indent ?\s))
         (hanging (make-string (+ pdf-reflow--indent (string-width (or label "")))
                               ?\s))
         beg)
    (setq pdf-reflow--label nil)
    (pdf-reflow--separate)
    (setq beg (point))
    (insert (or label "") text "\n")
    (add-text-properties
     beg (point)
     `(pdf-reflow-element ,element line-prefix ,indent wrap-prefix ,hanging
                          ,@properties))))

(defun pdf-reflow--illustration-text (element)
  "Return the text that stands for illustration ELEMENT."
  (let-alist element
    (let ((name (cl-case .kind
                  (formula "Formula")
                  (form "Form")
                  (t "Figure")))
          (alt (and .alt (string-trim .alt))))
      (cond
       ((and alt (not (equal alt "")))
        (format "[%s: %s]" name alt))
       ((and .actual-text (not (equal (string-trim .actual-text) "")))
        (format "[%s: %s]" name (string-trim .actual-text)))
       ((and (eq .kind 'formula) (not (equal (string-trim (or .text "")) "")))
        (format "[%s: %s]" name (string-trim .text)))
       (t (format "[%s without alternative text]" name))))))

(defun pdf-reflow--insert-illustration (node)
  "Insert the illustration NODE."
  (pdf-reflow--insert-block (pdf-reflow--illustration-text (car node))
                            (car node)
                            'face 'pdf-reflow-figure))

(defun pdf-reflow--insert-heading (node level)
  "Insert the heading NODE of LEVEL, which is 0 for an H."
  (let* ((level (min 6 (max 1 level)))
         (text (pdf-reflow--text node)))
    (unless (equal text "")
      (pdf-reflow--insert-block
       text (car node)
       'face (intern (format "pdf-reflow-heading-%d" level))
       'pdf-reflow-heading level))
    (mapc #'pdf-reflow--insert-illustration
          (pdf-reflow--illustrations (cdr node)))))

(defun pdf-reflow--list-label (numbering index)
  "Return the label of the INDEXth item of a list with NUMBERING.

INDEX starts at 0.  NUMBERING is as returned by
`pdf-info-structure'."
  (let ((n (1+ index)))
    (cl-case numbering
      (decimal (format "%d." n))
      (lower-alpha (format "%c." (+ ?a (% index 26))))
      (upper-alpha (format "%c." (+ ?A (% index 26))))
      (lower-roman (concat (downcase (pdf-reflow--roman n)) "."))
      (upper-roman (concat (pdf-reflow--roman n) "."))
      (circle "◦")
      (square "▪")
      (t "•"))))

(defun pdf-reflow--roman (n)
  "Return N in Roman numerals."
  (let ((result ""))
    (dolist (pair '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
                    (100 . "C") (90 . "XC") (50 . "L") (40 . "XL")
                    (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I")))
      (while (>= n (car pair))
        (setq result (concat result (cdr pair))
              n (- n (car pair)))))
    result))

(defun pdf-reflow--insert-list (node)
  "Insert the list NODE, one item per line.

A list nested in another one is indented by `pdf-reflow-list-indent'."
  (let ((index 0))
    (pdf-reflow--separate)
    (let ((pdf-reflow--tight t)
          (pdf-reflow--indent (if pdf-reflow--in-list
                                  (+ pdf-reflow--indent pdf-reflow-list-indent)
                                pdf-reflow--indent))
          (pdf-reflow--in-list t))
      (dolist (item (cdr node))
        (if (not (eq (pdf-reflow--kind item) 'list-item))
            (pdf-reflow--insert-node item)
          (let* ((label-node (cl-find 'list-label (cdr item)
                                      :key #'pdf-reflow--kind))
                 (label (if (and label-node
                                 (not (equal (pdf-reflow--text label-node) "")))
                            (pdf-reflow--text label-node)
                          (pdf-reflow--list-label
                           (alist-get 'list-numbering (car item)) index)))
                 (parts (remq label-node (cdr item))))
            (setq pdf-reflow--label label)
            (cond
             ((null parts)
              (pdf-reflow--insert-block (pdf-reflow--text item) (car item)))
             (t
              (dolist (part parts)
                (pdf-reflow--insert-node part))))
            ;; An item without any text still shows its label.
            (when pdf-reflow--label
              (pdf-reflow--insert-block "" (car item)))
            (cl-incf index)))))))

(defun pdf-reflow--table-rows (node)
  "Return the rows of the table NODE, and whether they are header rows.

Return a list of (ROW . HEADER-P)."
  (cl-loop for child in (cdr node)
           for kind = (pdf-reflow--kind child)
           if (eq kind 'table-row)
           collect (cons child
                         (and (cdr child)
                              (cl-every (lambda (c)
                                          (eq (pdf-reflow--kind c)
                                              'table-heading))
                                        (cdr child))))
           else if (eq kind 'table-header)
           append (mapcar (lambda (r) (cons (car r) t))
                          (pdf-reflow--table-rows child))
           else if (memq kind '(table-body table-footer))
           append (pdf-reflow--table-rows child)))

(defun pdf-reflow--cell-text (node)
  "Return the text of the table cell NODE."
  (let ((text (pdf-reflow--text node)))
    (string-join
     (delete "" (cons text (mapcar (lambda (n)
                                     (pdf-reflow--illustration-text (car n)))
                                   (pdf-reflow--illustrations (cdr node)))))
     " ")))

(defun pdf-reflow--table-cells (rows)
  "Return the cell texts of ROWS as a list of lists of strings.

ROWS is as returned by `pdf-reflow--table-rows'.  Cells spanning
several columns or rows are not known, so their columns may not
line up."
  (mapcar (lambda (row)
            (mapcar #'pdf-reflow--cell-text
                    (cl-remove-if-not
                     (lambda (c)
                       (memq (pdf-reflow--kind c)
                             '(table-heading table-data)))
                     (cdr (car row)))))
          rows))

(defun pdf-reflow--insert-table (node)
  "Insert the table NODE like an Org table, one row per line."
  (let* ((rows (pdf-reflow--table-rows node))
         (cells (pdf-reflow--table-cells rows))
         (ncols (apply #'max 0 (mapcar #'length cells)))
         (widths (make-list ncols 1)))
    (dolist (row cells)
      (cl-loop for cell in row
               for i from 0
               do (setf (nth i widths) (max (nth i widths)
                                            (string-width cell)))))
    ;; Captions and the like come first.
    (dolist (child (cdr node))
      (unless (memq (pdf-reflow--kind child)
                    '(table-row table-header table-body table-footer))
        (pdf-reflow--insert-node child)))
    (pdf-reflow--separate)
    (let ((pdf-reflow--tight t))
      (cl-loop
       for (row . header-p) in rows
       for row-cells in cells
       for next in (append (cdr rows) (list nil))
       do (pdf-reflow--insert-block
           (concat "| "
                   (mapconcat
                    #'identity
                    (cl-loop for i below ncols
                             for cell = (or (nth i row-cells) "")
                             collect (concat cell
                                             (make-string
                                              (- (nth i widths)
                                                 (string-width cell))
                                              ?\s)))
                    " | ")
                   " |")
           (car row) 'face 'pdf-reflow-table)
       (when (and header-p next (not (cdr next)))
         (pdf-reflow--insert-block
          (concat "|"
                  (mapconcat (lambda (w) (make-string (+ w 2) ?-))
                             widths "+")
                  "|")
          (car row) 'face 'pdf-reflow-table))))))

(defun pdf-reflow--insert-node (node)
  "Insert the structure tree NODE and its subtree."
  (let* ((kind (pdf-reflow--kind node))
         (level (pdf-a11y-heading-level kind)))
    (cond
     (level (pdf-reflow--insert-heading node level))
     ((memq kind pdf-a11y--illustration-kinds)
      (pdf-reflow--insert-illustration node))
     ((eq kind 'list) (pdf-reflow--insert-list node))
     ((eq kind 'table) (pdf-reflow--insert-table node))
     ((eq kind 'list-label)
      ;; A label outside of a list, e.g. of a caption, goes before the
      ;; next block.
      (let ((text (pdf-reflow--text node)))
        (unless (equal text "")
          (setq pdf-reflow--label (if pdf-reflow--label
                                      (concat pdf-reflow--label " " text)
                                    text)))))
     ((or (memq kind pdf-a11y--grouping-kinds)
          (equal (pdf-reflow--text node) "")
          (cl-some #'pdf-reflow--block-child-p (cdr node)))
      (mapc #'pdf-reflow--insert-node (cdr node)))
     (t
      (pdf-reflow--insert-block (pdf-reflow--text node) (car node))
      (mapc #'pdf-reflow--insert-illustration
            (pdf-reflow--illustrations (cdr node)))))))



;; * ================================================================== *
;; * Untagged PDFs
;; * ================================================================== *

(defun pdf-reflow--paragraphs (text)
  "Split the page TEXT into paragraphs.

TEXT has a line break at the end of each line.  A line that is
shorter than `pdf-reflow-paragraph-line-ratio' times the longest
line, or an empty one, ends a paragraph.  A word broken by a
hyphen at the end of a line is joined again."
  (let* ((lines (mapcar #'string-trim (split-string text "\n")))
         (longest (apply #'max 1 (mapcar #'string-width lines)))
         paragraphs current)
    (dolist (line lines)
      (if (equal line "")
          (when current
            (push current paragraphs)
            (setq current nil))
        (setq current
              (cond
               ((null current) line)
               ((and (string-match-p "[[:alpha:]]-\\'" current)
                     (string-match-p "\\`[[:lower:]]" line))
                (concat (substring current 0 -1) line))
               (t (concat current " " line))))
        (when (< (string-width line)
                 (* pdf-reflow-paragraph-line-ratio longest))
          (push current paragraphs)
          (setq current nil))))
    (when current
      (push current paragraphs))
    (nreverse paragraphs)))

(defun pdf-reflow--insert-untagged (buffer)
  "Insert the text of each page of the PDF in BUFFER."
  (let ((pages (pdf-info-number-of-pages buffer)))
    (dotimes (i pages)
      (let ((page (1+ i)))
        (pdf-reflow--insert-block
         (format "Page %d of %d" page pages)
         `((kind . page) (page . ,page) (text . ""))
         'face 'pdf-reflow-page
         'pdf-reflow-heading 1)
        (dolist (paragraph (pdf-reflow--paragraphs
                            (pdf-info-gettext page '(0 0 1 1) 'glyph buffer)))
          (pdf-reflow--insert-block
           paragraph
           `((kind . paragraph) (page . ,page) (text . ,paragraph))))))))



;; * ================================================================== *
;; * The reflow buffer
;; * ================================================================== *

(defvar pdf-reflow-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'pdf-reflow-show-block)
    (define-key map [mouse-2] #'pdf-reflow-mouse-show-block)
    (define-key map (kbd "C-o") #'pdf-reflow-display-block)
    (define-key map (kbd "o") #'pdf-reflow-display-block)
    (define-key map (kbd "n") #'pdf-reflow-next-block)
    (define-key map (kbd "p") #'pdf-reflow-previous-block)
    (define-key map (kbd "M-n") #'pdf-reflow-next-heading)
    (define-key map (kbd "M-p") #'pdf-reflow-previous-heading)
    map)
  "Keymap used in `pdf-reflow-mode'.")

(define-derived-mode pdf-reflow-mode special-mode "PDF Reflow"
  "Major mode for reading the text of a PDF reflowed.

The text is wrapped by `visual-line-mode'.  Every block of text
knows its page in the PDF: \\[pdf-reflow-show-block] shows it
there.  Headings fold with TAB (in Emacs 29 or later) and are
listed by `imenu'.

\\{pdf-reflow-mode-map}"
  (setq-local revert-buffer-function #'pdf-reflow--revert)
  (setq-local imenu-create-index-function #'pdf-reflow--imenu-index)
  (setq truncate-lines nil)
  (visual-line-mode 1)
  (when (fboundp 'outline-search-text-property)
    (setq-local outline-search-function #'pdf-reflow--outline-search)
    (setq-local outline-level #'pdf-reflow--outline-level)
    (setq-local outline-minor-mode-cycle t)
    (setq-local outline-minor-mode-highlight nil)
    (setq-local outline-blank-line t)
    (outline-minor-mode 1)))

(defun pdf-reflow--outline-search (&optional bound move backward looking-at)
  "Find the next heading, see `outline-search-function'.

BOUND, MOVE, BACKWARD and LOOKING-AT are as described there."
  (outline-search-text-property 'pdf-reflow-heading nil
                                bound move backward looking-at))

(defun pdf-reflow--outline-level ()
  "Return the level of the heading at point."
  (or (get-text-property (point) 'pdf-reflow-heading) 1))

(defun pdf-reflow--imenu-index ()
  "Return an `imenu' index of the headings."
  (let ((pos (point-min)) index)
    (while (setq pos (text-property-not-all pos (point-max)
                                            'pdf-reflow-heading nil))
      (let ((level (get-text-property pos 'pdf-reflow-heading))
            (end (or (next-single-property-change pos 'pdf-reflow-heading)
                     (point-max))))
        (push (cons (concat (make-string (* 2 (1- level)) ?\s)
                            (string-trim (buffer-substring-no-properties
                                          pos end)))
                    pos)
              index)
        (setq pos end)))
    (nreverse index)))

(defun pdf-reflow--render (buffer)
  "Insert the reflowed text of the PDF in BUFFER into the current buffer."
  (let* ((inhibit-read-only t)
         (structure-p (pdf-info-structure-tree-p))
         (elements (and structure-p (pdf-info-structure buffer t)))
         (file (pdf-info--normalize-file-or-buffer buffer)))
    (erase-buffer)
    (pdf-reflow-mode)
    (setq pdf-a11y--pdf-buffer buffer)
    (insert (propertize (format "Reflowed text of %s\n"
                                (file-name-nondirectory file))
                        'face 'pdf-a11y-heading)
            (propertize
             (cond
              (elements
               "In the reading order of the document's structure tree.\n")
              (structure-p
               "This PDF is not tagged, so the reading order is a guess: \
it is the order in which poppler reads the text of each page.\n")
              (t
               "The reading order is a guess: epdfinfo cannot read the \
structure tree (it needs poppler-glib >= 0.26), so this is the order in \
which poppler reads the text of each page.\n"))
             'face 'pdf-reflow-note))
    (insert "\n")
    (setq pdf-reflow--body-start (point))
    (let ((pdf-reflow--tight nil)
          (pdf-reflow--indent 0)
          (pdf-reflow--label nil))
      (if elements
          (mapc #'pdf-reflow--insert-node (pdf-reflow--tree elements))
        (pdf-reflow--insert-untagged buffer)))
    (set-buffer-modified-p nil)))

(defun pdf-reflow--revert (&rest _)
  "Read the PDF of the current reflow buffer again."
  (unless (buffer-live-p pdf-a11y--pdf-buffer)
    (user-error "The PDF buffer is gone"))
  (let ((page (pdf-reflow--block-page)))
    (pdf-reflow--render pdf-a11y--pdf-buffer)
    (if page
        (pdf-reflow-goto-page page)
      (goto-char (point-min)))))

(defun pdf-reflow--block-page (&optional pos)
  "Return the page of the block at POS, or nil."
  (alist-get 'page (get-text-property (or pos (point)) 'pdf-reflow-element)))

(defun pdf-reflow-goto-page (page)
  "Move point to the first block of the PDF's PAGE.

If no block is on PAGE, go to the first block on a later page.
Return the position, or nil if there is no such block."
  (interactive "nPage: ")
  (let ((pos (point-min)) exact later)
    (while (and (not exact)
                (setq pos (text-property-not-all pos (point-max)
                                                 'pdf-reflow-element nil)))
      (let ((p (pdf-reflow--block-page pos)))
        (cond
         ((null p))
         ((= p page) (setq exact pos))
         ((and (> p page) (null later)) (setq later pos))))
      (setq pos (or (next-single-property-change pos 'pdf-reflow-element)
                    (point-max))))
    (when (or exact later)
      (goto-char (or exact later)))))

(defun pdf-reflow--element-at (pos)
  "Return the element of the block at POS, or signal an error."
  (or (get-text-property pos 'pdf-reflow-element)
      (user-error "No block of the PDF here")))

(defun pdf-reflow-show-block (&optional pos)
  "Show the block at POS (point by default) in the PDF and select it."
  (interactive)
  (pdf-a11y-show-element (pdf-reflow--element-at (or pos (point)))))

(defun pdf-reflow-display-block ()
  "Show the block at point in the PDF without selecting it."
  (interactive)
  (pdf-a11y-show-element (pdf-reflow--element-at (point)) t))

(defun pdf-reflow-mouse-show-block (event)
  "Show the block clicked on in EVENT in the PDF."
  (interactive "e")
  (mouse-set-point event)
  (pdf-reflow-show-block))

(defun pdf-reflow--move (n property)
  "Move to the start of the Nth next stretch of text with PROPERTY.

Move backward if N is negative."
  (let ((pos (point)))
    (dotimes (_ (abs n))
      (if (> n 0)
          (progn
            (when (get-text-property pos property)
              (setq pos (or (next-single-property-change pos property)
                            (point-max))))
            (setq pos (or (text-property-not-all pos (point-max) property nil)
                          (user-error "No more blocks"))))
        (let ((p pos) found)
          (while (and (not found) (> p (point-min)))
            (setq p (or (previous-single-property-change p property)
                        (point-min)))
            (when (and (get-text-property p property) (< p pos))
              (setq found p)))
          (setq pos (or found (user-error "No more blocks"))))))
    (goto-char pos)))

(defun pdf-reflow-next-block (&optional n)
  "Move to the start of the Nth next block."
  (interactive "p")
  (pdf-reflow--move (or n 1) 'pdf-reflow-element))

(defun pdf-reflow-previous-block (&optional n)
  "Move to the start of the Nth previous block."
  (interactive "p")
  (pdf-reflow--move (- (or n 1)) 'pdf-reflow-element))

(defun pdf-reflow-next-heading (&optional n)
  "Move to the Nth next heading."
  (interactive "p")
  (pdf-reflow--move (or n 1) 'pdf-reflow-heading))

(defun pdf-reflow-previous-heading (&optional n)
  "Move to the Nth previous heading."
  (interactive "p")
  (pdf-reflow--move (- (or n 1)) 'pdf-reflow-heading))

(defun pdf-reflow--buffer-name (buffer)
  "Return the name of the reflow buffer of the PDF in BUFFER."
  (format "*PDF Reflow %s*" (buffer-name buffer)))

;;;###autoload
(defun pdf-reflow (&optional buffer refresh)
  "Show the text of the PDF in BUFFER reflowed in reading order.

BUFFER defaults to the current buffer.  The text is shown in a
buffer of its own, at the current page of the PDF, in another
window.  For a tagged PDF it is read from the structure tree, with
headings, paragraphs, lists, tables and the alternative text of
figures; for one that is not tagged it is the text of each page in
the order poppler reads it, which is a guess.

The reflow buffer is made once and reused; with a prefix argument,
or if REFRESH is non-nil, it is made again.  In it,
\\<pdf-reflow-mode-map>\\[pdf-reflow-show-block] shows the block at \
point in the PDF.

Return the reflow buffer."
  (interactive (list nil current-prefix-arg))
  (let* ((buffer (or buffer (current-buffer)))
         (name (pdf-reflow--buffer-name buffer))
         (reflow (get-buffer name))
         (window (get-buffer-window buffer))
         page)
    (pdf-util-assert-pdf-buffer buffer)
    (setq page (and window (pdf-view-current-page window)))
    (unless (and reflow (not refresh)
                 (eq (buffer-local-value 'pdf-a11y--pdf-buffer reflow)
                     buffer))
      (setq reflow (get-buffer-create name))
      (with-current-buffer reflow
        (pdf-reflow--render buffer)
        (goto-char (point-min))))
    (with-current-buffer reflow
      (when page
        (pdf-reflow-goto-page page)))
    (when (called-interactively-p 'any)
      (let ((reflow-window (display-buffer reflow
                                           '(nil (inhibit-same-window . t)))))
        (set-window-point reflow-window (with-current-buffer reflow (point)))
        (select-window reflow-window)))
    reflow))

(provide 'pdf-reflow)

;;; pdf-reflow.el ends here
