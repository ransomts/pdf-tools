;;; pdf-a11y.el --- Review the accessibility of tagged PDFs. -*- lexical-binding: t -*-

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
;; `pdf-a11y-report' lists what a person has to judge in a tagged PDF
;; and a conformance checker like veraPDF cannot: whether the reading
;; order makes sense, whether the headings form a sensible outline
;; and whether figures have real alternative text rather than a file
;; name or nothing.  It also shows whether the document is tagged at
;; all, its language, pages without tagged content and fonts that are
;; not embedded.
;;
;; Every figure, heading and reading order entry in the report is a
;; button: RET (or a click) shows it in the PDF, highlighting its
;; area when one is known.  The server reads an area only for
;; block-level elements with a BBox; for headings and paragraphs the
;; area is found by searching the element's text on its page, and
;; figures are only located by page.

(require 'pdf-info)
(require 'pdf-util)
(require 'pdf-view)
(require 'pdf-cache)
(require 'let-alist)
(require 'cl-lib)
(require 'subr-x)

;;; Code:



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-a11y nil
  "Review the accessibility of tagged PDFs."
  :group 'pdf-tools)

(defcustom pdf-a11y-file-name-regexp
  (concat "\\.\\(?:png\\|jpe?g\\|gif\\|svg\\|pdf\\|eps\\|ps\\|tiff?"
          "\\|bmp\\|webp\\|heic\\|tex\\|tikz\\)\\'")
  "Regexp matching alternative text that ends like a file name.

Alternative text without any whitespace that contains an
underscore, a slash or a backslash is treated as a file name as
well, see `pdf-a11y-alt-text-problem'."
  :group 'pdf-a11y
  :type 'regexp)

(defcustom pdf-a11y-generic-alt-texts
  '("image" "img" "figure" "fig" "picture" "pic" "graphic" "graphics"
    "photo" "chart" "diagram" "logo" "icon" "placeholder" "untitled"
    "alt" "alt text")
  "Alternative texts that describe nothing.

They are compared case-insensitively, ignoring a trailing number
like in \"Figure 3\"."
  :group 'pdf-a11y
  :type '(repeat string))

(defcustom pdf-a11y-highlight-color '("white" "steel blue" 0.35)
  "How to highlight an element in the PDF.

A list (FOREGROUND BACKGROUND ALPHA) as used by
`pdf-info-renderpage-highlight'."
  :group 'pdf-a11y
  :type '(list (color :tag "Foreground")
               (color :tag "Background")
               (number :tag "Alpha")))

(defface pdf-a11y-problem
  '((t (:inherit error)))
  "Face for problems in the accessibility report."
  :group 'pdf-a11y)

(defface pdf-a11y-ok
  '((t (:inherit success)))
  "Face for things that look fine in the accessibility report."
  :group 'pdf-a11y)

(defface pdf-a11y-heading
  '((t (:inherit bold)))
  "Face for section headings of the accessibility report."
  :group 'pdf-a11y)



;; * ================================================================== *
;; * Analysis
;; * ================================================================== *

(defconst pdf-a11y--tag-names
  '((annot . "Annot") (article . "Art") (bibentry . "BibEntry")
    (blockquote . "BlockQuote") (caption . "Caption") (code . "Code")
    (div . "Div") (document . "Document") (figure . "Figure")
    (form . "Form") (formula . "Formula") (heading . "H")
    (heading-1 . "H1") (heading-2 . "H2") (heading-3 . "H3")
    (heading-4 . "H4") (heading-5 . "H5") (heading-6 . "H6")
    (index . "Index") (link . "Link") (list . "L")
    (list-body . "LBody") (list-item . "LI") (list-label . "Lbl")
    (nonstruct . "NonStruct") (note . "Note") (paragraph . "P")
    (part . "Part") (private . "Private") (quote . "Quote")
    (reference . "Reference") (ruby . "Ruby") (section . "Sect")
    (span . "Span") (table . "Table") (table-body . "TBody")
    (table-data . "TD") (table-footer . "TFoot")
    (table-header . "THead") (table-heading . "TH")
    (table-row . "TR") (toc . "TOC") (toc-item . "TOCI")
    (warichu . "Warichu"))
  "Map element kinds returned by `pdf-info-structure' to PDF tags.")

(defconst pdf-a11y--grouping-kinds
  '(document part article section div nonstruct private index toc
             list blockquote)
  "Element kinds that only group others and are not read themselves.")

(defconst pdf-a11y--illustration-kinds '(figure formula form)
  "Element kinds that need alternative text and are always listed.")

(defun pdf-a11y-tag-name (kind)
  "Return the PDF structure tag for element KIND, a symbol."
  (or (cdr (assq kind pdf-a11y--tag-names))
      (capitalize (symbol-name kind))))

(defun pdf-a11y-heading-level (kind)
  "Return the heading level of element KIND, or nil if it is no heading.

An unnumbered heading (H) has level 0."
  (cond
   ((eq kind 'heading) 0)
   ((string-match "\\`heading-\\([1-6]\\)\\'" (symbol-name kind))
    (string-to-number (match-string 1 (symbol-name kind))))))

(defun pdf-a11y-alt-text-problem (alt)
  "Return what is wrong with alternative text ALT, or nil.

ALT is nil if the element has no alternative text at all.  The
result is one of the symbols `missing', `empty', `file-name' (ALT
matches `pdf-a11y-file-name-regexp', or has no whitespace but an
underscore or a path separator) and `generic' (ALT is one of
`pdf-a11y-generic-alt-texts').

Text that passes may still be wrong: only a person can tell if it
describes the figure."
  (let ((text (and alt (string-trim alt)))
        (case-fold-search t))
    (cond
     ((null alt) 'missing)
     ((equal text "") 'empty)
     ((or (string-match-p pdf-a11y-file-name-regexp text)
          (and (not (string-match-p "[[:space:]]" text))
               (string-match-p "[_/\\]" text)))
      'file-name)
     ((member (downcase
               (replace-regexp-in-string "[[:space:][:punct:][:digit:]]+\\'"
                                         "" text))
              pdf-a11y-generic-alt-texts)
      'generic))))

(defun pdf-a11y--reading-order (elements)
  "Return the entries of a reading order list for ELEMENTS.

ELEMENTS is the flattened structure tree as returned by
`pdf-info-structure'.  Grouping elements are left out, and so is
everything nested in an element already listed, except for
figures and other illustrations.  Each entry is a cons (LEVEL
. ELEMENT), where LEVEL counts the listed ancestors of ELEMENT.

An entry whose page is lower than that of an earlier entry gets
an additional key AFTER-PAGE, the highest page read before it.
Screen readers follow this order, so such an element is read
out of place (a float that moved, say)."
  (let (stack result (max-page 0))
    (dolist (elt elements)
      (let-alist elt
        (while (and stack (>= (car stack) .depth))
          (pop stack))
        (when (and (not (memq .kind pdf-a11y--grouping-kinds))
                   (or (null stack)
                       (memq .kind pdf-a11y--illustration-kinds)))
          (when (null stack)
            (when (and (> .page 0) (< .page max-page))
              (setq elt (cons (cons 'after-page max-page) elt)))
            (setq max-page (max max-page .page)))
          (push (cons (length stack) elt) result)
          (push .depth stack))))
    (nreverse result)))

(defun pdf-a11y--headings (elements)
  "Return the headings in ELEMENTS with their problems.

Each heading's alist gets a key PROBLEM, which is nil or one of
`no-text', `first-not-h1' (the first numbered heading is
not H1) and `skipped-level' (e.g. H2 follows H4 fine, but H3 must
not follow H1)."
  (let (result previous)
    (dolist (elt elements)
      (let ((level (pdf-a11y-heading-level (alist-get 'kind elt))))
        (when level
          (let-alist elt
            (push (cons
                   (cons 'problem
                         (cond
                          ((equal "" (string-trim (or .actual-text .text "")))
                           'no-text)
                          ((and (> level 0) (null previous) (> level 1))
                           'first-not-h1)
                          ((and (> level 0) previous (> level (1+ previous)))
                           'skipped-level)))
                   elt)
                  result))
          (when (> level 0)
            (setq previous level)))))
    (nreverse result)))

(defun pdf-a11y--xmp-value (xmp regexp)
  "Return the first group of REGEXP matched in XMP, or nil."
  (and xmp (string-match regexp xmp)
       (string-trim (match-string 1 xmp))))

(defun pdf-a11y-analyze (&optional file-or-buffer)
  "Collect the accessibility information of FILE-OR-BUFFER.

Return an alist with the following keys.

FILE -- The document's file name.
PAGES -- The number of pages.
TITLE -- The document title from its metadata, or nil.
TAGGED -- t if it has a structure tree, nil if not, or `unknown'
  if the server cannot read structure trees.
ELEMENTS -- The structure tree, see `pdf-info-structure'.
LANGUAGE -- The XMP dc:language, or nil.
LANGUAGES -- The other languages set on elements, as a list of
  (LANG . COUNT).
UA-PART -- The PDF/UA part claimed in XMP (a string), or nil.
UNTAGGED-PAGES -- Pages with no tagged content (if tagged).
FIGURES -- Figures and other illustrations, each with a PROBLEM
  key, see `pdf-a11y-alt-text-problem'.
HEADINGS -- See `pdf-a11y--headings'.
READING-ORDER -- See `pdf-a11y--reading-order'.
FONTS -- Fonts that are not embedded, see `pdf-info-fonts'."
  (let* ((pages (pdf-info-number-of-pages file-or-buffer))
         (structure-p (pdf-info-structure-tree-p))
         (elements (and structure-p (pdf-info-structure file-or-buffer)))
         (xmp (pdf-info-xmp-metadata file-or-buffer))
         (title (alist-get 'title (pdf-info-metadata file-or-buffer)))
         (fonts (cl-remove-duplicates
                 (cl-remove-if (lambda (f) (alist-get 'embedded f))
                               (pdf-info-fonts file-or-buffer))
                 :test #'equal))
         languages tagged-pages untagged-pages figures)
    (dolist (elt elements)
      (let-alist elt
        (when (> .page 0)
          (push .page tagged-pages))
        (when .lang
          (let ((cell (assoc .lang languages)))
            (if cell
                (cl-incf (cdr cell))
              (push (cons .lang 1) languages))))
        (when (memq .kind pdf-a11y--illustration-kinds)
          (push (cons (cons 'problem (pdf-a11y-alt-text-problem .alt))
                      elt)
                figures))))
    (when elements
      (dotimes (i pages)
        (unless (memq (1+ i) tagged-pages)
          (push (1+ i) untagged-pages))))
    `((file . ,(pdf-info--normalize-file-or-buffer file-or-buffer))
      (pages . ,pages)
      (title . ,(and title (not (equal "" title)) title))
      (tagged . ,(cond ((not structure-p) 'unknown)
                       (elements t)))
      (elements . ,elements)
      (language
       . ,(pdf-a11y--xmp-value
           xmp (concat "<dc:language>\\(?:.\\|\n\\)*?<rdf:li[^>]*>"
                       "\\([^<]*\\)</rdf:li>")))
      (languages . ,(nreverse languages))
      (ua-part
       . ,(pdf-a11y--xmp-value
           xmp "pdfuaid:part\\(?:>\\|=[\"']\\)\\([0-9]+\\)"))
      (untagged-pages . ,(nreverse untagged-pages))
      (figures . ,(nreverse figures))
      (headings . ,(pdf-a11y--headings elements))
      (reading-order . ,(pdf-a11y--reading-order elements))
      (fonts . ,fonts))))



;; * ================================================================== *
;; * Showing elements in the PDF
;; * ================================================================== *

(defvar-local pdf-a11y--pdf-buffer nil
  "The PDF buffer a report buffer describes.")

(defun pdf-a11y--search-candidates (text)
  "Return strings to search for to locate TEXT on its page."
  (let* ((text (string-trim text))
         ;; Search at most the first few words; long text wraps over
         ;; lines and hyphenates, which page search may not match.
         (words (split-string text "[[:space:]]+" t))
         (short (mapconcat #'identity (cl-subseq words 0 (min 6 (length words)))
                           " "))
         ;; Section numbers are often laid out apart from the title.
         (unnumbered (and (string-match "\\`[0-9.]+[[:space:]]+\\(.+\\)" short)
                          (match-string 1 short))))
    (delete-dups (delq nil (list short unnumbered)))))

(defun pdf-a11y--text-edges (element buffer)
  "Return the edges of ELEMENT's text in BUFFER as a list, or nil.

This is a guess: it is the first match of the start of the
element's text on its page."
  (let-alist element
    (unless (memq .kind pdf-a11y--illustration-kinds)
      (let ((text (or .actual-text .text ""))
            (case-fold-search t)
            edges)
        (when (>= (length (string-trim text)) 2)
          (cl-loop for candidate in (pdf-a11y--search-candidates text)
                   until edges
                   do (let ((matches (ignore-errors
                                       (pdf-info-search-string
                                        candidate .page buffer))))
                        (setq edges (alist-get 'edges (car matches))))))
        edges))))

(defun pdf-a11y--highlight (page edges window)
  "Highlight the list of relative EDGES on PAGE in WINDOW."
  (let ((size (pdf-view-image-size nil window)))
    (pdf-view-display-image
     (pdf-view-create-image
         (pdf-cache-renderpage-highlight
          page (car size)
          (append pdf-a11y-highlight-color edges))
       :map (pdf-view-apply-hotspot-functions window page size)
       :width (car size))
     (when pdf-view-roll-minor-mode page)
     window)
    (pdf-util-scroll-to-edges
     (pdf-util-scale-relative-to-pixel (car edges)))))

(defun pdf-a11y-show-element (element &optional no-select)
  "Show structure ELEMENT in its PDF buffer.

Go to ELEMENT's page and highlight its area if it is known.
Select the PDF's window, unless NO-SELECT is non-nil."
  (let ((buffer pdf-a11y--pdf-buffer)
        window)
    (unless (buffer-live-p buffer)
      (user-error "The PDF buffer of this report is gone"))
    (setq window (display-buffer buffer))
    (with-selected-window window
      (let-alist element
        (if (<= .page 0)
            (message "This element is not associated with a page")
          (pdf-view-goto-page .page)
          (let ((edges (if .edges
                           (list .edges)
                         (pdf-a11y--text-edges element buffer))))
            (if edges
                (pdf-a11y--highlight .page edges window)
              (message "Page %d (the element's area is unknown)"
                       .page))))))
    (unless no-select
      (select-window window))))

(define-button-type 'pdf-a11y-element
  'follow-link t
  'help-echo "mouse-2, RET: Show this element in the PDF"
  'action (lambda (button)
            (pdf-a11y-show-element (button-get button 'pdf-a11y-element))))

(defun pdf-a11y-display-element ()
  "Show the element at point in the PDF without selecting it."
  (interactive)
  (let ((button (button-at (point))))
    (unless button
      (user-error "No element at point"))
    (pdf-a11y-show-element (button-get button 'pdf-a11y-element) t)))



;; * ================================================================== *
;; * The report
;; * ================================================================== *

(defvar pdf-a11y-report-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "n") #'forward-button)
    (define-key map (kbd "p") #'backward-button)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "C-o") #'pdf-a11y-display-element)
    (define-key map (kbd "SPC") #'pdf-a11y-display-element)
    map)
  "Keymap used in `pdf-a11y-report-mode'.")

(define-derived-mode pdf-a11y-report-mode special-mode "PDF A11y"
  "Major mode for the accessibility report of a PDF.

Every figure, heading and reading order entry is a button that
shows the element in the PDF.

\\{pdf-a11y-report-mode-map}"
  (setq-local revert-buffer-function #'pdf-a11y--revert-report)
  (setq truncate-lines t))

(defun pdf-a11y--revert-report (&rest _)
  "Recreate the report of the current buffer."
  (unless (buffer-live-p pdf-a11y--pdf-buffer)
    (user-error "The PDF buffer of this report is gone"))
  (pdf-a11y--render (pdf-a11y-analyze pdf-a11y--pdf-buffer)
                    pdf-a11y--pdf-buffer))

(defun pdf-a11y--excerpt (string &optional width)
  "Return STRING shortened to WIDTH (default 60) characters."
  (truncate-string-to-width (or string "") (or width 60) nil nil "..."))

(defun pdf-a11y--insert-heading (title &optional count)
  "Insert a report section TITLE, with COUNT if non-nil."
  (insert "\n" (propertize (if count (format "%s (%d)" title count) title)
                           'face 'pdf-a11y-heading)
          "\n"))

(defun pdf-a11y--insert-field (label value &optional face)
  "Insert a summary line LABEL: VALUE, with VALUE in FACE."
  (insert (format "  %-20s " (concat label ":"))
          (if face (propertize value 'face face) value)
          "\n"))

(defun pdf-a11y--insert-element (element label &optional width)
  "Insert a button showing ELEMENT, labelled LABEL.

Pad it with spaces to WIDTH characters."
  (insert-text-button label
                      'type 'pdf-a11y-element
                      'pdf-a11y-element element)
  (when (and width (> width (length label)))
    (insert (make-string (- width (length label)) ?\s))))

(defun pdf-a11y--problem-string (problem)
  "Return a short description of PROBLEM, a symbol."
  (cl-case problem
    (missing "no alt text")
    (empty "empty alt text")
    (file-name "alt text is a file name")
    (generic "alt text describes nothing")
    (no-text "heading has no text")
    (first-not-h1 "first heading is not H1")
    (skipped-level "skips a level")
    (t (symbol-name problem))))

(defun pdf-a11y--alt-description (element)
  "Return a description of ELEMENT's alternative text."
  (let-alist element
    (cond
     ((null .alt) "no /Alt")
     ((equal "" (string-trim .alt)) "/Alt is empty")
     (t (format "alt: %S" (pdf-a11y--excerpt .alt 70))))))

(defun pdf-a11y--insert-summary (report)
  "Insert the summary part of REPORT."
  (let-alist report
    (pdf-a11y--insert-field
     "Tagged"
     (cl-case .tagged
       ((t) (format "yes, %d structure elements" (length .elements)))
       (unknown "unknown (epdfinfo cannot read structure trees; \
it needs poppler-glib >= 0.26)")
       (t "no, there is no structure tree"))
     (if (eq .tagged t) 'pdf-a11y-ok 'pdf-a11y-problem))
    (pdf-a11y--insert-field
     "PDF/UA claim" (if .ua-part (format "PDF/UA-%s (XMP)" .ua-part) "none")
     (unless .ua-part 'pdf-a11y-problem))
    (pdf-a11y--insert-field
     "Title" (or .title "none") (unless .title 'pdf-a11y-problem))
    (pdf-a11y--insert-field
     "Language" (if .language (format "%s (XMP dc:language)" .language) "none")
     (unless .language 'pdf-a11y-problem))
    (when .languages
      (pdf-a11y--insert-field
       "Language changes"
       (mapconcat (lambda (l) (format "%s (%d elements)" (car l) (cdr l)))
                  .languages ", ")))
    (when (eq .tagged t)
      (pdf-a11y--insert-field
       "Untagged pages"
       (if .untagged-pages
           (mapconcat #'number-to-string .untagged-pages ", ")
         "none")
       (if .untagged-pages 'pdf-a11y-problem 'pdf-a11y-ok)))
    (pdf-a11y--insert-field
     "Fonts not embedded"
     (if .fonts
         (mapconcat (lambda (f)
                      (let-alist f
                        (if .substitute
                            (format "%s (shown as %s)" .name .substitute)
                          .name)))
                    .fonts ", ")
       "none")
     (if .fonts 'pdf-a11y-problem 'pdf-a11y-ok))))

(defun pdf-a11y--insert-figures (figures)
  "Insert the FIGURES part of a report."
  (pdf-a11y--insert-heading "Figures" (length figures))
  (if (null figures)
      (insert "  none\n")
    (insert "  Check that each description says what the figure shows.\n")
    (dolist (fig figures)
      (let-alist fig
        (insert "  ")
        (pdf-a11y--insert-element
         fig (format "p.%d %s" .page (pdf-a11y-tag-name .kind)) 13)
        (insert " "
                (if .problem
                    (propertize (format "[%s] " (pdf-a11y--problem-string
                                                 .problem))
                                'face 'pdf-a11y-problem)
                  "")
                (pdf-a11y--alt-description fig)
                "\n")))))

(defun pdf-a11y--insert-headings (headings)
  "Insert the HEADINGS part of a report."
  (pdf-a11y--insert-heading "Headings in reading order" (length headings))
  (if (null headings)
      (insert (propertize "  none: a reader cannot navigate by heading\n"
                          'face 'pdf-a11y-problem))
    (dolist (h headings)
      (let-alist h
        (let ((level (pdf-a11y-heading-level .kind)))
          (insert "  ")
          (insert (format "p.%-3d " .page)
                  (make-string (* 2 (max 0 (1- level))) ?\s))
          (pdf-a11y--insert-element h (pdf-a11y-tag-name .kind) 3)
          (insert " " (pdf-a11y--excerpt (or .actual-text .text))
                  (if .problem
                      (propertize (format "  [%s]"
                                          (pdf-a11y--problem-string
                                           .problem))
                                  'face 'pdf-a11y-problem)
                    "")
                  "\n"))))))

(defun pdf-a11y--insert-reading-order (entries)
  "Insert the reading order ENTRIES of a report by page."
  (pdf-a11y--insert-heading "Reading order by page")
  (if (null entries)
      (insert "  nothing tagged\n")
    (insert "  The order a screen reader follows; content read away \
from its page is marked.\n")
    (let ((page -1) (n 0))
      (dolist (entry entries)
        (let ((level (car entry))
              (elt (cdr entry)))
          (let-alist elt
            (unless (= .page page)
              (setq page .page n 0)
              (insert "\n  " (propertize (if (> page 0)
                                             (format "Page %d" page)
                                           "No page")
                                         'face 'pdf-a11y-heading)
                      "\n"))
            (insert "    ")
            (if (> level 0)
                (insert (make-string (+ 4 (* 2 level)) ?\s))
              (insert (format "%3d. " (cl-incf n))))
            (pdf-a11y--insert-element elt (pdf-a11y-tag-name .kind) 7)
            (insert " "
                    (if (memq .kind pdf-a11y--illustration-kinds)
                        (pdf-a11y--alt-description elt)
                      (pdf-a11y--excerpt (or .actual-text .text)))
                    (if .after-page
                        (propertize (format "  [read after page %d]"
                                            .after-page)
                                    'face 'pdf-a11y-problem)
                      "")
                    "\n")))))))

(defun pdf-a11y--render (report pdf-buffer)
  "Render REPORT about PDF-BUFFER into the current buffer."
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (pdf-a11y-report-mode)
    (setq pdf-a11y--pdf-buffer pdf-buffer)
    (let-alist report
      (insert (propertize (format "Accessibility review of %s"
                                  (file-name-nondirectory .file))
                          'face 'pdf-a11y-heading)
              "\n"
              "A conformance checker (veraPDF) cannot judge the reading order,\n"
              "the heading outline or whether alt text is real; check those here.\n\n")
      (pdf-a11y--insert-summary report)
      (when (eq .tagged t)
        (pdf-a11y--insert-figures .figures)
        (pdf-a11y--insert-headings .headings)
        (pdf-a11y--insert-reading-order .reading-order)))
    (goto-char (min pos (point-max)))
    (set-buffer-modified-p nil)))

;;;###autoload
(defun pdf-a11y-report (&optional buffer)
  "Show an accessibility review of the PDF in BUFFER.

BUFFER defaults to the current buffer.  The report shows whether
the document is tagged, its language, untagged pages and fonts
that are not embedded, and for review by a person: the figures and
their alternative text (flagging missing, empty, generic and file
name alt text), the heading outline and the reading order of each
page.  Entries are buttons that show the element in the PDF.

Return the report buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (pdf-util-assert-pdf-buffer buffer)
    (let ((report (pdf-a11y-analyze buffer))
          (report-buffer (get-buffer-create
                          (format "*PDF A11y %s*" (buffer-name buffer)))))
      (with-current-buffer report-buffer
        (pdf-a11y--render report buffer)
        (goto-char (point-min)))
      (when (called-interactively-p 'any)
        (pop-to-buffer report-buffer))
      report-buffer)))

(provide 'pdf-a11y)

;;; pdf-a11y.el ends here
