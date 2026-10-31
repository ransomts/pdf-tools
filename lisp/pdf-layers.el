;;; pdf-layers.el --- Show and hide layers of PDF documents -*- lexical-binding: t -*-

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
;; Layers, or optional content groups (OCGs), are parts of a PDF that
;; a viewer may show or hide: the layers of a CAD drawing or a map,
;; or the answers of an exam.  `pdf-layers' lists the layers of the
;; current document as a tree, in which `pdf-layers-toggle' shows or
;; hides the layer at point.  Showing a layer of a radio button group
;; hides the other layers of its group.
;;
;; Which layers are shown is state of the document opened by the
;; epdfinfo server, not of the file: it is never saved, and it is
;; reset to the document's defaults when the document is closed,
;; e.g. by reverting or saving its buffer.

(require 'pdf-view)
(require 'pdf-info)
(require 'pdf-cache)
(require 'pdf-util)
(require 'tablist)
(require 'cl-lib)
(require 'let-alist)

;;; Code:

(defgroup pdf-layers nil
  "Show and hide the layers of PDF documents."
  :group 'pdf-tools)

(defcustom pdf-layers-display-buffer-action
  '((display-buffer-reuse-window
     display-buffer-pop-up-window)
    (inhibit-same-window . t))
  "Display action used when displaying the list of layers."
  :type display-buffer--action-custom-type)

(defcustom pdf-layers-indent 2
  "The indentation per level of the tree in the list of layers."
  :type 'natnum)

(defface pdf-layers-label
  '((t (:inherit bold)))
  "Face for labels, i.e. entries that only group layers.")

(defvar pdf-layers-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-c C-l") #'pdf-layers)
    kmap)
  "Keymap used in `pdf-layers-minor-mode'.")

;;;###autoload
(define-minor-mode pdf-layers-minor-mode
  "Show and hide the layers of a PDF document.

Layers (optional content groups) are parts of a document that may
be shown or hidden, like the layers of a CAD drawing or map.  List
them with \\[pdf-layers].  Which layers are shown is not saved to
the file; reverting the buffer resets it.

\\{pdf-layers-minor-mode-map}"
  :group 'pdf-layers
  (pdf-util-assert-pdf-buffer))

(defvar-local pdf-layers-buffer nil
  "The buffer listing the layers of this PDF buffer.")

(defvar-local pdf-layers-document-buffer nil
  "The PDF buffer whose layers this buffer lists.")

(defun pdf-layers--assert-supported ()
  "Signal an error if the server cannot show and hide layers."
  (unless (pdf-info-layers-p)
    (error "Layers are not supported by this version of epdfinfo, \
rebuild it with `pdf-tools-install'")))

(defun pdf-layers--document-buffer ()
  "Return the PDF buffer the current command applies to."
  (cond
   ((derived-mode-p 'pdf-layers-list-mode)
    (unless (buffer-live-p pdf-layers-document-buffer)
      (error "The PDF buffer of this list was killed"))
    pdf-layers-document-buffer)
   (t
    (pdf-util-assert-pdf-buffer)
    (current-buffer))))

(defun pdf-layers-redisplay (&optional buffer)
  "Redisplay PDF BUFFER after its layers changed.

Clear the images cached for the document and redisplay it, in
BUFFER and in every other buffer showing the same file, since the
server keeps one state per file.  Update their lists of layers.
BUFFER defaults to the current buffer."
  (let ((file (with-current-buffer (or buffer (current-buffer))
                (pdf-view-buffer-file-name))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'pdf-view-mode)
                   (equal (pdf-view-buffer-file-name) file))
          (pdf-cache-clear-images)
          (pdf-view-redisplay t)
          (pdf-layers--update-list buf))))))

;; * ================================================================== *
;; * The list of layers
;; * ================================================================== *

(defvar pdf-layers-list-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "RET") #'pdf-layers-toggle)
    (define-key kmap (kbd "SPC") #'pdf-layers-toggle)
    (define-key kmap (kbd "t") #'pdf-layers-toggle)
    (define-key kmap (kbd "+") #'pdf-layers-show)
    (define-key kmap (kbd "-") #'pdf-layers-hide)
    kmap)
  "Keymap used in `pdf-layers-list-mode'.")

(defun pdf-layers--entry (layer index)
  "Return a `tabulated-list-entries' entry for LAYER.

INDEX is LAYER's position in the list, which identifies a label."
  (let-alist layer
    (list (or .id (intern (format "label-%d" index)))
          (vector
           (cond
            ((null .id) "")
            (.radio-group (if .visible "(*)" "( )"))
            (t (if .visible "[x]" "[ ]")))
           (concat (make-string (* pdf-layers-indent (1- .depth)) ?\s)
                   (if .id
                       .title
                     (propertize .title 'face 'pdf-layers-label)))))))

(defun pdf-layers-list-entries ()
  "Return the layers of the PDF buffer as `tabulated-list-entries'."
  (unless (buffer-live-p pdf-layers-document-buffer)
    (error "The PDF buffer of this list was killed"))
  (let ((index 0))
    (mapcar (lambda (layer)
              (pdf-layers--entry layer (cl-incf index)))
            (pdf-info-layers pdf-layers-document-buffer))))

(define-derived-mode pdf-layers-list-mode tablist-mode "Layers"
  "Major mode for the list of layers of a PDF document.

The list shows the document's layers as a tree.  Layers marked
\[x] are shown, those marked [ ] hidden.  Layers marked (*) and
\( ) belong to a radio button group, of which at most one layer
is shown.  Entries in bold are labels grouping the layers below
them.

Which layers are shown is not saved to the file; reverting the
PDF buffer resets it.

\\{pdf-layers-list-mode-map}"
  ;; The order of the entries is the tree, so they are not sortable.
  (setq tabulated-list-format [("Shown" 5 nil :read-only t)
                               ("Layer" 40 nil :read-only t)]
        tabulated-list-entries #'pdf-layers-list-entries
        tabulated-list-padding 2)
  (set-keymap-parent pdf-layers-list-mode-map tablist-mode-map)
  (use-local-map pdf-layers-list-mode-map)
  (tabulated-list-init-header))

;;;###autoload
(defun pdf-layers (&optional no-select-window-p)
  "List the layers of the current PDF document.

The list shows the layers (optional content groups) as a tree,
see `pdf-layers-list-mode'.  Which layers are shown is not saved
to the file; reverting the PDF buffer resets it.

If the document has no layers, just say so.

With NO-SELECT-WINDOW-P non-nil, display the list but do not
select its window."
  (interactive "P")
  (pdf-util-assert-pdf-buffer)
  (pdf-layers--assert-supported)
  (let ((buffer (current-buffer)))
    (cond
     ((null (pdf-info-layers))
      (message "%s has no layers (optional content groups)"
               (buffer-name))
      nil)
     (t
      (let ((list-buffer
             (with-current-buffer
                 (get-buffer-create
                  (format "*%s's layers*"
                          (file-name-sans-extension (buffer-name))))
               (delay-mode-hooks
                 (unless (derived-mode-p 'pdf-layers-list-mode)
                   (pdf-layers-list-mode))
                 (setq pdf-layers-document-buffer buffer)
                 (tabulated-list-print t))
               (run-mode-hooks)
               (current-buffer))))
        (setq pdf-layers-buffer list-buffer)
        (add-hook 'pdf-info-close-document-hook
                  #'pdf-layers--schedule-update nil t)
        (let ((window (display-buffer list-buffer
                                      pdf-layers-display-buffer-action)))
          (unless no-select-window-p
            (select-window window)))
        list-buffer)))))

(defun pdf-layers--update-list (&optional buffer)
  "Update the list of layers of PDF BUFFER, if any.

BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (buffer-live-p pdf-layers-buffer)
      (with-current-buffer pdf-layers-buffer
        (when (buffer-live-p pdf-layers-document-buffer)
          (tabulated-list-print t))))))

(defun pdf-layers--schedule-update ()
  "Update the list of layers once the document was closed.

The server resets the layers when it closes the document, e.g.
when the buffer is reverted.  This runs in
`pdf-info-close-document-hook', where reopening the document
would be premature (it may be about to be killed), so update the
list a moment later."
  (let ((buffer (current-buffer)))
    (run-at-time 0 nil (lambda ()
                         (when (buffer-live-p buffer)
                           (pdf-layers--update-list buffer))))))

;; * ================================================================== *
;; * Showing and hiding layers
;; * ================================================================== *

(defun pdf-layers--read-layer (prompt buffer)
  "Read a layer of PDF BUFFER using PROMPT and return its alist."
  (let* ((layers (cl-remove-if-not
                  (lambda (layer) (alist-get 'id layer))
                  (pdf-info-layers buffer)))
         (candidates
          (mapcar (lambda (layer)
                    (let-alist layer
                      (cons (format "%s (%d, %s)" .title .id
                                    (if .visible "shown" "hidden"))
                            layer)))
                  layers)))
    (unless layers
      (user-error "%s has no layers" (buffer-name buffer)))
    (cdr (assoc (completing-read prompt candidates nil t)
                candidates))))

(defun pdf-layers--layer-at-point (buffer)
  "Return the alist of the layer at point in a list of layers.

BUFFER is the PDF buffer of the list."
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No layer at point"))
    (unless (natnump id)
      (user-error "Not a layer, but a label"))
    (or (cl-find id (pdf-info-layers buffer)
                 :key (lambda (layer) (alist-get 'id layer)))
        (error "No such layer: %d" id))))

(defun pdf-layers--layer-interactively (verb)
  "Return the layer a command shall act on.

In a list of layers, it is the layer at point; in a PDF buffer, it
is read with a prompt starting with VERB."
  (let ((buffer (pdf-layers--document-buffer)))
    (pdf-layers--assert-supported)
    (if (derived-mode-p 'pdf-layers-list-mode)
        (pdf-layers--layer-at-point buffer)
      (pdf-layers--read-layer (format "%s layer: " verb) buffer))))

(defun pdf-layers-set-visibility (id visible &optional buffer)
  "Show (VISIBLE non-nil) or hide the layer ID of PDF BUFFER.

ID is a layer's number as returned by `pdf-info-layers'.  Showing
a layer of a radio button group hides the others of its group.
Redisplay the document and return its layers like
`pdf-info-layers' does.  BUFFER defaults to the current buffer.

This is not saved to the file; reverting BUFFER resets it."
  (let* ((buffer (or buffer (current-buffer)))
         (layers (pdf-info-setlayer id visible buffer)))
    (pdf-layers-redisplay buffer)
    layers))

(defun pdf-layers--set-interactively (layer visible)
  "Set the visibility of LAYER to VISIBLE and say so."
  (let-alist layer
    (pdf-layers-set-visibility .id visible (pdf-layers--document-buffer))
    (message "%s layer %s" (if visible "Showing" "Hiding") .title)))

(defun pdf-layers-toggle (layer)
  "Show LAYER if it is hidden, and hide it otherwise.

Interactively, in a list of layers, LAYER is the one at point; in
a PDF buffer, read it in the minibuffer.  Showing a layer of a
radio button group hides the others of its group."
  (interactive (list (pdf-layers--layer-interactively "Toggle")))
  (pdf-layers--set-interactively
   layer (not (alist-get 'visible layer))))

(defun pdf-layers-show (layer)
  "Show LAYER, see `pdf-layers-toggle'."
  (interactive (list (pdf-layers--layer-interactively "Show")))
  (pdf-layers--set-interactively layer t))

(defun pdf-layers-hide (layer)
  "Hide LAYER, see `pdf-layers-toggle'."
  (interactive (list (pdf-layers--layer-interactively "Hide")))
  (pdf-layers--set-interactively layer nil))

;;;###autoload
(defun pdf-layers-perform-ocg-action (link &optional buffer)
  "Perform the action of LINK, an `ocg-state' link of BUFFER.

Such a link shows, hides or toggles some layers.  LINK is an alist
as returned by `pdf-info-pagelinks'.  Redisplay the document and
return its layers like `pdf-info-layers' does."
  (let* ((buffer (or buffer (current-buffer)))
         (layers (pdf-info-ocgstate (alist-get 'page link)
                                    (alist-get 'index link)
                                    buffer)))
    (pdf-layers-redisplay buffer)
    layers))

(provide 'pdf-layers)

;;; pdf-layers.el ends here
