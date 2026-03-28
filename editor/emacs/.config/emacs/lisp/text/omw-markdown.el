;;; omw-markdown.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: markdown, polymode
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Markdown mode configuration with Claude Code integration.

;;; Code:

;; ============================================================================
(defcustom omw/markdown-fill-column 150
  "Text fill-column width for Markdown buffers.
Controls both `fill-column' (hard wrap) and `visual-fill-column-width'
(soft wrap guide).  Increase for wide monitors; decrease for narrow ones."
  :type 'integer
  :group 'omw-emacs)

;; ============================================================================
(defcustom omw/markdown-colors '((header . "#46dcb0")
                                 (code-bg . "#293134")
                                 (code-fg . "#e0e2e4"))
  "Colors for Markdown syntax highlighting.
Includes header foreground color and code block background/foreground colors."
  :type 'alist
  :group 'omw-emacs)

(defconst omw/markdown-h1-height 1.30
  "Height scale for level-1 Markdown headers.")

(defconst omw/markdown-h2-height 1.20
  "Height scale for level-2 Markdown headers.")

(defconst omw/markdown-h3-height 1.15
  "Height scale for level-3 Markdown headers.")

(defconst omw/markdown-h4-height 1.1
  "Height scale for level-4 Markdown headers.")

(defconst omw/markdown-h5-height 1.05
  "Height scale for level-5 Markdown headers.")

(defconst omw/markdown-h6-height 1.0
  "Height scale for level-6 Markdown headers.")

(defun omw/markdown-faces-remap ()
  "Remap markdown buffer faces."
  (let* ((header-color (cdr (assq 'header omw/markdown-colors)))
         (code-bg (cdr (assq 'code-bg omw/markdown-colors)))
         (code-fg (cdr (assq 'code-fg omw/markdown-colors))))
    ;; Header faces - progressive size reduction for hierarchy
    (face-remap-add-relative 'markdown-header-face-1
     `(:foreground ,header-color :weight bold :height ,omw/markdown-h1-height))
    (face-remap-add-relative 'markdown-header-face-2
     `(:foreground ,header-color :weight bold :height ,omw/markdown-h2-height))
    (face-remap-add-relative 'markdown-header-face-3
     `(:foreground ,header-color :weight bold :height ,omw/markdown-h3-height))
    (face-remap-add-relative 'markdown-header-face-4
     `(:foreground ,header-color :weight bold :height ,omw/markdown-h4-height))
    (face-remap-add-relative 'markdown-header-face-5
     `(:foreground ,header-color :weight bold :height ,omw/markdown-h5-height))
    (face-remap-add-relative 'markdown-header-face-6
     `(:foreground ,header-color :weight bold :height ,omw/markdown-h6-height))
    ;; Code block faces - custom background and foreground
    (face-remap-add-relative 'markdown-pre-face
     `(:foreground ,code-fg :background ,code-bg :extend t))
    (face-remap-add-relative 'markdown-code-face
     `(:foreground ,code-fg :background ,code-bg :extend t))
    (face-remap-add-relative 'markdown-inline-code-face
     `(:foreground ,code-fg))))

;; ============================================================================
(use-package valign
  :ensure t
  :defer t
  :when (display-graphic-p)
  :hook (markdown-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;; ============================================================================
(use-package markdownfmt
  :ensure t
  :defer t)

;; ============================================================================
(use-package visual-fill-column
  :ensure t
  :defer t
  :config
  (setq visual-fill-column-enable-sensible-window-split t))

;; ============================================================================
(use-package olivetti
  :ensure t
  :defer t
  :hook (markdown-mode . olivetti-mode))

;; ============================================================================
(defun omw/markdown-align-all-tables ()
  "Align all markdown tables in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^|.*|" nil t)
      (goto-char (match-beginning 0))
      (when (markdown-table-at-point-p)
        (markdown-table-align))
      (forward-line 1))))

(defun omw/markdown-mode-setup ()
  "Apply custom settings for markdown mode."
  (require 'visual-fill-column)

  ;; Visual fill column configuration
  (setq-local fill-column omw/markdown-fill-column
              visual-fill-column-width omw/markdown-fill-column
              visual-fill-column-center-text t)

  ;; Markdown-specific settings
  (setq-local markdown-enable-math t
              markdown-hide-urls t)
  ;; Enable auto fill and visual fill column modes
  (auto-fill-mode 1)
  (visual-fill-column-mode 1)
  ;; Align tables
  (if (display-graphic-p)
      (valign-mode 1)
    (omw/markdown-align-all-tables))
  ;; Remap markdown faces
  (omw/markdown-faces-remap))

(use-package markdown-mode
  :ensure t
  :defer t
  :hook (markdown-mode . omw/markdown-mode-setup)
  :bind (:map markdown-mode-map
              ("M-n" . nil)
              ("M-p" . nil))
  :config
  (setq markdown-command "pandoc -s --mathjax --from=gfm"
        markdown-display-remote-images t
        markdown-enable-wiki-links t
        markdown-indent-on-enter 'indent-and-new-item
        markdown-fontify-code-blocks-natively t))

;; ============================================================================
;;; Provide features
(provide 'omw-markdown)

;;; omw-markdown.el ends here
