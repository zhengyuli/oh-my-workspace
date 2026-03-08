;;; init-markdown.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-08 13:04:15 Sunday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: markdown, polymode
;; Dependencies: (none)

;; This file is not part of GNU Emacs.

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
;; Markdown mode configuration with Claude Code integration.

;;; Code:

;; ==================================================================================
(defcustom omw/markdown-colors
  '((header-1 . "#46dcb0")
    (header-2 . "#46dcb0")
    (header-3 . "#46dcb0")
    (header-4 . "#46dcb0")
    (header-5 . "#46dcb0")
    (header-6 . "#46dcb0")
    (code-bg . "#293134")
    (code-fg . "#e0e2e4")
    (hr . "#586e75")
    (url . "#888888")
    (link . "#79b6e8")
    (bold . "#ff7a52")
    (italic . "#ffab52")
    (metadata . "#93a1a1")
    (table-bg . "#293134")
    (table-fg . "#e0e2e4")
    (blockquote . "#777777")
    (list-marker . "#75e349")
    (inline-code-fg . "#e0e2e4"))
  "Colors markdown rendering."
  :type 'alist
  :group 'omw/emacs-config)

(defun omw/markdown-faces-setup ()
  "Apply faces to markdown buffer."
  (let* ((colors omw/markdown-colors)
         (h1 (cdr (assq 'header-1 colors)))
         (h2 (cdr (assq 'header-2 colors)))
         (h3 (cdr (assq 'header-3 colors)))
         (h4 (cdr (assq 'header-4 colors)))
         (h5 (cdr (assq 'header-5 colors)))
         (h6 (cdr (assq 'header-6 colors)))
         (code-bg (cdr (assq 'code-bg colors)))
         (code-fg (cdr (assq 'code-fg colors)))
         (hr (cdr (assq 'hr colors)))
         (url (cdr (assq 'url colors)))
         (link (cdr (assq 'link colors)))
         (bold (cdr (assq 'bold colors)))
         (italic (cdr (assq 'italic colors)))
         (metadata (cdr (assq 'metadata colors)))
         (table-bg (cdr (assq 'table-bg colors)))
         (table-fg (cdr (assq 'table-fg colors)))
         (blockquote (cdr (assq 'blockquote colors)))
         (list-marker (cdr (assq 'list-marker colors)))
         (inline-code-fg (cdr (assq 'inline-code-fg colors))))
    ;; Header styles
    (face-remap-add-relative 'markdown-header-face-1 `(:foreground ,h1 :weight bold :height 1.5))
    (face-remap-add-relative 'markdown-header-face-2 `(:foreground ,h2 :weight bold :height 1.35))
    (face-remap-add-relative 'markdown-header-face-3 `(:foreground ,h3 :weight bold :height 1.2))
    (face-remap-add-relative 'markdown-header-face-4 `(:foreground ,h4 :weight bold :height 1.1))
    (face-remap-add-relative 'markdown-header-face-5 `(:foreground ,h5 :weight bold :height 1.05))
    (face-remap-add-relative 'markdown-header-face-6 `(:foreground ,h6 :weight bold :height 1.0))
    ;; Code block styles
    (face-remap-add-relative 'markdown-code-face `(:foreground ,code-fg :background ,code-bg :extend t))
    (face-remap-add-relative 'markdown-pre-face `(:foreground ,code-fg :background ,code-bg :extend t))
    (face-remap-add-relative 'markdown-inline-code-face `(:foreground ,inline-code-fg))
    ;; Link styles
    (face-remap-add-relative 'markdown-link-face `(:foreground ,link :weight semi-bold :underline t))
    (face-remap-add-relative 'markdown-url-face `(:foreground ,url :slant italic))
    (face-remap-add-relative 'markdown-reference-face `(:foreground ,link :weight semi-bold))
    ;; Emphasis styles
    (face-remap-add-relative 'markdown-bold-face `(:foreground ,bold :weight ultra-bold))
    (face-remap-add-relative 'markdown-italic-face `(:foreground ,italic :slant italic))
    (face-remap-add-relative 'markdown-bolditalic-face `(:foreground ,bold :weight ultra-bold :slant italic))
    ;; Blockquote styles
    (face-remap-add-relative 'markdown-blockquote-face `(:foreground ,blockquote :slant italic :extend t))
    (face-remap-add-relative 'markdown-markup-face `(:foreground ,blockquote))
    ;; List styles
    (face-remap-add-relative 'markdown-list-face `(:foreground ,list-marker :weight bold))
    (face-remap-add-relative 'markdown-footnote-marker-face `(:foreground ,list-marker :weight bold))
    ;; Horizontal rule
    (face-remap-add-relative 'markdown-hr-face `(:foreground ,hr :strike-through t :height 1.5))
    ;; Metadata (YAML front matter)
    (face-remap-add-relative 'markdown-metadata-key-face `(:foreground ,metadata :weight bold))
    (face-remap-add-relative 'markdown-metadata-value-face `(:foreground ,metadata :slant italic))
    ;; Table styles - dark background + gray-white foreground
    (face-remap-add-relative 'markdown-table-face `(:foreground ,table-fg :background ,table-bg :extend t))
    (face-remap-add-relative 'markdown-table-delimiter-face `(:foreground ,table-fg :background ,table-bg))))

;; ==================================================================================
(use-package valign
  :ensure t
  :when (display-graphic-p)
  :defer t
  :hook (markdown-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;; ==================================================================================
(use-package markdownfmt
  :ensure t
  :defer t)

;; ==================================================================================
(defun omw/visual-fill-column-mode-setup ()
  (setq-local fill-column 150
              visual-fill-column-width fill-column
              visual-fill-column-center-text t)
  ;; Use proportional font for main text，and enlarge the text locally
  ;; (variable-pitch-mode 1)
  ;; (face-remap-add-relative 'variable-pitch :height 1.05)
  ;; Enable auto-wrap and visual centering
  (auto-fill-mode 1)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t
  :defer t
  :hook (markdown-mode . omw/visual-fill-column-mode-setup)
  :config
  (setq visual-fill-column-enable-sensible-window-split t))

;; ==================================================================================
(use-package olivetti
  :ensure t
  :defer t
  :hook (markdown-mode . olivetti-mode))

;; ==================================================================================
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
  (unless (display-graphic-p)
    (omw/markdown-align-all-tables))
  (omw/markdown-faces-setup))

(use-package markdown-mode
  :ensure t
  :defer t
  :hook (markdown-mode . omw/markdown-mode-setup)
  :bind (:map markdown-mode-map
              ("M-n" . nil)
              ("M-p" . nil))
  :config
  (setq markdown-command "pandoc -s --mathjax --from=gfm"
        markdown-enable-math t
        markdown-display-remote-images t
        markdown-enable-wiki-links t
        markdown-indent-on-enter 'indent-and-new-item
        markdown-hide-urls t
        markdown-fontify-code-blocks-natively t))

;; ==================================================================================
;;; Provide features
(provide 'init-markdown)

;;; init-markdown.el ends here
