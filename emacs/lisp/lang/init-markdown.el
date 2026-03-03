;;; init-markdown.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 22:17:22 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: markdown, polymode
;; Dependencies: init-funcs

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
;; Valign - table alignment display
;; Provides visual alignment for Org and Markdown tables
(use-package valign
  :ensure t
  :defer t
  :hook (markdown-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;; ==================================================================================
;; Markdownfmt - code formatting
(use-package markdownfmt
  :ensure t
  :defer t)

;; ==================================================================================
(use-package visual-fill-column
  :ensure t
  :defer t
  :hook (markdown-mode . my/visual-fill-column-setup)
  :config
  ;; Define per-mode setup function
  (defun my/visual-fill-column-setup ()
    "Initialize visual-fill-column settings for markdown-mode (per buffer)."
    ;; Center text block for better writing experience
    (setq-local visual-fill-column-center-text t)
    ;; Avoid conflicts with auto-fill-mode
    (setq-local visual-fill-column-enable-sensible-window-split t)
    (visual-fill-column-mode 1)))

;; ==================================================================================
;; Markdown mode - main mode
(use-package markdown-mode
  :ensure t
  :defer t
  :hook (markdown-mode . my/markdown-mode-setup)
  :bind
  (:map markdown-mode-map
        ("M-n" . nil)
        ("M-p" . nil))
  :config
  (defun my/markdown-mode-setup ()
    "Setup markdown mode."
    (setq-local fill-column 120)
    (auto-fill-mode 1))

  ;; Customize variables - Claude Code compatibility optimization
  (setq markdown-command "pandoc -s --mathjax --from=gfm"
        markdown-enable-math t
        markdown-display-remote-images t
        markdown-enable-wiki-links t
        markdown-indent-on-enter 'indent-and-new-item
        markdown-hide-urls t
        markdown-fontify-code-blocks-natively t))

;; ==================================================================================
;; Markdown theme beautification
;; Based on Mou Sublime theme colors
(defgroup markdown-mou-theme nil
  "Mou Sublime style markdown rendering."
  :group 'markdown)

;; Mou Sublime theme colors (dark editor style)
(defcustom markdown-mou-colors
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
  "Colors for Mou Sublime theme markdown rendering."
  :type 'alist
  :group 'markdown-mou-theme)

(defun markdown-mou-apply-faces ()
  "Apply Mou Sublime style faces to markdown buffer."
  (let* ((colors markdown-mou-colors)
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
    ;; Header styles - Mou Sublime style, unified cyan-green
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

;; Add to markdown-mode-hook
(add-hook 'markdown-mode-hook #'markdown-mou-apply-faces)

;; ==================================================================================
;;; Provide features
(provide 'init-markdown)

;;; init-markdown.el ends here
