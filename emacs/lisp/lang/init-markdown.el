;;; init-markdown.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-08 23:27:14 Sunday by zhengyuli>

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
(defcustom omw/markdown-colors '((header . "#46dcb0")
                                 (code-bg . "#293134")
                                 (code-fg . "#e0e2e4"))
  "Colors markdown rendering."
  :type 'alist
  :group 'omw/emacs-config)

(defun omw/markdown-faces-remap ()
  "Remap markdown buffer faces."
  (let* ((header-color (cdr (assq 'header omw/markdown-colors)))
         (code-bg (cdr (assq 'code-bg omw/markdown-colors)))
         (code-fg (cdr (assq 'code-fg omw/markdown-colors))))
    ;; Header faces
    (face-remap-add-relative 'markdown-header-face-1 `(:foreground ,header-color :weight bold :height 1.30))
    (face-remap-add-relative 'markdown-header-face-2 `(:foreground ,header-color :weight bold :height 1.20))
    (face-remap-add-relative 'markdown-header-face-3 `(:foreground ,header-color :weight bold :height 1.15))
    (face-remap-add-relative 'markdown-header-face-4 `(:foreground ,header-color :weight bold :height 1.1))
    (face-remap-add-relative 'markdown-header-face-5 `(:foreground ,header-color :weight bold :height 1.05))
    (face-remap-add-relative 'markdown-header-face-6 `(:foreground ,header-color :weight bold :height 1.0))
    ;; Code block faces
    (face-remap-add-relative 'markdown-pre-face `(:foreground ,code-fg :background ,code-bg :extend t))
    (face-remap-add-relative 'markdown-code-face `(:foreground ,code-fg :background ,code-bg :extend t))
    (face-remap-add-relative 'markdown-inline-code-face `(:foreground ,code-fg))))

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
(use-package visual-fill-column
  :ensure t
  :defer t
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
  (require 'visual-fill-column)

  (setq-local fill-column 150
              visual-fill-column-width fill-column
              visual-fill-column-center-text t
              markdown-enable-math t
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

;; ==================================================================================
;;; Provide features
(provide 'init-markdown)

;;; init-markdown.el ends here
