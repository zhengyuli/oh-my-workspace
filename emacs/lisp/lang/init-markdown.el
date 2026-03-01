;;; init-markdown.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-16 19:22:15 Monday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
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
;; Features: GFM support, code block editing, TOC generation, LSP via marksman.

;;; Code:

(require 'init-funcs)

;; ==================================================================================
;; Valign - table alignment display
;; Provides visual alignment for Org and Markdown tables
(use-package valign
  :hook (markdown-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))                  ; Use fancy separator

;; ==================================================================================
;; Markdownfmt - code formatting
(use-package markdownfmt
  :defer t
  :config
  ;; Format on save (optional, can be toggled)
  (defun markdownfmt-enable-on-save ()
    "Enable markdownfmt on save."
    (add-hook 'before-save-hook #'markdownfmt-format-buffer nil t)))

;; ==================================================================================
;; Edit indirect - edit code blocks in separate buffer
;; Supports native mode editing for ```lang code blocks
(use-package edit-indirect
  :defer t
  :config
  (setq edit-indirect-guess-mode-function
   (lambda (_parent-buffer _beg _end)
     (let ((lang (save-excursion
                   (goto-char (marker-position edit-indirect--beg))
                   (when (re-search-forward "```\\([a-zA-Z0-9_-]+\\)" nil t)
                     (match-string 1)))))
       (when lang
         (pcase lang
           ("elisp" 'emacs-lisp-mode)
           ("emacs-lisp" 'emacs-lisp-mode)
           ("lisp" 'lisp-mode)
           ("python" 'python-mode)
           ("py" 'python-mode)
           ("shell" 'shell-script-mode)
           ("bash" 'shell-script-mode)
           ("sh" 'shell-script-mode)
           ("zsh" 'sh-mode)
           ("javascript" 'js-mode)
           ("js" 'js-mode)
           ("typescript" 'typescript-mode)
           ("ts" 'typescript-mode)
           ("go" 'go-mode)
           ("rust" 'rust-mode)
           ("c" 'c-mode)
           ("cpp" 'c++-mode)
           ("c++" 'c++-mode)
           ("java" 'java-mode)
           ("json" 'json-mode)
           ("yaml" 'yaml-mode)
           ("yml" 'yaml-mode)
           ("xml" 'xml-mode)
           ("html" 'html-mode)
           ("css" 'css-mode)
           ("sql" 'sql-mode)
           ("dockerfile" 'dockerfile-mode)
           ("docker" 'dockerfile-mode)
           ("makefile" 'makefile-mode)
           ("make" 'makefile-mode)
           ("cmake" 'cmake-mode)
           ("haskell" 'haskell-mode)
           ("ruby" 'ruby-mode)
           ("perl" 'perl-mode)
           ("php" 'php-mode)
           ("scala" 'scala-mode)
           ("kotlin" 'kotlin-mode)
           ("swift" 'swift-mode)
           ("lua" 'lua-mode)
           ("vim" 'vimrc-mode)
           (_ (intern (concat lang "-mode")))))))))

;; ==================================================================================
;; Markdown mode - main mode
(use-package markdown-mode
  :defer t
  :hook (markdown-mode . markdown-mode-setup)
  :bind (:map markdown-mode-map
              ("C-c C-e" . edit-indirect-region-or-buffer)  ; Edit code block
              ("C-c C-t" . markdown-toc-generate-or-refresh-toc)  ; Generate TOC
              ("C-c C-f" . markdownfmt-format-buffer))      ; Format
  :config
  (defun markdown-mode-setup ()
    "Setup markdown mode."
    ;; Set buffer column width to 120
    (setq-local fill-column 120)
    ;; Enable auto fill mode
    (auto-fill-mode 1))

  ;; Customize variables - Claude Code compatibility optimization
  (setq markdown-command "pandoc -s --mathjax --from=gfm"  ; GFM support
        markdown-enable-math t
        markdown-display-remote-images t
        markdown-enable-wiki-links t               ; Wiki link support
        markdown-underline-distinctive-headers t   ; Underline distinctive headers
        markdown-asymmetric-header t               ; Asymmetric header (# not closed)
        markdown-indent-on-enter 'indent-and-new-item  ; Auto indent on enter
        markdown-hide-urls t                       ; Hide URL (like org-mode)
        markdown-fontify-code-blocks-natively t    ; Native code block highlighting
        markdown-gfm-uppercase-radio t             ; GFM radio list
        markdown-list-item-bullets '("-" "*" "+")) ; List bullets

  ;; Code block font-lock optimization
  (setq markdown-code-lang-modes
        '(("elisp" . emacs-lisp-mode)
          ("emacs-lisp" . emacs-lisp-mode)
          ("lisp" . lisp-mode)
          ("python" . python-mode)
          ("py" . python-mode)
          ("shell" . shell-script-mode)
          ("bash" . shell-script-mode)
          ("sh" . sh-mode)
          ("zsh" . sh-mode)
          ("javascript" . js-mode)
          ("js" . js-mode)
          ("typescript" . typescript-mode)
          ("ts" . typescript-mode)
          ("go" . go-mode)
          ("rust" . rust-mode)
          ("c" . c-mode)
          ("cpp" . c++-mode)
          ("c++" . c++-mode)
          ("java" . java-mode)
          ("json" . json-mode)
          ("yaml" . yaml-mode)
          ("yml" . yaml-mode)
          ("xml" . xml-mode)
          ("html" . html-mode)
          ("css" . css-mode)
          ("sql" . sql-mode)
          ("dockerfile" . dockerfile-mode)
          ("docker" . dockerfile-mode)
          ("makefile" . makefile-mode)
          ("make" . makefile-mode)
          ("cmake" . cmake-mode)
          ("haskell" . haskell-mode)
          ("ruby" . ruby-mode)
          ("perl" . perl-mode)
          ("php" . php-mode)
          ("scala" . scala-mode)
          ("kotlin" . kotlin-mode)
          ("swift" . swift-mode)
          ("lua" . lua-mode)
          ("vim" . vimrc-mode)))

  ;; Highlight current line
  (add-hook 'markdown-mode-hook #'hl-line-mode))

;; ==================================================================================
;; Markdown TOC - table of contents generation
(use-package markdown-toc
  :defer t
  :after markdown-mode
  :config
  (setq markdown-toc-header-toc-start "<!-- TOC start -->"
        markdown-toc-header-toc-end "<!-- TOC end -->"))

;; ==================================================================================
;; GFM (GitHub Flavored Markdown) mode - Claude Code compatible
(use-package gfm-mode
  :ensure nil  ; Included in markdown-mode
  :defer t
  :after markdown-mode
  :config
  ;; GFM specific settings
  (setq gfm-enable-wiki-links t
        gfm-enable-math t))

;; ==================================================================================
;; Poly-markdown - multi-mode support (optional, for complex scenarios)
;; Allows embedding full language support in markdown
(use-package poly-markdown
  :defer t
  :after markdown-mode
  :hook (poly-markdown-mode . polymode-update-mode-line))

;; ==================================================================================
;; Markdown theme beautification
;; Based on Mou Sublime theme colors
(defgroup markdown-mou-theme nil
  "Mou Sublime style markdown rendering."
  :group 'markdown)

;; Mou Sublime theme colors (dark editor style)
(defcustom markdown-mou-colors
  '((header-1 . "#46dcb0")      ; Level 1 header - cyan-green
    (header-2 . "#46dcb0")      ; Level 2 header
    (header-3 . "#46dcb0")      ; Level 3 header
    (header-4 . "#46dcb0")      ; Level 4 header
    (header-5 . "#46dcb0")      ; Level 5 header
    (header-6 . "#46dcb0")      ; Level 6 header
    (code-bg . "#293134")       ; Code block background - dark gray-blue
    (code-fg . "#e0e2e4")       ; Code block foreground - gray-white
    (inline-code-fg . "#e0e2e4") ; Inline code foreground - gray-white
    (link . "#79b6e8")          ; Link - blue
    (url . "#888888")           ; URL - gray
    (bold . "#ff7a52")          ; Bold - orange-red
    (italic . "#ffab52")        ; Italic - orange
    (blockquote . "#777777")    ; Blockquote - gray
    (list-marker . "#75e349")   ; List marker - green
    (hr . "#586e75")            ; Horizontal rule
    (metadata . "#93a1a1")      ; Metadata - gray
    (table-bg . "#293134")      ; Table background - dark gray-blue
    (table-fg . "#e0e2e4"))     ; Table foreground - gray-white
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
         (inline-code-fg (cdr (assq 'inline-code-fg colors)))
         (link (cdr (assq 'link colors)))
         (url (cdr (assq 'url colors)))
         (bold (cdr (assq 'bold colors)))
         (italic (cdr (assq 'italic colors)))
         (blockquote (cdr (assq 'blockquote colors)))
         (list-marker (cdr (assq 'list-marker colors)))
         (hr (cdr (assq 'hr colors)))
         (metadata (cdr (assq 'metadata colors)))
         (table-bg (cdr (assq 'table-bg colors)))
         (table-fg (cdr (assq 'table-fg colors))))

    ;; Header styles - Mou Sublime style, unified cyan-green
    (face-remap-add-relative 'markdown-header-face-1
                             `(:foreground ,h1 :weight bold :height 1.5))
    (face-remap-add-relative 'markdown-header-face-2
                             `(:foreground ,h2 :weight bold :height 1.35))
    (face-remap-add-relative 'markdown-header-face-3
                             `(:foreground ,h3 :weight bold :height 1.2))
    (face-remap-add-relative 'markdown-header-face-4
                             `(:foreground ,h4 :weight bold :height 1.1))
    (face-remap-add-relative 'markdown-header-face-5
                             `(:foreground ,h5 :weight bold :height 1.05))
    (face-remap-add-relative 'markdown-header-face-6
                             `(:foreground ,h6 :weight bold :height 1.0))

    ;; Code block styles
    (face-remap-add-relative 'markdown-code-face
                             `(:foreground ,code-fg :background ,code-bg :extend t))
    (face-remap-add-relative 'markdown-pre-face
                             `(:foreground ,code-fg :background ,code-bg :extend t))
    (face-remap-add-relative 'markdown-inline-code-face
                             `(:foreground ,inline-code-fg))

    ;; Link styles
    (face-remap-add-relative 'markdown-link-face
                             `(:foreground ,link :weight semi-bold :underline t))
    (face-remap-add-relative 'markdown-url-face
                             `(:foreground ,url :slant italic))
    (face-remap-add-relative 'markdown-reference-face
                             `(:foreground ,link :weight semi-bold))

    ;; Emphasis styles
    (face-remap-add-relative 'markdown-bold-face
                             `(:foreground ,bold :weight ultra-bold))
    (face-remap-add-relative 'markdown-italic-face
                             `(:foreground ,italic :slant italic))
    (face-remap-add-relative 'markdown-bolditalic-face
                             `(:foreground ,bold :weight ultra-bold :slant italic))

    ;; Blockquote styles
    (face-remap-add-relative 'markdown-blockquote-face
                             `(:foreground ,blockquote :slant italic :extend t))
    (face-remap-add-relative 'markdown-markup-face
                             `(:foreground ,blockquote))

    ;; List styles
    (face-remap-add-relative 'markdown-list-face
                             `(:foreground ,list-marker :weight bold))
    (face-remap-add-relative 'markdown-footnote-marker-face
                             `(:foreground ,list-marker :weight bold))

    ;; Horizontal rule
    (face-remap-add-relative 'markdown-hr-face
                             `(:foreground ,hr :strike-through t :height 1.5))

    ;; Metadata (YAML front matter)
    (face-remap-add-relative 'markdown-metadata-key-face
                             `(:foreground ,metadata :weight bold))
    (face-remap-add-relative 'markdown-metadata-value-face
                             `(:foreground ,metadata :slant italic))

    ;; Table styles - dark background + gray-white foreground
    (face-remap-add-relative 'markdown-table-face
                             `(:foreground ,table-fg :background ,table-bg :extend t))
    (face-remap-add-relative 'markdown-table-delimiter-face
                             `(:foreground ,table-fg :background ,table-bg))))

;; Add to markdown-mode-hook
(add-hook 'markdown-mode-hook #'markdown-mou-apply-faces)

;; ==================================================================================
;; Visual enhancements
(use-package visual-fill-column
  :defer t
  :hook (markdown-mode . visual-fill-column-mode)
  :config
  (setq visual-fill-column-width 100          ; Content width
        visual-fill-column-center-text t))     ; Center display

;; ==================================================================================
;; Auto-associate file extensions
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("CHANGELOG\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("CONTRIBUTING\\.md\\'" . gfm-mode))

;; ==================================================================================
;; LSP support (via marksman) - optional
;; Install: brew install marksman (macOS)
(defun markdown-setup-lsp ()
  "Setup LSP for markdown mode if marksman is available."
  (when (and (executable-find "marksman")
             (featurep 'eglot))
    (eglot-ensure)))

(add-hook 'markdown-mode-hook #'markdown-setup-lsp)

;; ==================================================================================
;; Markdown Tools Validation
;; Markdown tools validation (optional)
(defvar optional-markdown-tools
  '((marksman . "brew install marksman")
    (pandoc . "brew install pandoc"))
  "List of optional Markdown tools.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'markdown-tools
 (lambda () (config-dependency-validate-executables optional-markdown-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-markdown)

;;; init-markdown.el ends here
