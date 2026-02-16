;;; init-markdown.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-16 19:22:15 Monday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none

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

;; ==================================================================================
;; Valign - 表格对齐显示
;; 提供 Org 和 Markdown 表格的可视化对齐
(use-package valign
  :hook (markdown-mode . valign-mode)
  :custom
  (valign-fancy-bar t))                  ; 使用 fancy 分隔符

;; ==================================================================================
;; Markdownfmt - 代码格式化
(use-package markdownfmt
  :defer t
  :config
  ;; Format on save (optional, can be toggled)
  (defun markdownfmt-enable-on-save ()
    "Enable markdownfmt on save."
    (add-hook 'before-save-hook #'markdownfmt-format-buffer nil t)))

;; ==================================================================================
;; Edit indirect - 在独立 buffer 中编辑代码块
;; 支持 ```lang 代码块的原生模式编辑
(use-package edit-indirect
  :defer t
  :custom
  (edit-indirect-guess-mode-function
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
;; Markdown mode - 主模式
(use-package markdown-mode
  :defer t
  :hook (markdown-mode . markdown-mode-setup)
  :bind (:map markdown-mode-map
              ("C-c C-e" . edit-indirect-region-or-buffer)  ; 编辑代码块
              ("C-c C-t" . markdown-toc-generate-or-refresh-toc)  ; 生成 TOC
              ("C-c C-f" . markdownfmt-format-buffer))      ; 格式化
  :config
  (defun markdown-mode-setup ()
    "Setup markdown mode."
    ;; Set buffer column width to 120
    (setq-local fill-column 120)
    ;; Enable auto fill mode
    (auto-fill-mode 1))

  ;; Customize variables - Claude Code 兼容性优化
  (setq markdown-command "pandoc -s --mathjax --from=gfm"  ; GFM 支持
        markdown-enable-math t
        markdown-display-remote-images t
        markdown-enable-wiki-links t               ; Wiki 链接支持
        markdown-underline-distinctive-headers t   ; 下划线区分标题
        markdown-asymmetric-header t               ; 非对称标题 (# 不闭合)
        markdown-indent-on-enter 'indent-and-new-item  ; 回车自动缩进
        markdown-hide-urls t                       ; 隐藏 URL (类似 org-mode)
        markdown-fontify-code-blocks-natively t    ; 原生高亮代码块
        markdown-gfm-uppercase-radio t             ; GFM 无线电列表
        markdown-list-item-bullets '("-" "*" "+")) ; 列表符号

  ;; 代码块字体锁定优化
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

  ;; 高亮当前行
  (add-hook 'markdown-mode-hook #'hl-line-mode))

;; ==================================================================================
;; Markdown TOC - 目录生成
(use-package markdown-toc
  :defer t
  :after markdown-mode
  :custom
  (markdown-toc-header-toc-start "<!-- TOC start -->")
  (markdown-toc-header-toc-end "<!-- TOC end -->"))

;; ==================================================================================
;; GFM (GitHub Flavored Markdown) 模式 - Claude Code 兼容
(use-package gfm-mode
  :ensure nil  ; 包含在 markdown-mode 中
  :defer t
  :after markdown-mode
  :config
  ;; GFM 特定设置
  (setq gfm-enable-wiki-links t
        gfm-enable-math t))

;; ==================================================================================
;; Poly-markdown - 多模式支持（可选，用于复杂场景）
;; 允许在 markdown 中嵌入其他语言的完整支持
(use-package poly-markdown
  :defer t
  :after markdown-mode
  :hook (poly-markdown-mode . polymode-update-mode-line))

;; ==================================================================================
;; Markdown 主题美化
;; 基于 Mou Sublime 主题配色
(defgroup markdown-mou-theme nil
  "Mou Sublime style markdown rendering."
  :group 'markdown)

;; Mou Sublime 主题配色 (深色编辑器风格)
(defcustom markdown-mou-colors
  '((header-1 . "#46dcb0")      ; 一级标题 - 青绿色
    (header-2 . "#46dcb0")      ; 二级标题
    (header-3 . "#46dcb0")      ; 三级标题
    (header-4 . "#46dcb0")      ; 四级标题
    (header-5 . "#46dcb0")      ; 五级标题
    (header-6 . "#46dcb0")      ; 六级标题
    (code-bg . "#293134")       ; 代码块背景 - 深灰蓝
    (code-fg . "#e0e2e4")       ; 代码块前景 - 灰白色
    (inline-code-fg . "#e0e2e4") ; 行内代码前景 - 灰白色
    (link . "#79b6e8")          ; 链接 - 蓝色
    (url . "#888888")           ; URL - 灰色
    (bold . "#ff7a52")          ; 粗体 - 橙红色
    (italic . "#ffab52")        ; 斜体 - 橙色
    (blockquote . "#777777")    ; 引用 - 灰色
    (list-marker . "#75e349")   ; 列表标记 - 绿色
    (hr . "#586e75")            ; 分隔线
    (metadata . "#93a1a1")      ; 元数据 - 灰色
    (table-bg . "#293134")      ; 表格背景 - 深灰蓝
    (table-fg . "#e0e2e4"))     ; 表格前景 - 灰白色
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

    ;; 标题样式 - Mou Sublime 风格，统一青绿色
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

    ;; 代码块样式
    (face-remap-add-relative 'markdown-code-face
                             `(:foreground ,code-fg :background ,code-bg
                               :extend t :family "Source Code Pro"))
    (face-remap-add-relative 'markdown-pre-face
                             `(:foreground ,code-fg :background ,code-bg :extend t))
    (face-remap-add-relative 'markdown-inline-code-face
                             `(:foreground ,inline-code-fg
                               :family "Source Code Pro"))

    ;; 链接样式
    (face-remap-add-relative 'markdown-link-face
                             `(:foreground ,link :weight semi-bold :underline t))
    (face-remap-add-relative 'markdown-url-face
                             `(:foreground ,url :slant italic))
    (face-remap-add-relative 'markdown-reference-face
                             `(:foreground ,link :weight semi-bold))

    ;; 强调样式
    (face-remap-add-relative 'markdown-bold-face
                             `(:foreground ,bold :weight ultra-bold))
    (face-remap-add-relative 'markdown-italic-face
                             `(:foreground ,italic :slant italic))
    (face-remap-add-relative 'markdown-bolditalic-face
                             `(:foreground ,bold :weight ultra-bold :slant italic))

    ;; 引用块样式
    (face-remap-add-relative 'markdown-blockquote-face
                             `(:foreground ,blockquote :slant italic :extend t))
    (face-remap-add-relative 'markdown-markup-face
                             `(:foreground ,blockquote))

    ;; 列表样式
    (face-remap-add-relative 'markdown-list-face
                             `(:foreground ,list-marker :weight bold))
    (face-remap-add-relative 'markdown-footnote-marker-face
                             `(:foreground ,list-marker :weight bold))

    ;; 分隔线
    (face-remap-add-relative 'markdown-hr-face
                             `(:foreground ,hr :strike-through t :height 1.5))

    ;; 元数据 (YAML front matter)
    (face-remap-add-relative 'markdown-metadata-key-face
                             `(:foreground ,metadata :weight bold))
    (face-remap-add-relative 'markdown-metadata-value-face
                             `(:foreground ,metadata :slant italic))

    ;; 表格样式 - 深色背景 + 灰白前景 + 等宽字体
    (let ((mono-font "Source Code Pro"))
      (face-remap-add-relative 'markdown-table-face
                               `(:foreground ,table-fg :background ,table-bg :family ,mono-font :extend t))
      (face-remap-add-relative 'markdown-table-delimiter-face
                               `(:foreground ,table-fg :background ,table-bg :family ,mono-font)))))

;; 添加到 markdown-mode-hook
(add-hook 'markdown-mode-hook #'markdown-mou-apply-faces)

;; ==================================================================================
;; 可视化增强
(use-package visual-fill-column
  :defer t
  :hook (markdown-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)          ; 内容宽度
  (visual-fill-column-center-text t))     ; 居中显示

;; ==================================================================================
;; 自动关联文件扩展名
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("CHANGELOG\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("CONTRIBUTING\\.md\\'" . gfm-mode))

;; ==================================================================================
;; LSP 支持 (via marksman) - 可选
;; 安装: brew install marksman (macOS)
(defun my/markdown-setup-lsp ()
  "Setup LSP for markdown mode if marksman is available."
  (when (and (executable-find "marksman")
             (featurep 'eglot))
    (eglot-ensure)))

(add-hook 'markdown-mode-hook #'my/markdown-setup-lsp)

;; ==================================================================================
;;; Provide features
(provide 'init-markdown)

;;; init-markdown.el ends here
