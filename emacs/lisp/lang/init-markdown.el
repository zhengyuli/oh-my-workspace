;;; init-markdown.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025 zhengyu li
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
(use-package valign
  :defer t
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
;; Impatient mode - 实时预览
(use-package impatient-mode
  :defer t)

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
              ("C-c C-f" . markdownfmt-format-buffer)       ; 格式化
              ("C-c C-p" . markdown-live-preview))          ; 实时预览
  :config
  (defun markdown-impatient-mode-filter (buffer)
    "Markdown filter for impatient-mode with glow-style CSS"
    (princ
     (with-temp-buffer
       (let ((tmpname (buffer-name)))
         (set-buffer buffer)
         (set-buffer (markdown tmpname))
         (format
          "<!DOCTYPE html>
           <html>
             <head>
               <title>Markdown Preview (Glow Style)</title>
               <meta charset='utf-8'>
               <meta name='viewport' content='width=device-width, initial-scale=1'>
               <link rel='stylesheet'
                     href='https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/atom-one-dark.min.css'>
               <style>
                 /* Glow-style dark theme */
                 :root {
                   --bg-primary: #0d1117;
                   --bg-secondary: #161b22;
                   --text-primary: #e6edf3;
                   --text-secondary: #8b949e;
                   --accent-red: #ff7b72;
                   --accent-orange: #ffa657;
                   --accent-yellow: #d2a8ff;
                   --accent-green: #7ee787;
                   --accent-blue: #79c0ff;
                   --accent-purple: #d2a8ff;
                   --accent-cyan: #a5d6ff;
                   --code-bg: #161b22;
                   --border-color: #30363d;
                 }

                 * {
                   box-sizing: border-box;
                 }

                 body {
                   background: var(--bg-primary);
                   color: var(--text-primary);
                   font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
                   font-size: 16px;
                   line-height: 1.6;
                   margin: 0;
                   padding: 0;
                 }

                 .markdown-body {
                   max-width: 900px;
                   margin: 0 auto;
                   padding: 48px 24px;
                 }

                 /* Headers with glow colors */
                 h1 {
                   color: var(--accent-red);
                   font-size: 2.25em;
                   font-weight: 700;
                   border-bottom: 2px solid var(--border-color);
                   padding-bottom: 0.3em;
                   margin-bottom: 0.5em;
                 }

                 h2 {
                   color: var(--accent-orange);
                   font-size: 1.75em;
                   font-weight: 600;
                   border-bottom: 1px solid var(--border-color);
                   padding-bottom: 0.3em;
                 }

                 h3 { color: var(--accent-yellow); font-size: 1.4em; font-weight: 600; }
                 h4 { color: var(--accent-green); font-size: 1.2em; font-weight: 600; }
                 h5 { color: var(--accent-blue); font-size: 1.1em; font-weight: 600; }
                 h6 { color: var(--accent-purple); font-size: 1em; font-weight: 600; font-style: italic; }

                 /* Links */
                 a {
                   color: var(--accent-blue);
                   text-decoration: none;
                   font-weight: 500;
                 }

                 a:hover {
                   text-decoration: underline;
                 }

                 /* Code blocks */
                 pre {
                   background: var(--code-bg);
                   border: 1px solid var(--border-color);
                   border-radius: 8px;
                   padding: 16px;
                   overflow-x: auto;
                   font-family: 'SF Mono', 'Menlo', 'Monaco', 'Courier New', monospace;
                   font-size: 0.9em;
                 }

                 code {
                   font-family: 'SF Mono', 'Menlo', 'Monaco', 'Courier New', monospace;
                 }

                 /* Inline code */
                 :not(pre) > code {
                   background: rgba(56, 139, 253, 0.15);
                   color: var(--accent-cyan);
                   padding: 0.2em 0.4em;
                   border-radius: 4px;
                   font-size: 0.9em;
                 }

                 /* Blockquotes */
                 blockquote {
                   border-left: 4px solid var(--accent-blue);
                   color: var(--text-secondary);
                   background: var(--bg-secondary);
                   padding: 16px 20px;
                   margin: 16px 0;
                   border-radius: 0 8px 8px 0;
                 }

                 blockquote p {
                   margin: 0;
                 }

                 /* Lists */
                 ul, ol {
                   padding-left: 2em;
                 }

                 li {
                   margin: 0.25em 0;
                 }

                 li::marker {
                   color: var(--accent-cyan);
                   font-weight: bold;
                 }

                 /* Tables */
                 table {
                   border-collapse: collapse;
                   width: 100%;
                   margin: 16px 0;
                 }

                 th, td {
                   border: 1px solid var(--border-color);
                   padding: 10px 14px;
                   text-align: left;
                 }

                 th {
                   background: var(--bg-secondary);
                   color: var(--accent-orange);
                   font-weight: 600;
                 }

                 tr:nth-child(even) {
                   background: var(--bg-secondary);
                 }

                 /* Horizontal rule */
                 hr {
                   border: none;
                   height: 2px;
                   background: linear-gradient(to right,
                     transparent,
                     var(--border-color) 20%,
                     var(--border-color) 80%,
                     transparent);
                   margin: 32px 0;
                 }

                 /* Bold and italic */
                 strong { color: var(--accent-red); font-weight: 700; }
                 em { color: var(--accent-purple); font-style: italic; }

                 /* Images */
                 img {
                   max-width: 100%;
                   border-radius: 8px;
                   border: 1px solid var(--border-color);
                 }

                 /* Footnotes */
                 .footnotes {
                   border-top: 1px solid var(--border-color);
                   padding-top: 1em;
                   margin-top: 2em;
                   font-size: 0.9em;
                   color: var(--text-secondary);
                 }

                 /* Source line numbers */
                 .lineno {
                   color: var(--text-secondary);
                   margin-right: 1em;
                 }

                 /* Task lists */
                 input[type='checkbox'] {
                   margin-right: 0.5em;
                   accent-color: var(--accent-green);
                 }
               </style>
             </head>
             <body>
               <article class='markdown-body'>
                 %s
               </article>
               <script src='https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js'></script>
               <script id='MathJax-script' src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>
               <script>
                 hljs.highlightAll();
               </script>
             </body>
           </html>" (buffer-string))))
     (current-buffer)))

  (defun markdown-live-preview ()
    "Markdown live preview."
    (interactive)
    (imp-visit-buffer))

  (defun markdown-mode-setup ()
    "Setup markdown mode."
    ;; Enable impatient mode
    (impatient-mode 1)
    ;; Setup user defined impatient mode filter
    (imp-set-user-filter 'markdown-impatient-mode-filter)
    ;; Set buffer column width to 120
    (setq-local fill-column 120)
    ;; Enable auto fill mode
    (auto-fill-mode 1)
    ;; Enable valign mode
    (valign-mode 1))

  ;; Customize variables - Claude Code 兼容性优化
  (setq markdown-live-preview-window-function 'pop-to-buffer
        markdown-command "pandoc -s --mathjax --from=gfm"  ; GFM 支持
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

  ;; 高亮当前行代码块
  (add-hook 'markdown-mode-hook
            (lambda ()
              (when (fboundp 'hl-line-mode)
                (hl-line-mode 1)))))

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
;; Glow 风格美化渲染
;; 自定义 face 配色，模拟终端 glow 工具的效果
(defgroup markdown-glow nil
  "Glow-style markdown rendering."
  :group 'markdown)

;; Glow 暗色主题配色
(defcustom markdown-glow-colors
  '((header-1 . "#ff6b6b")      ; 红色 - 一级标题
    (header-2 . "#ffa94d")      ; 橙色 - 二级标题
    (header-3 . "#ffd43b")      ; 黄色 - 三级标题
    (header-4 . "#69db7c")      ; 绿色 - 四级标题
    (header-5 . "#4dabf7")      ; 蓝色 - 五级标题
    (header-6 . "#da77f2")      ; 紫色 - 六级标题
    (code-bg . "#1a1b26")       ; 代码块背景
    (code-fg . "#a9b1d6")       ; 代码块前景
    (inline-code-bg . "#3d59a1") ; 行内代码背景
    (inline-code-fg . "#c0caf5") ; 行内代码前景
    (link . "#7aa2f7")          ; 链接
    (url . "#565f89")           ; URL
    (bold . "#f7768e")          ; 粗体
    (italic . "#bb9af7")        ; 斜体
    (blockquote . "#565f89")    ; 引用
    (list-marker . "#7dcfff")   ; 列表标记
    (hr . "#414868")            ; 分隔线
    (metadata . "#565f89"))     ; 元数据
  "Colors for glow-style markdown rendering."
  :type 'alist
  :group 'markdown-glow)

(defun markdown-glow-apply-faces ()
  "Apply glow-style faces to markdown buffer."
  (let* ((colors markdown-glow-colors)
         (h1 (cdr (assq 'header-1 colors)))
         (h2 (cdr (assq 'header-2 colors)))
         (h3 (cdr (assq 'header-3 colors)))
         (h4 (cdr (assq 'header-4 colors)))
         (h5 (cdr (assq 'header-5 colors)))
         (h6 (cdr (assq 'header-6 colors)))
         (code-bg (cdr (assq 'code-bg colors)))
         (code-fg (cdr (assq 'code-fg colors)))
         (inline-code-bg (cdr (assq 'inline-code-bg colors)))
         (inline-code-fg (cdr (assq 'inline-code-fg colors)))
         (link (cdr (assq 'link colors)))
         (url (cdr (assq 'url colors)))
         (bold (cdr (assq 'bold colors)))
         (italic (cdr (assq 'italic colors)))
         (blockquote (cdr (assq 'blockquote colors)))
         (list-marker (cdr (assq 'list-marker colors)))
         (hr (cdr (assq 'hr colors)))
         (metadata (cdr (assq 'metadata colors))))

    ;; 标题样式 - 大小递减，颜色不同
    (face-remap-add-relative 'markdown-header-face-1
                             `(:foreground ,h1 :weight ultra-bold :height 1.8 :underline t))
    (face-remap-add-relative 'markdown-header-face-2
                             `(:foreground ,h2 :weight bold :height 1.5))
    (face-remap-add-relative 'markdown-header-face-3
                             `(:foreground ,h3 :weight bold :height 1.3))
    (face-remap-add-relative 'markdown-header-face-4
                             `(:foreground ,h4 :weight semi-bold :height 1.2))
    (face-remap-add-relative 'markdown-header-face-5
                             `(:foreground ,h5 :weight semi-bold :height 1.1))
    (face-remap-add-relative 'markdown-header-face-6
                             `(:foreground ,h6 :weight normal :height 1.0 :slant italic))

    ;; 代码块样式
    (face-remap-add-relative 'markdown-code-face
                             `(:foreground ,code-fg :background ,code-bg
                               :extend t :family "Source Code Pro"))
    (face-remap-add-relative 'markdown-pre-face
                             `(:foreground ,code-fg :background ,code-bg :extend t))
    (face-remap-add-relative 'markdown-inline-code-face
                             `(:foreground ,inline-code-fg :background ,inline-code-bg
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

    ;; 表格
    (face-remap-add-relative 'markdown-table-face
                             '(:family "Source Code Pro"))))

;; 添加到 markdown-mode-hook
(add-hook 'markdown-mode-hook #'markdown-glow-apply-faces)

;; ==================================================================================
;; 可视化增强 - 类似 glow 的视觉效果
(use-package visual-fill-column
  :defer t
  :hook (markdown-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)          ; 内容宽度
  (visual-fill-column-center-text t))     ; 居中显示

;; 混合字体 - 标题用变宽字体，代码用等宽字体
(use-package mixed-pitch
  :defer t
  :hook (markdown-mode . mixed-pitch-mode)
  :custom
  (mixed-pitch-set-height t)
  (mixed-pitch-fixed-font "Source Code Pro")
  (mixed-pitch-variable-font "Times New Roman"))

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
