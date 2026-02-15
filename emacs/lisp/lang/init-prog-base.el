;;; init-prog-base.el -*- lexical-binding: t; -*-
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
;; Base programming configuration: smartparens, rainbow-delimiters,
;; flycheck, format-all, lsp-mode, etc.

;;; Code:

;; ==================================================================================
;; Utility function
(defun jump-to-matched-paren ()
  "Jump to the matched parenthese."
  (interactive)
  (cond ((looking-at "[ \t]*[[\"({]")
         (forward-sexp)
         (backward-char))
        ((or (looking-at "[]\")}]")
             (looking-back "[]\")}][ \t]*" nil))
         (if (< (point) (point-max))
             (forward-char))
         (backward-sexp))
        (t (message "couldn't find matched paren"))))

;; ==================================================================================
;; Smartparens - 括号自动配对
(use-package smartparens
  :defer t
  :hook (prog-mode . smartparens-mode)
  :custom
  (sp-highlight-pair-overlay nil)        ; 禁用高亮配对，减少视觉干扰
  (sp-highlight-wrap-overlay nil)        ; 禁用 wrap 高亮
  (sp-highlight-wrap-tag-overlay nil)    ; 禁用 tag wrap 高亮
  (sp-cancel-autoskip-on-backward-movement nil)  ; 允许向后跳过
  :config
  (require 'smartparens-config))

;; ==================================================================================
;; Hungry delete
(use-package hungry-delete
  :defer t
  :hook (prog-mode . hungry-delete-mode))

;; ==================================================================================
;; Rainbow delimiters
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ==================================================================================
;; Highlight TODO
(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-highlight-punctuation ":")    ; 高亮冒号
  :config
  ;; 自定义 TODO 关键词颜色
  (setq hl-todo-keyword-faces
        '(("TODO" . "#FF0000")
          ("FIXME" . "#FF0000")
          ("DEBUG" . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB" . "#1E90FF")
          ("NOTE" . "#90EE90")
          ("HACK" . "#FFD700"))))

;; ==================================================================================
;; Flycheck - 语法检查
(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-indication-mode 'left-margin)  ; 错误显示在左边距
  (flycheck-check-syntax-automatically '(save idle-change))  ; 保存和空闲时检查
  (flycheck-idle-change-delay 0.5)         ; 空闲 0.5 秒后检查
  (flycheck-display-errors-delay 0.3)      ; 0.3 秒后显示错误
  (flycheck-highlighting-mode 'symbols)    ; 高亮符号级别
  :config
  ;; 定义错误级别 fringe 位图
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0]))

;; ==================================================================================
;; Whitespace cleanup
(use-package whitespace-cleanup-mode
  :defer t
  :hook (prog-mode . whitespace-cleanup-mode))

;; ==================================================================================
;; Quickrun
(use-package quickrun
  :defer t)

;; ==================================================================================
;; Dumb jump
(use-package dumb-jump
  :defer t
  :config
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;; ==================================================================================
;; Format all - 代码格式化
;; 注意: 大文件不自动格式化，避免卡顿
(defvar format-all--max-file-size (* 500 1024)  ; 500KB
  "Maximum file size for auto-formatting on save.")

(defun my/format-all-mode-maybe ()
  "Enable format-all-mode only for reasonably sized files."
  (when (< (buffer-size) format-all--max-file-size)
    (format-all-mode 1)))

(use-package format-all
  :defer t
  :hook (prog-mode . my/format-all-mode-maybe)
  :config
  ;; 手动格式化快捷键
  (lazy-set-key
   '(("C-c f" . format-all-buffer))
   prog-mode-map))

;; ==================================================================================
;; Devdocs
(use-package devdocs
  :defer t)

;; ==================================================================================
;; Eglot - 轻量级 LSP 客户端 (Emacs 29+ 内置)
(use-package eglot
  :ensure nil  ; 内置包
  :defer t
  :hook ((python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (dockerfile-mode . eglot-ensure)
         (cmake-mode . eglot-ensure))
  :config
  (setq eglot-sync-connect 1
        eglot-autoshutdown t
        eglot-ignored-server-capabilities '(:documentHighlightProvider))

  ;; 配置各语言的 LSP 服务器
  ;; Python: pip install python-lsp-server[all]
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pylsp")))

  ;; Go: go install golang.org/x/tools/gopls@latest
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls")))

  ;; C/C++: Xcode Command Line Tools 或 LLVM 提供 clangd
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd")))

  ;; Haskell: ghcup install hls
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))

  ;; YAML: npm install -g yaml-language-server
  (add-to-list 'eglot-server-programs
               '((yaml-mode yaml-ts-mode) . ("yaml-language-server" "--stdio")))

  ;; Bash: npm install -g bash-language-server
  (add-to-list 'eglot-server-programs
               '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))

  ;; Dockerfile: npm install -g dockerfile-language-server-nodejs
  (add-to-list 'eglot-server-programs
               '(dockerfile-mode . ("docker-lang-server" "--stdio")))

  ;; CMake: pip install cmake-language-server
  (add-to-list 'eglot-server-programs
               '((cmake-mode cmake-ts-mode) . ("cmake-language-server"))))

;; ==================================================================================
;; Programming mode keybindings
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Set tab width with 4 white spaces
            (setq-local tab-width 4)
            ;; Disable tab characters for indentation
            (setq-local indent-tabs-mode nil)
            ;; Enable linum mode
            (display-line-numbers-mode 1)
            ;; Enable show paren mode
            (show-paren-local-mode 1)
            ;; Enable prettify symbol mode
            (prettify-symbols-mode 1)
            ;; Enable hide show mode
            (hs-minor-mode 1)
            ;; Disable emojify mode
            (emojify-mode -1)))

;; Keybindings for prog-mode
(with-eval-after-load 'prog-mode
  (lazy-set-key
   '(("<return>" . newline-and-indent)
     ("RET" . newline-and-indent)
     ("C-c M-a" . beginning-of-defun)
     ("C-c M-e" . end-of-defun)
     ("C-]" . jump-to-matched-paren)
     ("C-c C-c" . comment-line)
     ("M-r" . xref-find-references)
     ("M-." . xref-find-definitions)
     ("M-," . xref-pop-marker-stack)
     ("C-x C-;" . quickrun)
     ("C-h C-d" . devdocs-lookup)
     ("C-h C-s" . devdocs-search))
   prog-mode-map))

;; ==================================================================================
;;; Provide features
(provide 'init-prog-base)

;;; init-prog-base.el ends here
