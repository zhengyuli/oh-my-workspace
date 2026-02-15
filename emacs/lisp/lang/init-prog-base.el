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
;; Smartparens
(use-package smartparens
  :defer t
  :hook (prog-mode . smartparens-mode)
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
  :hook (prog-mode . hl-todo-mode))

;; ==================================================================================
;; Flycheck
(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-indication-mode 'left-margin))

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
;; Format all
(use-package format-all
  :defer t
  :hook (prog-mode . format-all-mode))

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
