;;; omw-cc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:25:23 Friday by zhengyu.li>
;;
;; ============================================================================
;; omw-cc.el - C/C++ mode with clangd and Google style.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: c, cpp
;; Dependencies: omw-prog, omw-utils
;;
;; Copyright (C) 2026 zhengyu li
;;
;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.
;;
;;; Commentary:
;;
;; C/C++ mode configuration with clangd LSP and Google code style.
;; LSP server (clangd) is configured in prog.el.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; C/C++
;; ----------------------------------------------------------------------------

;; --- google-c-style ---
(use-package google-c-style
  :ensure t
  :defer t
  :hook ((c-mode . google-set-c-style)
         (c++-mode . google-set-c-style)))

;; --- Cc Tool Specs ---
(defconst omw/cc-tool-specs
  '(("clangd" "brew install llvm" "brew"))
  "Tool specs for C/C++ development.
clangd is provided by Homebrew llvm (in Brewfile).
Run `brew install llvm' if clangd is missing.")

;; --- Cc Mode Setup ---
(defun omw/cc-mode-setup ()
  "Check required C/C++ development tools and prompt to install if missing."
  (apply #'omw/tools-check-and-prompt omw/cc-tool-specs))

(use-package cc-mode
  :ensure nil
  :defer t
  :hook ((c-mode . omw/cc-mode-setup)
         (c++-mode . omw/cc-mode-setup)))

;; ============================================================================
;;; Provide features
(provide 'omw-cc)

;;; omw-cc.el ends here
