;;; omw-go.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:20:51 Friday by zhengyu.li>
;;
;; ============================================================================
;; omw-go.el - Go mode with gopls LSP.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: go, golang
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
;; Go mode configuration with LSP support via gopls.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Go Configuration
;; ----------------------------------------------------------------------------

;; --- Go Tool Specs ---
(defconst omw/go-tool-specs
  '(("gopls" "go install golang.org/x/tools/gopls@latest" "go")
    ("gofumpt" "go install mvdan.cc/gofumpt@latest" "go"))
  "Tool specs for Go development.")

(defun omw/install-go-tools ()
  "Install Go tools (gopls, gofumpt) via go install if not present."
  (interactive)
  (apply #'omw/tools-install omw/go-tool-specs))

;; --- Go Mode Setup ---
(defun omw/go-mode-setup ()
  "Check required Go development tools and prompt to install if missing."
  (apply #'omw/tools-check-and-prompt omw/go-tool-specs))

(use-package go-mode
  :ensure t
  :defer t
  :hook (go-mode . omw/go-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-go)

;;; omw-go.el ends here
