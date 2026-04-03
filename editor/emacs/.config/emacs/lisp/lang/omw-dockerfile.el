;;; omw-dockerfile.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:19:40 Friday by zhengyu.li>
;;
;; ============================================================================
;; omw-dockerfile.el - Dockerfile mode with LSP support.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: dockerfile, docker
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
;; Dockerfile mode configuration with LSP support.
;; LSP server docker-langserver is configured in omw-prog.el.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Dockerfile Configuration
;; ----------------------------------------------------------------------------

;; --- Dockerfile Tool Specs ---
(defconst omw/dockerfile-tool-specs
  '(("docker-langserver"
     "bun install -g dockerfile-language-server-nodejs" "bun"))
  "Tool specs for Dockerfile development.")

(defun omw/install-dockerfile-tools ()
  "Install Dockerfile LSP tools (docker-langserver) via bun if not present."
  (interactive)
  (apply #'omw/tools-install omw/dockerfile-tool-specs))

;; --- Dockerfile Mode Setup ---
(defun omw/dockerfile-mode-setup ()
  "Apply custom settings for dockerfile mode."
  (setq-local dockerfile-indent-offset 2)
  (apply #'omw/tools-check-and-prompt omw/dockerfile-tool-specs))

(use-package dockerfile-mode
  :ensure t
  :defer t
  :hook (dockerfile-mode . omw/dockerfile-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-dockerfile)

;;; omw-dockerfile.el ends here
