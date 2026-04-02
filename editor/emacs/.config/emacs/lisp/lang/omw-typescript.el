;;; omw-typescript.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:39:02 Friday by zhengyu.li>
;;
;; ============================================================================
;; omw-typescript.el - TypeScript mode with LSP support.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: typescript, ts
;; Dependencies: omw-prog, omw-utils
;;
;; Copyright (C) 2026 zhengyu li
;;
;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.
;; 2026-03-19       renamed from omw-javascript.el to omw-typescript.el.
;;
;;; Commentary:
;;
;; TypeScript mode configuration with LSP support.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; TypeScript Configuration
;; ----------------------------------------------------------------------------

(defconst omw/typescript-tool-specs
  '(("typescript-language-server"
     "bun install -g typescript-language-server typescript" "bun"))
  "Tool specs for TypeScript development.")

(defun omw/install-typescript-tools ()
  "Install TypeScript LSP tools (typescript-language-server) via bun
if not present."
  (interactive)
  (apply #'omw/tools-install omw/typescript-tool-specs))

;; --- Typescript Mode Setup ---
(defun omw/typescript-mode-setup ()
  "Apply custom settings for TypeScript mode."
  (setq-local typescript-indent-level 2)
  (apply #'omw/tools-check-and-prompt omw/typescript-tool-specs))

(use-package typescript-mode
  :ensure t
  :defer t
  :hook (typescript-mode . omw/typescript-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-typescript)

;;; omw-typescript.el ends here
