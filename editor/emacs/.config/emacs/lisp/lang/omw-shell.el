;;; omw-shell.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-23 20:11:45 Monday by zhengyu.li>
;;
;; ============================================================================
;; omw-shell.el - Shell script mode configuration.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: shell, bash, sh
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
;; Shell script mode configuration.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Shell Configuration
;; ----------------------------------------------------------------------------

;; --- Shell Tool Specs ---
(defconst omw/sh-tool-specs
  '(("bash-language-server" "bun install -g bash-language-server" "bun"))
  "Tool specs for shell script development.")

(defun omw/install-bash-tools ()
  "Install shell script LSP tools (bash-language-server) via bun
if not present."
  (interactive)
  (apply #'omw/tools-install omw/sh-tool-specs))

;; --- sh Mode Setup ---
(defun omw/sh-mode-setup ()
  "Apply custom settings for shell script mode."
  (setq-local sh-basic-offset 2)
  (setq-local sh-indent-after-continuation 'always)
  (apply #'omw/tools-check-and-prompt omw/sh-tool-specs))

(use-package sh-script
  :ensure nil
  :defer t
  :hook (sh-mode . omw/sh-mode-setup)
  :bind (:map sh-mode-map
              ("C-c C-c" . comment-line)))

;; ============================================================================
;;; Provide features
(provide 'omw-shell)

;;; omw-shell.el ends here
