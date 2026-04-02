;;; omw-yaml.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:30:18 Friday by zhengyu.li>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: yaml
;; Dependencies: omw-prog, omw-utils

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; YAML mode configuration with LSP support.
;; LSP server (yaml-language-server) is configured in omw-prog.el.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; YAML Configuration
;; ----------------------------------------------------------------------------

(defconst omw/yaml-tool-specs
  '(("yaml-language-server" "bun install -g yaml-language-server" "bun"))
  "Tool specs for YAML development.")

(defun omw/install-yaml-tools ()
  "Install YAML LSP tools (yaml-language-server) via bun if not present."
  (interactive)
  (apply #'omw/tools-install omw/yaml-tool-specs))

;; --- Yaml Mode Setup ---
(defun omw/yaml-mode-setup ()
  "Apply custom settings for yaml mode."
  (apply #'omw/tools-check-and-prompt omw/yaml-tool-specs))

(use-package yaml-mode
  :ensure t
  :defer t
  :hook (yaml-mode . omw/yaml-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-yaml)

;;; omw-yaml.el ends here
