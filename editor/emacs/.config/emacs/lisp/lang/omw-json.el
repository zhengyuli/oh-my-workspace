;;; omw-json.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-21 12:00:00 Saturday by zhengyu.li>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: json, data
;; Dependencies: omw-prog, omw-utils

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-21 12:00 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; JSON mode configuration with LSP support.
;; LSP server (vscode-json-languageserver) is configured in omw-prog.el.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; JSON
;; ----------------------------------------------------------------------------

;; --- Json Tool Specs ---
(defconst omw/json-tool-specs
  '(("vscode-json-languageserver"
     "bun install -g vscode-json-languageserver" "bun")
    ("prettier" "bun install -g prettier" "bun"))
  "Tool specs for JSON development.")

(defun omw/install-json-tools ()
  "Install JSON LSP and formatting tools via bun if not present."
  (interactive)
  (apply #'omw/tools-install omw/json-tool-specs))

;; --- Json Mode Setup ---
(defun omw/json-mode-setup ()
  "Apply custom settings for json mode."
  (setq-local js-indent-level 2)
  (apply #'omw/tools-check-and-prompt omw/json-tool-specs))

(use-package json-mode
  :ensure t
  :defer t
  :hook (json-mode . omw/json-mode-setup)
  :bind (:map json-mode-map
              ("C-c C-g" . jsons-print-path)))

;; --- json-snatcher ---
(use-package json-snatcher
  :ensure t
  :defer t)

;; ============================================================================
;;; Provide features
(provide 'omw-json)

;;; omw-json.el ends here
