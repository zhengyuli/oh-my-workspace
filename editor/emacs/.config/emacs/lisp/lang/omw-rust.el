;;; omw-rust.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:32:39 Friday by zhengyu.li>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: rust, rust-analyzer
;; Dependencies: omw-prog, omw-utils

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-19 17:00 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Rust mode configuration with LSP support via rust-analyzer.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Rust Configuration
;; ----------------------------------------------------------------------------

(defconst omw/rust-tool-specs
  '(("rust-analyzer" "rustup component add rust-analyzer" "rustup")
    ("rustfmt" "rustup component add rustfmt" "rustup"))
  "Tool specs for Rust development.")

(defun omw/install-rust-tools ()
  "Install Rust development tools via rustup if not present."
  (interactive)
  (apply #'omw/tools-install omw/rust-tool-specs))

;; --- Rust Mode Setup ---
(defun omw/rust-mode-setup ()
  "Check required Rust development tools and prompt to install if missing."
  (apply #'omw/tools-check-and-prompt omw/rust-tool-specs))

(use-package rust-mode
  :ensure t
  :defer t
  :hook (rust-mode . omw/rust-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-rust)

;;; omw-rust.el ends here
