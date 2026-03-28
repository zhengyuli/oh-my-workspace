;;; omw-rust.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:32:39 Friday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: rust, rust-analyzer
;; Dependencies: omw-prog, omw-utils

;; Copyright (C) 2026 zhengyu li

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; History:
;;
;; 2026-03-19 17:00 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Rust mode configuration with LSP support via rust-analyzer.

;;; Code:

;; ============================================================================
(defconst omw/rust-tool-specs
  '(("rust-analyzer" "rustup component add rust-analyzer" "rustup")
    ("rustfmt" "rustup component add rustfmt" "rustup"))
  "Tool specs for Rust development.")

(defun omw/install-rust-tools ()
  "Install Rust development tools via rustup if not present."
  (interactive)
  (apply #'omw/tools-install omw/rust-tool-specs))

;; ============================================================================
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
