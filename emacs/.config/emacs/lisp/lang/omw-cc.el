;;; omw-cc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: c, cpp
;; Dependencies: omw-prog

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
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; C/C++ mode configuration with clangd LSP and Google code style.
;; LSP server (clangd) is configured in prog.el.

;;; Code:

;; ============================================================================
(defvar omw/cc-tool-specs
  '(("clangd" "brew install llvm" "brew"))
  "Tool specs for C/C++ development.")

(defun omw/install-cc-tools ()
  "Install C/C++ development tools (clangd) via Homebrew if not present."
  (interactive)
  (require 'omw-utils)
  (apply #'omw/tools-install omw/cc-tool-specs))

;; ============================================================================
(defun omw/cc-mode-setup ()
  "Apply custom settings for C/C++ mode."
  (require 'omw-utils)
  (apply #'omw/tools-check-and-prompt omw/cc-tool-specs))

(use-package cc-mode
  :ensure nil
  :defer t
  :hook ((c-mode . omw/cc-mode-setup)
         (c++-mode . omw/cc-mode-setup)))

;; ============================================================================
(use-package google-c-style
  :ensure t
  :defer t
  :hook ((c-mode . google-set-c-style)
         (c++-mode . google-set-c-style)))

;; ============================================================================
;;; Provide features
(provide 'omw-cc)

;;; omw-cc.el ends here
