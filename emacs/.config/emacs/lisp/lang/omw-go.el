;;; omw-go.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:20:51 Friday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: go, golang
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
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Go mode configuration with LSP support via gopls.

;;; Code:

;; ============================================================================
(defvar omw/go-tool-specs
  '(("gopls" "go install golang.org/x/tools/gopls@latest" "go")
    ("gofumpt" "go install mvdan.cc/gofumpt@latest" "go"))
  "Tool specs for Go development.")

(defun omw/install-go-tools ()
  "Install Go development tools (gopls, gofumpt) via go install if not present."
  (interactive)
  (apply #'omw/tools-install omw/go-tool-specs))

;; ============================================================================
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
