;;; omw-javascript.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: typescript, ts
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
;; TypeScript mode configuration with LSP support.

;;; Code:

;; ============================================================================
(defvar omw/typescript-tool-specs
  '(("typescript-language-server"
     "bun install -g typescript-language-server typescript"
     "bun"))
  "Tool specs for TypeScript development.")

(defun omw/install-typescript-tools ()
  "Install TypeScript LSP tools (typescript-language-server) via bun if not present."
  (interactive)
  (require 'omw-utils)
  (apply #'omw/tools-install omw/typescript-tool-specs))

;; ============================================================================
(defun omw/typescript-mode-setup ()
  "Apply custom settings for TypeScript mode."
  (require 'omw-utils)
  (setq-local typescript-indent-level 2)
  (apply #'omw/tools-check-and-prompt omw/typescript-tool-specs))

(use-package typescript-mode
  :ensure t
  :defer t
  :hook (typescript-mode . omw/typescript-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-javascript)

;;; omw-javascript.el ends here
