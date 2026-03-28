;;; omw-dockerfile.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:19:40 Friday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: dockerfile, docker
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
;; Dockerfile mode configuration with LSP support.
;; LSP server (docker-langserver) is configured in omw-prog.el.

;;; Code:

;; ============================================================================
(defconst omw/dockerfile-tool-specs
  '(("docker-langserver" "bun install -g dockerfile-language-server-nodejs" "bun"))
  "Tool specs for Dockerfile development.")

(defun omw/install-dockerfile-tools ()
  "Install Dockerfile LSP tools (docker-langserver) via bun if not present."
  (interactive)
  (apply #'omw/tools-install omw/dockerfile-tool-specs))

;; ============================================================================
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
