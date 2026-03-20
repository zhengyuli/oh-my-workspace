;;; omw-shell.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:38:18 Friday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: shell, bash, sh
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
;; Shell script mode configuration.

;;; Code:

;; ============================================================================
(defvar omw/sh-tool-specs
  '(("bash-language-server" "bun install -g bash-language-server" "bun"))
  "Tool specs for shell script development.")

(defun omw/install-bash-tools ()
  "Install shell script LSP tools (bash-language-server) via bun
if not present."
  (interactive)
  (apply #'omw/tools-install omw/sh-tool-specs))

;; ============================================================================
(defun omw/sh-mode-setup ()
  "Apply custom settings for shell script mode."
  (setq-local sh-basic-offset 2)
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
