;;; omw-yaml.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: yaml
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
;; YAML mode configuration with LSP support.
;; LSP server (yaml-language-server) is configured in omw-prog.el.

;;; Code:

;; ============================================================================
(defvar omw/yaml-tool-specs
  '(("yaml-language-server" "bun install -g yaml-language-server" "bun"))
  "Tool specs for YAML development.")

(defun omw/install-yaml-tools ()
  "Install YAML LSP tools (yaml-language-server) via bun if not present."
  (interactive)
  (require 'omw-utils)
  (apply #'omw/tools-install omw/yaml-tool-specs))

;; ============================================================================
(defun omw/yaml-mode-setup ()
  "Apply custom settings for yaml mode."
  (require 'omw-utils)
  (setq-local yaml-indent-offset 2)
  (apply #'omw/tools-check-and-prompt omw/yaml-tool-specs))

(use-package yaml-mode
  :ensure t
  :defer t
  :hook (yaml-mode . omw/yaml-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-yaml)

;;; omw-yaml.el ends here
