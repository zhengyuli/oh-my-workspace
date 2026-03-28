;;; omw-json.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-21 12:00:00 Saturday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: json, data
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
;; 2026-03-21 12:00 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; JSON mode configuration with LSP support.
;; LSP server (vscode-json-languageserver) is configured in omw-prog.el.

;;; Code:

;; ============================================================================
(defconst omw/json-tool-specs
  '(("vscode-json-languageserver" "bun install -g vscode-json-languageserver" "bun")
    ("prettier" "bun install -g prettier" "bun"))
  "Tool specs for JSON development.")

(defun omw/install-json-tools ()
  "Install JSON LSP and formatting tools via bun if not present."
  (interactive)
  (apply #'omw/tools-install omw/json-tool-specs))

;; ============================================================================
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

;; ============================================================================
(use-package json-snatcher
  :ensure t
  :defer t)

;; ============================================================================
;;; Provide features
(provide 'omw-json)

;;; omw-json.el ends here
