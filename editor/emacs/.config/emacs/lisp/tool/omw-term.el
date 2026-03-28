;;; omw-term.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-28 19:56:12 Saturday by zhengyuli>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: terminal, vterm, eshell
;; Dependencies: (none)

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
;; Terminal emulation configuration with vterm for fast, native PTY support.

;;; Code:

;; ============================================================================
(defun omw/vterm-send-C-g ()
  "Send C-g to the vterm terminal."
  (interactive)
  (vterm-send-key "g" nil nil t))

(defun omw/vterm-mode-setup ()
  "Apply custom settings for vterm mode."
  (setq-local truncate-lines t)
  (hl-line-mode -1)
  (corfu-mode -1))

(use-package vterm
  :ensure t
  :defer t
  :hook (vterm-mode . omw/vterm-mode-setup)
  :bind (:map vterm-mode-map
              ("M-1" . nil)
              ("M-2" . nil)
              ("M-3" . nil)
              ("M-4" . nil)
              ("M-5" . nil)
              ("M-6" . nil)
              ("M-7" . nil)
              ("M-8" . nil)
              ("M-9" . nil)
              ("C-g" . omw/vterm-send-C-g)
              ("M-<backspace>" . vterm-send-meta-backspace)))

;; ============================================================================
(use-package multi-vterm
  :ensure t
  :defer t
  :bind ("C-x C-t" . multi-vterm))

;; ============================================================================
;;; Provide features
(provide 'omw-term)

;;; omw-term.el ends here
