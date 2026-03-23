;;; omw-elisp.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: elisp, emacs-lisp, lisp
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
;; Emacs Lisp mode with enhanced navigation and syntax highlighting.

;;; Code:

;; ============================================================================
(use-package elisp-slime-nav
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; ============================================================================
(use-package lisp-extra-font-lock
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))

;; ============================================================================
(use-package rainbow-mode
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . rainbow-mode))

;; ============================================================================
(defun omw/elisp-mode-setup ()
  "Apply custom settings for Emacs Lisp mode."
  (eldoc-mode 1))

(use-package elisp-mode
  :ensure nil
  :defer t
  :hook (emacs-lisp-mode . omw/elisp-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-elisp)

;;; omw-elisp.el ends here
