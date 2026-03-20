;;; omw-completion.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: completion, corfu, vertico, cape, orderless
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
;; Modern completion framework with vertico (UI), orderless (matching),
;; marginalia (annotations), cape (completion extensions), and corfu (popup).

;;; Code:

;; ============================================================================
(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; ============================================================================
(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode))

;; ============================================================================
(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

;; ============================================================================
(use-package cape
  :ensure t
  :demand t
  :config
  (add-to-list 'completion-at-point-functions (cape-capf-super
                                               #'yasnippet-capf
                                               #'cape-file
                                               #'cape-dabbrev)))

;; ============================================================================
(defun omw/corfu-mode-setup ()
  "Enable corfu auto-completion with documentation popup.
Enables global corfu mode with auto-completion and popup info display."
  (setq corfu-auto t)
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

(use-package corfu
  :ensure t
  :defer t
  :hook (after-init . omw/corfu-mode-setup))

(use-package corfu-terminal
  :ensure t
  :when (not (display-graphic-p))
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; ============================================================================
;;; Provide features
(provide 'omw-completion)

;;; omw-completion.el ends here
