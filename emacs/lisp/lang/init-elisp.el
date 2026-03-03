;;; init-elisp.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 21:27:03 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: elisp, emacs-lisp, lisp
;; Dependencies: (none)

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Emacs Lisp mode configuration with enhanced navigation and syntax highlighting.

;;; Code:

;; ==================================================================================
;; Elisp-slime-nav - navigation between function definitions and calls
(use-package elisp-slime-nav
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; ==================================================================================
;; Lisp extra font lock - enhanced syntax highlighting for Lisp code
(use-package lisp-extra-font-lock
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))

;; ==================================================================================
;; Rainbow mode - highlight color codes with their actual colors
(use-package rainbow-mode
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . rainbow-mode))

;; ==================================================================================
;; Emacs Lisp mode hooks - customize editing behavior
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flycheck-mode -1)    ; Disable flycheck (built-in linting is sufficient)
            (eldoc-mode 1)))       ; Enable eldoc for function documentation

;; ==================================================================================
;;; Provide features
(provide 'init-elisp)

;;; init-elisp.el ends here
