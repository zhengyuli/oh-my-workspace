;;; init-elisp.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-06 19:09:36 Friday by zhengyu.li>

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
(use-package elisp-slime-nav
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; ==================================================================================
(use-package lisp-extra-font-lock
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))

;; ==================================================================================
(use-package rainbow-mode
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . rainbow-mode))

;; ==================================================================================
(defun omw/elisp-mode-setup ()
  "Apply custom settings for Emacs Lisp mode."
  (eldoc-mode 1))

(use-package elisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . omw/elisp-mode-setup))

;; ==================================================================================
;;; Provide features
(provide 'init-elisp)

;;; init-elisp.el ends here
