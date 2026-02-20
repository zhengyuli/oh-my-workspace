;;; init-elisp.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
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
;; Emacs Lisp mode configuration.

;;; Code:

;; ==================================================================================
;; Elisp slime nav
(use-package elisp-slime-nav
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; ==================================================================================
;; Lisp extra font lock
(use-package lisp-extra-font-lock
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))

;; ==================================================================================
;; Rainbow mode
(use-package rainbow-mode
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . rainbow-mode))

;; ==================================================================================
;; Emacs lisp mode hook
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Disable flycheck
            (flycheck-mode -1)
            ;; Enable eldoc mode
            (eldoc-mode 1)))

;; ==================================================================================
;; Flycheck settings for elisp
(with-eval-after-load 'flycheck
  (setq flycheck-emacs-lisp-load-path "inherit"))

;; ==================================================================================
;;; Provide features
(provide 'init-elisp)

;;; init-elisp.el ends here
