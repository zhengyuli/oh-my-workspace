;;; package --- init-elisp-mode.el ---
;; Time-stamp: <2022-03-14 19:54:23 Monday by zhengyuli>

;; Copyright (C) 2021, 2022 zhengyu li
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: none

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

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'init-elisp-mode)

;;; Require:

;;; Code:
;; ==================================================================================
;; Customized settings for `elisp-mode'
(defun elisp-mode-settings ()
  "Settings for `elisp-mode'."

  ;; Require
  (require 'flycheck)
  (require 'eldoc)
  (require 'elisp-slime-nav)
  (require 'lisp-extra-font-lock)
  (require 'rainbow-mode)

  ;; ----------------------------------------------------------
  ;; Customize `elisp-mode' related variables
  (customize-set-variable 'flycheck-emacs-lisp-load-path "inherit")

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable eldoc mode
              (eldoc-mode 1)

              ;; Enable lisp extra font lock mode
			  (lisp-extra-font-lock-mode 1)

              ;; Enable elisp slime navigation mode
              (elisp-slime-nav-mode 1)

              ;; Enable rainbow mode
              (rainbow-mode 1))))

(eval-after-load "elisp-mode" '(elisp-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-elisp-mode)

;;; init-elisp-mode.el ends here
