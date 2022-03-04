;;; package --- init-basic.el ---
;; Time-stamp: <2022-03-04 11:40:10 Friday by zhengyu.li>

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
;;   (require 'init-basic)

;;; Require:
(require 'swiper-autoloads)
(require 'cursor-chg)
(require 'smooth-scrolling)
(require 'lazy-set-key)

;;; Code:
;; ==================================================================================
(defun get-mode-name ()
  "Display major mode and mode name."
  (interactive)
  (message "major-mode: %s, mode-name: %s" major-mode mode-name))

;; ==================================================================================
;; Customize `startup' related variables
(customize-set-variable 'inhibit-default-init t)
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'inhibit-startup-echo-area-message t)

;; Customize `recentf' related variables
(customize-set-variable 'recentf-max-saved-items 1000)
(customize-set-variable 'recentf-exclude '("/tmp/" "ssh:"))

;; Customize `files' related variables
(customize-set-variable 'backup-directory-alist
                        '((".*" . "~/.emacs.d/backup-files")))
(customize-set-variable 'backup-by-copying t)
(customize-set-variable 'delete-old-versions t)
(customize-set-variable 'version-control t)

;; Customize `uniquify' realted variables
(customize-set-variable 'uniquify-separator "/")
(customize-set-variable 'uniquify-buffer-name-style 'forward)

;; Customize line spacing
(customize-set-variable 'line-spacing 3)

;; Customize user and email
(customize-set-variable 'user-full-name "zhengyu li")
(customize-set-variable 'user-mail-address "lizhengyu419@outlook.com")

;; ==================================================================================
;; Customized settings for `ivy'
(defun ivy-settings ()
  "Settings for `ivy'."

  ;; ----------------------------------------------------------
  ;; Customize `ivy' related variables
  (customize-set-variable 'ivy-use-virtual-buffers t)
  (customize-set-variable 'ivy-count-format "")
  (customize-set-variable 'ivy-initial-inputs-alist nil)
  (customize-set-variable 'ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(eval-after-load "ivy" '(ivy-settings))

;;Customized settings for `tramp'
(defun tramp-settings ()
  "Settings for `tramp'."

  ;; ----------------------------------------------------------
  ;; Customize `tramp' related variables
  (customize-set-variable 'tramp-default-method "ssh")
  (customize-set-variable 'password-cache-expiry 3600)
  (customize-set-variable 'tramp-auto-save-directory "~/.emacs.d/tramp-auto-save"))

(eval-after-load "tramp" '(tramp-settings))

;; ==================================================================================
;; Global key bindings for `basic'
(lazy-set-key
 '(("C-x b" . ivy-switch-buffer)
   ("C-x B" . counsel-recentf)
   ("C-x C-b" . ibuffer)
   ("C-x C-f" . counsel-find-file)
   ("C-x k" . kill-this-buffer)
   ("M-x" . counsel-M-x)))

;; ==================================================================================
;; Replace yes-or-no-p with y-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Move the mouse to the upper-right corner on any keypress
(mouse-avoidance-mode "banish")

;; Enable cursor type change when idle
(toggle-cursor-type-when-idle 1)

;; Initialize mac system exec path
(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (exec-path-from-shell-initialize))

;; ==================================================================================
;; Disable blink cursor mode
(blink-cursor-mode -1)

;; Disable tool bar mode
(tool-bar-mode -1)

;; Disable scroll bar mode
(scroll-bar-mode -1)

;; Disable global menu bar mode
(menu-bar-mode -1)

;; Enable global auto revert mode
(global-auto-revert-mode 1)

;; Enable global recentf mode
(recentf-mode 1)

;; Enable global counsel mode
(counsel-mode 1)

;; Enable global ivy mode
(ivy-mode 1)

;; Enable global column number mode
(column-number-mode 1)

;; Enable global just-in-time lock mode
(jit-lock-mode 1)

;; Enable global change cursor mode
(change-cursor-mode 1)

;; Enable global smooth scrolling mode
(smooth-scrolling-mode 1)

;; ==================================================================================
;;; Provide features
(provide 'init-basic)

;;; init-basic.el ends here
