;;; package --- init-terminal.el ---
;; Time-stamp: <2022-03-10 10:21:20 Thursday by zhengyu.li>

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
;;   (require 'init-terminal)

;;; Require:
(require 'multi-term-autoloads)
(require 'lazy-set-key)

;;; Code:
;; ==================================================================================
;;Customized settings for `multi-term'
(defun multi-term-settings ()
  "Settings for `multi-term'."

  ;; ----------------------------------------------------------
  ;; Customize `terminal' related variables
  (customize-set-variable 'ring-bell-function 'ignore)

  ;; ----------------------------------------------------------
  ;; Key bindings for `multi-term'
  (customize-set-variable
   'term-unbind-key-list
   '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>" "<M-backspace>" "<M-DEL>"))

  (customize-set-variable
   'term-bind-key-alist
   '(("C-p" . term-send-raw)
     ("C-n" . term-send-raw)
     ("C-r" . term-send-raw)
     ("C-y" . term-paste)
     ("M-f" . term-send-raw-meta)
     ("M-b" . term-send-raw-meta)
     ("M-d" . term-send-raw-meta)
     ("<M-backspace>" . term-send-backward-kill-word)
	 ("<M-DEL>" . term-send-backward-kill-word)
     ("M-." . term-send-raw-meta)
     ("M-i" . term-interrupt-subjob)))

  ;; ----------------------------------------------------------
  ;; Hooks for `multi-term'
  (add-hook 'term-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Disable auto fill mode
              (auto-fill-mode -1)

              ;; Disable yasnippet mode
              (yas-minor-mode -1)

              ;; Disable company mode
              (company-mode -1))))

(eval-after-load "multi-term" '(multi-term-settings))

;; ==================================================================================
;; Settings after init
(add-hook 'after-init-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Global key bindings for `multi-term'
            (lazy-set-key
             '(("<f9>" . multi-term)))))

;; ==================================================================================
;;; Provide features
(provide 'init-terminal)

;;; init-terminal.el ends here
