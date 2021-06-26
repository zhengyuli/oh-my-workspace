;;; package --- init-multi-term.el ---
;; Time-stamp: <2021-06-26 02:29:45 Saturday by lizhengyu>

;; Copyright (C) 2013 zhengyu li
;;
;; Author: zhengyu li <lizhengyu419@gmail.com>
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
;;   (require 'init-multi-term)

;;; Require:
(require 'multi-term-autoloads)
(require 'lazy-set-key)

;;; Code:
;; ==================================================================================
(defun multi-term-settings ()
  "Settings for `multi-term'."

  ;; Require
  (require 'term)

  ;; ----------------------------------------------------------
  (defface term-color-yellow
    '((t :foreground "yellow" :background "yellow"))
    "Face used to render light yellow color code."
    :group 'term)

  (defface term-color-green
    '((t :foreground "green" :background "green"))
    "Face used to render light green color code."
    :group 'term)

  (defface term-color-red
    '((t :foreground "red2" :background "red2"))
    "Face used to render red2 color code."
    :group 'term)

  (defface term-color-blue
    '((t :foreground "DeepSkyBlue" :background "DeepSkyBlue"))
    "Face used to render skyblue color code."
    :group 'term)

  (defun term-send-yank ()
    "Yank in term mode."
    (interactive)
    (yank)
    (term-send-raw-string (current-kill 0)))

  ;; ----------------------------------------------------------
  ;; Customize multi term key bindings
  (customize-set-variable 'term-unbind-key-list
						  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>" "<M-backspace>" "<M-DEL>"))

  (customize-set-variable 'term-bind-key-alist
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
              (auto-fill-mode -1)
              (yas-minor-mode -1)
              (company-mode -1))))

(eval-after-load "multi-term" '(multi-term-settings))

;; ==================================================================================
;; Global key bindings for `multi-term'
(lazy-set-key
 '(("<f9>" . multi-term)))

;; ==================================================================================
;;; Provide features
(provide 'init-multi-term)

;;; init-multi-term.el ends here
