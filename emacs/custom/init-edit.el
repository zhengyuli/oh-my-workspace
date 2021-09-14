;;; package --- init-edit.el ---
;; Time-stamp: <2021-09-11 01:16:18 Saturday by lizhengyu>

;; Copyright (C) 2021 zhengyu li
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
;;   (require 'init-edit)

;;; Require:
(require 'ascii-autoloads)
(require 'undo-tree-autoloads)
(require 'browse-kill-ring-autoloads)
(require 'expand-region-autoloads)
(require 'multiple-cursors-autoloads)
(require 'visual-regexp-steroids-autoloads)
(require 'goto-chg-autoloads)
(require 'color-rg-autoloads)
(require 'lazy-set-key)

;;; Code:
;; ==================================================================================
(defun indent-buffer ()
  "Automatic format current buffer."
  (interactive)
  (save-excursion
	(indent-region (point-min) (point-max) nil)
	(delete-trailing-whitespace)
	(untabify (point-min) (point-max))))

(defun smart-indent ()
  "If mark is active, indent region, else indent all buffer."
  (interactive)
  (save-excursion
	(if mark-active
		(call-interactively 'indent-region)
	  (call-interactively 'indent-buffer))))

(defun copy-region ()
  "Copy region."
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end)))

(defun copy-curr-line ()
  "Copy current line."
  (interactive)
  (let ((end (min (point-max) (line-end-position))))
	(copy-region-as-kill (line-beginning-position) end)))

(defun smart-copy ()
  "If mark is active, copy region, else copy current line."
  (interactive)
  (save-excursion
	(if mark-active
		(call-interactively 'copy-region)
	  (call-interactively 'copy-curr-line))))

(defun smart-kill ()
  "If mark is active, kill region, else kill whole line."
  (interactive)
  (if mark-active
	  (call-interactively 'kill-region)
	(call-interactively 'kill-whole-line)))

;; ==================================================================================
;; Customize `time-stamp' related variables
(customize-set-variable 'time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S %:a by %u")

;; ==================================================================================
;; Customized settings for `undo-tree'
(defun undo-tree-settings ()
  "Settings for `undo-tree'."

  ;; ----------------------------------------------------------
  ;; Customize `undo-tree' related variables
  (customize-set-variable 'undo-tree-auto-save-history nil)

  ;; ----------------------------------------------------------
  ;; Key unbindings for `undo-tree'
  (lazy-unset-key
   '("C-x u" "M-_")
   undo-tree-map)

  ;; Key bindings for `undo-tree'
  (lazy-set-key
   '(("M-_"  . undo-tree-visualize))
   undo-tree-map))

(eval-after-load "undo-tree" '(undo-tree-settings))

;; Customized settings for `color-rg'
(defun color-rg-settings ()
  "Settings for `color-rg'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `color-rg'
  (lazy-set-key
   '(("p" . color-rg-jump-prev-keyword)
     ("n" . color-rg-jump-next-keyword)
     ("o" . color-rg-open-file-and-stay))
   color-rg-mode-map))

(eval-after-load "color-rg" '(color-rg-settings))

;; Customized settings for `visual-regexp-steroids'
(defun visual-regexp-steroids-settings ()
  "Settings for `visual-regexp-steroids'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `vr/minibuffer-keymap'
  (lazy-set-key
   '(("C-p" . previous-history-element)
     ("C-n" . next-history-element))
   vr/minibuffer-keymap))

(eval-after-load "visual-regexp-steroids" '(visual-regexp-steroids-settings))

;; Customized settings for `ispell'
(defun ispell-settings ()
  "Settings for `ispell'."

  ;; ----------------------------------------------------------
  ;; Customize `ispell' related variables
  (customize-set-variable 'ispell-program-name "aspell")
  (customize-set-variable 'ispell-extra-args '("--lang=en" "--reverse"))
  (customize-set-variable 'ispell-silently-savep t)
  (customize-set-variable 'ispell-dictionary "english"))

(eval-after-load "ispell" '(ispell-settings))

;; ==================================================================================
;; Global key bindings for `edit'
(lazy-set-key
 '(("C-x <tab>" . smart-indent)
   ("C-x TAB" . smart-indent)
   ("C-x m" . set-rectangular-region-anchor)
   ("C-x M" . mc/mark-all-dwim)
   ("C-x C-u" . color-rg-search-input-in-current-file)
   ("C-x g" . color-rg-search-input)
   ("C-x G" . color-rg-search-input-in-project)
   ("M-w" . smart-copy)
   ("M-k" . smart-kill)
   ("M-g" . goto-line)
   ("M-m" . set-mark-command)
   ("M-M" . er/expand-region)
   ("M-o" . goto-last-change)
   ("M-y" . browse-kill-ring)
   ("M-_" . text-scale-decrease)
   ("M-+" . text-scale-increase)))

;; ==================================================================================
;; Update timestamp before saving files
(add-hook 'before-save-hook 'time-stamp)

;; Update copyright before saving files
(add-hook 'before-save-hook 'copyright-update)

;; Delete trailing whitespace before saving files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ==================================================================================
;; Enable global undo tree mode
(global-undo-tree-mode 1)

;; ==================================================================================
;;; Provide features
(provide 'init-edit)

;;; init-edit.el ends here
