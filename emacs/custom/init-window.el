;;; package --- init-window.el ---
;; Time-stamp: <2022-03-10 10:22:40 Thursday by zhengyu.li>

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
;;   (require 'init-window)

;;; Require:
(require 'winum-autoloads)
(require 'lazy-set-key)

;;; Code:
;; ==================================================================================
(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
	(set-frame-parameter
	 nil
	 'fullscreen
	 (if (equal 'fullboth current-value)
		 (if (boundp 'old-fullscreen)
			 old-fullscreen
		   nil)
	   (progn
		 (setq old-fullscreen current-value)
		 'fullboth)))))

;; ==================================================================================
(defun winum-settings ()
  "Settings for `winum'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `winum'
  (lazy-set-key
   '(("M-0" . winum-select-window-0-or-10)
     ("M-1" . winum-select-window-1)
     ("M-2" . winum-select-window-2)
     ("M-3" . winum-select-window-3)
     ("M-4" . winum-select-window-4)
     ("M-5" . winum-select-window-5)
     ("M-6" . winum-select-window-6)
     ("M-7" . winum-select-window-7)
     ("M-8" . winum-select-window-8)
     ("M-9" . winum-select-window-9))
   winum-keymap))

(eval-after-load "winum" '(winum-settings))

;; ==================================================================================
;; Settings after init
(add-hook 'after-init-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Global key bindings for `window'
            (lazy-set-key
             '(("C-<f10>" . toggle-fullscreen)
               ("C-<left>" . shrink-window-horizontally)
               ("C-<right>" . enlarge-window-horizontally)
               ("C-<down>" . shrink-window)
               ("C-<up>" . enlarge-window)))

            ;; ----------------------------------------------------------
            ;; Enable winum mode
            (winum-mode 1)

            ;; Toggle full screen
            (toggle-fullscreen)))

;; ==================================================================================
;;; Provide features
(provide 'init-window)

;;; init-window.el ends here
