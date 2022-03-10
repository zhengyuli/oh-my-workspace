;;; package --- init-centaur-tabs.el ---
;; Time-stamp: <2022-03-10 11:04:20 Thursday by zhengyu.li>

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
;;   (require 'init-centaur-tabs)

;;; Require:
(require 'centaur-tabs-autoloads)

;;; Code:
;; ==================================================================================
(defun centaur-tabs-settings ()
  "Settings for `centaur-tabs'."

  ;; Require
  (require 'all-the-icons)

  ;; ----------------------------------------------------------
  ;; Customize `centaur-tabs' realted faces
  (custom-set-faces
   `(centaur-tabs-default ((t :background ,emacs-config-background)))
   '(centaur-tabs-unselected ((t :background "#333333" :foreground "#999999")))
   '(centaur-tabs-selected ((t :background "#666666" :foreground "#00BFFF")))
   '(centaur-tabs-unselected-modified	((t :background "#333333" :foreground "#FFD700")))
   '(centaur-tabs-selected-modified ((t :background "#666666" :foreground "#FFD700"))))

  ;; ----------------------------------------------------------
  ;; Customize `centaur-tabs' realted variables
  (customize-set-variable 'centaur-tabs-height 25)
  (customize-set-variable 'centaur-tabs-style "zigzag")
  (customize-set-variable 'centaur-tabs-set-icons t)
  (customize-set-variable 'centaur-tabs-gray-out-icons 'buffer)
  (customize-set-variable 'centaur-tabs-set-close-button nil)
  (customize-set-variable 'centaur-tabs-cycle-scope 'tabs)

  ;; ----------------------------------------------------------
  ;; Key bindings for `centaur-tabs'
  (lazy-set-key
   '(("M-p" . centaur-tabs-backward)
     ("M-n" . centaur-tabs-forward)
     ("M-P" . centaur-tabs-counsel-switch-group)
     ("M-N" . centaur-tabs-counsel-switch-group))
   centaur-tabs-mode-map)

  ;; ----------------------------------------------------------
  ;; Make headline use centaur tabs default face
  (centaur-tabs-headline-match))

(eval-after-load "centaur-tabs" '(centaur-tabs-settings))

;; ==================================================================================
;; Settings after init
(add-hook 'after-init-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Enable centaur tabs mode
            (centaur-tabs-mode 1)))

;; ==================================================================================
;;; Provide features
(provide 'init-centaur-tabs)

;;; init-centaur-tabs.el ends here
