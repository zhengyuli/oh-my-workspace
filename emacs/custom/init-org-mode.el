;;; package --- init-org-mode.el -*- lexical-binding:t -*-
;; Time-stamp: <2022-03-23 18:13:11 Wednesday by zhengyuli>

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
;;   (require 'init-org-mode)

;;; Require:

;;; Code:
;; ==================================================================================
;; Customized settings for `org-mode'
(defun org-mode-settings ()
  "Settings for `org-mode'."

  ;; Require
  (require 'org-bullets)

  ;; ----------------------------------------------------------
  ;; Customized `org-mode' related faces
  (unless (display-graphic-p)
    (set-face-foreground 'org-hide "black"))

  ;; ----------------------------------------------------------
  ;; Customize `org-mode' related variables
  (customize-set-variable 'org-startup-indented t)
  (customize-set-variable 'org-pretty-entities t)
  (customize-set-variable 'org-hide-emphasis-markers t)
  (customize-set-variable 'org-startup-with-inline-images t)
  (customize-set-variable 'org-image-actual-width '(300))
  (customize-set-variable 'org-catch-invisible-edits t)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'org-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable org bullets mode
              (org-bullets-mode 1))))

(eval-after-load "org" '(org-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-org-mode)

;;; init-org-mode.el ends here
