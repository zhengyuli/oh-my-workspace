;;; package --- init-plantuml-mode.el -*- lexical-binding:t -*-

;; Copyright (c) 2022 Zhengyu Li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'init-plantuml-mode)

;;; Require:


;;; Code:
;; ==================================================================================
(defun plantuml-mode-settings ()
  "Settings for `plantuml-mode'."

  ;; ----------------------------------------------------------
  (defun plantuml-completion-at-point ()
  "Function used for `completion-at-point-functions' in `plantuml-mode'."
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (keywords plantuml-kwdList))
    (when (and bounds keywords)
      (list (car bounds)
            (cdr bounds)
            keywords
            :exclusve 'no
            :company-docsig #'identity))))

  (defun plantuml-format-buffer ()
    "Format plantuml buffer."
    (interactive)
    (smart-indent))

  ;; ----------------------------------------------------------
  ;; Fix, add destroy indent for activate block
  (defvar plantuml-indent-regexp-activate-end "^\s*\\(deactivate\\|destroy\\)\s+.+$")

  ;; ----------------------------------------------------------
  ;; Customize `plantuml-mode' related variables
  (customize-set-variable 'plantuml-default-exec-mode 'executable)
  (customize-set-variable 'plantuml-indent-level 4)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'plantuml-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              (add-hook 'completion-at-point-functions 'plantuml-completion-at-point nil t)

              ;; Format buffer before save
              (add-hook 'before-save-hook 'plantuml-format-buffer nil t))))

(eval-after-load "plantuml-mode" '(plantuml-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-plantuml-mode)

;;; init-plantuml-mode.el ends here
