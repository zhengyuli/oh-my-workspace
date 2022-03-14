;;; package --- init-lsp-mode.el ---
;; Time-stamp: <2022-03-14 21:56:50 Monday by zhengyuli>

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
;;   (require 'init-lsp-mode)

;;; Require:

;;; Code:
;; ==================================================================================
(defun lsp-mode-settings ()
  "Settings for `lsp-mode'."

  ;; Require
  (require 'lsp-modeline)
  (require 'lsp-ui)

  ;; ----------------------------------------------------------
  ;; Customize `gc-cons-threshold' to 100MB
  (customize-set-variable 'gc-cons-threshold (* 100 1024 1024))

  ;; Customize `read-process-output-max' to 3MB, default 4k is too low
  (customize-set-variable 'read-process-output-max (* 3 1024 1024))

  ;; Customize `lsp-mode' related variables
  (customize-set-variable 'lsp-headerline-breadcrumb-enable nil)
  (customize-set-variable 'lsp-idle-delay 0.5))

(eval-after-load "lsp-mode" '(lsp-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-lsp-mode)

;;; init-lsp-mode.el ends here
