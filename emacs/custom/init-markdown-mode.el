;;; package --- init-markdown-mode.el -*- lexical-binding:t -*-
;; Time-stamp: <2022-03-31 18:19:57 Thursday by zhengyuli>

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
;;   (require 'init-markdown-mode)

;;; Require:

;;; Code:
;; ==================================================================================
;; Customized settings for `markdown-mode'
(defun markdown-mode-settings ()
  "Settings for `markdown-mode'."

  ;; Require
  (require 'valign)
  (require 'markdownfmt)

  ;; ----------------------------------------------------------
  ;; Customize `markdown-mode' related variables
  (customize-set-variable 'markdown-command "pandoc")
  (customize-set-variable 'markdown-enable-math t)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'markdown-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Set buffer column width to 120
              (setq-local fill-column 120)

              ;; Enable auto fill mode
              (auto-fill-mode 1)

              ;; enable valign mode
              (valign-mode 1)

              ;; Format buffer before save
              (add-hook 'before-save-hook 'markdownfmt-format-buffer nil t))))

(eval-after-load "markdown-mode" '(markdown-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-markdown-mode)

;;; init-markdown-mode.el ends here
