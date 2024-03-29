;;; package --- init-sh-script-mode.el -*- lexical-binding:t -*-
;; Time-stamp: <2022-11-22 08:47:21 Tuesday by zhengyuli>

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
;;   (require 'ininit-sh-script-mode
;;; Require:

;;; Code:
;; ==================================================================================
;; Customized settings for `sh-script' mode
(defun sh-script-settings ()
  "Settings for `sh-script'."

  ;; Require
  (require 'lsp-mode)

  ;; ----------------------------------------------------------
  ;; Key bindings for `sh-mode'
  (lazy-set-key
   '(("C-c C-c" . comment-line))
   sh-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'sh-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable lsp mode
              (lsp-deferred))))

(eval-after-load "sh-script" '(sh-script-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-sh-script-mode)

;;; init-sh-script-mode.el ends here
