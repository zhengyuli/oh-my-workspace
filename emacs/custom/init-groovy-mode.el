;;; package --- init-groovy-mode.el -*- lexical-binding:t -*-
;; Time-stamp: <2022-03-31 19:20:12 Thursday by zhengyuli>

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
;;   (require 'init-groovy-mode)

;;; Require:

;;; Code:
;; ==================================================================================
;; Customized settings for `groovy-mode'
(defun groovy-mode-settings ()
  "Settings for `groovy-mode'."

  ;; Require
  (require 'lsp-mode)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'groovy-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable lsp mode
              (lsp-deferred))))

(eval-after-load "groovy-mode" '(groovy-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-groovy-mode)

;;; init-groovy-mode.el ends here
