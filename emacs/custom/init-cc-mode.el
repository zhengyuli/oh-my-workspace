;;; package --- init-cc-mode.el -*- lexical-binding:t -*-
;; Time-stamp: <2022-11-22 08:47:00 Tuesday by zhengyuli>

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
;;   (require 'init-cc-mode)

;;; Require:

;;; Code:
;; ==================================================================================
;; Customized settings for `cc-mode'
(defun cc-mode-settings ()
  "settings for `cc-mode'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `cc-mode'
  (lazy-set-key
   '(("C-c C-c" . comment-line))
   c-mode-base-map))

(eval-after-load "cc-mode" '(cc-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-cc-mode)

;;; init-cc-mode.el ends here
