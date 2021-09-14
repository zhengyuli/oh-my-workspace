;;; package --- init-cmake-mode.el ---
;; Time-stamp: <2021-09-14 03:30:57 Tuesday by lizhengyu>

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
;;   (require 'init-cmake-mode)

;;; Require:
(require 'cmake-mode-autoloads)

;;; Code:
;; ==================================================================================
;; Customized settings for `cmake-mode'
(defun cmake-mode-settings ()
  "Settings for `cmake-mode'."

  ;; Require

  ;; ----------------------------------------------------------
  ;; Set `prog-mode-map' as the parent of `cmake-mode-map'
  (set-keymap-parent cmake-mode-map prog-mode-map))

(eval-after-load "cmake-mode" '(cmake-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-cmake-mode)

;;; init-cmake-mode.el ends here
