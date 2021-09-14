;;; package --- init-wangyi-music.el ---
;; Time-stamp: <2021-09-10 07:35:33 Friday by lizhengyu>

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
;;   (require 'init-wangyi-music)

;;; Require:
(require 'wangyi-music-autoloads)

;;; Code:
;; ==================================================================================
(defun wangyi-music-settings ()
  "Settings for `wangyi-music'."

  ;; ----------------------------------------------------------
  ;; Customize `wangyi-music' related variables
  (customize-set-variable 'wangyi-music-display-album t))

(eval-after-load "wangyi-music" '(wangyi-music-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-wangyi-music)

;;; init-wangyi-music.el ends here
