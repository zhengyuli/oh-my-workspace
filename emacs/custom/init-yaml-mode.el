;;; package --- init-yaml-mode.el ---
;; Time-stamp: <2021-09-14 03:35:04 Tuesday by lizhengyu>

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
;;   (require 'init-yaml-mode)

;;; Require:
(require 'yaml-mode-autoloads)

;;; Code:
;; ==================================================================================
;; Customized settings for `yaml-mode'
(defun yaml-mode-settings ()
  "Settings for `yaml-mode'."

  ;; Require

  ;; ----------------------------------------------------------
  ;; Set `text-mode-map' as the parent of `yaml-mode-map'
  (set-keymap-parent yaml-mode-map text-mode-map))

(eval-after-load "yaml-mode" '(yaml-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-yaml-mode)

;;; init-yaml-mode.el ends here
