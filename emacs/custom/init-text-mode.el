;;; package --- init-text-mode.el -*- lexical-binding:t -*-

;; Copyright (c) 2022, 2023 zhengyuli
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
;;   (require 'init-text-mode)

;;; Require:

;;; Code:
;; ==================================================================================
;; Customized settings for `text-mode'
(defun text-mode-settings ()
  "Settings for `text-mode'."

  ;; Require
  (require 'mixed-pitch)
  (require 'flyspell)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'text-mode-hook
            (lambda ()
              ;; -----------------------------------------------
              ;; Enable mixed pitch mode
              ;; (mixed-pitch-mode 1)

              ;; Enable visual line mode
              (visual-line-mode 1)

              ;; Enable flyspell mode
              (flyspell-mode 1))))

(eval-after-load "text-mode" '(text-mode-settings))

;;; init-text-mode.el ends here
