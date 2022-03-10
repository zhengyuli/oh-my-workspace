;;; package --- init-magit.el ---
;; Time-stamp: <2022-03-10 21:51:24 Thursday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2022 zhengyu li
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
;;   (require 'init-magit)

;;; Require:
(require 'magit-autoloads)

;;; Code:
;; ==================================================================================
(defun get-git-user-name ()
  "Get git user name."
  (interactive)
  (print (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.name"))))

(defun get-git-user-email ()
  "Get git user email."
  (interactive)
  (print (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.email"))))

;; ==================================================================================
;; Define `git' related aliases
(defalias 'git-status 'magit-status)
(defalias 'git-log 'magit-log-all)

;; ==================================================================================
;; Customized settings for `magit'
(defun magit-settings ()
  "Settings for `magit'."

  ;; ----------------------------------------------------------
  ;; Customize magit related faces
  (custom-set-faces
   '(magit-diff-added ((t (:background "#919191" :foreground "white"))))
   '(magit-diff-removed ((t (:background "#474747" :foreground "white"))))
   '(magit-diff-added-highlight ((t (:background "#EE1289" :foreground "white"))))
   '(magit-diff-removed-highlight ((t (:background "#54FF9F" :foreground "black"))))))

(eval-after-load "magit" '(magit-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-magit)

;;; init-magit.el ends here
