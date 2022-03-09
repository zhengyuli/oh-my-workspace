;;; package --- init-session.el ---
;; Time-stamp: <2022-03-09 23:43:56 Wednesday by zhengyu.li>

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
;;   (require 'init-session)

;;; Require:
(require 'workgroups2-autoloads)

;;; Code:
;; ==================================================================================
(defun workgroups2-settings ()
  "settings for `workgroups2'."

  ;; ----------------------------------------------------------
  ;; Customize workgroups2 related variables
  (customize-set-variable 'wg-session-file "~/.emacs.d/workgroups2"))

(eval-after-load "workgroups2" '(workgroups2-settings))

;; ==================================================================================
;; Settings after init
(add-hook 'after-init-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Enable global workgroups mode
            (workgroups-mode 1)))

;; ==================================================================================
;;; Provide features
(provide 'init-session)

;;; init-session.el ends here
