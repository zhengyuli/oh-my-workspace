;;; package --- init-translate.el ---
;; Time-stamp: <2021-09-11 01:46:18 Saturday by lizhengyu>

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
;;   (require 'init-translate)

;;; Require:
(require 'google-translate-autoloads)
(require 'lazy-set-key)

;;; Code:
;; ==================================================================================
;; Customized settings for `google-translate'
(defun google-translate-settings ()
  "Settings for `google-translate'."

  ;; ----------------------------------------------------------
  ;; Customize `google-translate' related variables
  (customize-set-variable 'google-translate-translation-directions-alist '(("en" . "zh-CN") ("zh-CN" . "en"))))

(eval-after-load "google-translate-smooth-ui" '(google-translate-settings))

;; ==================================================================================
;; Global Key bindings for `google-translate'
(lazy-set-key
 '(("C-x p" . google-translate-smooth-translate)))

;; ==================================================================================
;;; Provide features
(provide 'init-translate)

;;; init-translate.el ends here
