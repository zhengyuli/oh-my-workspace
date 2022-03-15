;;; package --- init-w3m.el ---
;; Time-stamp: <2022-03-15 14:33:01 Tuesday by zhengyuli>

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
;;   (require 'init-w3m)

;;; Require:
(load-library "w3m-autoloads")

;;; Code:
;; ==================================================================================
;; Customized settings for `w3m'
(defun w3m-settings ()
  "Settings for `w3m'."

  ;; Require
  (require 'browse-url)
  (require 'w3m-favicon)
  (require 'w3m-session)
  (require 'w3m-lnum)

  ;; ----------------------------------------------------------
  ;; Customize `browse-url' related variables
  (customize-set-variable 'browse-url-browser-function 'w3m-browse-url)
  (customize-set-variable 'browse-url-new-window-flag t)

  ;; Customize `w3m' related variables
  (customize-set-variable 'w3m-make-new-session t)
  (customize-set-variable 'w3m-use-header-line-title t)
  (customize-set-variable 'w3m-default-display-inline-images t)
  (customize-set-variable 'w3m-favicon-use-cache-file t)
  (customize-set-variable 'w3m-session-load-crashed-sessions t)

  ;; ----------------------------------------------------------
  ;; Key bindings for `w3m'
  (lazy-set-key
   '(("1" . w3m-session-save)
     ("2" . w3m-session-select)
     ("b" . w3m-previous-form)
     ("f" . w3m-next-form)
     ("B" . w3m-previous-anchor)
     ("<tab>" . w3m-next-anchor)
     ("TAB" . w3m-next-anchor)
     ("e" . w3m-edit-current-url)
     ("+" . w3m-zoom-in-image)
     ("-" . w3m-zoom-out-image)
     ("n" . next-line)
     ("p" . previous-line)
     ("h" . w3m-history)
     ("P" . w3m-view-previous-page)
     ("F" . w3m-view-next-page)
     ("c" . w3m-delete-buffer)
     ("C" . w3m-delete-other-buffers)
     ("d" . w3m-wget)
     ("o" . w3m-lnum-goto)
     ("<" . w3m-shift-left)
     (">" . w3m-shift-right)
     ("<f5>" . w3m-reload-this-page)
     ("M-<f5>" . w3m-reload-all-pages))
   w3m-mode-map))

(eval-after-load "w3m" '(w3m-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-w3m)

;;; init-w3m.el ends here
