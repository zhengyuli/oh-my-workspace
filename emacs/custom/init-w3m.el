;;; package --- init-w3m.el ---
;; Time-stamp: <2021-09-11 06:19:44 Saturday by lizhengyu>

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
;;   (require 'init-w3m)

;;; Require:
(require 'w3m-extension-autoloads)
(require 'lazy-set-key)

;;; Code:
;; ==================================================================================
;; Customized settings for `w3m'
(defun w3m-settings ()
  "Settings for `w3m'."

  ;; Require
  (require 'browse-url)
  (require 'w3m-favicon)
  (require 'w3m-session)
  (require 'w3m-wget)
  (require 'w3m-lnum)
  (require 'w3m-extension)

  ;; ----------------------------------------------------------
  ;; Customize `w3m' related faces
  (custom-set-faces
   `(w3m-tab-background ((t (:background ,emacs-config-default-background))))
   '(w3m-tab-mouse ((t (:background "#666666" :foreground "#FFD700"))))
   '(w3m-tab-selected ((t (:background "#666666" :foreground "#00BFFF"))))
   '(w3m-tab-selected-background ((t (:background "#666666" :foreground "#00BFFF"))))
   '(w3m-tab-selected-retrieving ((t (:background "#666666" :foreground "#00BFFF"))))
   '(w3m-tab-unselected ((t (:background "#333333" :foreground "#999999"))))
   '(w3m-tab-unselected-retrieving ((t (:background "#333333" :foreground "#999999"))))
   '(w3m-tab-unselected-unseen ((t (:background "#333333" :foreground "#999999")))))

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
     ("3" . w3m-search-dict-cn)
     ("4" . w3m-search-google-web-en)
     ("5" . w3m-search-emacswiki)
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
     ("<down>" . w3m-visual-scroll-up)
     ("<up>" . w3m-visual-scroll-down)
     ("h" . w3m-history)
     ("P" . w3m-view-previous-page)
     ("F" . w3m-view-next-page)
     ("c" . w3m-delete-buffer)
     ("C" . w3m-delete-other-buffers)
     ("d" . w3m-wget)
     ("o" . w3m-lnum-goto)
     ("l" . w3m-copy-link-in-region)
     ("<" . w3m-shift-left)
     (">" . w3m-shift-right)
     ("<f5>" . w3m-reload-this-page)
     ("M-<f5>" . w3m-reload-all-pages))
   w3m-mode-map))

(eval-after-load "w3m" '(w3m-settings))

;; ==================================================================================
;; Global key bindings for `w3m'
(lazy-set-key
 '(("C-x C-z" . toggle-w3m-with-other-buffer)
   ("<f8>" . w3m-search-google-web-en)))

;; ==================================================================================
;;; Provide features
(provide 'init-w3m)

;;; init-w3m.el ends here
