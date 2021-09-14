;;; package --- init-emms.el ---
;; Time-stamp: <2021-09-10 07:34:09 Friday by lizhengyu>

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
;;   (require 'init-emms)

;;; Require:
(require 'emms-auto)

;;; Code:
;; ==================================================================================
(defun emms-music ()
  "Emms play default directory."
  (interactive)
  (emms-play-directory-tree "~/Music")
  (emms))

;; ==================================================================================
(defun emms-settings ()
  "Settings for `emms'."

  ;; ----------------------------------------------------------
  ;; Require
  (require 'emms-info)
  (require 'emms-info-libtag)
  (require 'emms-player-mplayer)
  (require 'emms-player-simple)
  (require 'emms-playlist-mode)
  (require 'emms-playlist-sort)
  (require 'emms-source-file)

  ;; ----------------------------------------------------------
  ;; Customize `emms' related faces
  (custom-set-faces
   '(emms-playlist-track-face ((t (:foreground "#00FF00"))))
   '(emms-playlist-selected-face ((t (:foreground "#00FFFF")))))

  ;; ----------------------------------------------------------
  ;; Customize `emms' related variables
  (customize-set-variable 'emms-player-list '(emms-player-mplayer emms-player-mpg321 emms-player-ogg123))
  (customize-set-variable 'emms-info-asynchronously nil)
  (customize-set-variable 'emms-info-functions '(emms-info-libtag))
  (customize-set-variable 'emms-track-initialize-functions '(emms-info-initialize-track))
  (customize-set-variable 'emms-playlist-default-major-mode 'emms-playlist-mode)
  (customize-set-variable 'emms-playlist-sort-function 'emms-playlist-sort-by-natural-order)

  ;; ----------------------------------------------------------
  ;; Key bindings for `emms-playlist-mode'
  (lazy-set-key
   '(("p" . emms-previous)
     ("n" . emms-next)
     ("r" . emms-random)
     (">" . emms-seek-forward)
     ("<" . emms-seek-backward)
     ("x" . emms-pause)
     ("s" . emms-stop)
     ("z" . emms-show)
     ("?" . describe-mode)
     ("l" . emms-lyrics-toggle-display-on-minibuffer)
     ("L" . emms-lyrics-disable))
   emms-playlist-mode-map)

  ;; ----------------------------------------------------------
  ;; Setup emms
  (emms-all))

(eval-after-load "emms" '(emms-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-emms)

;;; init-emms.el ends here
