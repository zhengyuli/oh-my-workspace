;; -*- Emacs-Lisp -*-
;;; init.el ---
;; Time-stamp: <2021-06-26 05:19:58 Saturday by lizhengyu>

;; Copyright (C) 2013 zhengyu li
;;
;; Author: chieftain <lizhengyu419@gmail.com>
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
;;   (require 'init)

;;; Require:

;;; Code:
;; Emacs configuration root path
(defconst emacs-config-root-path "_EMACS_CONFIG_ROOT_PATH_"
  "Emacs configuration root path.")

;; Emacs configuration custom path to load custom settings
(defconst emacs-config-custom-path (concat emacs-config-root-path "custom/")
  "Emacs configuration custom path.")

;; Emacs configuration site packages path to load 3rd party settings
(defconst emacs-config-site-packages-path (concat emacs-config-root-path "site-packages/")
  "Emacs configuration site packages path.")

;; Emacs configuration default background
(defconst emacs-config-default-background "#252525" "Emacs configuration default background.")

;; Emacs configuration default foreground
(defconst emacs-config-default-foreground "#FFEFDF" "Emacs configuration default foreground.")

;; Emacs configuration default fixed font
(defconst emacs-config-default-fixed-font "Source Code Pro" "Emacs configuration default fixed font.")

;; Emacs configuration default variable font
(defconst emacs-config-default-variable-font "Sans Serif" "Emacs configuration default variable font.")

(defun add-subdirs-to-load-path (base-dir)
  "Add subdirs to load path.
Look up all subdirs under `BASE-DIR' recrusively and add them into load path."
  (let ((default-directory (concat base-dir "/")))
    (add-to-list 'load-path base-dir)
    (normal-top-level-add-subdirs-to-load-path)))

;; Add all sub-directories under custom and site packages to load-path
(add-subdirs-to-load-path emacs-config-custom-path)
(add-subdirs-to-load-path emacs-config-site-packages-path)

;; Load librares
(load-library "init-basic")
(load-library "init-theme")
(load-library "init-powerline")
(load-library "init-centaur-tabs")
(load-library "init-window-number")
(load-library "init-kill-ring")
(load-library "init-undo-tree")
(load-library "init-yasnippet")
(load-library "init-company-mode")
(load-library "init-comint")
(load-library "init-multi-term")
(load-library "init-multiple-cursors")
(load-library "init-cua-mode")
(load-library "init-iman")
(load-library "init-ascii")
(load-library "init-tty-format")
(load-library "init-color-rg")
(load-library "init-ispell")
(load-library "init-template")
(load-library "init-calendar")
(load-library "init-eperiodic")
(load-library "init-google-translate")
(load-library "init-visual-regex")
(load-library "init-dired")
(load-library "init-tramp")
(load-library "init-emms")
(load-library "init-w3m")
(load-library "init-magit")
(load-library "init-ivy-mode")
(load-library "init-view-mode")
(load-library "init-hexview-mode")
(load-library "init-prog-mode")
(load-library "init-elisp-mode")
(load-library "init-cc-mode")
(load-library "init-c&c++-mode")
(load-library "init-haskell-mode")
(load-library "init-go-mode")
(load-library "init-scala-mode")
(load-library "init-sh-script-mode")
(load-library "init-python-mode")
(load-library "init-groovy-mode")
(load-library "init-yaml-mode")
(load-library "init-markdown-mode")
(load-library "init-json-mode")
(load-library "init-dockerfile-mode")
(load-library "init-cmake-mode")
(load-library "init-wangyi-music")
(load-library "init-workgroups2")
(load-library "init-proxy")

;; Load user custom settings finally
(if (not (file-exists-p "~/.emacs.d/custom.el"))
    (shell-command "touch ~/.emacs.d/custom.el")
  (load-file "~/.emacs.d/custom.el"))

(toggle-fullscreen)

;;; init.el ends here
