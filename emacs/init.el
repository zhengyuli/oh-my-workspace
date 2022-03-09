;;; package --- init.el ---
;; Time-stamp: <2022-03-09 14:05:55 Wednesday by zhengyu.li>

;; Copyright (C) 2021, 2022 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
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
;;Check Emacs version
(if (< (string-to-number emacs-version) 26.3)
	(error "The Emacs version must be >= 26.3."))

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
(defconst emacs-config-default-background "#282a36" "Emacs configuration default background.")

;; Emacs configuration default foreground
(defconst emacs-config-default-foreground "#f8f8f2" "Emacs configuration default foreground.")

;; Emacs configuration default fixed font
(defconst emacs-config-default-fixed-font "Source Code Pro" "Emacs configuration default fixed font.")

;; Emacs configuration default variable font
(defconst emacs-config-default-variable-font "Sans Serif" "Emacs configuration default variable font.")

(defun add-subdirs-to-load-path (base-dir)
  "Add subdirs to load path.
Look up all subdirs under `BASE-DIR' recrusively and add them into load path."
  (progn
    (add-to-list 'load-path base-dir)
    (setq default-directory base-dir)
    (normal-top-level-add-subdirs-to-load-path)))

;; Add all sub-directories under custom and site packages to load-path
(add-subdirs-to-load-path emacs-config-custom-path)
(add-subdirs-to-load-path emacs-config-site-packages-path)

;; Load librares
(load-library "init-basic-config")
(load-library "init-auto-complete")
(load-library "init-dired")
(load-library "init-terminal")
(load-library "init-w3m")
(load-library "init-git")
(load-library "init-prog-mode")
(load-library "init-cc-mode")
(load-library "init-sh-script-mode")
(load-library "init-python-mode")
(load-library "init-go-mode")
(load-library "init-elisp-mode")
(load-library "init-haskell-mode")
(load-library "init-scala-mode")
(load-library "init-groovy-mode")
(load-library "init-cmake-mode")
(load-library "init-dockerfile-mode")
(load-library "init-yaml-mode")
(load-library "init-markdown-mode")
(load-library "init-emms")
(load-library "init-netease-cloud-music")
(load-library "init-centaur-tabs")
(load-library "init-modeline")
(load-library "init-window")
(load-library "init-theme")
(load-library "init-session")

;; Load user custom settings
(if (not (file-exists-p "~/.emacs.d/custom.el"))
    (shell-command "touch ~/.emacs.d/custom.el"))
(load-file "~/.emacs.d/custom.el")

;;; init.el ends here
