;;; early-init.el -*- lexical-binding: t; -*-

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: emacs, config
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;; Licensed under the GPL License version 3.0

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; History:
;;
;; 2026-03-16 11:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Early initialization file for Emacs 27+.
;; This file is loaded before the package system is initialized,
;; making it the correct place to set package-user-dir and user-emacs-directory.
;;
;; XDG variables defined here can be used directly in init.el.

;;; Code:

;; ==================================================================================
;; XDG Base Directory Configuration
;; Must be set before package system initializes (Emacs 27+ auto-initializes packages)

;; Define XDG directories as global variables for use in init.el
(defvar omw/xdg-data-home (or (getenv "XDG_DATA_HOME")
                              (expand-file-name "~/.local/share/"))
  "XDG data home directory.")

(defvar omw/xdg-cache-home (or (getenv "XDG_CACHE_HOME")
                               (expand-file-name "~/.cache/"))
  "XDG cache home directory.")

(defvar omw/xdg-state-home (or (getenv "XDG_STATE_HOME")
                               (expand-file-name "~/.local/state/"))
  "XDG state home directory.")

;; Emacs directory configuration
(setq user-emacs-directory (expand-file-name "emacs/" omw/xdg-data-home))

;; Package installation directory
(setq package-user-dir (expand-file-name "emacs/elpa/" omw/xdg-data-home))

;; ==================================================================================
;;; Provide features
(provide 'early-init)

;;; early-init.el ends here
