;;; init-shell.el --- Shell script mode configuration -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-13 15:10:45 Friday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: shell, bash, sh
;; Dependencies: init-prog

;; This file is not part of GNU Emacs.

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
;; Shell script mode configuration.
;;

;;; Code:

;; ==================================================================================
(defun omw/ensure-bash-tools ()
  "Ensure bash-language-server is installed for LSP support."
  (unless (executable-find "bash-language-server")
    (message "Installing bash-language-server...")
    (shell-command "npm install -g bash-language-server")
    (message "bash-language-server installed successfully")))

(use-package sh-script
  :ensure nil
  :defer t
  :hook (sh-mode . omw/ensure-bash-tools))

;; ==================================================================================
;;; Provide features
(provide 'init-shell)

;;; init-shell.el ends here
