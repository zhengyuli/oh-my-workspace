;;; init-shell.el -*- lexical-binding: t; -*-

;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: shell, bash, sh
;; Dependencies: init-prog

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
;; 2026-03-14 15:30 chieftain <lizhengyu419@outlook.com> created.

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
