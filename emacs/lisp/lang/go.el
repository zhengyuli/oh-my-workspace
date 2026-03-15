;;; go.el -*- lexical-binding: t; -*-

;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: go, golang
;; Dependencies: prog

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
;; Go mode configuration with LSP support via gopls.

;;; Code:

;; ==================================================================================
(defun omw/ensure-go-tools ()
  "Ensure Go development tools (gopls, gofumpt) are installed."
  ;; Check and install each tool if missing
  (dolist (spec '(("gopls"   "go install golang.org/x/tools/gopls@latest")
                  ("gofumpt" "go install mvdan.cc/gofumpt@latest")))
    ;; spec format: (executable-name install-command)
    (let ((exe (nth 0 spec))
          (cmd (nth 1 spec)))
      (unless (executable-find exe)
        (message "Installing %s..." exe)
        (shell-command cmd)
        (message "%s installed successfully" exe)))))

(use-package go-mode
  :ensure t
  :defer t
  :hook (go-mode . omw/ensure-go-tools))

;; ==================================================================================
;;; Provide features
(provide 'go)

;;; go.el ends here
