;;; omw-go.el -*- lexical-binding: t; -*-

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: go, golang
;; Dependencies: omw-prog

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
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Go mode configuration with LSP support via gopls.

;;; Code:

;; ==================================================================================
(defun omw/ensure-go-tools ()
  "Install Go development tools (gopls, gofumpt) via go install if not present."
  (interactive)
  (require 'omw-utils)
  (omw/tools-install
   '("gopls" "go install golang.org/x/tools/gopls@latest" "go")
   '("gofumpt" "go install mvdan.cc/gofumpt@latest" "go")))

(use-package go-mode
  :ensure t
  :defer t)

;; ==================================================================================
;;; Provide features
(provide 'omw-go)

;;; omw-go.el ends here
