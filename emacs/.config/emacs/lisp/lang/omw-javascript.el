;;; omw-javascript.el -*- lexical-binding: t; -*-

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: typescript, ts
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
;; TypeScript mode configuration with LSP support.

;;; Code:

;; ==================================================================================
(defun omw/ensure-typescript-tools ()
  (interactive)
  (require 'omw-utils)
  (omw/tools-install '("typescript-language-server"
                       "bun install -g typescript-language-server typescript"
                       "bun")))

(use-package typescript-mode
  :ensure t
  :defer t)

;; ==================================================================================
;;; Provide features
(provide 'omw-javascript)

;;; omw-javascript.el ends here
