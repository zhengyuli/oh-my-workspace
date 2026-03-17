;;; omw-shell.el -*- lexical-binding: t; -*-

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: shell, bash, sh
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
;; Shell script mode configuration.

;;; Code:

;; ==================================================================================
(defun omw/ensure-bash-tools ()
  (interactive)
  (require 'omw-utils)
  (omw/tools-install '("bash-language-server"
                       "bun install -g bash-language-server"
                       "bun")))

(use-package sh-script
  :ensure nil
  :defer t
  :bind (:map sh-mode-map
              ("C-c C-c" . comment-line)))

;; ==================================================================================
;;; Provide features
(provide 'omw-shell)

;;; omw-shell.el ends here
