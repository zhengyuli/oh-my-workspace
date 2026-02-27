;;; init-shell.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: init-funcs, init-prog

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
;; Shell script mode configuration.

;;; Code:

;; ==================================================================================
;; Shell mode hook
;; eglot already configured for sh-mode eglot-ensure in init-prog.el
;; C-c C-c already bound in prog-mode-map, sh-mode inherits this binding

;; ==================================================================================
;; Shell Tools Validation
;; Shell LSP server validation
(defvar required-shell-tools
  '((bash-language-server . "npm install -g bash-language-server"))
  "Shell/Bash LSP server.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'shell-tools
 (lambda () (config-dependency-validate-executables required-shell-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-shell)

;;; init-shell.el ends here
