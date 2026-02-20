;;; init-yaml.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: init-functions, init-prog-base

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
;; YAML mode configuration.

;;; Code:

;; ==================================================================================
;; YAML mode
;; eglot already configured for yaml-mode eglot-ensure in init-prog-base.el
(use-package yaml-mode
  :ensure t
  :defer t)

;; ==================================================================================
;; YAML Tools Validation
;; YAML LSP server validation
(defvar required-yaml-tools
  '((yaml-language-server . "npm install -g yaml-language-server"))
  "YAML LSP server.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'yaml-tools
 (lambda () (config-dependency-validate-executables required-yaml-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-yaml)

;;; init-yaml.el ends here
