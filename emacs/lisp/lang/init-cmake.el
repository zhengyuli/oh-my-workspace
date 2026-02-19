;;; init-cmake.el -*- lexical-binding: t; -*-
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
;; CMake mode configuration.

;;; Code:

;; ==================================================================================
;; CMake mode
;; eglot already configured for cmake-mode eglot-ensure in init-prog-base.el
(use-package cmake-mode
  :defer t)

;; ==================================================================================
;; CMake Tools Validation
;; CMake LSP server validation
(defvar required-cmake-tools
  '((cmake-language-server . "pip install cmake-language-server"))
  "CMake LSP server.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'cmake-tools
 (lambda () (config-dependency-validate-executables required-cmake-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-cmake)

;;; init-cmake.el ends here
