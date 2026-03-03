;;; init-cmake.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 21:48:46 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: cmake
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
;; CMake mode configuration with LSP support.
;; LSP server (cmake-language-server) is configured in init-prog.el.

;;; Code:

;; ==================================================================================
;; CMake mode - major mode for CMake build system files
;; Provides syntax highlighting and basic editing support for CMakeLists.txt
(use-package cmake-mode
  :ensure t
  :defer t)

;; ==================================================================================
;;; Provide features
(provide 'init-cmake)

;;; init-cmake.el ends here
