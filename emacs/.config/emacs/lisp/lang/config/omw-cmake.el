;;; omw-cmake.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: cmake
;; Dependencies: omw-prog, omw-utils

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
;; CMake mode configuration with LSP support.
;; LSP server (cmake-language-server) is configured in omw-prog.el.

;;; Code:

;; ============================================================================
(defvar omw/cmake-tool-specs
  '(("cmake-language-server" "uv tool install cmake-language-server" "uv"))
  "Tool specs for CMake development.")

(defun omw/install-cmake-tools ()
  "Install CMake LSP tools (cmake-language-server) via uv if not present."
  (interactive)
  (require 'omw-utils)
  (apply #'omw/tools-install omw/cmake-tool-specs))

;; ============================================================================
(defun omw/cmake-mode-setup ()
  "Apply custom settings for cmake mode."
  (require 'omw-utils)
  (setq-local cmake-tab-width 2)
  (apply #'omw/tools-check-and-prompt omw/cmake-tool-specs))

(use-package cmake-mode
  :ensure t
  :defer t
  :hook (cmake-mode . omw/cmake-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-cmake)

;;; omw-cmake.el ends here
