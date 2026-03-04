;;; init-cc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-04 13:36:34 Wednesday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: c, cpp
;; Dependencies: init-prog

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
;; C/C++ mode configuration with clangd LSP and Google code style.
;; LSP server (clangd) is configured in init-prog.el.

;;; Code:

;; ==================================================================================
;; C/C++ utility functions
(defun my/generate-compile-commands (root-dir)
  "Generate compile_commands.json for clangd LSP indexing by running cmake."
  (interactive (list (read-directory-name "Project root directory: " "./")))
  (let* ((build-dir (expand-file-name "build" root-dir))
         (quoted-source (shell-quote-argument root-dir))
         (quoted-build (shell-quote-argument build-dir)))
    (shell-command
     (format "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -S %s -B %s" quoted-source quoted-build)
     nil "*_CMAKE_Export_Errors_*")))

(defun my/cc-get-debug-target-program-path ()
  "Get absolute path of target C/C++ program to debug with smart project defaults."
  (interactive)
  (let* ((project-path (vc-root-dir))
         (project-name (when project-path
                         (file-name-nondirectory
                          (directory-file-name project-path))))
         (default-directory (if project-path
                                (expand-file-name "build/" project-path)
                              (file-name-directory (buffer-file-name))))
         (target-path (read-file-name "C/C++ program to debug: " nil project-name)))
    (expand-file-name target-path)))

;; ==================================================================================
;; Google C/C++ code style - indentation and formatting standards
(use-package google-c-style
  :ensure t
  :defer t)

;; ==================================================================================
(use-package cc-mode
  :ensure nil
  :defer t
  :hook ((c-mode . google-set-c-style)
         (c++-mode . google-set-c-style)))

;; ==================================================================================
;;; Provide features
(provide 'init-cc)

;;; init-cc.el ends here
