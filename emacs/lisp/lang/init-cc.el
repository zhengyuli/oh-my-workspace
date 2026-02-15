;;; init-cc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none

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
;; C/C++ mode configuration.

;;; Code:

;; ==================================================================================
;; Google C style
(use-package google-c-style
  :defer t)

;; ==================================================================================
;; Utility function
(defun generate-compile-commands (root-dir)
  "Call `cmake' to generate `compile_commands.json' for clangd to index."
  (interactive (list (read-directory-name "Project root directory: " "./")))
  (let ((source-dir root-dir)
        (build-dir (expand-file-name "build" root-dir)))
    (shell-command
     (format "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -S %s -B %s" source-dir build-dir)
     nil "*_CMAKE_Export_Errors_*")))

(defun cc-debug-target-program-path ()
  "The function to get the path of the c&c++ program to be debugged."
  (interactive)
  (let* ((project-path (vc-root-dir))
         (project-name (if project-path
                           (file-name-nondirectory
                            (directory-file-name project-path))
                         nil))
         (default-directory (if project-path
                                (expand-file-name "build/" project-path)
                              (file-name-directory (buffer-file-name))))
         (target-path (read-file-name "The c&c++ program to be debugged: " nil project-name)))
    (expand-file-name target-path)))

;; ==================================================================================
;; C/C++ mode hooks
;; eglot 已在 init-prog-base.el 中配置 c-mode/c++-mode 的 eglot-ensure
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook
            (lambda ()
              ;; Enable google cc style
              (google-set-c-style))))

;; ==================================================================================
;;; Provide features
(provide 'init-cc)

;;; init-cc.el ends here
