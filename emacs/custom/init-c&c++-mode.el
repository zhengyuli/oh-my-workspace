;;; package --- init-c&c++-mode.el -*- lexical-binding:t -*-
;; Time-stamp: <2023-06-28 15:17:15 星期三 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023 zhengyu li
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
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

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'init-c&c++-mode)

;;; Require:

;;; Code:
;; ==================================================================================
(defun generate-compile-commands (root-dir)
  "Call `cmake' to generate `compile_commands.json' for clangd to index."
  (interactive (list (read-directory-name "Project root directory: " "./")))
  (let ((source-dir root-dir)
		(build-dir (expand-file-name "build" root-dir)))
	(shell-command
	 (format "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -S %s -B %s" source-dir build-dir)
	 nil "*_CMAKE_Export_Errors_*")))

;; ==================================================================================
;; Customized settings for `c-mode' and `c++-mode' mode
(defun c&c++-mode-settings ()
  "Settings for `c-mode' and `c++-mode' mode."

  ;; Require
  (require 'google-c-style)
  (require 'doxymacs)
  (require 'lsp-mode)
  (require 'dap-mode)
  (require 'dap-lldb)

  ;; ----------------------------------------------------------
  (defun c&c++-debug-target-program-path ()
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

  ;; ----------------------------------------------------------
  ;; Hooks
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook
              (lambda ()
                ;; ----------------------------------------------------------
                (setq-local dap-lldb-debug-program `(,(executable-find "lldb-vscode")))

                ;; Set c&c++ dap lldb debugged program function
                (setq-local dap-lldb-debugged-program-function
                            'c&c++-debug-target-program-path)

                ;; Enable google cc style
    		    (google-set-c-style)

                ;; Enable doxymacs mode
                (doxymacs-mode 1)

                ;; Enable lsp mode
                (lsp-deferred)))))

(eval-after-load "cc-mode" '(c&c++-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-c&c++-mode)

;;; init-c&c++-mode.el ends here
