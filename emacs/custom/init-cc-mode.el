;;; package --- init-cc-mode.el ---
;; Time-stamp: <2022-03-10 21:28:19 Thursday by zhengyu.li>

;; Copyright (C) 2021, 2022 zhengyu li
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
;;   (require 'init-cc-mode)

;;; Require:

;;; Code:
;; ==================================================================================
(defun rtags-cmake-export-commands (root-dir)
  "Call `cmake' to export `compile_commands.json' for rtags to index."
  (interactive (list (read-directory-name "Project root directory: " "./")))
  (let ((source-dir root-dir)
		(build-dir (expand-file-name ".cmake_rtags_build" root-dir)))
	(shell-command
	 (format "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -S %s -B %s" source-dir build-dir)
	 nil "*_CMAKE_Export_Errors_*")))

(defun rtags-index-project (root-dir)
  "Call `rtags-rc' to index project."
  (interactive (list (read-directory-name "Project root directory: " "./")))
  (let ((rtags-exec-path (if (executable-find "rc")
							 (executable-find "rc")
						   (executable-find "rtags-rc")))
		(source-dir root-dir)
		(build-dir (expand-file-name ".cmake_rtags_build" root-dir)))
	(cond ((file-exists-p (expand-file-name "CMakeLists.txt" source-dir))
		   (rtags-cmake-export-commands source-dir))
		  (t (error "Unsupported C/C++ project, should be managed by cmake...")))
	(shell-command (concat rtags-exec-path " -J " build-dir) nil "*_RTAGS_Index_Errors_*")))

;; ==================================================================================
;; Customized settings for `cc-mode'
(defun cc-mode-settings ()
  "Settings for `cc-mode'."

  ;; Require
  (require 'ctypes)
  (require 'rtags)
  (require 'rtags-xref)
  (require 'flycheck-rtags)
  (require 'google-c-style)

  ;; ----------------------------------------------------------
  ;; Customize `c-mode' and `c++-mode' mode related variables
  (customize-set-variable 'ctypes-file-name "~/.emacs.d/ctypes")
  (customize-set-variable 'ctypes-write-types-at-exit t)
  (customize-set-variable 'rtags-completions-enabled t)
  (customize-set-variable 'rtags-autostart-diagnostics t)

  ;; ----------------------------------------------------------
  ;; Key bindings for `cc-mode'
  (lazy-set-key
   '(("C-c C-c" . smart-comment)
     ("C-c k" . smart-uncomment))
   c-mode-base-map)

  ;; ----------------------------------------------------------
  ;; Hooks for `cc-mode'
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook
              (lambda ()
                ;; ----------------------------------------------------------
                (if (not (vc-registered (buffer-file-name)))
                    (flycheck-select-checker 'c/c++-clang)

                  ;; Set flycheck checker with `rtags'
                  (flycheck-select-checker 'rtags)

                  ;; Enable rtags xref
                  (rtags-xref-enable)

                  ;; Start rtags process if any
                  (rtags-start-process-unless-running))

                ;; Enable ctypes auto parse mode
    		    (ctypes-auto-parse-mode 1)

                ;; Load ctypes saved previously
    		    (ctypes-read-file nil nil t t)

                ;; Enable google C&C++ style
    		    (google-set-c-style)))))

(eval-after-load "cc-mode" '(cc-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-cc-mode)

;;; init-cc-mode.el ends here
