;;; package --- init-cc-mode.el ---
;; Time-stamp: <2022-03-14 20:05:59 Monday by zhengyuli>

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
(defun generate-compile-commands (root-dir)
  "Call `cmake' to generate `compile_commands.json' for clangd to index."
  (interactive (list (read-directory-name "Project root directory: " "./")))
  (let ((source-dir root-dir)
		(build-dir (expand-file-name "build" root-dir)))
	(shell-command
	 (format "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -S %s -B %s" source-dir build-dir)
	 nil "*_CMAKE_Export_Errors_*")))

;; ==================================================================================
;; Customized settings for `cc-mode'
(defun cc-mode-settings ()
  "settings for `cc-mode'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `cc-mode'
  (lazy-set-key
   '(("C-c C-c" . smart-comment)
     ("C-c k" . smart-uncomment))
   c-mode-base-map))

(eval-after-load "cc-mode" '(cc-mode-settings))

;; Customized settings for `c-mode' and `c++-mode' mode
(defun c&c++-mode-settings ()
  "Settings for `c-mode' and `c++-mode' mode."

  ;; Require
  (require 'ctypes)
  (require 'google-c-style)
  (require 'lsp-mode)

  ;; ----------------------------------------------------------
  ;; Customize `ctypes' related variables
  (customize-set-variable 'ctypes-file-name "~/.emacs.d/ctypes")
  (customize-set-variable 'ctypes-write-types-at-exit t)

  ;; ----------------------------------------------------------
  ;; Hooks
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook
              (lambda ()
                ;; Enable ctypes auto parse mode
    		    (ctypes-auto-parse-mode 1)

                ;; Load ctypes saved previously
    		    (ctypes-read-file nil nil t t)

                ;; Enable google cc style
    		    (google-set-c-style)

                ;; Enable lsp mode
                (lsp-deferred)))))

(eval-after-load "cc-mode" '(c&c++-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-cc-mode)

;;; init-cc-mode.el ends here
