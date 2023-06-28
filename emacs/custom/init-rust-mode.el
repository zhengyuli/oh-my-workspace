;;; package --- init-rust-mode.el -*- lexical-binding:t -*-

;; Copyright (c) 2022, 2023 Zhengyu Li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'init-rust-mode)

;;; Require:


;;; Code:
;; ==================================================================================
;; Customized settings for `rust-mode'
(defun rust-mode-settings ()
  "Settings for `rust-mode'."

  ;; Require
  (require 'rust-rustfmt)
  (require 'lsp-mode)
  (require 'lsp-rust)
  (require 'dap-mode)
  (require 'dap-lldb)

  ;; ----------------------------------------------------------
  (defun rust-debug-target-program-path ()
    "The function to get the path of the rust program to be debugged."
    (interactive)
    (let* ((project-path (vc-root-dir))
           (project-name (if project-path
                             (file-name-nondirectory
                              (directory-file-name project-path))
                           nil))
           (default-directory (if project-path
                                  (expand-file-name "target/debug/" project-path)
                                (file-name-directory (buffer-file-name))))
           (target-path (read-file-name "The rust program to be debugged: " nil project-name)))
      (expand-file-name target-path)))

  ;; ----------------------------------------------------------
  ;; Customize `rust-mode' related variables
  (customize-set-variable 'rust-format-on-save t)

  ;; Customize `lsp-rust' related variables
  (customize-set-variable 'lsp-rust-analyzer-server-display-inlay-hints t)
  (customize-set-variable 'lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (customize-set-variable 'lsp-rust-analyzer-display-chaining-hints t)
  (customize-set-variable 'lsp-rust-analyzer-display-chaining-hints t)
  (customize-set-variable 'lsp-rust-analyzer-display-closure-return-type-hints t)

  ;; ----------------------------------------------------------
  ;; Key bindings for `rust-mode'
  (lazy-unset-key
   '("C-c C-c C-u"
     "C-c C-c C-k"
     "C-c C-c C-t"
     "C-c C-c C-r"
     "C-c C-c C-l")
   rust-mode-map)

  (lazy-set-key
   '(("C-c C-c" . comment-line)
     ("C-x C-;" . rust-run))
   rust-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'rust-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              (setq-local dap-lldb-debug-program `(,(executable-find "lldb-vscode")))

              ;; Set rust dap lldb debugged program function
              (setq-local dap-lldb-debugged-program-function
                          'rust-debug-target-program-path)

              ;; Enable lsp mode
              (lsp-deferred))))

(eval-after-load "rust-mode" '(rust-mode-settings))

;;; init-rust-mode.el ends here
