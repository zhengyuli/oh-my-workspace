;;; package --- init-python-mode.el -*- lexical-binding:t -*-
;; Time-stamp: <2024-05-10 08:36:14 Friday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024 zhengyu li
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
;;   (require 'init-python-mode)

;;; Require:

;;; Code:
;; ==================================================================================
(defvar python-dev-packages
  '("python-lsp-server[all]"
    "pylint"
    "black"
    "black-macchiato"
    "debugpy")
  "A list of packages to setup python development environment.")

(defun pip-check-and-install (package)
  "Check if a Python package is installed. If not, install it."
  (let ((pkg (if (string-match ".*\\[.*\\]" package)
                 (car (split-string package "\\["))
               package)))
    (if (string-match-p (concat "not found: " pkg)
                        (shell-command-to-string (concat "pip show " pkg)))
        (shell-command (regexp-quote (concat "pip install " package))))))

(defun after-poetry-venv-workon (&rest _)
  "Function to be run after `poetry-venv-workon'."
  (dolist (package python-dev-packages)
    (pip-check-and-install package)))

(defun after-pyvenv-workon (&rest _)
  "Function to be run after `pyvenv-workon'."
  (dolist (package python-dev-packages)
    (pip-check-and-install package)))

(advice-add 'poetry-venv-workon :after #'after-poetry-venv-workon)
(advice-add 'pyvenv-workon :after #'after-pyvenv-workon)

;; ==================================================================================
;; Customized settings for `pyvenv'
(defun pyvenv-settings ()
  "Settings for `pyvenv'."

  ;; Require
  (require 'lsp-mode)

  ;; ----------------------------------------------------------
  ;; Enable global pyvenv mode
  (pyvenv-mode 1)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Restart python
              (pyvenv-restart-python)

              ;; Restart lsp workspace
              (call-interactively 'lsp-workspace-restart))))

(eval-after-load "pyvenv" '(pyvenv-settings))

;; ==================================================================================
;; Customized settings for `dap-python'
(defun dap-python-settings ()
  "Settings for `dap-python'."

  ;; Require
  (require 'with-venv)

  ;; ----------------------------------------------------------
  ;; Redefine `dap-python--pyenv-executable-find' defined in `dap-python'
  (defun dap-python--pyenv-executable-find (command)
    "Find executable COMMAND, taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil. This works around
https://github.com/pyenv/pyenv-which-ext."
    (with-venv (executable-find command)))

  ;; ----------------------------------------------------------
  ;; Customize `dap-python' related variables
  (customize-set-variable 'dap-python-debugger 'debugpy)

  (dap-register-debug-template "Python :: Run file (buffer)"
                               (list :type "python"
                                     :args ""
                                     :justMyCode nil
                                     :cwd nil
                                     :module nil
                                     :program nil
                                     :request "launch"
                                     :name "Python :: Run file (buffer)"))

  (dap-register-debug-template "Python :: Run file from project directory"
                               (list :type "python"
                                     :args ""
                                     :justMyCode nil
                                     :cwd "${workspaceFolder}"
                                     :module nil
                                     :program nil
                                     :request "launch")))

(eval-after-load "dap-python" '(dap-python-settings))

;; ==================================================================================
;; Customized settings for `python-mode'
(defun python-mode-settings ()
  "Settings for `python-mode'."

  ;; Require
  (require 'flycheck)
  (require 'sphinx-doc)
  (require 'python-docstring)
  (require 'py-isort)
  (require 'python-black)
  (require 'lsp-mode)
  (require 'dap-mode)
  (require 'dap-python)

  ;; ----------------------------------------------------------
  (defun sphinx-doc-format ()
    "Sphinx documentation format."
    (interactive)
    (sphinx-doc)
    (python-docstring-fill))

  ;; ----------------------------------------------------------
  ;; Customize `python-mode' related variables
  (customize-set-variable 'python-indent-guess-indent-offset-verbose nil)
  (customize-set-variable 'python-indent-offset 4)

  ;; ----------------------------------------------------------
  ;; Key bindings for `python-mode'
  (lazy-set-key
   '(("C-c C-c" . comment-line)
     ("C-c d f" . sphinx-doc-format))
   python-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'python-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable sphinx doc mode
              (sphinx-doc-mode 1)

              ;; Enable python docstring mode
              (python-docstring-mode 1)

              ;; Copy the global before-save-hook to a local hook
              (setq-local before-save-hook
                          (default-value 'before-save-hook))

              ;; Format buffer before save
              (add-hook 'before-save-hook 'py-isort-before-save nil t)

              ;; Enable python black format mode
              (python-black-on-save-mode 1)

              ;; Enable lsp mode
              (lsp-deferred))))

(eval-after-load "python" '(python-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-python-mode)

;;; init-python-mode.el ends here
