;;; init-python.el -*- lexical-binding: t; -*-
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
;; Python mode configuration.

;;; Code:

;; ==================================================================================
;; Python development packages
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
;; Pyvenv
(use-package pyvenv
  :defer t
  :hook (python-mode . pyvenv-mode)
  :config
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              ;; Restart python
              (pyvenv-restart-python)
              ;; Restart lsp workspace
              (call-interactively 'lsp-workspace-restart))))

;; ==================================================================================
;; Poetry
(use-package poetry
  :defer t)

;; ==================================================================================
;; Sphinx doc
(use-package sphinx-doc
  :defer t
  :hook (python-mode . sphinx-doc-mode))

;; ==================================================================================
;; Python docstring
(use-package python-docstring
  :defer t
  :hook (python-mode . python-docstring-mode))

;; ==================================================================================
;; Python black
(use-package python-black
  :defer t)

;; ==================================================================================
;; Py-isort
(use-package py-isort
  :defer t)

;; ==================================================================================
;; With-venv
(use-package with-venv
  :defer t)

;; ==================================================================================
;; Python mode settings
(defun sphinx-doc-format ()
  "Sphinx documentation format."
  (interactive)
  (sphinx-doc)
  (python-docstring-fill))

(add-hook 'python-mode-hook
          (lambda ()
            ;; Enable lsp mode
            (lsp-deferred)
            ;; Copy the global before-save-hook to a local hook
            (setq-local before-save-hook
                        (default-value 'before-save-hook))
            ;; Format buffer before save
            (add-hook 'before-save-hook 'py-isort-before-save nil t)
            ;; Enable python black format mode
            (python-black-on-save-mode 1)))

;; ==================================================================================
;; Python mode keybindings
(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-offset 4)
  (lazy-set-key
   '(("C-c C-c" . comment-line)
     ("C-c d f" . sphinx-doc-format))
   python-mode-map))

;; ==================================================================================
;;; Provide features
(provide 'init-python)

;;; init-python.el ends here
