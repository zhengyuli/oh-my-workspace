;;; init-python.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-13 15:10:36 Friday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: python, pyvenv, poetry
;; Dependencies: (none)

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
;; Python mode configuration with virtual environment and Poetry support.
;;

;;; Code:

;; ==================================================================================
(defun omw/pyvenv-mode-line-indicator ()
  "Return pyvenv indicator for Python buffers."
  (when (and (eq major-mode 'python-mode)
             (bound-and-true-p pyvenv-virtual-env))
    (let* ((name (file-name-nondirectory
                  (directory-file-name pyvenv-virtual-env)))
           (icon (nerd-icons-devicon "nf-dev-python"
                                     :face 'nerd-icons-blue
                                     :height 0.9
                                     :v-adjust -0.05)))
      (concat icon (propertize
                    (format " [venv/%s] " name)
                    'face 'nerd-icons-blue
                    'help-echo (format "Python virtualenv: %s" pyvenv-virtual-env))))))

(defun omw/update-pyvenv-mode-line-indicator ()
  "Add mode-line indicator to current buffer."
  (setq-local mode-line-misc-info
              (cons '(:eval (omw/pyvenv-mode-line-indicator))
                    (remove '(:eval (omw/pyvenv-mode-line-indicator))
                            mode-line-misc-info))))

(defun omw/remove-pyvenv-mode-line-indicator ()
  "Remove mode-line indicator from current buffer."
  (setq-local mode-line-misc-info
              (remove '(:eval (omw/pyvenv-mode-line-indicator))
                      mode-line-misc-info)))

;; ==================================================================================
(use-package pyvenv
  :ensure t
  :demand t
  :config
  (add-to-list 'pyvenv-post-activate-hooks #'omw/update-pyvenv-mode-line-indicator)
  (add-to-list 'pyvenv-post-deactivate-hooks #'omw/remove-pyvenv-mode-line-indicator)
  (pyvenv-tracking-mode 1))

;; ==================================================================================
(use-package poetry
  :ensure t
  :demand t
  :config
  (poetry-tracking-mode 1))

;; ==================================================================================
(defun omw/ensure-python-tools ()
  "Ensure Python development tools (pylsp, black, isort, pylint) are installed."
  (dolist (spec '(("pylsp"  "pip install 'python-lsp-server[all]'")
                  ("black"  "pip install black black-macchiato")
                  ("isort"  "pip install isort")
                  ("pylint" "pip install pylint")))
    (let ((exe (nth 0 spec))
          (cmd (nth 1 spec)))
      (unless (executable-find exe)
        (message "Installing %s..." exe)
        (shell-command cmd)
        (message "%s installed successfully" exe)))))

(use-package python
  :ensure nil
  :defer t
  :hook (python-mode . omw/ensure-python-tools)
  :bind (:map python-mode-map
              ("C-c C-c" . comment-line))
  :config
  (setq python-indent-guess-indent-offset nil
        python-indent-guess-indent-offset-verbose nil))

;; ==================================================================================
;;; Provide features
(provide 'init-python)

;;; init-python.el ends here
