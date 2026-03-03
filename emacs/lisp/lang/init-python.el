;;; init-python.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 21:46:48 星期一 by zhengyu.li>

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

;;; Code:

;; ==================================================================================
;; Pyvenv - Python virtual environment management
;; Automatic tracking of virtualenv directories for seamless project switching
(use-package pyvenv
  :ensure t
  :defer t
  :hook (python-mode . pyvenv-tracking-mode)
  :init
  (defun my/pyvenv-mode-line-indicator ()
    "Return pyvenv indicator for mode line."
    (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env
         (let ((name (file-name-nondirectory (directory-file-name pyvenv-virtual-env))))
           (propertize (format " 🐍[%s] " name)
                       'face 'font-lock-constant-face
                       'help-echo pyvenv-virtual-env))))
  (add-to-list 'mode-line-misc-info
               '(:eval (my/pyvenv-mode-line-indicator))
               'append))

;; ==================================================================================
;; Poetry - Python dependency and packaging manager
;; Integration with Poetry projects for modern Python development
(use-package poetry
  :ensure t
  :defer t
  :hook (python-mode . poetry-tracking-mode)
  :init
  (defun my/poetry-mode-line-indicator ()
    "Return Poetry project indicator for mode line."
    (and (fboundp 'poetry-find-project-name)
         (let ((name (poetry-find-project-name)))
           (and name
                (propertize (format " 📦%s " name)
                            'face 'font-lock-keyword-face
                            'help-echo "Poetry project")))))
  (add-to-list 'mode-line-misc-info
               '(:eval (my/poetry-mode-line-indicator))
               'append))

;; ==================================================================================
;; Python mode - built-in Python editing support
;; Use fixed 4-space indentation (PEP 8 standard) without auto-detection
(use-package python
  :ensure nil
  :defer t
  :bind
  (:map python-mode-map
        ("C-c C-c" . comment-line))
  :config
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil
        python-indent-guess-indent-offset-verbose nil))

;; ==================================================================================
;;; Provide features
(provide 'init-python)

;;; init-python.el ends here
