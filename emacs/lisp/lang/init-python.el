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

;; 缓存已安装的包列表，避免重复 shell 调用
(defvar python--installed-packages-cache nil
  "Cache of installed packages for current venv.")
(defvar python--cached-venv-path nil
  "Path of venv for which cache is valid.")

(defun python--get-package-base-name (package)
  "Extract base package name from PACKAGE (remove extras like [all])."
  (if (string-match "\\`\\([^\\[]+\\)" package)
      (match-string 1 package)
    package))

(defun python--get-installed-packages ()
  "Get list of installed packages, using cache if valid."
  (let ((current-venv (or (getenv "VIRTUAL_ENV") "system")))
    ;; 如果 venv 变化，清除缓存
    (unless (equal current-venv python--cached-venv-path)
      (setq python--installed-packages-cache nil
            python--cached-venv-path current-venv))
    ;; 获取或刷新缓存
    (or python--installed-packages-cache
        (setq python--installed-packages-cache
              (let ((output (shell-command-to-string "pip list --format=freeze 2>/dev/null")))
                (mapcar (lambda (line)
                          (car (split-string line "==")))
                        (split-string output "\n" t)))))))

(defun python--ensure-dev-packages ()
  "Ensure all dev packages are installed, with caching for performance."
  (let ((installed (python--get-installed-packages))
        (missing nil))
    ;; 收集缺失的包
    (dolist (package python-dev-packages)
      (let ((base-name (python--get-package-base-name package)))
        (unless (member base-name installed)
          (push package missing))))
    ;; 批量安装缺失的包
    (when missing
      (message "Installing missing Python packages: %s" (string-join missing " "))
      (shell-command (concat "pip install " (string-join missing " ")))
      ;; 清除缓存以刷新列表
      (setq python--installed-packages-cache nil))))

(defun after-poetry-venv-workon (&rest _)
  "Function to be run after `poetry-venv-workon'."
  (python--ensure-dev-packages))

(defun after-pyvenv-workon (&rest _)
  "Function to be run after `pyvenv-workon'."
  (python--ensure-dev-packages))

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
              ;; Restart eglot if active
              (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
                (eglot-reconnect)))))

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
  ;; C-c C-c 已在 prog-mode-map 中绑定，无需重复
  (lazy-set-key
   '(("C-c d f" . sphinx-doc-format))
   python-mode-map))

;; ==================================================================================
;;; Provide features
(provide 'init-python)

;;; init-python.el ends here
