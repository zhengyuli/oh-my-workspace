;;; init-python.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-21 21:48:36 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: init-functions

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
  "A list of packages to set up the Python development environment.")

;; Cache installed packages list to avoid repeated shell calls
(defvar python-installed-packages-cache nil
  "Cache of installed packages for current venv.")
(defvar python-cached-venv-path nil
  "Path of venv for which cache is valid.")

(defun python-get-package-base-name (package)
  "Extract base package name from PACKAGE (remove extras like [all])."
  (if (string-match "\\`\\([^\\[]+\\)" package)
      (match-string 1 package)
    package))

(defun python-get-installed-packages ()
  "Get list of installed packages, using cache if valid.
Returns an empty list if pip is not available or fails.
Logs errors instead of silently suppressing them."
  (let ((current-venv (or (getenv "VIRTUAL_ENV") "system")))
    ;; Clear cache if venv changes
    (unless (equal current-venv python-cached-venv-path)
      (setq python-installed-packages-cache nil
            python-cached-venv-path current-venv))
    ;; Get or refresh cache
    (or python-installed-packages-cache
        (setq python-installed-packages-cache
              (condition-case err
                  (let ((output (shell-command-to-string "pip list -format=freeze 2>/dev/null")))
                    (mapcar (lambda (line)
                              (car (split-string line "==")))
                            (split-string output "\n" t)))
                (error
                 (message "Warning: Failed to get pip packages: %s" (error-message-string err))
                 nil))))))

(defun python-get-installed-packages-async (callback)
  "Get installed packages asynchronously, call CALLBACK with result.
Uses cache if valid, otherwise spawns async process.
CALLBACK is called with a list of package names (strings)."
  (let ((current-venv (or (getenv "VIRTUAL_ENV") "system")))
    ;; Clear cache if venv changes
    (unless (equal current-venv python-cached-venv-path)
      (setq python-installed-packages-cache nil
            python-cached-venv-path current-venv))
    ;; Return cached result or fetch asynchronously
    (if python-installed-packages-cache
        (funcall callback python-installed-packages-cache)
      ;; Async fetch
      (let ((proc (make-process
                   :name "pip-list"
                   :buffer " *pip-list*"
                   :command '("pip" "list" "-format=freeze")
                   :sentinel
                   (lambda (proc _event)
                     (when (eq (process-status proc) 'exit)
                       (let ((buf (process-buffer proc)))
                         (with-current-buffer buf
                           (let ((packages
                                  (condition-case err
                                      (mapcar (lambda (line)
                                                (car (split-string line "==")))
                                              (split-string (buffer-string) "\n" t))
                                    (error
                                     (message "Warning: Failed to parse pip output: %s"
                                              (error-message-string err))
                                     nil))))
                             (kill-buffer buf)
                             (setq python-installed-packages-cache packages)
                             (funcall callback packages)))))))))
        proc))))

(defun python-validate-package-name (package)
  "Validate PACKAGE name contains only safe characters.
Returns non-nil if PACKAGE is a valid Python package name.
Valid format: letters, numbers, underscores, hyphens,
with optional extras like [all]."
  (string-match-p "\\`[a-zA-Z0-9_.-]+\\(\\[[a-zA-Z0-9_,]+\\]\\)?\\'" package))

(defun python-ensure-dev-packages ()
  "Ensure all dev packages are installed, with caching for performance.
Uses async package listing and installation to avoid blocking Emacs.
Package names are validated and properly shell-quoted to prevent injection."
  (python-get-installed-packages-async
   (lambda (installed)
     (let ((missing nil)
           (safe-packages nil))
       ;; Collect missing packages
       (dolist (package python-dev-packages)
         (let ((base-name (python-get-package-base-name package)))
           (unless (member base-name installed)
             (push package missing))))
       ;; Validate package names for security
       (dolist (package missing)
         (if (python-validate-package-name package)
             (push package safe-packages)
           (message "Warning: Skipping suspicious package name: %s" package)))
       ;; Install missing packages asynchronously with proper escaping
       (when safe-packages
         (let* ((quoted-packages (mapcar #'shell-quote-argument safe-packages))
                (packages-str (string-join quoted-packages " ")))
           (message "Installing missing Python packages (async): %s" (string-join safe-packages " "))
           (async-shell-command
            (concat "pip install " packages-str)
            "*Python Package Install*")))))))

(defun poetry-venv-activate-hook (&rest _)
  "Function to be run after `poetry-venv-workon'."
  (python-ensure-dev-packages))

(defun pyvenv-activate-hook (&rest _)
  "Function to be run after `pyvenv-workon'."
  (python-ensure-dev-packages))

(advice-add 'poetry-venv-workon :after #'poetry-venv-activate-hook)
(advice-add 'pyvenv-workon :after #'pyvenv-activate-hook)

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
                (eglot-reconnect (eglot-current-server))))))

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
  ;; C-c C-c already bound in prog-mode-map, no need to repeat
  (lazy-set-key
   '(("C-c d f" . sphinx-doc-format))
   python-mode-map))

;; ==================================================================================
;; Python Tools Validation
;; Python LSP server validation
(defvar required-python-tools
  '((pylsp . "pip install python-lsp-server[all]"))
  "Python LSP server and tools.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'python-tools
 (lambda () (config-dependency-validate-executables required-python-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-python)

;;; init-python.el ends here
