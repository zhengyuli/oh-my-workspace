;;; omw-python.el -*- lexical-binding: t; -*-

;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: python, pyvenv, poetry
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;; Licensed under the GPL License version 3.0

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; History:
;;
;; 2026-03-14 15:30 chieftain <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Python mode configuration with virtual environment and Poetry support.

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
  "Ensure Python development tools (basedpyright, ruff) are installed via uv."
  (dolist (spec '(("basedpyright" . "uv tool install basedpyright")
                  ("ruff" . "uv tool install ruff")))
    (let ((exe (car spec))
          (cmd (cdr spec)))
      (unless (executable-find exe)
        (message "Installing %s via uv..." exe)
        (shell-command cmd)
        (message "%s installed successfully" exe)))))

;; ==================================================================================
(defun omw/python-format-buffer ()
  "Format current Python buffer using ruff."
  (when (and (executable-find "ruff")
             (buffer-file-name))
    (let ((temp-file (make-temp-file "ruff-format-")))
      (write-region (point-min) (point-max) temp-file nil 'silent)
      (call-process "ruff" nil nil nil "format" temp-file)
      (insert-file-contents temp-file nil nil nil t)
      (delete-file temp-file))))

(defun omw/python-before-save ()
  "Run Python-specific actions before saving."
  (when (eq major-mode 'python-mode)
    (omw/python-format-buffer)))

(define-minor-mode omw/python-before-save-mode
  "Minor mode for Python buffers to run custom before-save hooks."
  :lighter " PySave"
  :global nil
  (if omw/python-before-save-mode
      (add-hook 'before-save-hook #'omw/python-before-save nil t)
    (remove-hook 'before-save-hook #'omw/python-before-save t)))

;; ==================================================================================
(use-package python
  :ensure nil
  :defer t
  :hook ((python-mode . omw/ensure-python-tools)
         (python-mode . omw/python-before-save-mode))
  :bind (:map python-mode-map
              ("C-c C-c" . comment-line))
  :config
  (setq python-indent-guess-indent-offset nil
        python-indent-guess-indent-offset-verbose nil))

;; ==================================================================================
;;; Provide features
(provide 'omw-python)

;;; omw-python.el ends here
