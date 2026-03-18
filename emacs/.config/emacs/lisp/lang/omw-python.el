;;; omw-python.el -*- lexical-binding: t; -*-

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: python, uv, ruff, pet
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
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.
;; 2026-03-16       drop pyvenv/poetry, adopt pet.el for uv venv detection;
;;                  ruff format via stdin; mode-line driven by pet.

;;; Commentary:
;;
;; Python mode configuration with uv and pet support.

;;; Code:

;; ==================================================================================
(use-package pet
  :ensure t
  :defer t)

;; ==================================================================================
(defvar-local omw/pet-virtualenv-root nil
  "Buffer-local venv root path detected by pet.")

(defun omw/pet-mode-line-indicator ()
  "Return pyvenv indicator for Python buffers."
  (when (and (derived-mode-p 'python-mode)
             omw/pet-virtualenv-root)
    (let* ((venv-dir (directory-file-name omw/pet-virtualenv-root))
           (venv-name (file-name-nondirectory venv-dir))
           (display-name (if (string= venv-name ".venv")
                             (file-name-nondirectory
                              (directory-file-name
                               (file-name-directory venv-dir)))
                           venv-name))
           (icon (nerd-icons-devicon "nf-dev-python"
                                     :face 'nerd-icons-blue
                                     :height 0.9
                                     :v-adjust -0.05)))
      (concat icon (propertize
                    (format " [venv/%s] " display-name)
                    'face 'nerd-icons-blue
                    'help-echo (format "Python virtualenv: %s"
                                       omw/pet-virtualenv-root))))))

(defvar omw/pet-mode-line-entry
  '(:eval (omw/pet-mode-line-indicator))
  "Mode-line entry for pet-based venv indicator.")

(defun omw/update-pet-mode-line-indicator ()
  "Add mode-line indicator to current buffer."
  (unless (member omw/pet-mode-line-entry mode-line-misc-info)
    (setq-local mode-line-misc-info
                (cons omw/pet-mode-line-entry mode-line-misc-info))))

;; ==================================================================================
(defun omw/pet-setup ()
  "Setup pet for current Python buffer."
  (require 'pet)
  (when-let ((venv (pet-virtualenv-root)))
    (setq-local omw/pet-virtualenv-root venv)
    (omw/update-pet-mode-line-indicator)
    (setq-local python-shell-interpreter (pet-executable-find "python")
                python-shell-interpreter-args "-i")
    (pet-eglot-setup)))

;; ==================================================================================
(defun omw/ensure-python-tools ()
  (interactive)
  (require 'omw-utils)
  (omw/tools-install '("ruff" "uv tool install ruff" "uv")))

;; ==================================================================================
(defun omw/python-format-buffer ()
  "Format current Python buffer using ruff."
  (when (and (executable-find "ruff")
             (buffer-file-name))
    (let* ((original (buffer-string))
           (filename (buffer-file-name))
           (formatted
            (with-temp-buffer
              (insert original)
              (when (zerop (call-process-region
                            (point-min) (point-max)
                            "ruff" t t nil
                            "format" "--stdin-filename" filename "-"))
                (buffer-string)))))
      (when (and formatted (not (string= original formatted)))
        (erase-buffer)
        (insert formatted)))))

(defun omw/python-before-save ()
  "Run Python-specific actions before saving."
  (when (derived-mode-p 'python-mode)
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
  :hook ((python-mode . omw/pet-setup)
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
