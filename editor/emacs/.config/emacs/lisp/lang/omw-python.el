;;; omw-python.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:32:09 Friday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: python, uv, ruff, pet
;; Dependencies: pet, omw-utils

;; Copyright (C) 2026 zhengyu li

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.
;; 2026-03-16       drop pyvenv/poetry, adopt pet.el for uv venv detection;
;;                  ruff format via stdin; mode-line driven by pet.

;;; Commentary:
;;
;; Python mode configuration with uv and pet support.

;;; Code:

;; ============================================================================
(use-package pet
  :ensure t
  :defer t)

;; ============================================================================
(defvar-local omw/pet-virtualenv-root nil
  "Buffer-local venv root path detected by pet.")

(defun omw/pet-mode-line-indicator ()
  "Return pet venv indicator for Python buffers."
  (when (and (derived-mode-p 'python-mode)
             omw/pet-virtualenv-root)
    (let* ((venv-dir (directory-file-name omw/pet-virtualenv-root))
           (venv-name (file-name-nondirectory venv-dir))
           (display-name (if (string= venv-name ".venv")
                             (file-name-nondirectory
                              (directory-file-name
                               (file-name-directory venv-dir)))
                           venv-name))
           (icon (when (fboundp 'nerd-icons-devicon)
                   (nerd-icons-devicon "nf-dev-python"
                                       :face 'nerd-icons-blue
                                       :height 0.9
                                       :v-adjust -0.05))))
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

;; ============================================================================
(defun omw/pet-setup ()
  "Setup pet for current Python buffer."
  (when-let ((venv (pet-virtualenv-root)))
    (setq-local omw/pet-virtualenv-root venv)
    (omw/update-pet-mode-line-indicator)
    (setq-local python-shell-interpreter (pet-executable-find "python")
                python-shell-interpreter-args "-i")
    (pet-eglot-setup)))

;; ============================================================================
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

;; ============================================================================
(defvar omw/python-tool-specs
  '(("basedpyright" "uv tool install basedpyright" "uv")
    ("ruff" "uv tool install ruff" "uv"))
  "Tool specs for Python development.")

(defun omw/install-python-tools ()
  "Install Python development tools (basedpyright, ruff) via uv
if not present."
  (interactive)
  (apply #'omw/tools-install omw/python-tool-specs))

;; ============================================================================
(defun omw/python-mode-setup ()
  "Apply custom settings for Python mode."
  (omw/pet-setup)
  (omw/python-before-save-mode 1)
  (apply #'omw/tools-check-and-prompt omw/python-tool-specs))

(use-package python
  :ensure nil
  :defer t
  :hook (python-mode . omw/python-mode-setup)
  :bind (:map python-mode-map
              ("C-c C-c" . comment-line))
  :config
  (setq python-indent-guess-indent-offset nil
        python-indent-guess-indent-offset-verbose nil))

;; ============================================================================
;;; Provide features
(provide 'omw-python)

;;; omw-python.el ends here
