;;; omw-python.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:32:09 Friday by zhengyu.li>
;;
;; ============================================================================
;; omw-python.el - Python mode with uv and pet support.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: python, uv, ruff, pet
;; Dependencies: pet, omw-utils
;;
;; Copyright (C) 2026 zhengyu li
;;
;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.
;; 2026-03-16       drop pyvenv/poetry, adopt pet.el for uv venv detection;
;;                  ruff format via stdin; mode-line driven by pet.
;;
;;; Commentary:
;;
;; Python mode configuration with uv and pet support.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Python
;; ----------------------------------------------------------------------------

;; --- Pet ---
(use-package pet
  :ensure t
  :defer t)

;; --- Pet Virtualenv Root ---
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

(defconst omw/pet-mode-line-entry
  '(:eval (omw/pet-mode-line-indicator))
  "Mode-line entry for pet-based venv indicator.")

(defun omw/update-pet-mode-line-indicator ()
  "Add mode-line indicator to current buffer."
  (unless (member omw/pet-mode-line-entry mode-line-misc-info)
    (setq-local mode-line-misc-info
                (cons omw/pet-mode-line-entry mode-line-misc-info))))

;; --- Pet Setup ---
(defun omw/pet-setup ()
  "Setup pet for current Python buffer."
  (when-let ((venv (pet-virtualenv-root)))
    (setq-local omw/pet-virtualenv-root venv)
    (omw/update-pet-mode-line-indicator)
    (setq-local python-shell-interpreter (pet-executable-find "python")
                python-shell-interpreter-args "-i")
    (pet-eglot-setup)))

;; --- Python Format Buffer ---
(defun omw/ruff-format-string (content filename)
  "Format CONTENT as Python using ruff with stdin-filename FILENAME.
Returns the formatted string, or nil if formatting fails."
  (unless (stringp content)
    (error "omw/ruff-format-string: CONTENT must be a string, got %S" content))
  (unless (stringp filename)
    (error "omw/ruff-format-string: FILENAME must be a string, got %S"
           filename))
  (with-temp-buffer
    (insert content)
    (when (zerop (call-process-region
                  (point-min) (point-max)
                  "ruff" t t nil
                  "format" "--stdin-filename" filename "-"))
      (buffer-string))))

(defun omw/replace-buffer-content (new-content)
  "Replace current buffer content with NEW-CONTENT using diff-based replace."
  (unless (stringp new-content)
    (error "omw/replace-buffer-content: NEW-CONTENT must be a string, got %S"
           new-content))
  (let ((src (generate-new-buffer " *ruff*")))
    (unwind-protect
        (progn
          (with-current-buffer src (insert new-content))
          (replace-buffer-contents src))
      (kill-buffer src))))

(defun omw/python-format-buffer ()
  "Format current Python buffer using ruff."
  (when (and (executable-find "ruff") (buffer-file-name))
    (let* ((original (buffer-string))
           (filename (buffer-file-name))
           (formatted (omw/ruff-format-string original filename)))
      (when (and formatted (not (string= original formatted)))
        (omw/replace-buffer-content formatted)))))

(defun omw/python-before-save ()
  "Run Python-specific actions before saving."
  (when (derived-mode-p 'python-mode)
    (omw/python-format-buffer)))

(define-minor-mode omw/python-before-save-mode
  "Minor mode for Python buffers to run custom before-save hooks."
  :lighter " PySave"
  :global nil
  :group 'omw-emacs
  ;; Separate minor mode ensures buffer-local hook lifecycle:
  ;; the hook is removed when the mode is disabled or the buffer is killed.
  (if omw/python-before-save-mode
      (add-hook 'before-save-hook #'omw/python-before-save nil t)
    (remove-hook 'before-save-hook #'omw/python-before-save t)))

;; --- Python Tool Specs ---
(defconst omw/python-tool-specs
  '(("basedpyright" "uv tool install basedpyright" "uv")
    ("ruff" "uv tool install ruff" "uv"))
  "Tool specs for Python development.")

(defun omw/install-python-tools ()
  "Install Python development tools (basedpyright, ruff) via uv
if not present."
  (interactive)
  (apply #'omw/tools-install omw/python-tool-specs))

;; --- Python Mode Setup ---
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
