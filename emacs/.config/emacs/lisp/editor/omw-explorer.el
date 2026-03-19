;;; omw-explorer.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: explorer, dirvish, file manager
;; Dependencies: dired-custom-extension

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

;;; Commentary:
;;
;; Dired configuration with Dirvish for modern file management.
;; Features: single-buffer navigation, async operations, GPG encryption.

;;; Code:

;; ============================================================================
(use-package dired-hacks-utils
  :ensure t
  :defer t)

;; ============================================================================
(use-package dirvish
  :ensure t
  :defer t
  :bind (:map dirvish-mode-map
              ("TAB" . dirvish-subtree-toggle)
              ("<tab>" . dirvish-subtree-toggle))
  :config
  ;; Add dirvish extensions dir to load path
  (let* ((dir (file-name-directory (locate-library "dirvish")))
         (ext (expand-file-name "extensions" dir)))
    (add-to-list 'load-path ext))

  (require 'dirvish-vc)
  (require 'dirvish-subtree)
  (require 'dirvish-icons)
  (require 'dirvish-collapse)

  (setq dirvish-use-header-line 'global
        dirvish-header-line-format '(:left (path) :right (free-space))
        dirvish-mode-line-bar-image-width 0
        dirvish-mode-line-format '(:left (sort file-time " " file-size symlink) :right (omit yank index))
        dirvish-attributes '(vc-state subtree-state nerd-icons git-msg file-modes file-time file-size)
        dirvish-large-directory-threshold 20000))

;; ============================================================================
(defun omw/dired-open-externally ()
  "Open file (or marked files) externally."
  (interactive)
  (let* ((command (cond
                   ((eq system-type 'darwin) "open")
                   (t (error "Unsupported system"))))
         (files (or (dired-get-marked-files)
                    (list (dired-get-file-for-visit)))))
    (dolist (file files)
      (start-process "dired-open" nil command file))))

(define-minor-mode omw/omit-global-mode
  "Global minor mode to control dired-omit-mode across all dired buffers.
When enabled, dired-omit-mode is enabled in all dired buffers."
  :global t
  :lighter " Omit"
  :group 'omw-emacs
  :init-value t
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'dired-mode)
        (dired-omit-mode (if omw/omit-global-mode 1 -1))))))

(defun omw/dired-mode-setup ()
  "Apply custom settings for dired mode."
  (dired-omit-mode (if omw/omit-global-mode 1 -1)))

(use-package dired
  :ensure nil
  :defer t
  :hook (dired-mode . omw/dired-mode-setup)
  :bind (("C-x d" . dired)
         ("C-x C-d" . dired)
         ("C-x j" . dired-jump)
         (:map dired-mode-map
               ;; Navigation
               ("RET" . dired-single-buffer)
               ("<return>" . dired-single-buffer)
               ("h" . dired-up-directory-single)
               ("p" . dired-hacks-previous-file)
               ("n" . dired-hacks-next-file)
               ("k" . omw/smart-kill-buffer)
               ("M-<" . dired-goto-first-line)
               ("M->" . dired-goto-last-line)
               ("M-o" . dired-omit-mode)

               ;; File operations
               ("v" . dired-view-file)
               ("o" . omw/dired-open-externally)
               ("C-k" . dired-do-delete)
               ("r" . wdired-change-to-wdired-mode)
               ("E" . dired-do-touch)
               ("B" . dired-backup-file)
               ("d" . dired-diff)
               ("D" . ediff-directories)
               ("z" . dired-do-compress)
               ("Z" . dired-do-compress)

               ;; Cryptography (GPG/PGP operations)
               (": e" . epa-dired-do-encrypt)
               (": d" . epa-dired-do-decrypt)
               (": s" . epa-dired-do-sign)
               (": v" . epa-dired-do-verify)

               ;; Copy/Cut/Paste (clipboard operations)
               ("M-w" . dired-copy-files)
               ("M-k" . dired-cut-files)
               ("C-y" . dired-paste-files)

               ;; File path utilities (copy to clipboard)
               ("; n" . dired-get-file-name-without-path)
               ("; N" . dired-get-file-name-with-path)
               ("; p" . dired-get-file-name-only-path)))

  :config
  (require 'dired-x)
  (require 'dired-async)
  (require 'dired-custom-extension)

  ;; Navigation behavior
  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-deletion-confirmer 'y-or-n-p
        dired-auto-revert-buffer (not (file-remote-p default-directory)))

  ;; Omit filter rules:
  ;; - Hidden files/directories starting with . (e.g., .git, .env)
  ;; - Specified directories (exact name match to avoid false positives)
  ;; - macOS special file (exact match)
  (setq dired-omit-files (concat
                          "^\\.\\|"
                          "\\(node_modules\\|__pycache__\\|.venv\\|venv\\|dist\\|build\\|target\\)\\'\\|"
                          "\\.DS_Store\\'"))

  ;; Supplementary file extension filter
  (setq dired-omit-extensions
        (append dired-omit-extensions '(".pyc" ".elc" ".o" ".class" ".jar" ".log" ".lock")))

  ;; Listing switches
  (if (executable-find "gls")
      (setq insert-directory-program "gls"
            dired-listing-switches "-alh --group-directories-first")
    (setq dired-listing-switches "-alh"))

  ;; Dirvish integration
  (dirvish-override-dired-mode 1)
  (dired-async-mode 1))

;; ============================================================================
;;; Provide features
(provide 'omw-explorer)

;;; omw-explorer.el ends here
