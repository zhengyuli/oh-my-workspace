;;; init-prog.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-08 23:03:17 Sunday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: prog, programming, hooks
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
;; Base programming configuration: smartparens, rainbow-delimiters,
;; eglot, etc.

;;; Code:

;; ==================================================================================
(defun jump-to-matched-paren ()
  "Jump to the matched parenthesis/bracket/brace for the current position.
Behavior:
- If cursor is on/after an opening delimiter ([{\"), jump to its closing match
- If cursor is on/before a closing delimiter (]})\"), jump to its opening match
- If no delimiter is found, show an error message"
  (interactive)
  (cond ((looking-at "[ \t]*[[\"({]")
         (forward-sexp)
         (backward-char))
        ((or (looking-at "[]\")}]")
             (looking-back "[]\")}][ \t]*" nil))
         (if (< (point) (point-max))
             (forward-char))
         (backward-sexp))
        (t (message "couldn't find matched paren"))))

;; ==================================================================================
(use-package copyright
  :ensure nil
  :defer t
  :config
  (setq copyright-query nil
        copyright-names-regexp
        (format "[Cc]opyright\\s *(C)\\s *\\([0-9]+\\),[ \t]*\\([0-9]+\\)[ \t]*%s"
                omw/emacs-user-name)))

;; ==================================================================================
(use-package smartparens
  :ensure t
  :defer t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; ==================================================================================
(use-package hungry-delete
  :ensure t
  :defer t
  :hook (prog-mode . hungry-delete-mode))

;; ==================================================================================
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ==================================================================================
(use-package hl-todo
  :ensure t
  :defer t
  :hook (prog-mode . hl-todo-mode))

;; ==================================================================================
(use-package whitespace-cleanup-mode
  :ensure t
  :defer t
  :hook (prog-mode . whitespace-cleanup-mode))

;; ==================================================================================
(use-package quickrun
  :ensure t
  :defer t)

;; ==================================================================================
(use-package dumb-jump
  :ensure t
  :demand t)

;; ==================================================================================
(use-package xref
  :ensure nil
  :defer t
  :config
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;; ==================================================================================
(use-package eglot
  :ensure nil
  :defer t
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (dockerfile-mode . eglot-ensure)
         (cmake-mode . eglot-ensure)
         (yaml-mode . eglot-ensure))
  :config
  (setq eglot-sync-connect nil
        eglot-autoshutdown t))

;; ==================================================================================
(defun omw/prog-before-save ()
  "Function to run before saving programming files."
  (ignore-errors (copyright-update))
  (ignore-errors (time-stamp))
  (untabify (point-min) (point-max))
  nil)

(define-minor-mode omw/prog-before-save-mode
  "Minor mode for programming buffers to run custom before-save hooks."
  :lighter " SaveHook"
  :global nil
  (if omw/prog-before-save-mode
      (add-hook 'before-save-hook #'omw/prog-before-save nil t)
    (remove-hook 'before-save-hook #'omw/prog-before-save t)))

;; ==================================================================================
(defun omw/prog-mode-setup ()
  (setq-local tab-width 4
              indent-tabs-mode nil)
  (display-line-numbers-mode 1)
  (prettify-symbols-mode 1)
  (omw/prog-before-save-mode 1))

(use-package prog-mode
  :ensure nil
  :demand t
  :hook ((prog-mode . omw/prog-mode-setup))
  :bind (:map prog-mode-map
              ;; Navigation
              ("C-c M-a" . beginning-of-defun)
              ("C-c M-e" . end-of-defun)
              ("C-]" . jump-to-matched-paren)
              ;; Comment toggle
              ("C-c C-c" . comment-line)
              ;; Xref
              ("M-." . xref-find-definitions)
              ("M-," . xref-pop-marker-stack)
              ("M-r" . xref-find-references)
              ;; Newline + indent
              ("RET" . newline-and-indent)
              ("<return>" . newline-and-indent)))

;; ==================================================================================
;;; Provide features
(provide 'init-prog)

;;; init-prog.el ends here
