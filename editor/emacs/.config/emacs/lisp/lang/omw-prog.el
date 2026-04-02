;;; omw-prog.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-19 00:00:00 Thursday by zhengyu.li>
;;
;; ============================================================================
;; omw-prog.el - Base programming configuration and hooks.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: prog, programming, hooks
;; Dependencies: (none)
;;
;; Copyright (C) 2026 zhengyu li
;;
;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.
;;
;;; Commentary:
;;
;; Base programming configuration: smartparens, rainbow-delimiters,
;; eglot, etc.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Programming
;; ----------------------------------------------------------------------------

;; --- Jump to Matched Paren ---
(defun omw/jump-to-matched-paren ()
  "Jump to the matched parenthesis/bracket/brace for the current position.
If cursor is on/after an opening delimiter, jump to its closing match.
If cursor is on/before a closing delimiter, jump to its opening match.
If no delimiter is found, show an error message."
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

;; --- copyright ---
(use-package copyright
  :ensure nil
  :defer t
  :config
  ;; Copyright query settings
  (setq copyright-query nil
        copyright-names-regexp
        (format
         "[Cc]opyright\\s *(C)\\s *\\([0-9]+\\),[ \t]*\\([0-9]+\\)[ \t]*%s"
         (regexp-quote omw/emacs-user-name))))

;; --- smartparens ---
(use-package smartparens
  :ensure t
  :defer t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; --- hungry-delete ---
(use-package hungry-delete
  :ensure t
  :defer t
  :hook (prog-mode . hungry-delete-mode))

;; --- rainbow-delimiters ---
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; --- hl-todo ---
(use-package hl-todo
  :ensure t
  :defer t
  :hook (prog-mode . hl-todo-mode))

;; --- whitespace-cleanup-mode ---
(use-package whitespace-cleanup-mode
  :ensure t
  :defer t
  :hook (prog-mode . whitespace-cleanup-mode))

;; --- quickrun ---
(use-package quickrun
  :ensure t
  :defer t)

;; --- eglot ---
(use-package eglot
  :ensure nil
  :defer t
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (cmake-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (json-mode . eglot-ensure)
         (dockerfile-mode . eglot-ensure))
  :config
  ;; Async connection and auto-shutdown for better performance
  (setq eglot-sync-connect nil
        eglot-autoshutdown t))

;; --- Prog Before Save ---
(defun omw/prog-before-save ()
  "Run pre-save hooks for programming buffers.
Updates copyright, timestamp, and untabifies."
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

;; --- Prog Mode Setup ---
(defun omw/prog-mode-setup ()
  "Apply custom settings for programming modes."
  (setq-local tab-width 4
              indent-tabs-mode nil)
  (display-line-numbers-mode 1)
  (prettify-symbols-mode 1)
  (omw/prog-before-save-mode 1))

(use-package prog-mode
  :ensure nil
  :demand t
  :hook (prog-mode . omw/prog-mode-setup)
  :bind (:map prog-mode-map
              ;; Navigation
              ("C-c M-a" . beginning-of-defun)
              ("C-c M-e" . end-of-defun)
              ("C-]" . omw/jump-to-matched-paren)
              ;; Comment toggle
              ("C-c C-c" . comment-line)
              ;; Xref
              ("M-." . xref-find-definitions)
              ("M-," . xref-go-back)
              ("M-r" . xref-find-references)
              ;; Newline + indent
              ("RET" . newline-and-indent)
              ("<return>" . newline-and-indent)))

;; ============================================================================
;;; Provide features
(provide 'omw-prog)

;;; omw-prog.el ends here
