;;; init-prog.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 21:25:36 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: prog, programming, hooks
;; Dependencies: init-funcs

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
;; flycheck, format-all, eglot, etc.

;;; Code:

;; ==================================================================================
;; Smartparens - automatic parenthesis pairing
(use-package smartparens
  :ensure t
  :defer t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; ==================================================================================
;; Hungry delete
(use-package hungry-delete
  :ensure t
  :defer t
  :hook (prog-mode . hungry-delete-mode))

;; ==================================================================================
;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ==================================================================================
;; Highlight TODO
(use-package hl-todo
  :ensure t
  :defer t
  :hook (prog-mode . hl-todo-mode))

;; ==================================================================================
;; Flycheck - syntax checking
(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode))

;; ==================================================================================
;; Whitespace cleanup
(use-package whitespace-cleanup-mode
  :ensure t
  :defer t
  :hook (prog-mode . whitespace-cleanup-mode))

;; ==================================================================================
;; Quickrun
(use-package quickrun
  :ensure t
  :defer t)

;; ==================================================================================
;; Dumb jump
(use-package dumb-jump
  :ensure t
  :demand t
  :config
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;; ==================================================================================
;; Eglot - lightweight LSP client (Emacs 29+ built-in)
;; Performance optimization: async connection, deferred startup
(use-package eglot
  :ensure nil
  :defer t
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (cmake-mode . eglot-ensure)
         (dockerfile-mode . eglot-ensure)
         (yaml-mode . eglot-ensure))
  :config
  (setq eglot-sync-connect nil
        completion-category-defaults nil
        completion-category-overrides '((eglot (styles orderless flex)))))

;; ==================================================================================
;; Jump to the matched parenthesis/bracket/brace
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

;; Core prog-mode hook
(defun my/prog-mode-setup ()
  "Apply custom buffer-local settings for all programming modes.
These settings only affect the current buffer and do not modify global Emacs state."
  ;; Indentation Configuration (Consistent across all programming modes)
  (setq-local tab-width 4
              indent-tabs-mode nil)
  ;; Line Numbers: Enable relative/absolute line numbers for code navigation
  (display-line-numbers-mode 1)
  ;; Requires a font that supports unicode symbols (e.g., Fira Code, Source Code Pro)
  (prettify-symbols-mode 1)
  ;; Code Folding (Hide-Show Mode): Allow collapsing/expanding code blocks (functions/classes)
  (hs-minor-mode 1)
  ;; Custom Keybinding (Buffer-Local): Jump to matching parenthesis/bracket/brace
  (local-set-key (kbd "C-]") #'jump-to-matched-paren))

;; Attach the setup function to prog-mode hook
(add-hook 'prog-mode-hook #'my/prog-mode-setup)

;; Core prog-mode keybindings (built-in)
(use-package prog-mode
  :ensure nil
  :defer t
  :bind
  (:map prog-mode-map
        ;; Navigation
        ("C-c M-a" . beginning-of-defun)
        ("C-c M-e" . end-of-defun)
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
(require 'copyright)

(define-minor-mode my/prog-save-mode
  "Minor mode for programming buffers to run custom before-save hooks."
  :lighter " SaveHook"
  :global nil
  ;; Code to run when the minor mode is enabled or disabled
  (if my/prog-save-mode
      ;; When enabled, add the custom function to the buffer-local before-save-hook
      (add-hook 'before-save-hook #'my/prog-before-save nil t)
    ;; When disabled, remove the function from the buffer-local before-save-hook
    (remove-hook 'before-save-hook #'my/prog-before-save t)))

;; Automatically enable this minor mode for all programming modes
(add-hook 'prog-mode-hook #'my/prog-save-mode)

;; ==================================================================================
;;; Provide features
(provide 'init-prog)

;;; init-prog.el ends here
