;;; init-editing.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

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
;; Editing enhancements: vundo, move-text, expand-region, multiple-cursors, etc.

;;; Code:

;; ==================================================================================
;; Vundo - visual undo tree (replaces undo-tree)
(use-package vundo
  :ensure t
  :bind ("M-_" . vundo)
  :config
  (setq vundo-compact-display t))

;; ==================================================================================
;; Move text
(use-package move-text
  :defer t)

;; ==================================================================================
;; Expand region
(use-package expand-region
  :defer t)

;; ==================================================================================
;; Multiple cursors
(use-package multiple-cursors
  :defer t)

;; ==================================================================================
;; Visual regexp steroids - enhanced visual regexp replacement
(use-package visual-regexp-steroids
  :defer t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark))
  :config
  ;; Key bindings for `vr/minibuffer-keymap'
  (lazy-set-key
   '(("C-p" . previous-history-element)
     ("C-n" . next-history-element))
   vr/minibuffer-keymap))

;; ==================================================================================
;; Browse kill ring
(use-package browse-kill-ring
  :defer t)

;; ==================================================================================
;; Goto last change
(use-package goto-chg
  :defer t)

;; ==================================================================================
;; Goto line preview
(use-package goto-line-preview
  :defer t)

;; ==================================================================================
;; Flyspell - spell checking
;; Default to aspell (works out of the box), hunspell requires extra dictionary config
(use-package flyspell
  :ensure nil  ; Built-in package
  :defer t
  :config
  ;; Choose spell checker: aspell (default) > hunspell > ispell
  ;; hunspell requires dictionary files, aspell works out of the box
  (cond
   ;; Prefer aspell (more common, no extra config needed)
   ((executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-dictionary "american"
          ispell-extra-args '("--sug-mode=ultra" "--run-together")))
   ;; hunspell requires dictionary path configuration
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell"
          ispell-dictionary "en_US"
          ;; macOS Homebrew hunspell dictionary path
          ispell-hunspell-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
    ;; Set dictionary path if it exists
    (let ((dict-path (expand-file-name "~/Library/Spelling")))
      (when (file-directory-p dict-path)
        (setenv "DICPATH" dict-path))))
   (t
    (setq ispell-program-name "ispell")))

  (setq ispell-personal-dictionary "~/.emacs.d/ispell-personal-dict"
        flyspell-issue-message-flag nil     ; Disable message output
        flyspell-mark-duplications-flag t   ; Mark duplicate words
        flyspell-delay 0.5)                 ; Check delay

  ;; Create personal dictionary file if not exists
  (unless (file-exists-p ispell-personal-dictionary)
    (make-directory (file-name-directory ispell-personal-dictionary) t)
    (write-region "" nil ispell-personal-dictionary))

  ;; Key unbindings (avoid conflict with other key bindings)
  (lazy-unset-key '("C-," "C-." "C-;") flyspell-mode-map))

;; ==================================================================================
;; Flyspell correct
(use-package flyspell-correct
  :defer t
  :config
  (require 'flyspell-correct-avy-menu))

;; ==================================================================================
;; Auto insert
(use-package autoinsert
  :ensure nil  ; Built-in package
  :hook (after-init . auto-insert-mode)
  :config
  (defun define-auto-insert-custom (condition action)
    "Custom implementation of `define-auto-insert'."
    (let ((elt (assoc condition auto-insert-alist)))
      (if elt
          (setcdr elt action)
        (add-to-list 'auto-insert-alist (cons condition action)))))

  (defun autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (setq auto-insert 'other
        auto-insert-directory (concat emacs-config-root "/templates/"))

  ;; Templates
  (define-auto-insert-custom
    '("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C / C++ header")
    ["template.h" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'" . "C / C++ program")
    ["template.c" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.py\\'" . "Python header")
    ["template.py" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.go\\'" . "Golang header")
    ["template.go" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.el\\'" . "Emacs Lisp header")
    ["template.el" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.hs\\'" . "Haskell header")
    ["template.hs" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.sh\\'" . "Shell script header")
    ["template.sh" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.org\\'" . "Org header")
    ["template.org" autoinsert-yas-expand]))

;; ==================================================================================
;; Smart kill buffer - close saved buffers directly, ask for modified ones
(defun smart-kill-buffer ()
  "Smart buffer close: kill modified file buffers with prompt, kill others directly."
  (interactive)
  (if (and buffer-file-name (buffer-modified-p))
      (kill-buffer (current-buffer))
    (kill-current-buffer)))

;; Set key binding directly (not in after-init-hook)
(global-set-key (kbd "C-x k") #'smart-kill-buffer)

;; ==================================================================================
;; Global editing keybindings
(add-hook 'after-init-hook
          (lambda ()
            (lazy-set-key
             '(;; Undo (explicit binding to prevent override)
               ("C-/" . undo)
               ;; Smart edit
               ("C-x TAB" . smart-indent)
               ("M-w" . smart-copy)
               ("M-k" . smart-kill)
               ;; Expand region
               ("M-M" . er/expand-region)
               ;; Move text
               ("C-S-p" . move-text-up)
               ("C-S-n" . move-text-down)
               ;; Multi cursors
               ("C-x m" . set-rectangular-region-anchor)
               ("C-x M" . mc/mark-all-dwim)
               ;; Browse kill ring
               ("M-Y" . browse-kill-ring)
               ;; Goto last change
               ("M-o" . goto-last-change)
               ;; Flyspell correct
               ("C-: c" . flyspell-correct-wrapper)
               ("C-: p" . flyspell-correct-previous)
               ("C-: n" . flyspell-correct-next)))))

;; ==================================================================================
;; Spell Tools Validation
;; Spell checking tools validation
(defvar required-spell-tools
  '((aspell . "brew install aspell")
    (hunspell . "brew install hunspell"))
  "List of spell checking tools.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'spell-tools
 (lambda () (config-dependency-validate-executables required-spell-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-editing)

;;; init-editing.el ends here
