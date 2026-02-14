;;; init-editing.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none

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
;; Editing enhancements: undo-tree, move-text, expand-region, multiple-cursors, etc.

;;; Code:

;; ==================================================================================
;; Undo tree
(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil)
  ;; Key unbindings
  (lazy-unset-key '("C-x u" "M-_") undo-tree-map)
  ;; Key bindings
  (lazy-set-key
   '(("M-_" . undo-tree-visualize))
   undo-tree-map))

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
;; Visual regexp steroids
(use-package visual-regexp-steroids
  :defer t
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
;; Flyspell
(use-package flyspell
  :defer t
  :config
  (setq ispell-program-name "aspell"
        ispell-dictionary "american"
        flyspell-issue-message-flag nil)
  ;; Key unbindings
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
        auto-insert-directory (concat emacs-config-root-path "/templates/"))

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
;; Global editing keybindings
(add-hook 'after-init-hook
          (lambda ()
            (lazy-set-key
             '(;; Smart edit
               ("C-x <tab>" . smart-indent)
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
               ("M-g g" . goto-line-preview)
               ("M-g M-g" . goto-line-preview)
               ;; Flyspell correct
               ("C-: c" . flyspell-correct-wrapper)
               ("C-: p" . flyspell-correct-previous)
               ("C-: n" . flyspell-correct-next)))))

;; ==================================================================================
;;; Provide features
(provide 'init-editing)

;;; init-editing.el ends here
