;;; init-editing.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 22:15:49 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: editing, deletion, whitespace, pairs
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
;; Editing enhancements: smart text operations, visual undo,
;; expand region, kill ring browser, and file templates.

;;; Code:

;; ==================================================================================
;; Buffer utilities - smart text and buffer operations
(defun indent-entire-buffer ()
  "Format entire buffer: indent, delete trailing whitespace, convert tabs to spaces."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

(defun smart-indent-region ()
  "Indent region if mark active, otherwise indent entire buffer."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'indent-region)
      (call-interactively 'indent-entire-buffer))))

(defun copy-region ()
  "Copy active region to kill ring."
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end)))

(defun copy-current-line ()
  "Copy current line (or from point to end of line) to kill ring."
  (interactive)
  (let ((end (min (point-max) (line-end-position))))
    (copy-region-as-kill (line-beginning-position) end)))

(defun smart-copy-region ()
  "Copy region if mark active, otherwise copy current line."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'copy-region)
      (call-interactively 'copy-current-line))))

(defun smart-kill-region ()
  "Kill region if mark active, otherwise kill entire line."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'kill-whole-line)))

(defun toggle-buffer-writable ()
  "Toggle buffer read-only state."
  (interactive)
  (if buffer-read-only
      (read-only-mode -1)
    (read-only-mode 1)))

(defun smart-kill-buffer ()
  "Smart buffer close: prompt for file buffers with changes, kill others directly.
If buffer has unsaved changes and is a file, offer to save.
Otherwise kill buffer without confirmation."
  (interactive)
  (if (and buffer-file-name (buffer-modified-p))
      (if (yes-or-no-p (format "Buffer %s has unsaved changes. Save before killing? "
                               (buffer-name)))
          (progn (save-buffer) (kill-current-buffer))
        (kill-current-buffer))
    (kill-current-buffer)))

;; ==================================================================================
;; Vundo - visual undo history with tree navigation
;; Replaces undo-tree with better performance and visualization
(use-package vundo
  :ensure t
  :defer t
  :bind ("M-_" . vundo))

;; ==================================================================================
;; Expand region - incremental text selection
;; Expand selection semantically: word → sentence → paragraph → block
(use-package expand-region
  :ensure t
  :defer t
  :bind ("M-M" . er/expand-region))

;; ==================================================================================
;; Browse kill ring - visualize and select from kill ring history
(use-package browse-kill-ring
  :ensure t
  :defer t
  :bind ("M-Y" . browse-kill-ring))

;; ==================================================================================
;; Goto last change - jump to last edit position
(use-package goto-chg
  :ensure t
  :defer t
  :bind ("M-o" . goto-last-change))

;; ==================================================================================
;; Auto insert - automatic file template insertion
;; Insert predefined templates when creating new files based on file extension
(use-package autoinsert
  :ensure nil  ; Built-in package
  :defer t
  :hook (after-init . auto-insert-mode)
  :config
  (defun define-auto-insert-custom (condition action)
    "Add or update auto-insert rule for CONDITION with ACTION.
CONDITION is a regex matching file names.
ACTION is a template file or function to insert."
    (let ((elt (assoc condition auto-insert-alist)))
      (if elt
          (setcdr elt action)
        (add-to-list 'auto-insert-alist (cons condition action)))))

  (defun autoinsert-yas-expand ()
    "Expand YASnippet template in current buffer."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (setq auto-insert 'other                        ; Query before inserting
        auto-insert-directory (concat emacs-config-root "/templates/"))

  ;; File templates: expand YASnippet template for matching file types
  (define-auto-insert-custom
    '("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C/C++ header")
    ["template.h" autoinsert-yas-expand])
  (define-auto-insert-custom
    '("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'" . "C/C++ source")
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
;; Built-in Emacs keybinding overrides
(use-package emacs
  :ensure nil
  :bind
  (;; Undo and redo
   ("C-/" . undo)           ; Undo last change
   ("C-?" . undo-redo)      ; Redo last undone change (Emacs 28+)

   ;; Active mark (selection)
   ("M-m" . set-mark-command)))  ; Set mark at point

;; ==================================================================================
;; Personal editing utilities keybindings
;; Custom functions defined in this file:
;;   - smart-indent-region: Smart indentation
;;   - smart-copy-region: Smart copy (region or line)
;;   - smart-kill-region: Smart kill (region or line)
;;   - smart-kill-buffer: Smart buffer kill with save prompt
(use-package personal-editing
  :ensure nil
  :bind
  (;; Smart editing commands
   ("C-x TAB" . smart-indent-region)  ; Indent region or buffer
   ("M-w" . smart-copy-region)        ; Copy region or line
   ("M-k" . smart-kill-region)        ; Kill region or line

   ;; Buffer management
   ("C-x k" . smart-kill-buffer)))    ; Smart buffer kill

;; ==================================================================================
;; Global hooks
;; Time-stamp: update timestamp in files with Time-stamp marker on save
(add-hook 'before-save-hook #'time-stamp)

;; ==================================================================================
;;; Provide features
(provide 'init-editing)

;;; init-editing.el ends here
