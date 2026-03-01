;;; init-editing.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-01 21:57:43 Sunday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
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
;; Editing enhancements: buffer utilities, vundo, move-text, expand-region,
;; multiple-cursors, smart copy/kill, etc.

;;; Code:

;; ==================================================================================
;; Buffer utilities
(defun current-major-mode-name ()
  "Display major mode and mode name."
  (interactive)
  (message "major-mode: %s, mode-name: %s" major-mode mode-name))

(defun indent-entire-buffer ()
  "Automatic format current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

(defun smart-indent-region ()
  "If mark is active, indent region, else indent all buffer."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'indent-region)
      (call-interactively 'indent-entire-buffer))))

(defun copy-region ()
  "Copy region."
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end)))

(defun copy-current-line ()
  "Copy current line."
  (interactive)
  (let ((end (min (point-max) (line-end-position))))
    (copy-region-as-kill (line-beginning-position) end)))

(defun smart-copy-region ()
  "If mark is active, copy region, else copy current line."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'copy-region)
      (call-interactively 'copy-current-line))))

(defun smart-kill-region ()
  "If mark is active, kill region, else kill whole line."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'kill-whole-line)))

(defun toggle-buffer-writable ()
  "Toggle buffer writable."
  (interactive)
  (if buffer-read-only
      (read-only-mode -1)
    (read-only-mode 1)))

(defun smart-kill-buffer ()
  "Smart buffer close: kill modified file buffers with prompt, kill others directly.
If buffer has unsaved changes and is associated with a file, prompt to save.
Otherwise kill the buffer directly."
  (interactive)
  (if (and buffer-file-name (buffer-modified-p))
      (if (yes-or-no-p (format "Buffer %s has unsaved changes. Save before killing? "
                               (buffer-name)))
          (progn (save-buffer) (kill-current-buffer))
        (kill-current-buffer))
    (kill-current-buffer)))

;; Set key binding directly (not in after-init-hook)
(global-set-key (kbd "C-x k") #'smart-kill-buffer)

;; ==================================================================================
;; Vundo - visual undo tree (replaces undo-tree)
(use-package vundo
  :ensure t
  :defer t
  :bind ("M-_" . vundo))

;; ==================================================================================
;; Expand region
(use-package expand-region
  :ensure t
  :defer t)

;; ==================================================================================
;; Browse kill ring
(use-package browse-kill-ring
  :ensure t
  :defer t)

;; ==================================================================================
;; Goto last change
(use-package goto-chg
  :ensure t
  :defer t)

;; ==================================================================================
;; Auto insert
(use-package autoinsert
  :ensure nil  ; Built-in package
  :defer t
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
(lazy-set-key
 '(;; Undo (explicit binding to prevent override)
   ("C-/" . undo)
   ;; Smart edit
   ("C-x TAB" . smart-indent-region)
   ("M-w" . smart-copy-region)
   ("M-k" . smart-kill-region)
   ;; Expand region
   ("M-M" . er/expand-region)
   ;; Browse kill ring
   ("M-Y" . browse-kill-ring)
   ;; Goto last change
   ("M-o" . goto-last-change)))

;; ==================================================================================
;;; Provide features
(provide 'init-editing)

;;; init-editing.el ends here
