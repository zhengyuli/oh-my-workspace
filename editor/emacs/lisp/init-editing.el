;;; init-editing.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-11 08:52:13 Wednesday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: editing, deletion, whitespace, pairs
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
;; Editing enhancements: smart text operations, visual undo,
;; expand region, kill ring browser, and file templates.

;;; Code:

;; ==================================================================================
(defun omw/indent-entire-buffer ()
  "Format entire buffer.
 Indent, delete trailing whitespace, convert tabs to spaces."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

(defun omw/smart-indent-region ()
  "Indent region.
If mark active, otherwise indent entire buffer."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'indent-region)
      (call-interactively 'omw/indent-entire-buffer))))

;; ==================================================================================
(defun omw/copy-region ()
  "Copy active region to kill ring."
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end)))

(defun omw/copy-current-line ()
  "Copy current line to kill ring."
  (interactive)
  (let ((end (min (point-max) (line-end-position))))
    (copy-region-as-kill (line-beginning-position) end)))

(defun omw/smart-copy-region ()
  "Copy region.
If mark active, copy current line."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'omw/copy-region)
      (call-interactively 'omw/copy-current-line))))

;; ==================================================================================
(defun omw/smart-kill-region ()
  "Kill region.
If mark active, kill entire line."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'kill-whole-line)))

;; ==================================================================================
(defun omw/smart-kill-buffer ()
  "Smart buffer close.
Prompt for file buffers with changes, kill others directly.
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
(use-package vundo
  :ensure t
  :defer t
  :bind ("M-u" . vundo))

;; ==================================================================================
(use-package expand-region
  :ensure t
  :defer t
  :bind ("M-M" . er/expand-region))

;; ==================================================================================
(use-package goto-chg
  :ensure t
  :defer t
  :bind ("M-o" . goto-last-change))

;; ==================================================================================
(use-package wgrep
  :ensure t
  :defer t
  :bind (:map grep-mode-map
              ("r" . wgrep-change-to-wgrep-mode))
  :config
  (setq wgrep-auto-save-buffer t))

;; ==================================================================================
(use-package autoinsert
  :ensure nil
  :defer t
  :hook (after-init . auto-insert-mode)
  :config
  (defun omw/define-auto-insert-custom (condition action)
    "Add or update auto-insert rule for CONDITION with ACTION.
CONDITION is a regex matching file names.
ACTION is a template file or function to insert."
    (let ((elt (assoc condition auto-insert-alist)))
      (if elt
          (setcdr elt action)
        (add-to-list 'auto-insert-alist (cons condition action)))))

  (defun omw/autoinsert-yas-expand ()
    "Expand YASnippet template in current buffer."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (setq auto-insert 'other
        auto-insert-directory (concat omw/emacs-config-root-path "/templates/"))

  (omw/define-auto-insert-custom
    '("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C/C++ header")
    ["template.h" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'" . "C/C++ source")
    ["template.c" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.py\\'" . "Python header")
    ["template.py" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.go\\'" . "Golang header")
    ["template.go" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.el\\'" . "Emacs Lisp header")
    ["template.el" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.hs\\'" . "Haskell header")
    ["template.hs" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.sh\\'" . "Shell script header")
    ["template.sh" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.org\\'" . "Org header")
    ["template.org" omw/autoinsert-yas-expand]))

;; ==================================================================================
(use-package emacs
  :ensure nil
  :demand t
  :bind (("C-/" . undo)
         ("C-?" . undo-redo)
         ("C-x TAB" . omw/smart-indent-region)
         ("M-m" . set-mark-command)
         ("M-w" . omw/smart-copy-region)
         ("M-k" . omw/smart-kill-region)
         ("C-x k" . omw/smart-kill-buffer)))

;; ==================================================================================
;;; Provide features
(provide 'init-editing)

;;; init-editing.el ends here
