;;; omw-edit.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>
;;
;; ============================================================================
;; omw-edit.el - Editing enhancements and smart text operations.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: editing, deletion, whitespace, pairs
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
;; Editing enhancements: smart text operations, visual undo,
;; expand region, kill ring browser, and file templates.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Editing
;; ----------------------------------------------------------------------------

;; --- Smart Indent ---
(defun omw/indent-entire-buffer ()
  "Format entire buffer.
Indent, delete trailing whitespace, convert tabs to spaces."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

(defun omw/smart-indent-region ()
  "Indent region if mark is active; otherwise indent entire buffer."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively #'indent-region)
      (call-interactively #'omw/indent-entire-buffer))))

;; --- Copy Region ---
(defun omw/copy-region ()
  "Copy active region to kill ring."
  (copy-region-as-kill (region-beginning) (region-end)))

(defun omw/copy-current-line ()
  "Copy current line to kill ring."
  (let ((end (line-end-position)))
    (copy-region-as-kill (line-beginning-position) end)))

(defun omw/smart-copy-region ()
  "Copy region if mark is active; otherwise copy current line."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively #'omw/copy-region)
      (call-interactively #'omw/copy-current-line))))

;; --- Smart Kill Region ---
(defun omw/smart-kill-region ()
  "Kill region if mark is active; otherwise kill entire line."
  (interactive)
  (if mark-active
      (call-interactively #'kill-region)
    (call-interactively #'kill-whole-line)))

;; --- Smart Kill Buffer ---
(defun omw/smart-kill-buffer ()
  "Smart buffer close.
Prompt for file buffers with changes, kill others directly.
If buffer has unsaved changes and is a file, offer to save.
Otherwise kill buffer without confirmation."
  (interactive)
  ;; Only prompt for save if buffer has unsaved changes and is visiting a file
  (if (and buffer-file-name (buffer-modified-p))
      (if (y-or-n-p
           (format "Buffer %s has unsaved changes. Save before killing? "
                   (buffer-name)))
          (progn (save-buffer) (kill-current-buffer))
        (kill-current-buffer))
    (kill-current-buffer)))

;; --- Vundo ---
(use-package vundo
  :ensure t
  :defer t
  :bind ("M-u" . vundo))

;; --- Expand Region ---
(use-package expand-region
  :ensure t
  :defer t
  :bind ("M-M" . er/expand-region))

;; --- Goto Chg ---
(use-package goto-chg
  :ensure t
  :defer t
  :bind ("M-o" . goto-last-change))

;; --- Emacs ---
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

;; ============================================================================
;;; Provide features
(provide 'omw-edit)

;;; omw-edit.el ends here
