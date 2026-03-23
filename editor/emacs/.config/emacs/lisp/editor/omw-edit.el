;;; omw-edit.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: editing, deletion, whitespace, pairs
;; Dependencies: (none)

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
;; Editing enhancements: smart text operations, visual undo,
;; expand region, kill ring browser, and file templates.

;;; Code:

;; ============================================================================
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
    ;; Use region if active, otherwise indent entire buffer
    (if mark-active
        (call-interactively 'indent-region)
      (call-interactively 'omw/indent-entire-buffer))))

;; ============================================================================
(defun omw/copy-region ()
  "Copy active region to kill ring."
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end)))

(defun omw/copy-current-line ()
  "Copy current line to kill ring."
  (interactive)
  (let ((end (line-end-position)))
    (copy-region-as-kill (line-beginning-position) end)))

(defun omw/smart-copy-region ()
  "Copy region if mark is active; otherwise copy current line."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'omw/copy-region)
      (call-interactively 'omw/copy-current-line))))

;; ============================================================================
(defun omw/smart-kill-region ()
  "Kill region if mark is active; otherwise kill entire line."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'kill-whole-line)))

;; ============================================================================
(defun omw/smart-kill-buffer ()
  "Smart buffer close.
Prompt for file buffers with changes, kill others directly.
If buffer has unsaved changes and is a file, offer to save.
Otherwise kill buffer without confirmation."
  (interactive)
  ;; Only prompt for save if buffer has unsaved changes and is visiting a file
  (if (and buffer-file-name (buffer-modified-p))
      (if (yes-or-no-p
           (format "Buffer %s has unsaved changes. Save before killing? "
                   (buffer-name)))
          (progn (save-buffer) (kill-current-buffer))
        (kill-current-buffer))
    (kill-current-buffer)))

;; ============================================================================
(use-package vundo
  :ensure t
  :defer t
  :bind ("M-u" . vundo))

;; ============================================================================
(use-package expand-region
  :ensure t
  :defer t
  :bind ("M-M" . er/expand-region))

;; ============================================================================
(use-package goto-chg
  :ensure t
  :defer t
  :bind ("M-o" . goto-last-change))

;; ============================================================================
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
