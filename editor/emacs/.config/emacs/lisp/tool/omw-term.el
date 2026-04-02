;;; omw-term.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-28 19:56:12 Saturday by zhengyuli>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: terminal, vterm, eshell
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Terminal emulation configuration with vterm for fast, native PTY support.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Terminal Emulation
;; ----------------------------------------------------------------------------

;; --- vterm Helpers ---
(defun omw/vterm-send-C-g ()
  "Send C-g to the vterm terminal."
  (interactive)
  (vterm-send-key "g" nil nil t))

(defun omw/vterm-mode-setup ()
  "Apply custom settings for vterm mode."
  (setq-local truncate-lines t)
  (hl-line-mode -1)
  (corfu-mode -1))

;; --- vterm ---
(use-package vterm
  :ensure t
  :defer t
  :hook (vterm-mode . omw/vterm-mode-setup)
  :bind (:map vterm-mode-map
              ("M-1" . nil)
              ("M-2" . nil)
              ("M-3" . nil)
              ("M-4" . nil)
              ("M-5" . nil)
              ("M-6" . nil)
              ("M-7" . nil)
              ("M-8" . nil)
              ("M-9" . nil)
              ("C-g" . omw/vterm-send-C-g)
              ("M-<backspace>" . vterm-send-meta-backspace)))

;; --- multi-vterm ---
(use-package multi-vterm
  :ensure t
  :defer t
  :bind ("C-x C-t" . multi-vterm))

;; ============================================================================
;;; Provide features
(provide 'omw-term)

;;; omw-term.el ends here
