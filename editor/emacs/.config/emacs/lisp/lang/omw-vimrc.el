;;; omw-vimrc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 11:10:00 Friday by zhengyu.li>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: vim, vimrc, configuration
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-19 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Vim configuration file mode setup.
;; Covers: vimrc, .vimrc, and *.vim files.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Vim Configuration Mode
;; ----------------------------------------------------------------------------

(use-package vimrc-mode
  :ensure t
  :defer t
  :mode (("vimrc\\'" . vimrc-mode)
         ("\\.vimrc\\'" . vimrc-mode)
         ("\\.vim\\'" . vimrc-mode)))

;; ============================================================================
;;; Provide features
(provide 'omw-vimrc)

;;; omw-vimrc.el ends here
