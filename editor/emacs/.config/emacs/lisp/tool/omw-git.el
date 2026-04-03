;;; omw-git.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-22 13:51:42 Sunday by zhengyu.li>
;;
;; ============================================================================
;; omw-git.el - Version control with Magit.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: vc, git, diff, merge
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
;; Version control integration with Magit for powerful Git operations.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Version Control
;; ----------------------------------------------------------------------------

(use-package magit
  :ensure t
  :defer t
  :bind (("C-c g s" . magit-status)
         ("C-c g S" . magit-status-here)
         ("C-c g l" . magit-log-all)
         ("C-c g b" . magit-blame)
         ("C-c g d" . magit-dispatch)
         :map magit-status-mode-map
         ("q" . magit-kill-this-buffer)
         :map magit-section-mode-map
         ("M-1" . nil)
         ("M-2" . nil)
         ("M-3" . nil)
         ("M-4" . nil))
  :config
  ;; Full refinement needed for detailed code review
  (setq magit-diff-refine-hunk 'all))

;; ============================================================================
;;; Provide features
(provide 'omw-git)

;;; omw-git.el ends here
