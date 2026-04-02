;;; omw-gitconfig.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-21 15:39:57 Saturday by zhengyuli>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: git, gitconfig, gitignore, configuration
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-19 16:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Git configuration file modes setup.
;; Covers: gitconfig, gitignore, and related files.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Git Configuration Modes
;; ----------------------------------------------------------------------------

(use-package git-modes
  :ensure t
  :defer t
  :mode (("/git/config\\'" . gitconfig-mode)
         ("/git/config\\.local\\'" . gitconfig-mode)
         ("/git/config\\.local\\.example\\'" . gitconfig-mode)
         ("/git/ignore\\'" . gitignore-mode)
         ("\\.gitignore_global\\'" . gitignore-mode)
         ("\\.dockerignore\\'" . gitignore-mode)
         ("\\.stow-local-ignore\\'" . gitignore-mode)))

;; ============================================================================
;;; Provide features
(provide 'omw-gitconfig)

;;; omw-gitconfig.el ends here
