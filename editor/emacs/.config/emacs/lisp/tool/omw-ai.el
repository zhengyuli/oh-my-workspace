;;; omw-ai.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>
;;
;; ============================================================================
;; omw-ai.el - AI coding assistant integration.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: ai, claude, coding-assistant, copilot, gptel
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
;; AI coding assistants: Claude Code IDE integration.
;; Future: GitHub Copilot, GPTel, Aider support.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; AI Integration
;; ----------------------------------------------------------------------------

;; --- Claude Code IDE ---
;; Install from GitHub (not yet in ELPA/MELPA)
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :defer t
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

;; --- Future AI Tools ---
;; Future AI tools that can be added:
;; - GitHub Copilot (copilot.el)
;; - GPTel (gptel)
;; - Aider (aider.el)
;; - Ellama (ellama)

;; ============================================================================
;;; Provide features
(provide 'omw-ai)

;;; omw-ai.el ends here
