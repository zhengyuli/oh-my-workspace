;;; omw-pdf.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-28 19:47:29 Saturday by zhengyuli>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: pdf, pdf-tools, document-viewer
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; PDF viewing and navigation configuration using pdf-tools.
;; Features: PDF viewing, editing, navigation, and state restoration.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; PDF Viewer
;; ----------------------------------------------------------------------------

;; --- pdf-tools ---
(use-package pdf-tools
  :ensure t
  :defer t
  :when (display-graphic-p))

;; --- pdf-view ---
(use-package pdf-view
  :ensure nil
  :defer t
  :when (display-graphic-p)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-fit-height-to-window)
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("+" . pdf-view-enlarge)
              ("-" . pdf-view-shrink))
  :config
  (pdf-tools-install :no-query))

;; --- Pdf View Restore Path ---
(defconst omw/pdf-view-restore-path
  (expand-file-name "emacs/pdf-view-restore" omw/xdg-state-home)
  "Path to the pdf-view-restore state file (under XDG_STATE_HOME).")

(use-package pdf-view-restore
  :ensure t
  :defer t
  :when (display-graphic-p)
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename omw/pdf-view-restore-path))

;; ============================================================================
;;; Provide features
(provide 'omw-pdf)

;;; omw-pdf.el ends here
