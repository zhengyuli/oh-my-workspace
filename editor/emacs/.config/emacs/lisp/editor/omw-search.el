;;; omw-search.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: search, consult, embark, xref
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-14 19:15 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Search and navigation with consult, embark, wgrep, and xref integration.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Search and Navigation
;; ----------------------------------------------------------------------------

;; --- wgrep ---
(use-package wgrep
  :ensure t
  :defer t
  :bind (:map grep-mode-map
              ("r" . wgrep-change-to-wgrep-mode))
  :config
  (setq wgrep-auto-save-buffer t))

;; --- dumb-jump ---
(use-package dumb-jump
  :ensure t
  :defer t
  :commands (dumb-jump-xref-activate))

;; --- xref ---
(use-package xref
  :ensure nil
  :defer t
  :config
  (setq xref-search-program 'ripgrep
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; --- consult ---
(use-package consult
  :ensure t
  :defer t
  :bind (("C-s" . consult-line)
         ("C-r" . consult-line-multi)
         ("C-x b" . consult-buffer)
         ("C-x B" . consult-recent-file)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-x g" . consult-ripgrep)
         ("C-x f" . consult-find))
  :config
  ;; Async search settings
  (setq consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-preview-key '(:debounce 0.25 any))

  ;; Ripgrep arguments - extend defaults with additional ignore patterns
  ;; Use --no-config to ignore global ripgrep config
  ;; and ensure consistent behavior
  ;; See: https://github.com/minad/consult#consult-ripgrep
  (setq consult-ripgrep-args
        '("rg"
          "--null"
          "--line-buffered"
          "--color=never"
          "--max-columns=1000"
          "--path-separator=/"
          "--smart-case"
          "--no-heading"
          "--with-filename"
          "--line-number"
          "--search-zip"
          "--no-config"
          "--hidden"
          ;; build artifacts
          "--glob=!node_modules/"
          "--glob=!target/"
          "--glob=!dist/"
          "--glob=!__pycache__/"
          ;; generated/compiled files
          "--glob=!*.{lock,min.js,min.css,elc,pyc}")))

;; --- embark ---
;; Will be loaded by embark automatically.
(use-package embark-consult
  :ensure t
  :defer t)

(use-package embark
  :ensure t
  :defer t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

;; ============================================================================
;;; Provide features
(provide 'omw-search)

;;; omw-search.el ends here
