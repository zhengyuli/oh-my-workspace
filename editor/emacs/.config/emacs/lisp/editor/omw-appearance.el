;;; omw-appearance.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-22 10:40:32 Sunday by zhengyu.li>
;;
;; ============================================================================
;; omw-appearance.el - UI theme, icons, modeline, tabs, and dashboard.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: ui, theme, tabs, dashboard
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
;; UI configuration: theme, icons, modeline, tabs, window management,
;; and startup dashboard for a modern Emacs experience.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Appearance
;; ----------------------------------------------------------------------------

;; --- Emojify ---
(use-package emojify
  :ensure t
  :defer t
  :when (display-graphic-p)
  :hook (markdown-mode . emojify-mode))

;; --- Nerd Icons ---
(use-package nerd-icons
  :ensure t
  :defer t
  :when (display-graphic-p))

;; --- Doom Themes ---
(use-package doom-themes
  :ensure t
  :demand t
  :config
  (load-theme 'doom-dracula t))

;; --- Doom Modeline ---
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode))

;; --- Pulsar ---
(use-package pulsar
  :ensure t
  :defer t
  :hook (after-init . pulsar-global-mode))

;; --- Centaur Tabs Height ---
(defconst omw/centaur-tabs-height 25
  "Height in pixels for the centaur-tabs tab bar.")

(defface omw/centaur-tabs-base
  '((t :family "Monospace" :height 1.0))
  "Base face for centaur-tabs with fixed-pitch font."
  :group 'omw-emacs)

(use-package centaur-tabs
  :ensure t
  :defer t
  :hook (after-init . centaur-tabs-mode)
  :bind (:map centaur-tabs-mode-map
              ("M-p" . centaur-tabs-backward)
              ("M-n" . centaur-tabs-forward)
              ("M-P" . centaur-tabs-backward-group)
              ("M-N" . centaur-tabs-forward-group)
              ("s-p" . centaur-tabs-backward)
              ("s-n" . centaur-tabs-forward)
              ("s-P" . centaur-tabs-backward-group)
              ("s-N" . centaur-tabs-forward-group))
  :custom-face
  (centaur-tabs-selected
   ((t (:inherit omw/centaur-tabs-base :bold t
        :foreground "#28cd41" :height 1.0))))
  (centaur-tabs-selected-modified
   ((t (:inherit omw/centaur-tabs-base :bold t
        :foreground "#ff9300" :height 1.0))))
  (centaur-tabs-unselected
   ((t (:inherit omw/centaur-tabs-base :bold t
        :foreground "grey" :height 1.0))))
  (centaur-tabs-unselected-modified
   ((t (:inherit omw/centaur-tabs-base :bold t
        :foreground "#ff9300" :height 1.0))))
  (centaur-tabs-display-line
   ((t (:inherit omw/centaur-tabs-base
        :box nil :overline nil :underline nil))))
  :config
  (setq centaur-tabs-height omw/centaur-tabs-height
        centaur-tabs-set-close-button nil
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-show-count t
        centaur-tabs-cycle-scope 'tabs))

;; --- Winum ---
(use-package winum
  :ensure t
  :defer t
  :hook (after-init . winum-mode)
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9)))

;; --- Get Random Banner ---
(defun omw/get-random-banner ()
  "Return random banner path from banners directory.
Returns nil in terminal mode (uses official banner instead)."
  (when (display-graphic-p)
    (let* ((banners-dir
            (expand-file-name "banners" omw/emacs-config-root-path))
           (banners (directory-files banners-dir t "\\.png\\'")))
      (when banners
        (nth (random (length banners)) banners)))))

;; --- Dashboard ---
(use-package dashboard
  :ensure t
  :defer t
  :hook (after-init . dashboard-open)
  :config
  ;; Dashboard configuration for startup screen
  ;; Show icons only in GUI mode
  (setq dashboard-center-content t
        dashboard-set-navigator t
        dashboard-set-heading-icons (display-graphic-p)
        dashboard-set-file-icons (display-graphic-p)
        dashboard-banner-logo-title
        (format "Welcome to %s's Emacs" omw/emacs-user-name)
        dashboard-items '((recents . 5) (bookmarks . 5)
                          (projects . 5) (agenda . 5) (registers . 5))
        dashboard-projects-switch-function 'project-switch-project
        dashboard-startup-banner (or (omw/get-random-banner) 'official)
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))

;; ============================================================================
;;; Provide features
(provide 'omw-appearance)

;;; omw-appearance.el ends here
