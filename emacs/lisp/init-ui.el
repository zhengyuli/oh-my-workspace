;;; init-ui.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; UI configuration: theme, modeline, tabs, dashboard, window management.

;;; Code:

;; ==================================================================================
;; Font settings
(defun my/set-fonts ()
  "Set font faces."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font emacs-config-fixed-font :height 120)
    (set-face-attribute 'fixed-pitch nil :font emacs-config-fixed-font)
    (set-face-attribute 'fixed-pitch-serif nil :font emacs-config-fixed-serif-font)
    (set-face-attribute 'variable-pitch nil :font emacs-config-variable-font)))

;; Set fonts when GUI is ready
(add-hook 'emacs-startup-hook #'my/set-fonts)

;; Also set fonts when creating a new frame (for daemon mode)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (my/set-fonts))))

;; ==================================================================================
;; Theme - doom-themes (load immediately for theme availability)
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Load theme immediately
  (load-theme 'doom-xcode t))

;; ==================================================================================
;; Modeline - doom-modeline
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-mu4e t
        doom-modeline-icon (display-graphic-p))  ; Disable icons in terminal
  :config
  (doom-modeline-mode 1))

;; ==================================================================================
;; Tabs - centaur-tabs
(use-package centaur-tabs
  :ensure t
  :bind
  (:map centaur-tabs-mode-map
        ("M-p" . centaur-tabs-backward)
        ("M-n" . centaur-tabs-forward)
        ("M-P" . centaur-tabs-counsel-switch-group)
        ("M-N" . centaur-tabs-counsel-switch-group))
  :config
  (require 'centaur-tabs-elements)
  (require 'centaur-tabs-functions)

  ;; Redefinition of `centaur-tabs-buffer-groups'
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules."
    (list
     (cond
      ((derived-mode-p 'dashboard-mode)
       "Dashboard")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((derived-mode-p 'vterm-mode)
       "Vterm")
      ((or (string-prefix-p "*" (buffer-name))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode)))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "ProgMode")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  ;; Customized faces
  (custom-set-faces
   '(centaur-tabs-selected ((t (:bold t :foreground "#28cd41"))))
   '(centaur-tabs-selected-modified ((t (:bold t :foreground "#ff9300"))))
   '(centaur-tabs-unselected ((t (:bold t :foreground "grey"))))
   '(centaur-tabs-unselected-modified ((t (:bold t :foreground "#ff9300")))))

  (set-face-attribute centaur-tabs-display-line nil
                      :inherit 'default
                      :box nil :overline nil :underline nil)

  ;; Customize variables
  (setq centaur-tabs-height 25
        centaur-tabs-style "bar"
        centaur-tabs-set-close-button nil
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-show-count t
        centaur-tabs-cycle-scope 'tabs)

  ;; Enable centaur-tabs-mode
  (centaur-tabs-mode 1))

;; ==================================================================================
;; Window management - winum
(use-package winum
  :ensure t
  :bind
  (:map winum-keymap
        ("M-0" . winum-select-window-0-or-10)
        ("M-1" . winum-select-window-1)
        ("M-2" . winum-select-window-2)
        ("M-3" . winum-select-window-3)
        ("M-4" . winum-select-window-4)
        ("M-5" . winum-select-window-5)
        ("M-6" . winum-select-window-6)
        ("M-7" . winum-select-window-7)
        ("M-8" . winum-select-window-8)
        ("M-9" . winum-select-window-9))
  :config
  (winum-mode 1))

;; ==================================================================================
;; Window zoom
(use-package zoom
  :ensure t
  :defer t
  :config
  (defun size-callback ()
    (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
          (t '(0.5 . 0.5))))
  (setq zoom-size 'size-callback))

;; ==================================================================================
;; Dimmer - dim inactive buffers
(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.30)
  (dimmer-mode 1))

;; ==================================================================================
;; Switch window
(use-package switch-window
  :ensure t
  :defer t)

;; ==================================================================================
;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (require 'dashboard-widgets)
  (setq dashboard-center-content t
        dashboard-startup-banner (concat emacs-config-root-path "/banners/totoro.png")
        dashboard-banner-logo-title (format "Welcome to %s's Emacs" emacs-config-user)
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t
        dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

  ;; Hooks
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (centaur-tabs-local-mode 1)))

  ;; Set dashboard as initial buffer
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  ;; Setup dashboard and open it
  (dashboard-setup-startup-hook)
  (dashboard-open))

;; ==================================================================================
;; Beacon - highlight cursor position
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; ==================================================================================
;; Smooth scrolling - use built-in pixel-scroll-precision-mode (Emacs 29+)
;; Replaces smooth-scrolling package
(when (>= emacs-major-version 29)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (display-graphic-p)
                (pixel-scroll-precision-mode 1)))))

;; ==================================================================================
;; Emojify
(use-package emojify
  :ensure t
  :config
  (global-emojify-mode 1)
  (global-emojify-mode-line-mode 1))

;; ==================================================================================
;; Textsize
(use-package textsize
  :ensure t
  :config
  (setq textsize-monitor-size-thresholds
        '((0 . -2) (350 . 0) (500 . 1))
        textsize-pixel-pitch-thresholds
        '((0 . 6) (0.12 . 4) (0.18 . 2) (0.20 . 1) (0.25 . -1)))
  (textsize-mode 1))

;; ==================================================================================
;; Mixed pitch for variable pitch fonts
(use-package mixed-pitch
  :ensure t
  :defer t)

;; ==================================================================================
;; Winner mode - undo/redo window layout
(winner-mode 1)

;; ==================================================================================
;; Window configuration change hook
(add-hook 'window-configuration-change-hook
          (lambda ()
            ;; Adjust window split thresholds
            (adjust-window-split-thresholds)))

;; ==================================================================================
;;; Provide features
(provide 'init-ui)

;;; init-ui.el ends here
