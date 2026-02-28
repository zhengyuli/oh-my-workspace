;;; init-ui.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-28 18:28:58 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: ui, theme, tabs, dashboard
;; Dependencies: init-funcs

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
;; UI configuration: frame settings, theme, visual highlights,
;; tab/window management, and dashboard.
;;
;; Components:
;; - Frame UI suppression (tool-bar, scroll-bar, menu-bar)
;; - Theme and modeline (doom-themes, doom-modeline)
;; - Visual highlights (pulsar, emojify, winner-mode)
;; - Tab and window management (centaur-tabs, winum, fullscreen)
;; - Dashboard (random banner, startup screen)

;;; Code:

(require 'init-funcs)

;; ==================================================================================
;; Frame UI suppression (avoid startup flicker)
;; These should be called before the first frame is displayed
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; ==================================================================================
;; Theme - doom-themes
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-xcode t))

;; ==================================================================================
;; Modeline - doom-modeline
(use-package doom-modeline
  :defer t
  :custom
  (doom-modeline-icon (display-graphic-p))  ; Disable icons in terminal
  :hook (after-init . doom-modeline-mode))

;; ==================================================================================
;; Smooth scrolling - use built-in pixel-scroll-precision-mode (Emacs 29+)
(when (>= emacs-major-version 29)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (display-graphic-p)
                (pixel-scroll-precision-mode 1)))))

;; ==================================================================================
;; Visual highlights - pulsar (cursor highlighting)
(use-package pulsar
  :defer t
  :hook (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse-functions '(recenter-top-bottom
                                  move-to-window-line-top-bottom
                                  reposition-window
                                  bookmark-jump
                                  other-window
                                  delete-other-windows
                                  forward-page
                                  backward-page
                                  scroll-up-command
                                  scroll-down-command
                                  windmove-right
                                  windmove-left
                                  windmove-up
                                  windmove-down
                                  tab-new
                                  tab-close
                                  tab-next
                                  tab-previous)
        pulsar-delay 0.055))

;; ==================================================================================
;; Emojify - enable only in specific modes
(use-package emojify
  :defer t
  :hook ((org-mode . emojify-mode)
         (markdown-mode . emojify-mode)
         (text-mode . emojify-mode)))

;; ==================================================================================
;; Winner mode - undo/redo window layout
(winner-mode 1)

;; ==================================================================================
;; Nerd-icons - unified icon system (deferred)
(use-package nerd-icons
  :defer t)

;; ==================================================================================
;; Tabs - centaur-tabs
;;
;; Buffer grouping logic (default centaur-tabs behavior):
;;   - Magit modes (magit-status-mode, magit-log-mode, etc.) -> "Magit"
;;   - Terminal modes (vterm, shell, eshell) -> "Terminal"
;;   - Programming modes (python, go, c/c++, elisp) -> project-based groups
;;   - Special modes (dashboard, org) -> individual groups
;;
;; Note: To customize grouping behavior, use advice-add on
;;       centaur-tabs-buffer-groups rather than redefining it.
;; ==================================================================================
;; Tabs - centaur-tabs
(use-package centaur-tabs
  :hook (after-init . centaur-tabs-mode)
  :bind
  (:map centaur-tabs-mode-map
        ("M-p" . centaur-tabs-backward)
        ("M-n" . centaur-tabs-forward)
        ("M-P" . centaur-tabs-switch-group)
        ("M-N" . centaur-tabs-switch-group))
  :config
  (require 'centaur-tabs-elements)
  (require 'centaur-tabs-functions)

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
        centaur-tabs-set-icons nil
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-show-count t
        centaur-tabs-cycle-scope 'tabs)

  (centaur-tabs-mode 1))

;; ==================================================================================
;; Winum - window numbers
(use-package winum
  :defer t
  :hook (after-init . winum-mode)
  :config
  (setq winum-auto-setup-mode-line t
        winum-format " %s ")
  (dotimes (i 9)
    (global-set-key (kbd (format "M-%d" (1+ i)))
                    (intern (format "winum-select-window-%d" (1+ i))))))

;; ==================================================================================
;; Dashboard - random banner
(defun omw--get-random-banner ()
  "Return a random banner path from banners directory.
Returns nil in terminal mode (uses official banner instead)."
  (when (display-graphic-p)
    (let* ((banners-dir (concat emacs-config-root "/banners"))
           (banners (directory-files banners-dir t "\\.png\\'")))
      (when banners
        (nth (random (length banners)) banners)))))

;; Dashboard package
(use-package dashboard
  :defer t
  :custom
  (dashboard-center-content t)
  (dashboard-banner-logo-title (format "Welcome to %s's Emacs" emacs-user-name))
  (dashboard-set-heading-icons (display-graphic-p))
  (dashboard-set-file-icons (display-graphic-p))
  (dashboard-set-navigator t)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5)))
  (dashboard-projects-switch-function 'projectile-switch-project)
  :config
  (require 'dashboard-widgets)
  ;; Set random banner
  (add-hook 'dashboard-before-initialize-hook
            (lambda ()
              (setq dashboard-startup-banner
                    (or (omw--get-random-banner) 'official))))
  ;; Dashboard mode hook
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (centaur-tabs-local-mode 1)))
  ;; Set dashboard as initial buffer
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

;; Dashboard activation
(add-hook 'after-init-hook
          (lambda ()
            (require 'dashboard)
            (dashboard-open)))

;; ==================================================================================
;; UI keybindings
(add-hook 'after-init-hook
          (lambda ()
            (lazy-set-key
             '(;; Winner mode (window layout undo/redo)
               ("C-c w u" . winner-undo)
               ("C-c w r" . winner-redo)))))

;; ==================================================================================
;;; Provide features
(provide 'init-ui)

;;; init-ui.el ends here
