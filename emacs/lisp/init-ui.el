;;; init-ui.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-01 19:42:01 Sunday by zhengyuli>

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

;; ==================================================================================
;; Smooth scrolling - use built-in pixel-scroll-precision-mode (Emacs 29+)
(when (and (display-graphic-p)
           (fboundp 'pixel-scroll-precision-mode))
  (add-hook 'emacs-startup-hook #'pixel-scroll-precision-mode))

;; ==================================================================================
;; Emojify - enable only in specific modes
(use-package emojify
  :ensure t
  :defer t
  :hook ((org-mode . emojify-mode)
         (markdown-mode . emojify-mode)
         (text-mode . emojify-mode)))

;; ==================================================================================
;; Nerd-icons - unified icon system
(use-package nerd-icons
  :ensure t
  :defer t)

;; ==================================================================================
;; Theme - doom-themes
(use-package doom-themes
  :ensure t
  :demand t
  :config
  (load-theme 'doom-xcode t))

;; ==================================================================================
;; Modeline - doom-modeline
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon (display-graphic-p)))

;; ==================================================================================
;; Visual highlights - pulsar (cursor highlighting)
(use-package pulsar
  :ensure t
  :defer t
  :hook (after-init . pulsar-global-mode))

;; ==================================================================================
;; Tabs - centaur-tabs
(use-package centaur-tabs
  :ensure t
  :defer t
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
        centaur-tabs-cycle-scope 'tabs))

;; ==================================================================================
;; Winum - window numbers
(use-package winum
  :ensure t
  :defer t
  :hook (after-init . winum-mode)
  :config
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
  :ensure t
  :defer t
  :hook (after-init . dashboard-open)
  :config
  (require 'dashboard-widgets)

  (setq dashboard-center-content t
        dashboard-banner-logo-title (format "Welcome to %s's Emacs" emacs-user-name)
        dashboard-set-heading-icons (display-graphic-p)
        dashboard-set-file-icons (display-graphic-p)
        dashboard-set-navigator t
        dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))
        dashboard-projects-switch-function 'projectile-switch-project
        dashboard-startup-banner (or (omw--get-random-banner) 'official)
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

;; ==================================================================================
;;; Provide features
(provide 'init-ui)

;;; init-ui.el ends here
