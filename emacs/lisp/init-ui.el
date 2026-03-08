;;; init-ui.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-08 22:53:03 Sunday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: ui, theme, tabs, dashboard
;; Dependencies: (none)

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
;; UI configuration: theme, icons, modeline, tabs, window management,
;; and startup dashboard for a modern Emacs experience.

;;; Code:

;; ==================================================================================
(use-package emojify
  :ensure t
  :when (display-graphic-p)
  :defer t
  :hook (markdown-mode . emojify-mode))

;; ==================================================================================
(use-package nerd-icons
  :ensure t
  :when (display-graphic-p)
  :defer t)

;; ==================================================================================
(use-package doom-themes
  :ensure t
  :demand t
  :config
  (load-theme 'doom-xcode t))

;; ==================================================================================
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode))

;; ==================================================================================
(use-package pulsar
  :ensure t
  :defer t
  :hook (after-init . pulsar-global-mode))

;; ==================================================================================
(use-package centaur-tabs
  :ensure t
  :defer t
  :hook (after-init . centaur-tabs-mode)
  :bind (:map centaur-tabs-mode-map
              ("M-p" . centaur-tabs-backward)
              ("M-n" . centaur-tabs-forward)
              ("M-P" . centaur-tabs-switch-group)
              ("M-N" . centaur-tabs-switch-group))
  :custom-face
  (centaur-tabs-selected ((t (:inherit fixed-pitch :bold t :foreground "#28cd41" :height 1.0))))
  (centaur-tabs-selected-modified ((t (:inherit fixed-pitch :bold t :foreground "#ff9300" :height 1.0))))
  (centaur-tabs-unselected ((t (:inherit fixed-pitch :bold t :foreground "grey" :height 1.0))))
  (centaur-tabs-unselected-modified ((t (:inherit fixed-pitch :bold t :foreground "#ff9300" :height 1.0))))
  (centaur-tabs-display-line ((t (:inherit fixed-pitch :box nil :overline nil :underline nil))))
  :config
  (setq centaur-tabs-height 25
        centaur-tabs-set-close-button nil
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-show-count t
        centaur-tabs-cycle-scope 'tabs))

;; ==================================================================================
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

;; ==================================================================================
(defun omw/get-random-banner ()
  "Return random banner path from banners directory.
Returns nil in terminal mode (uses official banner instead)."
  (when (display-graphic-p)
    (let* ((banners-dir (concat omw/emacs-config-root-path "/banners"))
           (banners (directory-files banners-dir t "\\.png\\'")))
      (when banners
        (nth (random (length banners)) banners)))))

(use-package dashboard
  :ensure t
  :defer t
  :hook (after-init . dashboard-open)
  :config
  (setq dashboard-center-content t
        dashboard-set-navigator t
        dashboard-set-heading-icons (display-graphic-p)
        dashboard-set-file-icons (display-graphic-p)
        dashboard-banner-logo-title (format "Welcome to %s's Emacs" omw/emacs-user-name)
        dashboard-items '((recents . 5) (bookmarks . 5)
                          (projects . 5) (agenda . 5) (registers . 5))
        dashboard-projects-switch-function 'projectile-switch-project
        dashboard-startup-banner (or (omw/get-random-banner) 'official)
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  (dashboard-setup-startup-hook))

;; ==================================================================================
;;; Provide features
(provide 'init-ui)

;;; init-ui.el ends here
