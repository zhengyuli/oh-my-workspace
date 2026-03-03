;;; init-ui.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 22:15:31 星期一 by zhengyu.li>

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
;; Emojify - emoji display and insertion
;; Display emojis as graphics in org-mode and markdown-mode
(use-package emojify
  :ensure t
  :when (display-graphic-p)
  :defer t
  :hook ((org-mode . emojify-mode)
         (markdown-mode . emojify-mode)))

;; ==================================================================================
;; Nerd-icons - comprehensive icon font
;; Provides icons for file types, modes, and UI elements
(use-package nerd-icons
  :ensure t
  :defer t)

;; ==================================================================================
;; Doom themes - modern color themes
;; Using doom-xcode for clean, syntax-focused appearance
(use-package doom-themes
  :ensure t
  :demand t
  :config
  (load-theme 'doom-xcode t))

;; ==================================================================================
;; Doom modeline - enriched mode line
;; Display git branch, position, battery, and more in mode line
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon (display-graphic-p)))

;; ==================================================================================
;; Pulsar - highlight cursor position after jumps
;; Visual feedback for navigation commands (avy, goto-char, etc.)
(use-package pulsar
  :ensure t
  :defer t
  :hook (after-init . pulsar-global-mode))

;; ==================================================================================
;; Centaur Tabs - modern tab bar for buffer management
;; Provides visual tabs with project grouping and color-coded states
(use-package centaur-tabs
  :ensure t
  :defer t
  :hook (after-init . centaur-tabs-mode)
  :bind
  (:map centaur-tabs-mode-map
        ("M-p" . centaur-tabs-backward)            ; Previous tab
        ("M-n" . centaur-tabs-forward)             ; Next tab
        ("M-P" . centaur-tabs-switch-group)        ; Previous tab group
        ("M-N" . centaur-tabs-switch-group))       ; Next tab group

  :config
  ;; Tab appearance and behavior
  (setq centaur-tabs-height 25                    ; Tab height in pixels
        centaur-tabs-set-close-button nil         ; Hide close button
        centaur-tabs-gray-out-icons 'buffer       ; Gray out inactive icons
        centaur-tabs-show-count t                 ; Show buffer count in groups
        centaur-tabs-cycle-scope 'tabs)           ; Cycle within current group

  ;; Color scheme for different tab states
  (custom-set-faces
   '(centaur-tabs-selected ((t (:bold t :foreground "#28cd41"))))
   '(centaur-tabs-selected-modified ((t (:bold t :foreground "#ff9300"))))
   '(centaur-tabs-unselected ((t (:bold t :foreground "grey"))))
   '(centaur-tabs-unselected-modified ((t (:bold t :foreground "#ff9300")))))

  ;; Remove extra decorations from separator line
  (set-face-attribute centaur-tabs-display-line nil :inherit 'default :box nil
                      :overline nil :underline nil))

;; ==================================================================================
;; Winum - window number navigation
;; Assign numbers 1-9 to windows for quick switching (M-1 through M-9)
(use-package winum
  :ensure t
  :defer t
  :hook (after-init . winum-mode)
  :config
  ;; Bind M-1 through M-9 to select windows 1-9
  (dotimes (i 9)
    (global-set-key (kbd (format "M-%d" (1+ i))) (intern (format "winum-select-window-%d" (1+ i))))))

;; ==================================================================================
;; Dashboard - startup screen with project navigation
;; Provides quick access to recent files, bookmarks, and projects
(defun omw--get-random-banner ()
  "Return random banner path from banners directory.
Returns nil in terminal mode (uses official banner instead)."
  (when (display-graphic-p)
    (let* ((banners-dir (concat emacs-config-root "/banners"))
           (banners (directory-files banners-dir t "\\.png\\'")))
      (when banners
        (nth (random (length banners)) banners)))))

(use-package dashboard
  :ensure t
  :defer t
  :hook (after-init . dashboard-open)
  :config
  ;; Layout and appearance
  (setq dashboard-center-content t             ; Center content in window
        dashboard-banner-logo-title             ; Personalized welcome
        (format "Welcome to %s's Emacs" emacs-user-name)
        dashboard-set-heading-icons (display-graphic-p)  ; Icons for headings
        dashboard-set-file-icons (display-graphic-p)    ; Icons for files
        dashboard-set-navigator t                         ; Bottom navigator
        dashboard-items '((recents  . 5)                 ; Show 5 recent files
                        (bookmarks . 5)                 ; Show 5 bookmarks
                        (projects . 5)                  ; Show 5 projects
                        (agenda   . 5)                  ; Show 5 agenda items
                        (registers . 5))                ; Show 5 registers
        dashboard-projects-switch-function 'projectile-switch-project  ; Integration
        dashboard-startup-banner (or (omw--get-random-banner) 'official)  ; Random banner
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))  ; Startup buffer

  (dashboard-setup-startup-hook))

;; ==================================================================================
;;; Provide features
(provide 'init-ui)

;;; init-ui.el ends here
