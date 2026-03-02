;;; init-ui.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 22:15:31 星期一 by zhengyu.li>

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

;;; Code:

;; ==================================================================================
;; Emojify - enable only in specific modes
(use-package emojify
  :ensure t
  :when (display-graphic-p)
  :defer t
  :hook ((org-mode . emojify-mode)
         (markdown-mode . emojify-mode)))

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
;; Centaur Tabs - modern tab bar for Emacs
(use-package centaur-tabs
  :ensure t
  :defer t
  ;; Enable centaur-tabs globally after Emacs initialization
  :hook (after-init . centaur-tabs-mode)
  ;; Keybindings for tab navigation and group switching
  :bind
  (:map centaur-tabs-mode-map
        ;; Switch to previous tab
        ("M-p" . centaur-tabs-backward)
        ;; Switch to next tab
        ("M-n" . centaur-tabs-forward)
        ;; Switch to previous tab group
        ("M-P" . centaur-tabs-switch-group)
        ;; Switch to next tab group
        ("M-N" . centaur-tabs-switch-group))

  :config
  ;; --------------------------------------------------------------------------
  ;; Custom centaur tabs buffer groups
  (defun my/centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((string-match-p "\\[.*\\]$" (buffer-name)) nil)
      ((when-let* ((project-name (centaur-tabs-project-name)))
         project-name))
      ((memq major-mode '( magit-process-mode
                           magit-status-mode
                           magit-log-mode
                           magit-file-mode
                           magit-blob-mode
                           magit-blame-mode
                           magit-diff-mode
                           magit-revision-mode
                           magit-stash-mode))
       "Magit")
      ((derived-mode-p 'shell-mode) "Shell")
      ((derived-mode-p 'eshell-mode) "EShell")
      ((derived-mode-p 'dired-mode) "Dired")
      ((memq major-mode '( org-mode org-agenda-mode diary-mode)) "OrgMode")
      ((and centaur-tabs-custom-buffer-groups
            (funcall centaur-tabs-custom-buffer-groups)))
      ((derived-mode-p 'emacs-lisp-mode) "Elisp")
      ((string-equal "*" (substring (buffer-name) 0 1))
       "Emacs")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  ;; --------------------------------------------------------------------------
  ;; Core appearance and behavior settings
  (setq
   ;; Tab height in pixels
   centaur-tabs-height 25
   ;; Disable close button on tabs
   centaur-tabs-set-close-button nil
   ;; Gray out icons for inactive buffers
   centaur-tabs-gray-out-icons 'buffer
   ;; Show buffer count in tab groups
   centaur-tabs-show-count t
   ;; Limit tab cycling within the current tab group
   centaur-tabs-cycle-scope 'tabs
   ;; Set custom tabs buffer group
   centaur-tabs-buffer-groups-function #'my/centaur-tabs-buffer-groups)

  ;; --------------------------------------------------------------------------
  ;; Face customization (tab colors and states)
  ;; Selected tab appearance
  (custom-set-faces
   '(centaur-tabs-selected
     ((t (:bold t :foreground "#28cd41"))))

   ;; Selected tab with unsaved changes
   '(centaur-tabs-selected-modified
     ((t (:bold t :foreground "#ff9300"))))

   ;; Unselected tab appearance
   '(centaur-tabs-unselected
     ((t (:bold t :foreground "grey"))))

   ;; Unselected tab with unsaved changes
   '(centaur-tabs-unselected-modified
     ((t (:bold t :foreground "#ff9300")))))

  ;; --------------------------------------------------------------------------
  ;; Remove extra visual decorations from the tab separator line
  (set-face-attribute centaur-tabs-display-line nil
                      :inherit 'default
                      :box nil
                      :overline nil
                      :underline nil))

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

;; Dashboard package - startup screen for Emacs
(use-package dashboard
  :ensure t
  :defer t
  ;; Open dashboard after Emacs initialization
  :hook (after-init . dashboard-open)
  :config
  ;; ---------------------------------------------------------------------------
  ;; Layout and appearance
  (setq
   ;; Center the content in the dashboard window
   dashboard-center-content t
   ;; Banner title, including user name
   dashboard-banner-logo-title
   (format "Welcome to %s's Emacs" emacs-user-name)
   ;; Use heading icons (if in GUI)
   dashboard-set-heading-icons (display-graphic-p)
   ;; Use file icons for recent files/projects (if in GUI)
   dashboard-set-file-icons (display-graphic-p)
   ;; Enable navigator at the bottom
   dashboard-set-navigator t
   ;; Dashboard content items
   ;; Show recent files, bookmarks, projects, agenda, registers
   ;; with max 5 items each
   dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5))

   ;; Function to switch to a project (integrates with Projectile)
   dashboard-projects-switch-function 'projectile-switch-project
   ;; Banner image at startup, random if available
   dashboard-startup-banner (or (omw--get-random-banner) 'official)
   ;; Set initial buffer to the dashboard buffer
   initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  ;; ---------------------------------------------------------------------------
  ;; Enable dashboard at Emacs startup
  (dashboard-setup-startup-hook))

;; ==================================================================================
;;; Provide features
(provide 'init-ui)

;;; init-ui.el ends here
