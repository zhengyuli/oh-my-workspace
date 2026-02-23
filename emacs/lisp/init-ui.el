;;; init-ui.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-21 10:15:41 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
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
;; Random Banner Selection
(defun omw--get-random-banner ()
  "Return a random banner path from banners directory.
Returns nil in terminal mode (uses official banner instead)."
  (when (display-graphic-p)
    (let* ((banners-dir (concat emacs-config-root "/banners"))
           (banners (directory-files banners-dir t "\\.png\\'")))
      (when banners
        (nth (random (length banners)) banners)))))

;; ==================================================================================
;; GUI Frame Settings - fullscreen, size, etc.
(when (display-graphic-p)
  ;; Auto fullscreen on startup (using custom toggle-fullscreen)
  (add-hook 'emacs-startup-hook #'toggle-fullscreen))

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
;; Nerd-icons - unified icon system (deferred)
;; Only used by centaur-tabs (icons disabled) and dashboard (conditional on GUI)
(use-package nerd-icons
  :defer t)

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

  ;; Redefinition of `centaur-tabs-buffer-groups'
  ;; Grouping strategy (priority from high to low):
  ;; 1. Special UI buffers (Dashboard, AI assistants)
  ;; 2. Project-based grouping (all buffers in a project)
  ;; 3. Version control (Magit, non-project)
  ;; 4. Terminals (non-project)
  ;; 5. Language-specific groups (non-project files)
  ;; 6. Org mode (including agenda, diary)
  ;; 7. Dired file browser
  ;; 8. Text/Markup files (Markdown, YAML, etc.)
  ;; 9. Help/Documentation buffers
  ;; 10. Emacs internal buffers (starting with *)
  ;; 11. Default fallback

  ;; Cache for buffer group computations (performance optimization)
  (defvar centaur-tabs-buffer-group-cache (make-hash-table :weakness 'key)
    "Cache for buffer group computations. Weak-keyed to allow GC of killed buffers.")

  (defun centaur-tabs--compute-buffer-group ()
    "Compute buffer group (internal, uncached).
See `centaur-tabs-buffer-groups' for grouping strategy."
    (cond
     ;; ============================================
     ;; Special UI buffers (highest priority)
     ;; ============================================
     ((derived-mode-p 'dashboard-mode) "Dashboard")
     ((derived-mode-p 'claude-code-ide-mode) "Claude Code")

     ;; ============================================
     ;; Project-based grouping (all buffers in project)
     ;; ============================================
     ((when-let* ((project-name (centaur-tabs-project-name)))
        (and (stringp project-name)
             (not (string-empty-p project-name))
             project-name)))

     ;; ============================================
     ;; Version control (non-project only)
     ;; ============================================
     ((derived-mode-p 'magit-mode) "Magit")

     ;; ============================================
     ;; Terminals (non-project only)
     ;; ============================================
     ((memq major-mode '(vterm-mode shell-mode eshell-mode term-mode)) "Terminal")

     ;; ============================================
     ;; Language-specific groups (non-project files)
     ;; ============================================
     ((derived-mode-p 'python-mode) "Python")
     ((derived-mode-p 'go-mode) "Go")
     ((memq major-mode '(c-mode c++-mode c-or-c++-mode)) "C/C++")
     ((derived-mode-p 'emacs-lisp-mode) "Elisp")
     ((derived-mode-p 'sh-mode) "Shell Script")

     ;; ============================================
     ;; Org mode (including related modes)
     ;; ============================================
     ((or (derived-mode-p 'org-mode)
          (memq major-mode '(org-agenda-mode
                             org-agenda-clockreport-mode
                             diary-mode)))
      "Org")

     ;; ============================================
     ;; Dired file browser
     ;; ============================================
     ((derived-mode-p 'dired-mode) "Dired")

     ;; ============================================
     ;; Text/Markup files
     ;; ============================================
     ((or (derived-mode-p 'markdown-mode)
          (derived-mode-p 'yaml-mode)
          (derived-mode-p 'dockerfile-mode)
          (derived-mode-p 'text-mode)
          (memq major-mode '(cmake-mode cmake-ts-mode conf-mode conf-unix-mode conf-space-mode)))
      "Text")

     ;; ============================================
     ;; Help/Documentation buffers
     ;; ============================================
     ((or (memq major-mode '(help-mode helpful-mode info-mode Man-mode woman-mode
                             help-fns-mode help-mode-view))
          (string-match-p "\\`\\*Help\\*\\|\\*info\\*\\|\\*Man\\*" (buffer-name)))
      "Help")

     ;; ============================================
     ;; Emacs internal buffers (starting with *)
     ;; ============================================
     ((string-prefix-p "*" (buffer-name)) "Emacs")

     ;; ============================================
     ;; Default fallback
     ;; ============================================
     (t (centaur-tabs-get-group-name (current-buffer)))))

  (defun centaur-tabs-buffer-groups ()
    "Control buffers' group rules with caching.

Grouping strategy:
1. Special UI buffers (Dashboard, Claude Code)
2. Project-based grouping (all buffers in a project)
3. Mode-specific groups for non-project buffers

Uses `centaur-tabs-buffer-group-cache' to avoid recomputing on every call."
    (let ((cached (gethash (current-buffer) centaur-tabs-buffer-group-cache)))
      (if cached
          cached
        (let ((group (list (centaur-tabs--compute-buffer-group))))
          (puthash (current-buffer) group centaur-tabs-buffer-group-cache)
          group))))

  ;; Clear cache when buffer changes mode (for cache invalidation)
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (remhash (current-buffer) centaur-tabs-buffer-group-cache)))

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

  ;; Enable centaur-tabs-mode
  (centaur-tabs-mode 1))

;; ==================================================================================
;; Winum - show window numbers in mode-line, M-1/2/3... to switch windows
(use-package winum
  :defer t
  :hook (after-init . winum-mode)
  :config
  (setq winum-auto-setup-mode-line t
        winum-format " %s ")
  ;; M-1/2/3... to switch to corresponding window
  (dotimes (i 9)
    (global-set-key (kbd (format "M-%d" (1+ i)))
                    (intern (format "winum-select-window-%d" (1+ i))))))

;; ==================================================================================
;; Dashboard
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
  ;; Set random banner before dashboard initializes
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
  ;; Setup dashboard hook
  (dashboard-setup-startup-hook))

;; Activation via after-init hook (outside use-package block)
;; NOTE: Hook code must be outside :config because deferred packages
;; only execute :config when actually loaded.
(add-hook 'after-init-hook
          (lambda ()
            (require 'dashboard)
            (dashboard-open)))

;; ==================================================================================
;; Pulsar - cursor highlighting (replaces beacon)
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
;; Smooth scrolling - use built-in pixel-scroll-precision-mode (Emacs 29+)
(when (>= emacs-major-version 29)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (display-graphic-p)
                (pixel-scroll-precision-mode 1)))))

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
;;; Provide features
(provide 'init-ui)

;;; init-ui.el ends here
