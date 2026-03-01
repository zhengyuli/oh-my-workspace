;;; init-dired.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-01 13:01:25 Sunday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
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
;; Dired configuration and extensions.

;;; Code:

(require 'init-funcs)

;; ==================================================================================
;; Async - asynchronous operations
(use-package async
  :defer t
  :config
  (require 'dired-async)
  (dired-async-mode 1))

;; ==================================================================================
;; Dired-filter - dired filtering utilities
(use-package dired-filter
  :defer t
  :custom
  (dired-filter-stack '()))

;; ==================================================================================
;; Dired configuration - basic settings (Dirvish compatible)
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-deletion-confirmer 'y-or-n-p)          ; Use y/n to confirm deletion
  (dired-bind-info nil)
  :bind
  (:map dired-mode-map
        ("<return>" . dired-single-buffer)
        ("RET" . dired-single-buffer)
        ("v" . dired-view-file)                  ; View file (read-only)
        ("h" . dired-up-directory-single)        ; Parent directory
        ("p" . dired-hacks-previous-file)
        ("n" . dired-hacks-next-file)
        ("M-{" . dired-goto-first-line)
        ("M-}" . dired-goto-last-line)
        ("C-s" . isearch-forward)
        ("C-r" . isearch-backward)
        ("C-k" . dired-do-delete)
        ("r" . wdired-change-to-wdired-mode)
        ("M-o" . dired-omit-mode)
        ("E" . dired-do-touch)
        ("B" . dired-backup-file)
        ("d" . dired-diff)
        ("D" . ediff-directories)
        ("z" . dired-do-compress)
        ("Z" . dired-do-compress)
        (": e" . epa-dired-do-encrypt)
        (": d" . epa-dired-do-decrypt)
        (": s" . epa-dired-do-sign)
        (": v" . epa-dired-do-verify)
        ("M-w" . dired-copy-files)
        ("M-k" . dired-cut-files)
        ("C-y" . dired-paste-files)
        ("/m" . dired-mark-files-regexp)
        ("/*" . dired-filter-by-regexp)
        ("/." . dired-filter-by-extension)
        ("/ /" . dired-filter-pop)               ; Remove last filter
        ("/c" . dired-filter-pop-all)            ; Clear all filters
        ("/p" . dired-filter-pop)
        ("; n" . dired-get-file-name-without-path)
        ("; N" . dired-get-file-name-with-path)
        ("; p" . dired-get-file-name-only-path))
  :config
  ;; Built-in and local packages only
  (require 'epa-dired)
  (require 'dired-x)
  (require 'dired-custom-extension)

  ;; dired-x omit settings (must be after require dired-x)
  (setq dired-omit-extensions (append dired-omit-extensions '(".cache"))
        dired-omit-files (concat dired-omit-files "\\|^\\.\\|^semantic.cache$\\|^CVS$"))

  ;; macOS compatibility: BSD ls doesn't support --dired and --group-directories-first
  ;; If coreutils (gls) is installed, use GNU ls; otherwise disable ls-dired
  (if (executable-find "gls")
      (setq insert-directory-program "gls"
            dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
    (setq dired-use-ls-dired nil
          dired-listing-switches "-alh")))

;; ==================================================================================
;; Customization variables - Dirvish Quick Access
(defcustom dirvish-quick-access-dirs
  '(("h" "~/" "Home")
    ("d" "~/Downloads/" "Downloads")
    ("w" "~/oh-my-workspace/" "Workspace")
    ("e" "~/.emacs.d/" "Emacs Config")
    ("D" "~/Documents/" "Documents")
    ("p" "~/Projects/" "Projects")
    ("c" "~/.config/" "Config"))
  "Quick access directory candidates for Dirvish.
Each element is (KEY PATH DISPLAY-NAME).
Only existing directories will be shown in quick access menu.

To add custom entries, use:
  (with-eval-after-load \\='init-dired
    (add-to-list \\='dirvish-quick-access-dirs
                 \\='(\"m\" \"~/my-projects/\" \"My Projects\")))"
  :type '(repeat (list (string :tag "Key")
                       (directory :tag "Path")
                       (string :tag "Display Name")))
  :group 'omw-emacs-config)

;; Dirvish - modern file manager
;; Replaces: diredfl, nerd-icons-dired, dired-preview, dired-collapse
;; Docs: https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org
(use-package dirvish
  :custom
  (dirvish-use-header-line 'global)            ; Header line across all panes
  (dirvish-header-line-height 25)              ; Header line height
  (dirvish-mode-line-height 25)                ; Mode line height
  (dirvish-preview-delay 0.1)                  ; Preview delay in seconds
  (dirvish-large-directory-threshold 20000)    ; Use async for large dirs
  (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (dirvish-mode-line-bar-image-width 0)        ; Hide leading bar
  :bind
  (:map dirvish-mode-map
        ;; Help and menus
        ("?"   . dirvish-dispatch)              ; Help menu (cheatsheet)
        ("a"   . dirvish-setup-menu)            ; Attributes setup menu
        ("o"   . dirvish-quick-access)          ; Quick access
        ;; File operations
        ("f"   . dirvish-fd)                    ; fd search
        ("s"   . dirvish-quicksort)             ; Quick sort
        ("l"   . dirvish-ls-switches-menu)      ; ls switches menu
        ("v"   . dired-view-file)               ; View file (read-only)
        ("*"   . dirvish-mark-menu)             ; Mark menu
        ("y"   . dirvish-yank-menu)             ; Copy/paste menu
        ;; Navigation
        ("TAB" . dirvish-subtree-toggle)        ; Collapse/expand subdirectory
        ("N"   . dirvish-narrow)                ; Narrow search
        ("M-t" . dirvish-layout-toggle)         ; Toggle layout
        ("H"   . dirvish-history-jump)          ; Recent access
        ("^"   . dirvish-history-last)          ; Previous history
        ("M-f" . dirvish-history-go-forward)    ; History forward
        ("M-b" . dirvish-history-go-backward))  ; History backward
  :config
  ;; Quick access directories - dynamically generated, only existing dirs
  (defun dirvish-build-quick-access-entries ()
    "Build quick access entries from `dirvish-quick-access-dirs'.
Only includes directories that actually exist on the filesystem."
    (let (result)
      (dolist (entry dirvish-quick-access-dirs)
        (let ((path (expand-file-name (cadr entry))))
          (when (file-directory-p path)
            (push entry result))))
      (nreverse result)))

  (setq dirvish-quick-access-entries (dirvish-build-quick-access-entries))

  ;; Appearance attributes (conditional on graphical display)
  (setq dirvish-attributes
        (append (when (display-graphic-p) '(nerd-icons))
                '(vc-state subtree-state git-msg file-time file-size)))

  (setq dirvish-side-attributes
        (append (when (display-graphic-p) '(nerd-icons))
                '(vc-state collapse file-size)))

  (dirvish-override-dired-mode)
  :custom-face
  (dirvish-hl-line ((t (:background "#6495ED" :foreground "white")))))

;; ==================================================================================
;; View-mode vim-style navigation (for viewing files from dired)
(use-package view
  :ensure nil
  :defer t
  :bind
  (:map view-mode-map
        ;; Basic movement
        ("j" . next-line)
        ("k" . previous-line)
        ("h" . backward-char)
        ("l" . forward-char)
        ;; Word movement
        ("w" . forward-word)
        ("b" . backward-word)
        ("e" . forward-word)
        ;; Line movement
        ("0" . beginning-of-line)
        ("^" . beginning-of-line)
        ("$" . end-of-line)
        ;; Page movement (vim-style: C-f/C-b for full page, C-d/C-u for half page)
        ("C-f" . View-scroll-page-forward)
        ("C-b" . View-scroll-page-backward)
        ("C-d" . View-scroll-half-page-forward)
        ("C-u" . View-scroll-half-page-backward)
        ;; Alternative: f/d for page down, b/u for page up
        ("f" . View-scroll-page-forward)
        ("d" . View-scroll-half-page-forward)
        ("u" . View-scroll-half-page-backward)
        ;; Buffer navigation
        ("g" . beginning-of-buffer)
        ("G" . end-of-buffer)
        ;; Search
        ("/" . isearch-forward)
        ("?" . isearch-backward)
        ("n" . isearch-repeat-forward)
        ("N" . isearch-repeat-backward)))

;; ==================================================================================
;; Dired keybindings
(add-hook 'after-init-hook
          (lambda ()
            (lazy-set-key
             '(("C-x C-d" . dired)
               ("C-x d" . dired-jump)))))

;; ==================================================================================
;; Dired Tools Validation
;; Dired extension tools validation (macOS needs coreutils for gls)
(defvar required-dired-tools
  '((gls . "brew install coreutils (macOS GNU ls)")
    (fd . "brew install fd"))
  "List of Dired extension tools.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'dired-tools
 (lambda () (config-dependency-validate-executables required-dired-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-dired)

;;; init-dired.el ends here
