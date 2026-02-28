# Emacs Configuration Restructure Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Reorganize Emacs configuration into a modular, categorized structure following community best practices.

**Architecture:** Split the monolithic `init-utilities.el` into focused modules (`core/`, `ui/`, `editor/`, `tools/`), move related files into category directories, and update loading order in `init.el`.

**Tech Stack:** Emacs Lisp, use-package, directory-based module organization

---

## Phase 1: Create Directory Structure

### Task 1: Create category directories

**Files:**
- Create: `emacs/lisp/core/`
- Create: `emacs/lisp/ui/`
- Create: `emacs/lisp/editor/`
- Create: `emacs/lisp/tools/`

**Step 1: Create directories**

```bash
mkdir -p emacs/lisp/core emacs/lisp/ui emacs/lisp/editor emacs/lisp/tools
```

**Step 2: Verify directories exist**

Run: `ls -la emacs/lisp/`
Expected: See `core/`, `ui/`, `editor/`, `tools/`, `lang/` directories

**Step 3: Commit**

```bash
git add emacs/lisp/core emacs/lisp/ui emacs/lisp/editor emacs/lisp/tools
git commit -m "chore(emacs): create category directories for modular structure"
```

---

## Phase 2: Create Core Modules

### Task 2: Create core/init-funcs.el (rename from init-functions.el)

**Files:**
- Create: `emacs/lisp/core/init-funcs.el`
- Source: `emacs/lisp/init-functions.el`

**Step 1: Copy init-functions.el to core/init-funcs.el**

```bash
cp emacs/lisp/init-functions.el emacs/lisp/core/init-funcs.el
```

**Step 2: Update provide statement**

In `emacs/lisp/core/init-funcs.el`, change the last line:
```elisp
;; FROM:
(provide 'init-functions)
;; TO:
(provide 'init-funcs)
```

**Step 3: Add timer management system from init-utilities.el**

Add before the `;;; Provide features` section in `emacs/lisp/core/init-funcs.el`:

```elisp
;; ==================================================================================
;; Timer management system
;; Track and cleanup idle timers to prevent accumulation on config reload
(defvar config-timers nil
  "List of active config timers for cleanup.")

(defun run-config-timer (secs repeat function)
  "Run idle timer with tracking for cleanup.
SECS is delay in seconds, REPEAT is non-nil for repeating timers,
FUNCTION is the callback to execute."
  (let ((timer (run-with-idle-timer secs repeat function)))
    (push timer config-timers)
    timer))

(defun cleanup-config-timers ()
  "Cancel all tracked config timers.
Useful for cleaning up before config reload."
  (interactive)
  (let ((count (length config-timers)))
    (dolist (timer config-timers)
      (cancel-timer timer))
    (setq config-timers nil)
    (message "[Config] Cancelled %d timers" count)))
```

**Step 4: Move auto-package-upgrade-all to init-base.el**

Remove from `init-funcs.el` (will be added to init-base.el):
```elisp
;; DELETE this section from init-funcs.el:
(defun auto-package-upgrade-all ()
  "Upgrade all packages installed."
  (interactive)
  (require 'auto-package-update)
  (package-refresh-contents)
  (auto-package-update-now))
```

**Step 5: Update commentary**

Update the Commentary section:
```elisp
;;; Commentary:
;;
;; Core utility functions for Emacs configuration.
;; Includes: key binding utilities, proxy utilities, buffer utilities,
;; git utilities, timer management, and configuration validation system.
```

**Step 6: Commit**

```bash
git add emacs/lisp/core/init-funcs.el
git commit -m "feat(emacs): create core/init-funcs.el with timer management"
```

---

### Task 3: Create core/init-base.el

**Files:**
- Create: `emacs/lisp/core/init-base.el`

**Step 1: Create the file with header**

```elisp
;;; init-base.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 10:00:00 Thursday by zhengyuli>

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
;; Core configuration: restart-emacs, package management, aliases,
;; base configuration hooks, global modes, and before-save hooks.

;;; Code:
```

**Step 2: Add restart-emacs section**

```elisp
;; ==================================================================================
;; Restart Emacs
(use-package restart-emacs
  :defer t)
```

**Step 3: Add package management section**

```elisp
;; ==================================================================================
;; Auto package update
(use-package auto-package-update
  :defer t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-output t))

;; Check for package updates in background
(defun package-check-updates ()
  "Check for package updates in background and notify if updates available."
  (interactive)
  (message "Checking for package updates...")
  (package-refresh-contents)
  (let ((upgrades (package-menu--find-upgrades)))
    (if upgrades
        (let ((count (length upgrades)))
          (message "")
          (if (yes-or-no-p (format "Found %d package(s) with updates. Upgrade now? " count))
              (auto-package-upgrade-all)
            (message "Run `M-x auto-package-upgrade-all' to upgrade later.")))
      (message "All packages are up to date."))))

;; Check for updates after startup (after 60 seconds idle)
(run-config-timer 60 nil #'package-check-updates)

;; Package upgrade function (moved from init-funcs.el)
(defun auto-package-upgrade-all ()
  "Upgrade all packages installed."
  (interactive)
  (require 'auto-package-update)
  (package-refresh-contents)
  (auto-package-update-now))

;; Convenience aliases
(defalias 'package-upgrade-all 'auto-package-upgrade-all
  "Upgrade all packages interactively.")
(defalias 'upgrade-packages 'auto-package-upgrade-all)
```

**Step 4: Add aliases section**

```elisp
;; ==================================================================================
;; Aliases
(defalias 'refresh-auth-cache 'auth-source-forget-all-cached)
```

**Step 5: Add base configuration hooks**

```elisp
;; ==================================================================================
;; Base configuration hooks
(add-hook 'after-init-hook
          (lambda ()
            ;; Customize startup variables
            (setq inhibit-default-init t
                  inhibit-startup-screen t
                  inhibit-startup-message t
                  inhibit-startup-echo-area-message t
                  time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S %:a by %u"
                  recentf-max-saved-items 200
                  recentf-exclude '((expand-file-name package-user-dir)
                                    ".cache"
                                    ".cask"
                                    ".elfeed"
                                    "bookmarks"
                                    "cache"
                                    "ido.*"
                                    "persp-confs"
                                    "recentf"
                                    "undo-tree-hist"
                                    "url"
                                    "COMMIT_EDITMSG\\'"))

            ;; Backup settings
            (setq backup-directory-alist '((".*" . "~/.emacs.d/backup-files"))
                  backup-by-copying t
                  delete-old-versions t
                  version-control t)

            ;; Uniquify settings
            (setq uniquify-separator "/"
                  uniquify-buffer-name-style 'forward)

            ;; Disable ring bell
            (setq ring-bell-function 'ignore)

            ;; Customize user and email
            (setq user-full-name emacs-user-name
                  user-mail-address emacs-user-email)

            ;; Browser settings
            (when (featurep 'xwidget-internal)
              (setq browse-url-browser-function 'xwidget-webkit-browse-url))

            ;; Translate '<return>' to 'RET'
            (define-key key-translation-map (kbd "<return>") (kbd "RET"))

            ;; Basic keybindings
            (lazy-set-key
             '(;; Activate mark
               ("M-m" . set-mark-command)
               ;; Ibuffer
               ("C-x C-b" . ibuffer)))

            ;; Enable global auto revert mode
            (global-auto-revert-mode 1)
            ;; Enable save place mode
            (save-place-mode 1)
            ;; Enable global column number mode
            (column-number-mode 1)
            ;; Enable global just-in-time lock mode
            (jit-lock-mode 1)
            ;; Enable recent file mode
            (recentf-mode 1)))
```

**Step 6: Add before-save hooks**

```elisp
;; ==================================================================================
;; Before save hooks (global)
;; Time-stamp: globally enabled (only affects files with Time-stamp marker)
(add-hook 'before-save-hook #'time-stamp)
```

**Step 7: Add provide statement**

```elisp
;; ==================================================================================
;;; Provide features
(provide 'init-base)

;;; init-base.el ends here
```

**Step 8: Commit**

```bash
git add emacs/lisp/core/init-base.el
git commit -m "feat(emacs): create core/init-base.el with package management and base config"
```

---

### Task 4: Create core/init-env.el

**Files:**
- Create: `emacs/lisp/core/init-env.el`

**Step 1: Create the file**

```elisp
;;; init-env.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 10:00:00 Thursday by zhengyuli>

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
;; Environment integration: exec-path-from-shell, platform-specific settings.

;;; Code:

;; ==================================================================================
;; Exec path from shell (macOS)
;; Optimization: use -l instead of -i to avoid slow shell startup
(use-package exec-path-from-shell
  :when (memq window-system '(mac ns))
  :defer t)

;; Initialize exec-path from shell (deferred)
(when (memq window-system '(mac ns))
  (run-config-timer 1 nil
    (lambda ()
      (require 'exec-path-from-shell)
      (exec-path-from-shell-initialize))))

;; ==================================================================================
;; Mac system settings
(when (memq window-system '(mac ns))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

;; ==================================================================================
;; Environment Tools Validation
(defvar required-env-tools
  '((pass . "brew install pass"))
  "List of environment tools.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'env-tools
 (lambda () (config-dependency-validate-executables required-env-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-env)

;;; init-env.el ends here
```

**Step 2: Commit**

```bash
git add emacs/lisp/core/init-env.el
git commit -m "feat(emacs): create core/init-env.el with platform settings"
```

---

## Phase 3: Create UI Modules

### Task 5: Create ui/init-theme.el

**Files:**
- Create: `emacs/lisp/ui/init-theme.el`

**Step 1: Create the file**

```elisp
;;; init-theme.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 10:00:00 Thursday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: theme, appearance
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
;; Theme configuration: random banner, fullscreen, doom-themes,
;; doom-modeline, smooth scrolling.

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
;; Smooth scrolling - use built-in pixel-scroll-precision-mode (Emacs 29+)
(when (>= emacs-major-version 29)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (display-graphic-p)
                (pixel-scroll-precision-mode 1)))))

;; ==================================================================================
;;; Provide features
(provide 'init-theme)

;;; init-theme.el ends here
```

**Step 2: Commit**

```bash
git add emacs/lisp/ui/init-theme.el
git commit -m "feat(emacs): create ui/init-theme.el with theme and modeline"
```

---

### Task 6: Create ui/init-tabs.el

**Files:**
- Create: `emacs/lisp/ui/init-tabs.el`

**Step 1: Create the file with centaur-tabs and winum content**

Copy the centaur-tabs and winum sections from `init-ui.el` to `emacs/lisp/ui/init-tabs.el`:

```elisp
;;; init-tabs.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 10:00:00 Thursday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: tabs, windows
;; Dependencies: (none)

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tab and window number configuration: centaur-tabs, nerd-icons, winum.

;;; Code:

;; ==================================================================================
;; Nerd-icons - unified icon system (deferred)
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

  ;; [Include the full centaur-tabs-buffer-groups logic from init-ui.el]
  ;; ... (copy the entire buffer grouping logic)

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
;;; Provide features
(provide 'init-tabs)

;;; init-tabs.el ends here
```

**Step 2: Commit**

```bash
git add emacs/lisp/ui/init-tabs.el
git commit -m "feat(emacs): create ui/init-tabs.el with centaur-tabs and winum"
```

---

### Task 7: Create ui/init-dashboard.el

**Files:**
- Create: `emacs/lisp/ui/init-dashboard.el`

**Step 1: Create the file**

```elisp
;;; init-dashboard.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 10:00:00 Thursday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: dashboard
;; Dependencies: init-funcs, init-theme, init-tabs

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Dashboard configuration.

;;; Code:

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

;; Activation via after-init hook
(add-hook 'after-init-hook
          (lambda ()
            (require 'dashboard)
            (dashboard-open)))

;; ==================================================================================
;;; Provide features
(provide 'init-dashboard)

;;; init-dashboard.el ends here
```

**Step 2: Commit**

```bash
git add emacs/lisp/ui/init-dashboard.el
git commit -m "feat(emacs): create ui/init-dashboard.el"
```

---

### Task 8: Create ui/init-fonts.el

**Files:**
- Create: `emacs/lisp/ui/init-fonts.el` (move from `emacs/lisp/init-fonts.el`)

**Step 1: Move the file**

```bash
mv emacs/lisp/init-fonts.el emacs/lisp/ui/init-fonts.el
```

**Step 2: Commit**

```bash
git add emacs/lisp/ui/init-fonts.el
git rm emacs/lisp/init-fonts.el
git commit -m "refactor(emacs): move init-fonts.el to ui/init-fonts.el"
```

---

### Task 9: Create ui/init-highlight.el

**Files:**
- Create: `emacs/lisp/ui/init-highlight.el`

**Step 1: Create the file**

```elisp
;;; init-highlight.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 10:00:00 Thursday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: highlight, visual
;; Dependencies: (none)

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Visual highlights: pulsar, emojify, winner-mode.

;;; Code:

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
;; Winner keybindings
(add-hook 'after-init-hook
          (lambda ()
            (lazy-set-key
             '(;; Undo window layout (use C-c w u to avoid overriding C-/ undo)
               ("C-c w u" . winner-undo)
               ("C-c w r" . winner-redo)))))

;; ==================================================================================
;;; Provide features
(provide 'init-highlight)

;;; init-highlight.el ends here
```

**Step 2: Commit**

```bash
git add emacs/lisp/ui/init-highlight.el
git commit -m "feat(emacs): create ui/init-highlight.el with pulsar, emojify, winner"
```

---

## Phase 4: Create Editor Modules

### Task 10: Create editor/init-completion.el

**Files:**
- Move: `emacs/lisp/init-completion.el` → `emacs/lisp/editor/init-completion.el`

**Step 1: Move the file**

```bash
mv emacs/lisp/init-completion.el emacs/lisp/editor/init-completion.el
```

**Step 2: Commit**

```bash
git add emacs/lisp/editor/init-completion.el
git rm emacs/lisp/init-completion.el
git commit -m "refactor(emacs): move init-completion.el to editor/init-completion.el"
```

---

### Task 11: Create editor/init-editing.el (with merge)

**Files:**
- Move: `emacs/lisp/init-editing.el` → `emacs/lisp/editor/init-editing.el`
- Merge: switch-window from `init-utilities.el`
- Merge: text-mode hooks from `init-text.el`
- Merge: text-scale keybindings from `init-utilities.el`

**Step 1: Copy and modify init-editing.el**

```bash
cp emacs/lisp/init-editing.el emacs/lisp/editor/init-editing.el
```

**Step 2: Add switch-window section**

Add after the existing use-package declarations:

```elisp
;; ==================================================================================
;; Switch window - label windows with letters for quick switching
(use-package switch-window
  :defer t
  :custom
  (switch-window-shortcut-style 'qwerty)  ; Use QWERTY key layout
  (switch-window-timeout 5)               ; Auto-cancel after 5 seconds
  (switch-window-threshold 3))            ; Enable labels only with 3+ windows
```

**Step 3: Add text-mode hooks**

Add the text-mode hook section:

```elisp
;; ==================================================================================
;; Text mode hook
(add-hook 'text-mode-hook
          (lambda ()
            ;; Enable visual line mode
            (visual-line-mode 1)
            ;; Enable flyspell mode
            (flyspell-mode 1)))
```

**Step 4: Add utility keybindings**

Update the after-init-hook to include:

```elisp
;; Utility keybindings
(add-hook 'after-init-hook
          (lambda ()
            (lazy-set-key
             '(;; Switch window
               ("C-x o" . switch-window)
               ;; Scale text
               ("C-x =" . text-scale-increase)
               ("C-x _" . text-scale-decrease)
               ("C-x +" . text-scale-increase)
               ("C-x -" . text-scale-decrease)))))
```

**Step 5: Add aliases**

```elisp
;; ==================================================================================
;; Aliases
(defalias 'increase-text 'text-scale-increase)
(defalias 'decrease-text 'text-scale-decrease)
```

**Step 6: Commit**

```bash
git add emacs/lisp/editor/init-editing.el
git commit -m "feat(emacs): create editor/init-editing.el with merged content"
```

---

### Task 12: Create editor/init-projects.el

**Files:**
- Move: `emacs/lisp/init-projects.el` → `emacs/lisp/editor/init-projects.el`

**Step 1: Move the file**

```bash
mv emacs/lisp/init-projects.el emacs/lisp/editor/init-projects.el
```

**Step 2: Commit**

```bash
git add emacs/lisp/editor/init-projects.el
git rm emacs/lisp/init-projects.el
git commit -m "refactor(emacs): move init-projects.el to editor/init-projects.el"
```

---

### Task 13: Create editor/init-dired.el

**Files:**
- Move: `emacs/lisp/init-dired.el` → `emacs/lisp/editor/init-dired.el`

**Step 1: Move the file**

```bash
mv emacs/lisp/init-dired.el emacs/lisp/editor/init-dired.el
```

**Step 2: Commit**

```bash
git add emacs/lisp/editor/init-dired.el
git rm emacs/lisp/init-dired.el
git commit -m "refactor(emacs): move init-dired.el to editor/init-dired.el"
```

---

## Phase 5: Create Tools Modules

### Task 14: Create tools/init-vc.el

**Files:**
- Move: `emacs/lisp/init-vc.el` → `emacs/lisp/tools/init-vc.el`

**Step 1: Move the file**

```bash
mv emacs/lisp/init-vc.el emacs/lisp/tools/init-vc.el
```

**Step 2: Commit**

```bash
git add emacs/lisp/tools/init-vc.el
git rm emacs/lisp/init-vc.el
git commit -m "refactor(emacs): move init-vc.el to tools/init-vc.el"
```

---

### Task 15: Create tools/init-terminal.el

**Files:**
- Move: `emacs/lisp/init-terminal.el` → `emacs/lisp/tools/init-terminal.el`

**Step 1: Move the file**

```bash
mv emacs/lisp/init-terminal.el emacs/lisp/tools/init-terminal.el
```

**Step 2: Commit**

```bash
git add emacs/lisp/tools/init-terminal.el
git rm emacs/lisp/init-terminal.el
git commit -m "refactor(emacs): move init-terminal.el to tools/init-terminal.el"
```

---

### Task 16: Create tools/init-ai.el

**Files:**
- Move: `emacs/lisp/init-ai.el` → `emacs/lisp/tools/init-ai.el`

**Step 1: Move the file**

```bash
mv emacs/lisp/init-ai.el emacs/lisp/tools/init-ai.el
```

**Step 2: Commit**

```bash
git add emacs/lisp/tools/init-ai.el
git rm emacs/lisp/init-ai.el
git commit -m "refactor(emacs): move init-ai.el to tools/init-ai.el"
```

---

### Task 17: Create tools/init-auth.el

**Files:**
- Create: `emacs/lisp/tools/init-auth.el`

**Step 1: Create the file**

```elisp
;;; init-auth.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 10:00:00 Thursday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: auth, security
;; Dependencies: init-funcs

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Security and authentication: pinentry, epg, password-store.

;;; Code:

;; ==================================================================================
;; Pinentry
(use-package pinentry
  :defer t)

;; Start pinentry after idle (deferred)
(run-config-timer 2 nil
  (lambda ()
    (require 'pinentry)
    (pinentry-start)))

;; ==================================================================================
;; EPG config
(use-package epg-config
  :ensure nil
  :config
  (setq epg-pinentry-mode 'loopback
        epg-debug t))

;; ==================================================================================
;; Auth source pass
(use-package auth-source
  :ensure nil
  :config
  (require 'auth-source-pass)
  (auth-source-pass-enable))

;; ==================================================================================
;; Password store
(use-package password-store
  :defer t)

(use-package pass
  :defer t)

;; ==================================================================================
;;; Provide features
(provide 'init-auth)

;;; init-auth.el ends here
```

**Step 2: Commit**

```bash
git add emacs/lisp/tools/init-auth.el
git commit -m "feat(emacs): create tools/init-auth.el with security modules"
```

---

## Phase 6: Update Lang Modules

### Task 18: Rename lang/init-prog-base.el to lang/init-prog.el

**Files:**
- Rename: `emacs/lisp/lang/init-prog-base.el` → `emacs/lisp/lang/init-prog.el`

**Step 1: Move the file**

```bash
mv emacs/lisp/lang/init-prog-base.el emacs/lisp/lang/init-prog.el
```

**Step 2: Update provide statement**

In `emacs/lisp/lang/init-prog.el`:
```elisp
;; FROM:
(provide 'init-prog-base)
;; TO:
(provide 'init-prog)
```

**Step 3: Add prog-before-save-hook**

Add before the provide statement:

```elisp
;; ==================================================================================
;; Programming mode specific before-save hook
(defun prog-before-save-hook ()
  "Actions to run before saving programming files."
  ;; Update copyright (only in project files)
  (when (vc-root-dir)
    (copyright-update))
  ;; Delete trailing whitespace
  (delete-trailing-whitespace))

;; Enable only for programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'prog-before-save-hook nil t)))
```

**Step 4: Commit**

```bash
git add emacs/lisp/lang/init-prog.el
git rm emacs/lisp/lang/init-prog-base.el
git commit -m "refactor(emacs): rename init-prog-base.el to init-prog.el and add prog-before-save-hook"
```

---

### Task 19: Update lang module dependencies

**Files:**
- Modify: All `emacs/lisp/lang/init-*.el` files

**Step 1: Update init-cc.el dependency comment**

Change:
```elisp
;; Dependencies: init-functions, init-prog-base
```
To:
```elisp
;; Dependencies: init-funcs, init-prog
```

**Step 2: Update init-go.el dependency comment**

Change:
```elisp
;; Dependencies: init-functions, init-prog-base
```
To:
```elisp
;; Dependencies: init-funcs, init-prog
```

**Step 3: Update init-shell.el dependency comment**

Change:
```elisp
;; Dependencies: init-functions, init-prog-base
```
To:
```elisp
;; Dependencies: init-funcs, init-prog
```

**Step 4: Update init-yaml.el dependency comment**

Change:
```elisp
;; Dependencies: init-functions, init-prog-base
```
To:
```elisp
;; Dependencies: init-funcs, init-prog
```

**Step 5: Update init-cmake.el dependency comment**

Change:
```elisp
;; Dependencies: init-functions, init-prog-base
```
To:
```elisp
;; Dependencies: init-funcs, init-prog
```

**Step 6: Update init-dockerfile.el dependency comment**

Change:
```elisp
;; Dependencies: init-functions, init-prog-base
```
To:
```elisp
;; Dependencies: init-funcs, init-prog
```

**Step 7: Update init-python.el dependency comment**

Change:
```elisp
;; Dependencies: init-functions
```
To:
```elisp
;; Dependencies: init-funcs
```

**Step 8: Commit**

```bash
git add emacs/lisp/lang/
git commit -m "refactor(emacs): update lang module dependency references"
```

---

## Phase 7: Update init.el

### Task 20: Update init.el loading order

**Files:**
- Modify: `emacs/init.el`

**Step 1: Update module loading section**

Replace the current module loading section with:

```elisp
;; ==================================================================================
;; Load configuration modules
;; Helper function for safe module loading
(defun require-safe (module)
  "Require MODULE with error protection.
Logs error but continues if module fails to load."
  (condition-case err
      (require module)
    (error
     (message "[Config] Warning: Failed to load %s: %s" module (error-message-string err)))))

;; Load utility functions
(require-safe 'init-funcs)

;; Core modules
(require-safe 'init-base)
(require-safe 'init-env)

;; UI modules
(require-safe 'init-fonts)
(require-safe 'init-theme)
(require-safe 'init-tabs)
(require-safe 'init-highlight)
(require-safe 'init-dashboard)

;; Editor modules
(require-safe 'init-completion)
(require-safe 'init-editing)
(require-safe 'init-projects)
(require-safe 'init-dired)

;; Tool modules
(require-safe 'init-vc)
(require-safe 'init-terminal)
(require-safe 'init-ai)
(require-safe 'init-auth)

;; Language modules
(require-safe 'init-prog)
;; Note: init-text.el merged into init-editing.el
(require-safe 'init-elisp)
(require-safe 'init-cc)
(require-safe 'init-python)
(require-safe 'init-go)
(require-safe 'init-shell)
(require-safe 'init-dockerfile)
(require-safe 'init-cmake)
(require-safe 'init-yaml)
(require-safe 'init-markdown)
```

**Step 2: Commit**

```bash
git add emacs/init.el
git commit -m "refactor(emacs): update init.el with new module loading order"
```

---

## Phase 8: Cleanup

### Task 21: Remove old files

**Files:**
- Delete: `emacs/lisp/init-functions.el`
- Delete: `emacs/lisp/init-utilities.el`
- Delete: `emacs/lisp/init-ui.el`
- Delete: `emacs/lisp/init-text.el`

**Step 1: Remove old files**

```bash
git rm emacs/lisp/init-functions.el
git rm emacs/lisp/init-utilities.el
git rm emacs/lisp/init-ui.el
git rm emacs/lisp/init-text.el
```

**Step 2: Commit**

```bash
git commit -m "refactor(emacs): remove old module files after restructuring"
```

---

### Task 22: Verify configuration loads correctly

**Step 1: Start Emacs and verify**

```bash
emacs --debug-init
```

Expected: Emacs starts without errors

**Step 2: Check module loading**

In Emacs, run:
```elisp
M-x describe-variable RET config-dependency-validators RET
```

Expected: Shows all registered validators

**Step 3: Run validation**

In Emacs, run:
```elisp
M-x config-dependency-validate RET
```

Expected: Validation report shows all categories

---

### Task 23: Final commit

**Step 1: Review all changes**

```bash
git status
git log --oneline -25
```

**Step 2: Create summary commit if needed**

```bash
git add -A
git commit -m "refactor(emacs): complete module restructure

- Create core/ with init-funcs.el, init-base.el, init-env.el
- Create ui/ with init-theme.el, init-tabs.el, init-dashboard.el,
  init-fonts.el, init-highlight.el
- Create editor/ with init-completion.el, init-editing.el,
  init-projects.el, init-dired.el
- Create tools/ with init-vc.el, init-terminal.el, init-ai.el, init-auth.el
- Rename lang/init-prog-base.el to lang/init-prog.el
- Merge init-text.el into editor/init-editing.el
- Update init.el with new loading order
- Remove old files: init-functions.el, init-utilities.el, init-ui.el, init-text.el"
```

---

## Summary

| Phase | Tasks | Description |
|-------|-------|-------------|
| 1 | 1 | Create directory structure |
| 2 | 2-4 | Create core modules |
| 3 | 5-9 | Create UI modules |
| 4 | 10-13 | Create editor modules |
| 5 | 14-17 | Create tools modules |
| 6 | 18-19 | Update lang modules |
| 7 | 20 | Update init.el |
| 8 | 21-23 | Cleanup and verify |

**Total: 23 tasks, ~25 commits**
