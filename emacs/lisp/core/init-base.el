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

;; ==================================================================================
;; Restart Emacs
(use-package restart-emacs
  :defer t)

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

;; Package upgrade function
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

;; ==================================================================================
;; Aliases
(defalias 'refresh-auth-cache 'auth-source-forget-all-cached)

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

;; ==================================================================================
;; Before save hooks (global)
;; Time-stamp: globally enabled (only affects files with Time-stamp marker)
(add-hook 'before-save-hook #'time-stamp)

;; ==================================================================================
;;; Provide features
(provide 'init-base)

;;; init-base.el ends here
