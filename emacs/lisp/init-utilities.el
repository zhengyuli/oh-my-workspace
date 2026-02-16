;;; init-utilities.el -*- lexical-binding: t; -*-
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
;; Utility packages: exec-path-from-shell, restart-emacs, pinentry,
;; password-store, auto-package-update, etc.

;;; Code:

;; ==================================================================================
;; Exec path from shell (macOS)
(use-package exec-path-from-shell
  :when (memq window-system '(mac ns))
  :hook (after-init . exec-path-from-shell-initialize))

;; ==================================================================================
;; Restart Emacs
(use-package restart-emacs
  :defer t)

;; ==================================================================================
;; Switch window - 用字母标识窗口，快速切换
(use-package switch-window
  :ensure t
  :defer t
  :custom
  (switch-window-shortcut-style 'qwerty)  ; 使用 QWERTY 键位
  (switch-window-timeout 5)               ; 5 秒后自动取消
  (switch-window-threshold 3))            ; 3 个窗口以上才启用标识

;; ==================================================================================
;; Pinentry
(use-package pinentry
  :hook (after-init . pinentry-start))

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
;; Auto package update
(use-package auto-package-update
  :defer t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-output t)
  :config
  ;; 便捷函数：升级所有包
  (defun my/package-upgrade-all ()
    "Upgrade all packages interactively."
    (interactive)
    (package-refresh-contents)
    (auto-package-upgrade-all))

  ;; 后台检查包更新
  (defun my/package-check-updates ()
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
              (message "Run `M-x my/package-upgrade-all' to upgrade later.")))
        (message "All packages are up to date."))))

  ;; 启动后延迟检查更新（空闲 60秒后）
  (run-with-idle-timer 60 nil #'my/package-check-updates))

;; ==================================================================================
;; Aliases
(defalias 'refresh-auth-cache 'auth-source-forget-all-cached)
(defalias 'upgrade-packages 'auto-package-upgrade-all)
(defalias 'increase-text 'text-scale-increase)
(defalias 'decrease-text 'text-scale-decrease)

;; ==================================================================================
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
               ("C-x -" . text-scale-decrease)
               ;; Undo window layout (use C-c w u to avoid overriding C-/ undo)
               ("C-c w u" . winner-undo)
               ("C-c w r" . winner-redo)))))

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
            (setq user-full-name emacs-config-user
                  user-mail-address emacs-config-email)

            ;; Browser settings
            (when (featurep 'xwidget-internal)
              (setq browse-url-browser-function 'xwidget-webkit-browse-url))

            ;; Mac system settings
            (when (memq window-system '(mac ns))
              (setq mac-command-modifier 'super
                    mac-option-modifier 'meta))

            ;; Remap '<return>' to 'RET'
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

            ;; Defer recentf mode to after idle
            (run-with-idle-timer 2 nil (lambda () (recentf-mode 1)))))

;; ==================================================================================
;; Before save hook
(add-hook 'before-save-hook
          (lambda ()
            ;; Update timestamp
            (time-stamp)
            ;; Update copyright
            (copyright-update)
            ;; Delete trailing white space
            (delete-trailing-whitespace)))

;; ==================================================================================
;;; Provide features
(provide 'init-utilities)

;;; init-utilities.el ends here
