;;; init-base.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-01 16:22:47 Sunday by zhengyu.li>

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
;; Core configuration: restart-emacs, package auto-update, GC optimization,
;; base configuration hooks, global modes, and before-save hooks.

;;; Code:

(require 'init-funcs)

;; ==================================================================================
;; GC optimization with gcmh
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

;; ==================================================================================
;; Async - asynchronous operations
(use-package async
  :defer t)

;; ==================================================================================
;; Restart Emacs
(use-package restart-emacs
  :defer t)

;; ==================================================================================
;; Fullscreen toggle
(defvar emacs-old-fullscreen nil
  "Store the previous fullscreen state for toggle-fullscreen.")

(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter
     nil
     'fullscreen
     (if (equal 'fullboth current-value)
         (if (boundp 'emacs-old-fullscreen)
             emacs-old-fullscreen
           nil)
       (setq emacs-old-fullscreen current-value)
       'fullboth))))

;; Auto fullscreen on startup (GUI only)
(when (display-graphic-p)
  (add-hook 'emacs-startup-hook #'toggle-fullscreen))

;; ==================================================================================
;; HTTP Proxy configuration
(defcustom emacs-http-proxy nil
  "Emacs configuration http proxy.
Set to a proxy URL like \"127.0.0.1:7890\" to enable proxy."
  :type '(choice (const :tag "No proxy" nil)
                 (string :tag "Proxy address"))
  :group 'omw-emacs-config)

(defun show-http-proxy ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current http proxy is %s." (cdr (nth 1 url-proxy-services)))
    (message "No http proxy")))

(defun set-http-proxy (proxy)
  "Set HTTP/HTTPS proxy to PROXY URL.

PROXY should be a URL like \"127.0.0.1:7890\" or \"http://127.0.0.1:7890\".
Signals an error if PROXY is empty or cannot be parsed."
  (interactive (list (read-string
                      (format "HTTP Proxy Server [%s]: " emacs-http-proxy)
                      nil nil emacs-http-proxy)))
  (if (not (string-empty-p proxy))
      (condition-case err
          (let* ((proxy-url (if (string-match-p "https?://" proxy)
                                proxy
                              (concat "http://" proxy)))
                 (parsed-url (url-generic-parse-url proxy-url))
                 (host (url-host parsed-url))
                 (port (url-port parsed-url)))
            (unless (and host port)
              (error "Invalid proxy URL: missing host or port"))
            (let ((proxy-no-scheme (format "%s:%d" host port)))
              (setenv "http_proxy" proxy-url)
              (setenv "https_proxy" proxy-url)
              (setenv "all_proxy" proxy-url)
              (setq url-proxy-services
                    `(("no_proxy" . "^\\(127.0.0.1\\|localhost\\|10\\..*\\|192\\.168\\..*\\)")
                      ("http" . ,proxy-no-scheme)
                      ("https" . ,proxy-no-scheme)))
              (show-http-proxy)))
        (error
         (message "Error setting proxy: %s" (error-message-string err))))
    (user-error "Proxy URL cannot be empty")))

(defun enable-http-proxy ()
  "Enable HTTP proxy if `emacs-http-proxy' is configured.
Customize in `user-emacs-directory'/custom_settings.el:

  (setq emacs-http-proxy \"127.0.0.1:7890\")"
  (interactive)
  (if emacs-http-proxy
      (set-http-proxy emacs-http-proxy)
    (message "HTTP proxy not configured. Add to custom_settings.el: (setq emacs-http-proxy \"127.0.0.1:7890\")")))

(defun unset-http-proxy ()
  "Unset http/https proxy."
  (interactive)
  (setenv "http_proxy")
  (setenv "https_proxy")
  (setenv "all_proxy")
  (setq url-proxy-services nil)
  (show-http-proxy))

(defalias 'disable-http-proxy 'unset-http-proxy)

;; ==================================================================================
;; Exec path from shell (macOS)
;; Optimization: use -l instead of -i to avoid slow shell startup
(use-package exec-path-from-shell
  :when (memq window-system '(mac ns))
  :demand t
  :config
  (exec-path-from-shell-initialize))

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

            ;; Use y/n instead of yes/no (Emacs 28+)
            (when (boundp 'use-short-answers)
              (setq use-short-answers t))

            ;; Customize user and email
            (setq user-full-name emacs-user-name
                  user-mail-address emacs-user-email)

            ;; Browser settings
            (setq browse-url-browser-function 'xwidget-webkit-browse-url)

            ;; Mac system settings
            (when (memq window-system '(mac ns))
              (setq mac-command-modifier 'super
                    mac-option-modifier 'meta))

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
;; Startup completion hook
;; GC settings managed automatically by gcmh-mode, no manual restore needed
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time
                      (time-subtract after-init-time before-init-time))
                     gcs-done)))

;; ==================================================================================
;; Before save hooks (global)
;; Time-stamp: globally enabled (only affects files with Time-stamp marker)
(add-hook 'before-save-hook #'time-stamp)

;; ==================================================================================
;;; Provide features
(provide 'init-base)

;;; init-base.el ends here
