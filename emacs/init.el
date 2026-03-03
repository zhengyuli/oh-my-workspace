;;; init.el --- Emacs configuration entry point -*- lexical-binding:t -*-
;; Time-stamp: <2026-03-03 08:27:50 星期二 by zhengyu.li>

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
;; Emacs configuration entry point.

;;; Code:

;; ==================================================================================
;; Basic check
(unless (version<= "30.2" emacs-version)
  (error "The Emacs version must be >= 30.2 (current: %s)" emacs-version))

;; ==================================================================================
;; Frame UI suppression (avoid startup flicker)
;; These should be called before the first frame is displayed
(dolist (mode '(tool-bar-mode
                scroll-bar-mode
                menu-bar-mode))
  (funcall mode -1))

;; ==================================================================================
;; GC tuning - use large threshold during startup for faster initialization
;; gcmh will manage GC after startup with reasonable thresholds
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)

;; ==================================================================================
;; Customization group
(defgroup omw-emacs-config nil
  "Oh My Workspace Emacs configuration customization group."
  :group 'emacs
  :prefix "emacs-")

;; ==================================================================================
;; Customization variables - User info
(defcustom emacs-user-name "Zhengyu Li"
  "Emacs configuration user name.
Used for dashboard banner and setting `user-full-name'."
  :type 'string
  :group 'omw-emacs-config)

(defcustom emacs-user-email "lizhengyu419@outlook.com"
  "Emacs configuration email address.
Used for setting `user-mail-address'."
  :type 'string
  :group 'omw-emacs-config)

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
  "Set HTTP/HTTPS proxy to PROXY.
PROXY should be like \"127.0.0.1:7890\" or \"http://127.0.0.1:7890\"."
  (interactive
   (list (read-string
          (format "HTTP Proxy [%s]: " emacs-http-proxy)
          nil nil emacs-http-proxy)))
  (when (string-empty-p proxy)
    (user-error "Proxy cannot be empty"))
  (condition-case err
      (let* (;; Normalize proxy URL
             (proxy-url (if (string-match-p "\\`https?://" proxy)
                            proxy
                          (concat "http://" proxy)))
             (parsed (url-generic-parse-url proxy-url))
             (host (url-host parsed))
             (port (url-port parsed)))
        ;; Validate
        (unless (and host port)
          (error "Invalid proxy: missing host or port"))
        ;; Apply environment
        (dolist (var '("http_proxy" "https_proxy" "all_proxy"))
          (setenv var proxy-url))
        ;; Emacs internal proxy
        (setq url-proxy-services
              `(("no_proxy"
                 . "^\\(127\\.0\\.0\\.1\\|localhost\\|10\\..*\\|192\\.168\\..*\\)")
                ("http"  . ,(format "%s:%d" host port))
                ("https" . ,(format "%s:%d" host port))))
        (show-http-proxy))
    (error
     (message "Proxy error: %s" (error-message-string err)))))

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
;; Global variables
;; Emacs configuration root path (dynamically resolved, supports symlinks)
(defvar emacs-config-root
  (let ((config-file (or load-file-name buffer-file-name)))
    (if config-file
        (file-name-directory (file-chase-links config-file))
      default-directory))
  "Emacs configuration root path.
Automatically resolves symlinks to find the actual configuration directory.")

(defvar emacs-config-lisp-path (expand-file-name "lisp/" emacs-config-root)
  "Emacs configuration custom settings path.")

(defvar emacs-config-packages-path (expand-file-name "site-packages/" emacs-config-root)
  "Emacs configuration custom site packages path.")

;; Helper function for load-path
(defun emacs-add-subdirs-to-load-path (base-dir)
  "Add subdirs to load path.
Look up all subdirs under `BASE-DIR' recursively and add them into load path."
  (let ((default-directory base-dir))
    (add-to-list 'load-path base-dir)
    (normal-top-level-add-subdirs-to-load-path)))

;; Add custom directories to load-path first
(emacs-add-subdirs-to-load-path emacs-config-lisp-path)
(emacs-add-subdirs-to-load-path emacs-config-packages-path)

;; ==================================================================================
;; Package archives
(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Set priorities: melpa > nongnu > gnu
(setq package-archive-priorities
      '(("melpa" . 10)
        ("nongnu" . 5)
        ("gnu" . 0)))

;; Initialize packages
(package-initialize)

;; ==================================================================================
;; use-package setup
(require 'use-package)
(setq use-package-always-ensure t)

;; ==================================================================================
(use-package async
  :ensure t
  :defer t)

;; ==================================================================================
;; GC optimization with gcmh
(use-package gcmh
  :ensure t
  :defer t
  :hook (after-init . gcmh-mode))

;; ==================================================================================
;; Exec path from shell (macOS)
(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :demand t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; ==================================================================================
(use-package auto-package-update
  :ensure t
  :defer t
  :config
  (setq auto-package-update-prompt-before-update t
        auto-package-update-delete-old-versions t))

;; ==================================================================================
;; Which-key - key hints
(use-package which-key
  :ensure t
  :defer t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-show-remaining-keys t)
  (which-key-setup-side-window-right))

;; =============================================================================
;; Backup and version control
;; Always copy files when creating backups (safer for symlinks and versioned files)
(setq backup-by-copying t)
;; Store all backup files in a single directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup-files")))
;; Enable versioned backups
(setq version-control t)
;; Automatically delete old backup versions
(setq delete-old-versions t)

;; Startup behavior
;; Disable default initialization (useful when managing full configuration)
(setq inhibit-default-init t)
;; Disable startup echo message in minibuffer
(setq inhibit-startup-echo-area-message t)
;; Disable the startup screen
(setq inhibit-startup-screen t)

;; UI and interaction
;; Use shorter yes/no answers (y or n instead of full words)
(setq use-short-answers t)
;; Disable bell sound and visual bell
(setq ring-bell-function 'ignore)

;; File and buffer management
;; Maximum number of recent files saved
(setq recentf-max-saved-items 100)
;; Use forward-style buffer naming (e.g., dir/file instead of file<dir>)
(setq uniquify-buffer-name-style 'forward)
;; Separator used in uniquify buffer names
(setq uniquify-separator "/")

;; Timestamp configuration
;; Format used by time-stamp.el when updating file headers
(setq time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S %:a by %u")

;; User identity
;; These variables are expected to be defined elsewhere in your config
(setq user-full-name emacs-user-name
      user-mail-address emacs-user-email)

;; macOS specific settings
(when (eq system-type 'darwin)
  ;; Map Command key to Super and Option key to Meta
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

;; ==================================================================================
;; Basic keybindings
;; Translate '<return>' to 'RET'
(define-key key-translation-map (kbd "<return>") (kbd "RET"))

;; Use ibuffer instead of list-buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; ==================================================================================
;; ==================================================================================
;; Base configuration hooks
;; Run after Emacs initialization
(add-hook 'after-init-hook
          (lambda ()
            ;; --------------------------------------------------
            ;; File and buffer auto management
            ;; Automatically refresh buffers when files change on disk
            (global-auto-revert-mode 1)
            ;; Remember last cursor position in files
            (save-place-mode 1)
            ;; Track recently opened files
            (recentf-mode 1)

            ;; --------------------------------------------------
            ;; UI and editing enhancements
            ;; Show column number in mode line
            (column-number-mode 1)
            ;; Enable JIT font locking for better performance
            (jit-lock-mode 1)))

;; Startup completion hook
;; GC settings managed automatically by gcmh-mode, no manual restore needed
(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "Emacs ready in %.2f seconds with %d garbage collections."
             (float-time
              (time-subtract after-init-time before-init-time))
             gcs-done)))

;; ==================================================================================
;; Core modules
(require 'init-ui)
(require 'init-fonts)
(require 'init-editing)
(require 'init-completion)
(require 'init-dired)
(require 'init-vc)
(require 'init-terminal)
(require 'init-ai)
(require 'init-auth)

;; Language modules
(dolist (module '(init-prog
                  init-elisp
                  init-cc
                  init-python
                  init-go
                  init-typescript
                  init-dockerfile
                  init-cmake
                  init-yaml
                  init-markdown))
  (require module))

;; ==================================================================================
;; Custom file configuration
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

;; Load custom settings if the file exists
(when (file-readable-p custom-file)
  (load custom-file nil 'nomessage))

;; ==================================================================================
;;; init.el ends here
