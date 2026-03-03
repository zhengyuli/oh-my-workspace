;;; init.el --- Emacs configuration entry point -*- lexical-binding:t -*-
;; Time-stamp: <2026-03-03 08:27:50 星期二 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: init-funcs, init-completion

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
;;
;; This section provides HTTP/HTTPS proxy support for Emacs network operations.
;; Useful for:
;;   - Package installation behind corporate proxies or firewalls
;;   - Accessing MELPA/ELPA repositories through proxy servers
;;   - Git operations through proxy (Magit, vc.el)
;;   - External tool integration (LSP servers, formatters) that need proxy
;;
;; Configuration:
;;   Add to custom.el: (setq emacs-http-proxy "127.0.0.1:7890")
;;   Then run: M-x enable-http-proxy
;;
;; Common proxy formats:
;;   - HTTP proxy: "127.0.0.1:7890" or "http://127.0.0.1:7890"
;;   - SOCKS5 proxy: use "socks5://127.0.0.1:1080"
;;   - Auth proxy: "http://user:pass@proxy.example.com:8080"

(defcustom emacs-http-proxy nil
  "Default HTTP/HTTPS proxy for Emacs network operations.
Set to a proxy URL like \"127.0.0.1:7890\" to enable proxy.
This affects package installation, HTTP requests, and Git operations."
  :type '(choice (const :tag "No proxy" nil)
                 (string :tag "Proxy address"))
  :group 'omw-emacs-config)

(defun show-http-proxy ()
  "Display current HTTP/HTTPS proxy configuration."
  (interactive)
  (if url-proxy-services
      (message "Current http proxy is %s." (cdr (nth 1 url-proxy-services)))
    (message "No http proxy")))

(defun set-http-proxy (proxy)
  "Configure HTTP/HTTPS proxy for Emacs and subprocess environment.

Sets proxy for:
- Emacs internal URL library (url-proxy-services)
- Environment variables (http_proxy, https_proxy, all_proxy)
- Subprocesses (Git, curl, wget, etc.)

PROXY format: \"127.0.0.1:7890\" or \"http://127.0.0.1:7890\"
Also supports: \"socks5://127.0.0.1:1080\" or \"http://user:pass@host:port\"

Bypass rules (no_proxy): localhost, 127.0.0.1, 10.*, 192.168.*"
  (interactive
   (list (read-string
          (format "HTTP Proxy [%s]: " emacs-http-proxy)
          nil nil emacs-http-proxy)))
  (when (string-empty-p proxy)
    (user-error "Proxy cannot be empty"))
  (condition-case err
      (let* (;; Normalize proxy URL: add http:// prefix if missing
             (proxy-url (if (string-match-p "\\`https?://" proxy)
                            proxy
                          (concat "http://" proxy)))
             ;; Parse proxy URL to extract host and port
             (parsed (url-generic-parse-url proxy-url))
             (host (url-host parsed))
             (port (url-port parsed)))
        ;; Validate proxy configuration
        (unless (and host port)
          (error "Invalid proxy: missing host or port"))
        ;; Set environment variables for subprocesses (Git, curl, etc.)
        (dolist (var '("http_proxy" "https_proxy" "all_proxy"))
          (setenv var proxy-url))
        ;; Configure Emacs internal proxy with bypass rules
        (setq url-proxy-services
              `(("no_proxy"
                 ;; Bypass proxy for localhost and private networks
                 . "^\\(127\\.0\\.0\\.1\\|localhost\\|10\\..*\\|192\\.168\\..*\\)")
                ("http"  . ,(format "%s:%d" host port))
                ("https" . ,(format "%s:%d" host port))))
        (show-http-proxy))
    (error
     (message "Proxy error: %s" (error-message-string err)))))

(defun enable-http-proxy ()
  "Enable HTTP proxy using `emacs-http-proxy' custom variable.

Configuration:
  Add to custom.el:
    (setq emacs-http-proxy \"127.0.0.1:7890\")

  Then call: M-x enable-http-proxy

This automatically applies proxy settings on startup if configured."
  (interactive)
  (if emacs-http-proxy
      (set-http-proxy emacs-http-proxy)
    (message "HTTP proxy not configured. Add to custom.el: (setq emacs-http-proxy \"127.0.0.1:7890\")")))

(defun unset-http-proxy ()
  "Disable HTTP/HTTPS proxy for Emacs and subprocess environment.

Clears all proxy settings:
- Emacs internal proxy (url-proxy-services)
- Environment variables (http_proxy, https_proxy, all_proxy)

Useful for switching between proxy and direct connections."
  (interactive)
  (setenv "http_proxy")
  (setenv "https_proxy")
  (setenv "all_proxy")
  (setq url-proxy-services nil)
  (show-http-proxy))

;; Alias for convenience: M-x disable-http-proxy
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
;; Backup settings: use copying to avoid issues with symlinks in version control
(setq backup-by-copying t
      backup-directory-alist '((".*" . "~/.emacs.d/backup-files"))
      version-control t
      delete-old-versions t)

;; Startup behavior
(setq inhibit-default-init t)      ; Skip default.el to avoid conflicts with full config
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)

;; UI and interaction
(setq use-short-answers t)          ; Faster confirmation: y/n instead of yes/no
(setq ring-bell-function 'ignore)   ; Disable both audio and visual bell

;; File and buffer management
(setq recentf-max-saved-items 100)
;; Use forward-style buffer naming for clarity: dir/file instead of file<dir>
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")

;; Timestamp configuration
;; Format used by time-stamp.el when updating file headers
(setq time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S %:a by %u")

;; User identity
(setq user-full-name emacs-user-name
      user-mail-address emacs-user-email)

;; macOS specific settings
(when (eq system-type 'darwin)
  ;; Map Command key to Super and Option key to Meta
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

;; ==================================================================================
;; Built-in Emacs keybinding overrides
(use-package emacs
  :ensure nil
  :bind
  (("C-x C-b" . ibuffer)))

;; ==================================================================================
;; Key translation (must be set early, before init.el finishes)
(define-key key-translation-map (kbd "<return>") (kbd "RET"))

;; ==================================================================================
;; Base configuration hooks
(add-hook 'after-init-hook
          (lambda ()
            ;; File and buffer auto management
            (global-auto-revert-mode 1)   ; Auto-refresh when files change on disk
            (save-place-mode 1)           ; Remember cursor position
            (recentf-mode 1)              ; Track recently opened files

            ;; UI and editing enhancements
            (column-number-mode 1)        ; Show column number in mode line
            (jit-lock-mode 1)))           ; JIT font locking for performance

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
