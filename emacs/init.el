;;; init.el --- Emacs configuration entry point -*- lexical-binding:t -*-
;; Time-stamp: <2026-03-01 21:59:07 Sunday by zhengyuli>

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
;; This file performs early initialization and loads all configuration modules.

;;; Code:

(require 'cl-lib)

;; ==================================================================================
;; Basic check
(unless (version<= "30.2" emacs-version)
  (error "The Emacs version must be >= 30.2 (current: %s)" emacs-version))

;; ==================================================================================
;; Frame UI suppression (avoid startup flicker)
;; These should be called before the first frame is displayed
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

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
;; Key binding utilities
(defun lazy-set-key (key-alist &optional keymap key-prefix)
  "Define multiple key bindings with less typing.
KEYMAP is the keymap to add bindings to, default is `current-global-map'.
KEY-ALIST is an alist containing (KEY . COMMAND) pairs.
KEY-PREFIX is an optional prefix string for all keys, default is nil."
  (let (key def)
    (or keymap (setq keymap (current-global-map)))
    (if key-prefix
        (setq key-prefix (concat key-prefix " "))
      (setq key-prefix ""))
    (dolist (element key-alist)
      (setq key (car element))
      (setq def (cdr element))
      (cond ((stringp key) (setq key (read-kbd-macro (concat key-prefix key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key def))))

(defun lazy-unset-key (key-list &optional keymap)
  "Unset multiple key bindings with less typing.
KEYMAP is the keymap to remove bindings from, default is `current-global-map'.
KEY-LIST is a list of keys to unset.

Each key in KEY-LIST can be:
- A string (e.g., \"C-c f\") which is converted via `read-kbd-macro'
- A vector (e.g., [?\\C-c ?f]) which is used directly"
  (or keymap (setq keymap (current-global-map)))
  (dolist (key key-list)
    (cond ((stringp key) (setq key (read-kbd-macro (concat key))))
          ((vectorp key) nil)  ; Vectors are already in correct format, use as-is
          (t (signal 'wrong-type-argument (list 'array key))))
    (define-key keymap key nil)))

;; ==================================================================================
;; Configuration validation utilities
;; Validation system architecture:
;;   - This file: provides registration mechanism and generic validation functions
;;   - Feature modules: define required lists and register validators
;;
;; Usage:
;;   - Manual call: M-x config-dependency-validate
;;   - Automatic on startup: set environment variable EMACS_CONFIG_VALIDATE=1

;; Report theme faces
(defface config-dependency-report-title-face
  '((t :foreground "#46dcb0" :weight bold :height 1.3))
  "Face for validation report title.")

(defface config-dependency-report-category-face
  '((t :foreground "#79b6e8" :weight bold))
  "Face for validation report category headers.")

(defface config-dependency-report-success-face
  '((t :foreground "#90ee90" :weight bold))
  "Face for validation success messages.")

(defface config-dependency-report-error-face
  '((t :foreground "#ff6b6b"))
  "Face for validation error items.")

(defface config-dependency-report-hint-face
  '((t :foreground "#888888" :slant italic))
  "Face for validation installation hints.")

(defface config-dependency-report-separator-face
  '((t :foreground "#586e75"))
  "Face for validation report separators.")

;; Validator registry
(defvar config-dependency-validators nil
  "List of registered validators.
Each element is (CATEGORY . VALIDATOR-FN).
VALIDATOR-FN returns (INSTALLED . MISSING) where each is a list.")

;; Validator registration
(defun config-dependency-register (category validator-fn)
  "Register VALIDATOR-FN for CATEGORY.
VALIDATOR-FN is a function that returns (INSTALLED . MISSING).
CATEGORY is a symbol identifying the validation category."
  (push (cons category validator-fn) config-dependency-validators))

;; Generic validation functions
(defun config-dependency-validate-executables (items)
  "Validate executables in ITEMS.
ITEMS is a list of (EXECUTABLE-SYMBOL . INSTALL-INSTRUCTIONS).
Return (INSTALLED . MISSING) where each is a list of items."
  (let (installed missing)
    (dolist (item items)
      (if (executable-find (symbol-name (car item)))
          (push item installed)
        (push item missing)))
    (cons (nreverse installed) (nreverse missing))))

;; Report buffer helper
(defun config-dependency-insert-with-face (text face)
  "Insert TEXT with FACE."
  (insert (propertize text 'face face)))

(defun config-dependency-insert-line-with-face (text face)
  "Insert TEXT with FACE followed by newline."
  (config-dependency-insert-with-face text face)
  (insert "\n"))

;; Main validation orchestration
(defvar config-dependency-results nil
  "Alist of (CATEGORY . (INSTALLED . MISSING)) from last validation.")

(defun config-dependency-validate ()
  "Run all registered validators and display report.
Return t if all checks pass, nil otherwise."
  (interactive)
  (setq config-dependency-results nil)
  (dolist (validator config-dependency-validators)
    (let* ((category (car validator))
           (fn (cdr validator))
           (result (funcall fn)))
      ;; result is (installed . missing)
      (push (cons category result) config-dependency-results)))
  ;; Display report
  (config-dependency-show-report)
  ;; Return t if no missing items
  (cl-every (lambda (r) (null (cddr r))) config-dependency-results))

(defun config-dependency-show-report ()
  "Display validation report with colored output."
  (let ((report-buffer (get-buffer-create "*Emacs Config Validation*"))
        (total-installed 0)
        (total-missing 0))
    ;; Count totals
    (dolist (result config-dependency-results)
      (let ((installed (cadr result))
            (missing (cddr result)))
        (setq total-installed (+ total-installed (length installed))
              total-missing (+ total-missing (length missing)))))
    (with-current-buffer report-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; Title
      (config-dependency-insert-line-with-face
       "═══════════════════════════════════════════════════════════"
       'config-dependency-report-separator-face)
      (config-dependency-insert-line-with-face
       "          Emacs Configuration Validation Report"
       'config-dependency-report-title-face)
      (config-dependency-insert-line-with-face
       "═══════════════════════════════════════════════════════════"
       'config-dependency-report-separator-face)
      (insert "\n")
      ;; Details by category - show ALL registered categories
      (dolist (result config-dependency-results)
        (let* ((category (car result))
               (installed (cadr result))
               (missing (cddr result))
               (all-items (append installed missing)))
          ;; Show category header for ALL registered categories
          (config-dependency-insert-line-with-face
           (format "【%s】" (capitalize (symbol-name category)))
           'config-dependency-report-category-face)
          (if all-items
              (progn
                ;; Show installed items
                (dolist (item installed)
                  (if (consp item)
                      (progn
                        (config-dependency-insert-with-face "  ✓ " 'config-dependency-report-success-face)
                        (config-dependency-insert-with-face (symbol-name (car item)) 'config-dependency-report-success-face)
                        (insert "\n"))
                    (config-dependency-insert-with-face "  ✓ " 'config-dependency-report-success-face)
                    (config-dependency-insert-line-with-face item 'config-dependency-report-success-face)))
                ;; Show missing items
                (dolist (item missing)
                  (if (consp item)
                      (progn
                        (config-dependency-insert-with-face "  ✗ " 'config-dependency-report-error-face)
                        (config-dependency-insert-with-face (symbol-name (car item)) 'config-dependency-report-error-face)
                        (insert " - ")
                        (config-dependency-insert-line-with-face (cdr item) 'config-dependency-report-hint-face))
                    (config-dependency-insert-with-face "  ✗ " 'config-dependency-report-error-face)
                    (config-dependency-insert-line-with-face item 'config-dependency-report-error-face))))
            ;; No items for this category
            (config-dependency-insert-line-with-face "  (no items)" 'config-dependency-report-hint-face))
          (insert "\n")))
      ;; Installation hints (only if there are missing items)
      (when (> total-missing 0)
        (config-dependency-insert-line-with-face
         "───────────────────────────────────────────────────────────"
         'config-dependency-report-separator-face)
        (config-dependency-insert-line-with-face "Installation hints:" 'config-dependency-report-category-face)
        (config-dependency-insert-line-with-face "  • macOS: brew install <tool>" 'config-dependency-report-hint-face)
        (config-dependency-insert-line-with-face "  • LSP servers: pip/npm/go install <server>" 'config-dependency-report-hint-face)
        (insert "\n"))
      ;; Summary (at the end for better visibility)
      (config-dependency-insert-line-with-face
       "═══════════════════════════════════════════════════════════"
       'config-dependency-report-separator-face)
      (config-dependency-insert-with-face "  Summary: " 'config-dependency-report-category-face)
      (config-dependency-insert-with-face (format "%d installed" total-installed) 'config-dependency-report-success-face)
      (insert " / ")
      (config-dependency-insert-with-face (format "%d missing" total-missing)
               (if (> total-missing 0) 'config-dependency-report-error-face 'config-dependency-report-success-face))
      (insert "\n")
      ;; Footer
      (config-dependency-insert-line-with-face
       "───────────────────────────────────────────────────────────"
       'config-dependency-report-separator-face)
      (config-dependency-insert-line-with-face "  Press 'q' to close this buffer" 'config-dependency-report-hint-face)
      ;; Enable special-mode AFTER inserting content (makes buffer read-only)
      (special-mode))
    ;; Switch to report buffer
    (switch-to-buffer report-buffer)))

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
;; Note: use-package is critical infrastructure - ensure it's installed via setup.sh
;; or manually run M-x package-refresh-contents then M-x package-install RET use-package
(unless (package-installed-p 'use-package)
  ;; Try to install from cache first (avoids blocking refresh if package is cached)
  (condition-case err
      (progn
        (package-install 'use-package)
        (message "[Config] use-package installed from cache."))
    (error
     ;; Cache miss - provide guidance instead of blocking startup
     (message "[Config] Warning: use-package not in cache.")
     (message "[Config] Please run: M-x package-refresh-contents RET, then restart Emacs")
     (message "[Config] Continuing with reduced functionality..."))))

(require 'use-package)

;; ==================================================================================
;; GC optimization with gcmh
(use-package gcmh
  :ensure t
  :defer t
  :hook (after-init . gcmh-mode))

;; ==================================================================================
;; Async - asynchronous operations
(use-package async
  :ensure t
  :defer t)

;; ==================================================================================
;; Exec path from shell (macOS)
;; Optimization: use -l instead of -i to avoid slow shell startup
(use-package exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns))
  :demand t
  :config
  (exec-path-from-shell-initialize))

;; ==================================================================================
;; Auto package update
(use-package auto-package-update
  :ensure t
  :defer t
  :config
  (setq auto-package-update-delete-old-versions t))

;; ==================================================================================
;; Which-key - key hints
(use-package which-key
  :ensure t
  :defer t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-show-remaining-keys t)
  (which-key-setup-side-window-right))

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
;; Core modules
(require 'init-fonts)
(require 'init-ui)
(require 'init-completion)
(require 'init-editing)

(require 'init-dired)
(require 'init-projects)
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
                  init-shell
                  init-dockerfile
                  init-cmake
                  init-yaml
                  init-markdown))
  (require module))

;; ==================================================================================
;; Load user custom settings
(defun load-custom-settings ()
  "Load user custom settings file."
  (let ((custom-file (locate-user-emacs-file "custom_settings.el")))
    (unless (file-exists-p custom-file)
      (with-temp-buffer (write-file custom-file)))
    (load custom-file :noerror :nomessage)))

(load-custom-settings)

;; ==================================================================================
;;; init.el ends here
