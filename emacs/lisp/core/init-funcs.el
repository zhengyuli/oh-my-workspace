;;; init-funcs.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 13:26:00 Thursday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: (none - base module)

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
;; Core utility functions for Emacs configuration.
;; This is the foundational module that all other modules depend on.
;;
;; Provides:
;; - Key binding utilities (lazy-set-key, lazy-unset-key)
;; - Proxy utilities (set-http-proxy, enable-http-proxy, etc.)
;; - Buffer utilities (indent-entire-buffer, smart-copy-region, etc.)
;; - Git utilities (git-user-name-async, git-user-email-async)
;; - Timer management system (run-config-timer, cleanup-config-timers)
;; - Configuration validation system (config-dependency-validate, etc.)

;;; Code:

(require 'cl-lib)

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
;; Proxy utilities
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
;; Buffer utilities (from init-base.el)
(defun current-major-mode-name ()
  "Display major mode and mode name."
  (interactive)
  (message "major-mode: %s, mode-name: %s" major-mode mode-name))

(defun indent-entire-buffer ()
  "Automatic format current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

(defun smart-indent-region ()
  "If mark is active, indent region, else indent all buffer."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'indent-region)
      (call-interactively 'indent-entire-buffer))))

(defun copy-region ()
  "Copy region."
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end)))

(defun copy-current-line ()
  "Copy current line."
  (interactive)
  (let ((end (min (point-max) (line-end-position))))
    (copy-region-as-kill (line-beginning-position) end)))

(defun smart-copy-region ()
  "If mark is active, copy region, else copy current line."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'copy-region)
      (call-interactively 'copy-current-line))))

(defun smart-kill-region ()
  "If mark is active, kill region, else kill whole line."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'kill-whole-line)))

(defun toggle-buffer-writable ()
  "Toggle buffer writable."
  (interactive)
  (if buffer-read-only
      (read-only-mode -1)
    (read-only-mode 1)))

;; Toggle fullscreen state variable
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

;; ==================================================================================
;; Git utilities
(defvar git-user-name-cache nil
  "Cached git user name.")
(defvar git-user-email-cache nil
  "Cached git user email.")

(defun git-user-name-async (&optional callback)
  "Get git user name asynchronously.
If CALLBACK is provided, call it with the result;
otherwise display in minibuffer.
Result is cached in `git-user-name-cache'."
  (interactive)
  (if (and git-user-name-cache
           (not (called-interactively-p 'interactive)))
      (when callback (funcall callback git-user-name-cache))
    (let ((default-directory (or (vc-root-dir) default-directory)))
      (make-process
       :name "git-user-name"
       :command '("git" "config" "--get" "user.name")
       :sentinel
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (with-current-buffer (process-buffer proc)
             (let ((result (string-trim (buffer-string)))
                   (buf (process-buffer proc)))
               (setq git-user-name-cache
                     (unless (string-empty-p result) result))
               ;; Clean up process buffer after extracting result
               (kill-buffer buf)
               (if callback
                   (funcall callback git-user-name-cache)
                 (message "Git user name: %s"
                          (or git-user-name-cache "not set")))))))))))

(defun git-user-email-async (&optional callback)
  "Get git user email asynchronously.
If CALLBACK is provided, call it with the result;
otherwise display in minibuffer.
Result is cached in `git-user-email-cache'."
  (interactive)
  (if (and git-user-email-cache
           (not (called-interactively-p 'interactive)))
      (when callback (funcall callback git-user-email-cache))
    (let ((default-directory (or (vc-root-dir) default-directory)))
      (make-process
       :name "git-user-email"
       :command '("git" "config" "--get" "user.email")
       :sentinel
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (with-current-buffer (process-buffer proc)
             (let ((result (string-trim (buffer-string)))
                   (buf (process-buffer proc)))
               (setq git-user-email-cache
                     (unless (string-empty-p result) result))
               ;; Clean up process buffer after extracting result
               (kill-buffer buf)
               (if callback
                   (funcall callback git-user-email-cache)
                 (message "Git user email: %s"
                          (or git-user-email-cache "not set")))))))))))

;; Keep synchronous versions for backward compatibility (not recommended for frequent use)
(defalias 'git-user-name #'git-user-name-async
  "Get git user name.
\(Async version, use `git-user-name-async' directly for callbacks)")

(defalias 'git-user-email #'git-user-email-async
  "Get git user email.
\(Async version, use `git-user-email-async' directly for callbacks)")

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

;; ==================================================================================
;; Configuration validation utilities
;; Validation system architecture:
;;   - This file: provides registration mechanism and generic validation functions
;;   - Feature modules: define required lists and register validators
;; Registered validators:
;;   - init-vc.el: vc-tools (git)
;;   - init-completion.el: search-tools (rg, ag)
;;   - init-editing.el: spell-tools (aspell, hunspell)
;;   - init-dired.el: dired-tools (gls, fd)
;;   - init-utilities.el: utility-tools (pass)
;;   - init-markdown.el: markdown-tools (marksman)
;;   - init-terminal.el: terminal-tools (vterm)
;;   - init-python.el: python-tools (pylsp)
;;   - init-go.el: go-tools (gopls, gofumpt)
;;   - init-cc.el: cc-tools (clangd)
;;   - init-shell.el: shell-tools (bash-language-server)
;;   - init-yaml.el: yaml-tools (yaml-language-server)
;;   - init-cmake.el: cmake-tools (cmake-language-server)
;;   - init-dockerfile.el: docker-tools (docker-langserver)
;; ... .. .
;; Usage:
;;   - Manual call: M-x validate-config
;;   - Automatic on startup: set environment variable EMACS_CONFIG_VALIDATE=1

;; ----------------------------------------------------------------------------------
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

;; ----------------------------------------------------------------------------------
;; Validator registry
(defvar config-dependency-validators nil
  "List of registered validators.
Each element is (CATEGORY . VALIDATOR-FN).
VALIDATOR-FN returns (INSTALLED . MISSING) where each is a list.")

;; ----------------------------------------------------------------------------------
;; Validator registration
(defun config-dependency-register (category validator-fn)
  "Register VALIDATOR-FN for CATEGORY.
VALIDATOR-FN is a function that returns (INSTALLED . MISSING).
CATEGORY is a symbol identifying the validation category."
  (push (cons category validator-fn) config-dependency-validators))

;; ----------------------------------------------------------------------------------
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

;; ----------------------------------------------------------------------------------
;; Report buffer helper
(defun config-dependency-insert-with-face (text face)
  "Insert TEXT with FACE."
  (insert (propertize text 'face face)))

(defun config-dependency-insert-line-with-face (text face)
  "Insert TEXT with FACE followed by newline."
  (config-dependency-insert-with-face text face)
  (insert "\n"))

;; ----------------------------------------------------------------------------------
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
;;; Provide features
(provide 'init-funcs)

;;; init-funcs.el ends here
