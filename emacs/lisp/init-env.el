;;; init-env.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 22:21:06 星期五 by zhengyu.li>

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
;; Environment and platform settings: HTTP proxy, exec-path-from-shell,
;; and macOS settings.

;;; Code:

(require 'init-funcs)

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
  :defer t)

;; Initialize exec-path from shell (deferred)
;; Note: Must be outside use-package :config because :defer t means :config
;; only runs when the package is loaded. Without any autoload trigger,
;; the package never loads and initialize is never called.
(when (memq window-system '(mac ns))
  (run-config-timer 1 nil
    (lambda ()
      (require 'exec-path-from-shell)
      (exec-path-from-shell-initialize))))

;; ==================================================================================
;; Mac system settings
(add-hook 'after-init-hook
          (lambda ()
            (when (memq window-system '(mac ns))
              (setq mac-command-modifier 'super
                    mac-option-modifier 'meta))))

;; ==================================================================================
;; Enable HTTP proxy if configured
(enable-http-proxy)

;; ==================================================================================
;;; Provide features
(provide 'init-env)

;;; init-env.el ends here
