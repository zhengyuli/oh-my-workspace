;;; omw-proxy.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-29 14:51:55 Sunday by zhengyu.li>
;;
;; ============================================================================
;; omw-proxy.el - HTTP/HTTPS proxy configuration for Emacs.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: proxy, network, http
;; Dependencies: (none)
;;
;; Copyright (C) 2026 zhengyu li
;;
;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.
;;
;;; Commentary:
;;
;; HTTP/HTTPS proxy configuration for Emacs network operations.
;; Provides interactive commands to enable/disable proxy and configure
;; proxy settings for both Emacs and subprocess environment.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Proxy Configuration
;; ----------------------------------------------------------------------------

(defcustom omw/http-proxy nil
  "Default HTTP/HTTPS proxy for Emacs network operations.
Set to a proxy URL like \"127.0.0.1:7890\" to enable proxy.
This affects package installation, HTTP requests, and Git operations."
  :type '(choice (const :tag "No proxy" nil)
                 (string :tag "Proxy address"))
  :group 'omw-emacs)

;; --- Show Http Proxy ---
(defun omw/show-http-proxy ()
  "Display current HTTP/HTTPS proxy configuration."
  (interactive)
  (if url-proxy-services
      (message "Current http proxy is %s." (cdr (nth 1 url-proxy-services)))
    (message "No http proxy")))

;; --- Set Http Proxy ---
(defun omw/set-http-proxy (proxy)
  "Configure HTTP/HTTPS proxy for Emacs and subprocess environment.

Sets Emacs internal URL library (url-proxy-services), environment
variables (http_proxy, https_proxy, all_proxy), and subprocesses
(Git, curl, wget, etc.).

PROXY format: \"127.0.0.1:7890\" or \"http://127.0.0.1:7890\"
Also supports: \"socks5://127.0.0.1:1080\" or \"http://user:pass@host:port\"

Bypass rules (no_proxy): localhost, 127.0.0.1, 10.*, 192.168.*"
  (interactive
   (list (read-string
          (format "HTTP Proxy [%s]: " omw/http-proxy)
          nil nil omw/http-proxy)))
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
              `(("no_proxy" .
                 ,(concat "^\\(127\\.0\\.0\\.1\\|localhost\\|"
                          "10\\..*\\|192\\.168\\..*\\)"))
                ("http" . ,(format "%s:%d" host port))
                ("https" . ,(format "%s:%d" host port))))
        (omw/show-http-proxy))
    (error
     (message "Proxy error: %s" (error-message-string err)))))

;; --- Enable Http Proxy ---
(defun omw/enable-http-proxy ()
  "Enable HTTP proxy for Emacs.

Checks environment variables (HTTP_PROXY, http_proxy, HTTPS_PROXY,
https_proxy, ALL_PROXY, all_proxy) first, then falls back to
`omw/http-proxy'.  If an environment variable is found, it overrides
`omw/http-proxy'.  The selected value is passed to `omw/set-http-proxy'.

If neither source is configured, a warning is displayed."
  (interactive)
  (let ((proxy
         (or (getenv "HTTP_PROXY")
             (getenv "http_proxy")
             (getenv "HTTPS_PROXY")
             (getenv "https_proxy")
             (getenv "ALL_PROXY")
             (getenv "all_proxy")
             omw/http-proxy)))
    (if proxy
        (progn
          ;; Keep variable in sync with the effective proxy
          (setq omw/http-proxy proxy)
          (omw/set-http-proxy proxy))
      (message
       "No HTTP proxy.  Set HTTP_PROXY or `omw/http-proxy`."))))

;; --- Unset Http Proxy ---
(defun omw/unset-http-proxy ()
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
  (omw/show-http-proxy))

;; Alias for convenience: M-x omw/disable-http-proxy
(defalias 'omw/disable-http-proxy 'omw/unset-http-proxy)

;; ============================================================================
;;; Provide features
(provide 'omw-proxy)

;;; omw-proxy.el ends here
