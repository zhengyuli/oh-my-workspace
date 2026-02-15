;;; init-functions.el -*- lexical-binding: t; -*-
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
;; Utility functions for Emacs configuration.

;;; Code:

;; ==================================================================================
;; Font utilities
(defun ensure-font-installed (font)
  "Assure font is installed."
  (when (and
         (display-graphic-p)
         (null (x-list-fonts font)))
    (error (format "Missing \"%s\" font, please install." font))))

;; ==================================================================================
;; Package utilities
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it's not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (condition-case nil
           (package-install package)
         (error
          (package-refresh-contents)
          (package-install package)))))
   packages))

;; ==================================================================================
;; Key binding utilities
(defun lazy-set-key (key-alist &optional keymap key-prefix)
  "This function is to little type when define key binding.
`KEYMAP' is a add keymap for some binding, default is `current-global-map'.
`KEY-ALIST' is a alist contain main-key and command.
`KEY-PREFIX' is a add prefix for some binding, default is nil."
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
  "This function is to little type when unset key binding.
`KEYMAP' is add keymap for some binding, default is `current-global-map'
`KEY-LIST' is list contain key."
  (let (key)
    (or keymap (setq keymap (current-global-map)))
    (dolist (key key-list)
      (cond ((stringp key) (setq key (read-kbd-macro (concat key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key nil))))

;; ==================================================================================
;; Proxy utilities
(defun show-http-proxy ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current http proxy is %s." (cdr (nth 1 url-proxy-services)))
    (message "No http proxy")))

(defun set-http-proxy (proxy)
  "Set HTTP/HTTPS proxy."
  (interactive (list (read-string
                      (format "HTTP Proxy Server [%s]: " emacs-http-proxy)
                      nil nil emacs-http-proxy)))
  (if (not (string-empty-p proxy))
      (let* ((proxy (if (string-match-p "https?://" proxy)
                        proxy
                      (concat "http://" proxy)))
             (parsed-url (url-generic-parse-url proxy))
             (proxy-no-scheme
              (format "%s:%d" (url-host parsed-url) (url-port parsed-url))))
        (setenv "http_proxy" proxy)
        (setenv "https_proxy" proxy)
        (setenv "all_proxy" proxy)
        (setq url-proxy-services
              `(("no_proxy" . "^\\(127.0.0.1\\|localhost\\|10\\..*\\|192\\.168\\..*\\)")
                ("http" . ,proxy-no-scheme)
                ("https" . ,proxy-no-scheme)))
        (show-http-proxy))
    (warn "Proxy url could not be empty.")))

(defun enable-http-proxy ()
  (interactive)
  (if emacs-http-proxy
      (set-http-proxy emacs-http-proxy)))

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
(defun get-mode-name ()
  "Display major mode and mode name."
  (interactive)
  (message "major-mode: %s, mode-name: %s" major-mode (car mode-name)))

(defun indent-buffer ()
  "Automatic format current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

(defun smart-indent ()
  "If mark is active, indent region, else indent all buffer."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'indent-region)
      (call-interactively 'indent-buffer))))

(defun copy-region ()
  "Copy region."
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end)))

(defun copy-curr-line ()
  "Copy current line."
  (interactive)
  (let ((end (min (point-max) (line-end-position))))
    (copy-region-as-kill (line-beginning-position) end)))

(defun smart-copy ()
  "If mark is active, copy region, else copy current line."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'copy-region)
      (call-interactively 'copy-curr-line))))

(defun smart-kill ()
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

(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter
     nil
     'fullscreen
     (if (equal 'fullboth current-value)
         (if (boundp 'old-fullscreen)
             old-fullscreen
           nil)
       (setq old-fullscreen current-value)
       'fullboth))))

(defun adjust-window-split-thresholds ()
  "Adjust window split thresholds."
  (interactive)
  (if (>= (frame-pixel-width) (frame-pixel-height))
      (progn
        (setq split-height-threshold (frame-height))
        (setq split-width-threshold  (/ (frame-width) 2)))
    (progn
      (setq split-height-threshold (frame-height))
      (setq split-width-threshold  (frame-width)))))

;; ==================================================================================
;; Git utilities
(defun git-user-name ()
  "Get git user name."
  (interactive)
  (message (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.name"))))

(defun git-user-email ()
  "Get git user email."
  (interactive)
  (message (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.email"))))

;; ==================================================================================
;; Package update utility
(defun auto-package-upgrade-all ()
  "Upgrade all packages installed."
  (interactive)
  (require 'auto-package-update)
  (package-refresh-contents)
  (auto-package-update-now))

;; ==================================================================================
;;; Provide features
(provide 'init-functions)

;;; init-functions.el ends here
