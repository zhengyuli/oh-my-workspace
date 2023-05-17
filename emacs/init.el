;;; package --- init.el -*- lexical-binding:t -*-
;; Time-stamp: <2023-05-17 10:54:43 星期三 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023 zhengyu li
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

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'init)

;;; Require:
(load-library "package")

;;; Code:
;; ==================================================================================
;; Emacs configuration root path
(defvar emacs-config-root-path "_EMACS_CONFIG_ROOT_PATH_"
  "Emacs configuration root path.")

;; Emacs configuration temp packages path to load 3rd packages which are not stable
(defvar emacs-config-temp-packages-path (concat user-emacs-directory "temp-packages/")
  "Emacs configuration custom site packages path.")

;; Emacs configuration custom path to load custom settings
(defvar emacs-config-custom-settings-path (concat emacs-config-root-path "custom/")
  "Emacs configuration custom settings path.")

;; Emacs configuration custom site packages path to load 3rd party settings
(defvar emacs-config-custom-site-packages-path (concat emacs-config-root-path "site-packages/")
  "Emacs configuration custom site packages path.")

;; Emacs configuration fixed font
(defvar emacs-config-fixed-font "Source Code Pro" "Emacs configuration fixed font.")

;; Emacs configuration fixed serif font
(defvar emacs-config-fixed-serif-font "Source Serif Pro" "Emacs configuration fixed serif font.")

;; Emacs configuration variable font
(defvar emacs-config-variable-font "Times New Roman" "Emacs configuration variable font.")

;; Emacs configuration user
(defvar emacs-config-user "Zhengyu Li" "Emacs configuration user.")

;; Emacs configuration email
(defvar emacs-config-email "lizhengyu419@outlook.com" "Emacs configuration email.")

;; Emacs configuration proxy
(defvar emacs-http-proxy nil "Emacs configuration http proxy, default is nil.")

;; ==================================================================================
(defun ensure-font-installed (font)
  "Assure font is installed."
  (when (and
         (display-graphic-p)
         (null (x-list-fonts font)))
    (error (format "Missing \"%s\" font, please install." font))))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

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

(defun add-subdirs-to-load-path (base-dir)
  "Add subdirs to load path.
Look up all subdirs under `BASE-DIR' recursively and add them into load path."
  (let ((default-directory base-dir))
    (add-to-list 'load-path base-dir)
    (normal-top-level-add-subdirs-to-load-path)))

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
  (let* ((parsed-url (url-generic-parse-url proxy))
         (proxy-no-scheme
          (format "%s:%d" (url-host parsed-url) (url-port parsed-url))))
    (when (url-type parsed-url)
      (setenv "http_proxy" proxy)
      (setenv "https_proxy" proxy)
      (setenv "all_proxy" proxy))
    (setq url-proxy-services
          `(("no_proxy" . "^\\(127.0.0.1\\|localhost\\|10\\..*\\|192\\.168\\..*\\)")
            ("http" . ,proxy-no-scheme)
            ("https" . ,proxy-no-scheme)))
    (show-http-proxy)))

(defun enable-http-proxy ()
  (interactive)
  (if emacs-http-proxy
      (set-http-proxy emacs-http-proxy)
    (warn "Default emacs http proxy is nil, please configure it first.")))

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
;; Basic check
(unless (>= (string-to-number emacs-version) 27.1)
  (error "The Emacs version must be >= 27.1."))

(ensure-font-installed emacs-config-fixed-font)

(ensure-font-installed emacs-config-fixed-serif-font)

(ensure-font-installed emacs-config-variable-font)

;; ==================================================================================
;; Try to enable HTTP proxy
(enable-http-proxy)

;; Initialize package manager
;; Add package archives
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize packages
(package-initialize)

;; Refresh package list if any
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install packages
(ensure-package-installed
 ;; ==============================
 ;; init-base.el
 'beacon
 'smooth-scrolling
 ;; ******************************
 'exec-path-from-shell
 ;; ******************************
 'move-text
 'expand-region
 'multiple-cursors
 'visual-regexp-steroids
 ;; ******************************
 'which-key
 'undo-tree
 'browse-kill-ring
 'goto-chg
 ;; ******************************
 'all-the-icons
 ;; ******************************
 'ivy
 'ivy-rich
 'ivy-prescient
 'all-the-icons-ivy-rich
 ;; ******************************
 'swiper
 ;; ******************************
 'counsel
 'counsel-projectile
 ;; ******************************
 'color-moccur
 ;; ******************************
 'ag
 'wgrep-ag
 ;; ******************************
 'avy
 ;; ******************************
 'popup
 ;; ******************************
 'yasnippet
 ;; ******************************
 'company
 'company-prescient
 'company-box
 'company-quickhelp
 'company-quickhelp-terminal
 ;; ******************************
 'flyspell-correct
 'flyspell-correct-avy-menu
 ;; ******************************
 'winum
 'dimmer
 'switch-window
 'centaur-tabs
 'doom-modeline
 'textsize
 'doom-themes
 ;; ******************************
 'dashboard
 ;; ******************************
 'restart-emacs
 ;; ******************************
 'pinentry
 ;; ******************************
 'async
 'ztree
 'dired-single
 'dired-filter
 'dired-subtree
 'dired-hacks-utils
 'dired-filetype-face
 'all-the-icons-dired
 ;; ******************************
 'vterm
 'multi-vterm
 ;; ******************************
 'magit
 ;; ******************************
 'go-translate
 ;; ******************************
 'gnuplot
 ;; ******************************
 'org-roam
 ;; ******************************
 'mixed-pitch
 ;; ******************************
 'eww-lnum
 ;; ******************************
 'mu4e-alert
 ;; ******************************
 'password-store
 'pass
 ;; ******************************
 'gptel
 ;; ******************************
 'auto-package-update
 ;; ==============================
 ;; init-prog-mode.el
 'smartparens
 'hungry-delete
 'rainbow-delimiters
 'hl-todo
 'flycheck
 'whitespace-cleanup-mode
 'quickrun
 'dumb-jump
 'format-all
 'devdocs
 ;; ******************************
 'lsp-mode
 ;; ******************************
 'dap-mode
 ;; ==============================
 ;; init-c&c++-mode.el
 'google-c-style
 ;; ==============================
 ;; init-python-mode.el
 'sphinx-doc
 'python-docstring
 'pyvenv
 'python-black
 'py-isort
 ;; ==============================
 ;; init-go-mode.el
 'go-mode
 'go-eldoc
 ;; ==============================
 ;; init-elisp-mode.el
 'elisp-slime-nav
 'lisp-extra-font-lock
 'rainbow-mode
 ;; ==============================
 ;; init-haskell-mode.el
 'haskell-mode
 ;; ==============================
 ;; init-scala-mode.el
 'scala-mode
 ;; ==============================
 ;; init-rust-mode.el
 'rust-mode
 ;; ==============================
 ;; init-groovy-mode.el
 'groovy-mode
 ;; ==============================
 ;; init-cmake-mode.el
 'cmake-mode
 ;; ==============================
 ;; init-dockerfile-mode.el
 'dockerfile-mode
 ;; ==============================
 ;; init-yaml-mode.el
 'yaml-mode
 ;; ==============================
 ;; init-plantuml-mode.el
 'plantuml-mode
 ;; ==============================
 ;; init-org-mode.el
 'org-bullets
 'org-appear
 'org-tree-slide
 'valign
 'ob-rust
 ;; ==============================
 ;; init-markdown-mode.el
 'markdown-mode
 'markdownfmt
 'impatient-mode)

;; ==================================================================================
;; Create the temporary packages directory if any
(unless (file-directory-p emacs-config-temp-packages-path)
  (make-directory emacs-config-temp-packages-path))

;; Add custom directory to load-path
(add-subdirs-to-load-path emacs-config-temp-packages-path)
(add-subdirs-to-load-path emacs-config-custom-settings-path)
(add-subdirs-to-load-path emacs-config-custom-site-packages-path)

;; Load user defined libraries
(load-library "init-base")
(load-library "init-prog-mode")
(load-library "init-cc-mode")
(load-library "init-c&c++-mode")
(load-library "init-python-mode")
(load-library "init-go-mode")
(load-library "init-elisp-mode")
(load-library "init-haskell-mode")
(load-library "init-scala-mode")
(load-library "init-rust-mode")
(load-library "init-groovy-mode")
(load-library "init-sh-script-mode")
(load-library "init-dockerfile-mode")
(load-library "init-cmake-mode")
(load-library "init-text-mode")
(load-library "init-yaml-mode")
(load-library "init-plantuml-mode")
(load-library "init-org-mode")
(load-library "init-markdown-mode")

;; Load user custom settings
(if (not (file-exists-p "~/.emacs.d/custom_settings.el"))
    (shell-command "touch ~/.emacs.d/custom_settings.el"))
(load-file "~/.emacs.d/custom_settings.el")

;;; init.el ends here
