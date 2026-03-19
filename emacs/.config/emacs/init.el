;;; init.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-19 21:29:01 Thursday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: emacs, config
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Emacs configuration entry point.

;;; Code:

;; ============================================================================
(defgroup omw-emacs nil
  "Oh My Workspace configuration group."
  :group 'convenience
  :prefix "omw/")

;; ============================================================================
(defcustom omw/emacs-user-name "Zhengyu Li"
  "Emacs configuration user name.
Used for dashboard banner and setting `user-full-name'."
  :type 'string
  :group 'omw-emacs)

(defcustom omw/emacs-user-email "lizhengyu419@outlook.com"
  "Emacs configuration email address.
Used for setting `user-mail-address'."
  :type 'string
  :group 'omw-emacs)

;; ============================================================================
(defvar omw/emacs-custom-file-path (expand-file-name "custom.el" user-emacs-directory)
  "Emacs custom file path, which will be used to add extra customization.")

(defvar omw/emacs-config-root-path (let ((config-file (or load-file-name buffer-file-name)))
                                     (if config-file
                                         (file-name-directory (file-chase-links config-file))
                                       default-directory))
  "Emacs configuration root path.
Automatically resolves symlinks to find the actual configuration directory.")

(defvar omw/emacs-config-lisp-path (expand-file-name "lisp/" omw/emacs-config-root-path)
  "Emacs configuration custom settings path.")

(defvar omw/emacs-config-site-packages-path (expand-file-name "site-packages/" omw/emacs-config-root-path)
  "Emacs configuration custom site packages path.")

(defun omw/emacs-add-subdirs-to-load-path (base-dir)
  "Add subdirs to load path.
Look up all subdirs under `BASE-DIR' recursively and add them into load path."
  (let ((default-directory base-dir))
    (add-to-list 'load-path base-dir)
    (normal-top-level-add-subdirs-to-load-path)))

;; ============================================================================
;; Set custom file early to prevent Emacs from writing customizations to init.el
(setq custom-file omw/emacs-custom-file-path)

;; Recursively add emacs configuration custom path to load path
(omw/emacs-add-subdirs-to-load-path omw/emacs-config-lisp-path)
(omw/emacs-add-subdirs-to-load-path omw/emacs-config-site-packages-path)

;; ============================================================================
;; HTTP Proxy Configuration
(require 'omw-proxy)
(omw/enable-http-proxy)

;; ============================================================================
;; Package Management
(require 'use-package)

;; ============================================================================
(use-package emacs
  :ensure nil
  :demand t
  :config
  ;; Basic check
  (unless (version<= "30.2" emacs-version)
    (error "The Emacs version must be >= 30.2 (current: %s)" emacs-version))

  ;; Frame UI suppression (avoid startup flicker)
  ;; These should be called before the first frame is displayed
  (dolist (mode '(tool-bar-mode
                  scroll-bar-mode
                  menu-bar-mode
                  tooltip-mode))
    (funcall mode -1))

  ;; GC tuning - use large threshold during startup for faster initialization
  ;; gcmh will manage GC after startup with reasonable thresholds
  (setq gc-cons-threshold (* 100 1024 1024)
        gc-cons-percentage 0.6))

;; ============================================================================
(use-package package
  :ensure nil
  :demand t
  :config
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))

        ;; Set priorities: melpa < nongnu < gnu
        package-archive-priorities '(("melpa" . 0)
                                     ("nongnu" . 5)
                                     ("gnu" . 10)))
  ;; Initialize packages
  (package-initialize))

;; ============================================================================
(use-package async
  :ensure t
  :defer t)

;; ============================================================================
(use-package gcmh
  :ensure t
  :defer t
  :hook (after-init . gcmh-mode))

;; ============================================================================
(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :defer t
  :hook (after-init . exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-arguments '("-l")))

;; ============================================================================
(use-package auto-package-update
  :ensure t
  :defer t
  :config
  (setq auto-package-update-delete-old-versions t))

;; ============================================================================
(use-package which-key
  :ensure t
  :defer t
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-minibuffer))

;; ============================================================================
(defun omw/after-init-setup ()
  "Enable common post-initialization features.
This includes save-place-mode, recentf-mode, column-number-mode,
and global-auto-revert-mode."
  (save-place-mode 1)
  (recentf-mode 1)
  (column-number-mode 1)
  (global-auto-revert-mode 1))

(defun omw/emacs-startup-setup ()
  "Display startup timing and garbage collection statistics."
  (message "Emacs ready in %.2f seconds with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(use-package emacs
  :ensure nil
  :demand t
  :hook ((after-init . omw/after-init-setup)
         (emacs-startup . omw/emacs-startup-setup))
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; Autosave
  (setq auto-save-list-file-prefix (expand-file-name
                                    "emacs/auto-save-list/.saves-" omw/xdg-data-home))

  ;; Native compilation
  (setq native-compile-target-directory (expand-file-name
                                         "emacs/eln-cache/" omw/xdg-cache-home))

  ;; Backup and version control
  (setq backup-directory-alist (list (cons ".*" (expand-file-name
                                                 "emacs/backup/" omw/xdg-state-home)))
        backup-by-copying t
        version-control t
        delete-old-versions t)

  ;; File and buffer management
  (setq recentf-max-saved-items 200
        uniquify-buffer-name-style 'forward
        uniquify-separator "/")

  ;; Project management
  (setq project-list-file (expand-file-name "emacs/projects" omw/xdg-state-home))

  ;; User identity and timestamps
  (setq time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S %:a by %u"
        user-full-name omw/emacs-user-name
        user-mail-address omw/emacs-user-email)

  ;; Startup behavior
  (setq inhibit-default-init t
        inhibit-startup-echo-area-message t
        inhibit-startup-screen t)

  ;; UI and interaction
  (setq use-short-answers t
        ring-bell-function 'ignore)

  ;; macOS key modifiers
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super
          mac-option-modifier 'meta)))

;; ============================================================================
;; Load modules

;; Editor modules
(require 'omw-font)
(require 'omw-appearance)
(require 'omw-edit)
(require 'omw-search)
(require 'omw-template)
(require 'omw-completion)
(require 'omw-explorer)

;; Tool modules
(require 'omw-pass)
(require 'omw-git)
(require 'omw-term)
(require 'omw-pdf)
(require 'omw-ai)

;; Prog mode modules
(require 'omw-prog)
(require 'omw-cc)
(require 'omw-go)
(require 'omw-rust)
(require 'omw-python)
(require 'omw-typescript)
(require 'omw-elisp)
(require 'omw-shell)
(require 'omw-cmake)
(require 'omw-yaml)
(require 'omw-dockerfile)
(require 'omw-vimrc)
(require 'omw-gitconfig)

;; Text mode modules
(require 'omw-markdown)

;; ============================================================================
;; Load custom settings
(when (file-readable-p custom-file)
  (load custom-file nil 'nomessage))

;; ============================================================================
;;; Provide features
(provide 'init)

;;; init.el ends here
