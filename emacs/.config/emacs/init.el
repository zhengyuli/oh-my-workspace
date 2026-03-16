;;; init.el -*- lexical-binding: t; -*-

;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: emacs, config
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li
;; Licensed under the GPL License version 3.0

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; History:
;;
;; 2026-03-14 15:30 chieftain <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Emacs configuration entry point.

;;; Code:

;; ==================================================================================
(defgroup omw-emacs nil
  "Oh My Workspace configuration group."
  :group 'convenience
  :prefix "omw/")

;; ==================================================================================
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

;; ==================================================================================
;; XDG Base Directory Configuration for Emacs
(let ((xdg-data-home  (or (getenv "XDG_DATA_HOME")
                          (expand-file-name "~/.local/share/")))
      (xdg-cache-home (or (getenv "XDG_CACHE_HOME")
                          (expand-file-name "~/.cache/"))))
  (setq user-emacs-directory (expand-file-name "emacs/" xdg-data-home)
        package-user-dir (expand-file-name "emacs/elpa/" xdg-data-home)
        native-compile-target-directory (expand-file-name "emacs/eln-cache/" xdg-cache-home)))

;; ==================================================================================
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

;; ==================================================================================
;; Auto-save Configuration - Use XDG cache directory
;; Prevents auto-save files from polluting .config/emacs/
(let ((auto-save-dir (expand-file-name
                      (concat (or (getenv "XDG_CACHE_HOME")
                                (concat (getenv "HOME") "/.cache"))
                              "/emacs/auto-save-list/"))))
  ;; Ensure directory exists
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir))
  ;; Set auto-save list file prefix to XDG cache directory
  (setq auto-save-list-file-prefix auto-save-dir))

;; ==================================================================================
;; Set custom file early to prevent Emacs from writing customizations to init.el
(setq custom-file omw/emacs-custom-file-path)

;; Recursively add emacs configuration custom path to load path
(omw/emacs-add-subdirs-to-load-path omw/emacs-config-lisp-path)
(omw/emacs-add-subdirs-to-load-path omw/emacs-config-site-packages-path)

;; ==================================================================================
(require 'omw-proxy)
(omw/enable-http-proxy)

;; ==================================================================================
(require 'use-package)

;; ==================================================================================
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

;; ==================================================================================
(use-package package
  :ensure nil
  :demand t
  :config
  (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/"))

        ;; Set priorities: melpa < nongnu < gnu
        package-archive-priorities '(("melpa" . 0)
                                     ("nongnu" . 5)
                                     ("gnu" . 10)))
  ;; Initialize packages
  (package-initialize))

;; ==================================================================================
(use-package async
  :ensure t
  :defer t)

;; ==================================================================================
(use-package gcmh
  :ensure t
  :defer t
  :hook (after-init . gcmh-mode))

;; ==================================================================================
(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :defer t
  :hook (after-init . exec-path-from-shell-initialize))

;; ==================================================================================
(use-package auto-package-update
  :ensure t
  :defer t
  :config
  (setq auto-package-update-delete-old-versions t))

;; ==================================================================================
(use-package which-key
  :ensure t
  :defer t
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-minibuffer))

;; ==================================================================================
(defun omw/after-init-setup ()
  (save-place-mode 1)
  (recentf-mode 1)
  (column-number-mode 1)
  (global-auto-revert-mode 1))

(defun omw/emacs-startup-setup ()
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
  (setq inhibit-default-init t
        inhibit-startup-echo-area-message t
        inhibit-startup-screen t

        ;; UI and interaction
        use-short-answers t
        ring-bell-function 'ignore

        ;; Backup and version control
        backup-by-copying t
        backup-directory-alist (list (cons ".*" (expand-file-name
                                                 "backup-files" user-emacs-directory)))
        version-control t
        delete-old-versions t

        ;; File and buffer management
        recentf-max-saved-items 100
        uniquify-buffer-name-style 'forward
        uniquify-separator "/"

        ;; User identity and timestamps
        time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S %:a by %u"
        user-full-name omw/emacs-user-name
        user-mail-address omw/emacs-user-email)

  ;; MacOS key modifiers
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super
          mac-option-modifier 'meta))

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
  (require 'omw-python)
  (require 'omw-javascript)
  (require 'omw-elisp)
  (require 'omw-shell)
  (require 'omw-cmake)
  (require 'omw-yaml)
  (require 'omw-dockerfile)
  ;; Text mode modules
  (require 'omw-markdown)

  ;; Load custom settings
  (when (file-readable-p custom-file)
    (load custom-file nil 'nomessage)))

;; ==================================================================================
;;; Provide features
(provide 'init)

;;; init.el ends here
