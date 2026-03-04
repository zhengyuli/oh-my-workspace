;;; init.el --- Emacs configuration entry point -*- lexical-binding:t -*-
;; Time-stamp: <2026-03-04 23:51:32 Wednesday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: init-completion

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
(defgroup omw-emacs-config nil
  "Oh My Workspace Emacs configuration customization group."
  :group 'emacs
  :prefix "emacs-")

;; ==================================================================================
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
(defvar omw/emacs-config-root
  (let ((config-file (or load-file-name buffer-file-name)))
    (if config-file
        (file-name-directory (file-chase-links config-file))
      default-directory))
  "Emacs configuration root path.
Automatically resolves symlinks to find the actual configuration directory.")

(defvar omw/emacs-config-lisp-path (expand-file-name "lisp/" omw/emacs-config-root)
  "Emacs configuration custom settings path.")

(defvar omw/emacs-config-packages-path (expand-file-name "site-packages/" omw/emacs-config-root)
  "Emacs configuration custom site packages path.")

(defun omw/emacs-add-subdirs-to-load-path (base-dir)
  "Add subdirs to load path.
Look up all subdirs under `BASE-DIR' recursively and add them into load path."
  (let ((default-directory base-dir))
    (add-to-list 'load-path base-dir)
    (normal-top-level-add-subdirs-to-load-path)))

(omw/emacs-add-subdirs-to-load-path omw/emacs-config-lisp-path)
(omw/emacs-add-subdirs-to-load-path omw/emacs-config-packages-path)

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
                  menu-bar-mode))
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
  :defer t)

;; ==================================================================================
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
  (setq auto-package-update-delete-old-versions t))

;; ==================================================================================
(use-package which-key
  :ensure t
  :defer t
  :config
  (which-key-setup-minibuffer))

;; ==================================================================================
(defvar omw/emacs-custom-config-path
  (expand-file-name "custom.el" user-emacs-directory)
  "Emacs cusom configuration file path.")

(defun omw/after-init-setup ()
  ;;  global-auto-revert-mode: Auto-refresh buffers when files change on disk
  (global-auto-revert-mode 1)
  ;; save-place-mode: Remember cursor position in files
  (save-place-mode 1)
  ;; recentf-mode: Track recently opened files
  (recentf-mode 1)
  ;; column-number-mode: Show column number in mode line
  (column-number-mode 1)
  ;; jit-lock-mode: Enable JIT font locking for performance
  (jit-lock-mode 1))

(defun omw/emacs-startup-setup ()
  "Display startup completion message with timing information.
Shows elapsed time and number of garbage collections during init."
  (message "Emacs ready in %.2f seconds with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(use-package emacs
  :ensure nil
  :demand t
  :hook ((after-init . gcmh-mode)
         (after-init . which-key-mode)
         (after-init . omw/after-init-setup)
         (after-init . omw/emacs-startup-setup))
  :bind* ("C-x C-b" . ibuffer)
  :config
  (setq inhibit-default-init t
        inhibit-startup-echo-area-message t
        inhibit-startup-screen t

        ;; UI and interaction
        use-short-answers t
        ring-bell-function 'ignore

        ;; Backup and version control
        backup-by-copying t
        backup-directory-alist '((".*" . "~/.emacs.d/backup-files"))
        version-control t
        delete-old-versions t

        ;; File and buffer management
        recentf-max-saved-items 100
        uniquify-buffer-name-style 'forward
        uniquify-separator "/"

        ;; User identity and timestamps
        time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S %:a by %u"
        user-full-name emacs-user-name
        user-mail-address emacs-user-email)

  ;; Platform-specific: macOS key modifiers
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super   ; Map Command key to Super
          mac-option-modifier 'meta))   ; Map Option to Meta

  ;; Load all other modules
  ;; Core modules
  (require 'init-editing)
  (require 'init-completion)
  (require 'init-auth)
  (require 'init-proxy)
  (require 'init-fonts)
  (require 'init-ui)
  ;; Tool modules
  (require 'init-dired)
  (require 'init-magit)
  (require 'init-terminal)
  (require 'init-agent)
  ;; Language modules
  (require 'init-prog)
  (require 'init-elisp)
  (require 'init-cc)
  (require 'init-python)
  (require 'init-go)
  (require 'init-typescript)
  (require 'init-dockerfile)
  (require 'init-cmake)
  (require 'init-yaml)
  (require 'init-markdown)

  ;; File used for storing customization information.
  (setq custom-file omw/emacs-custom-config-path)
  ;; Load custom settings if the file exists
  (when (file-readable-p omw/emacs-custom-config-path)
    (load omw/emacs-custom-config-path nil 'nomessage)))

;; ==================================================================================
;;; init.el ends here
