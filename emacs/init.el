;;; init.el --- Emacs configuration entry point -*- lexical-binding:t -*-
;; Time-stamp: <2026-02-19 18:13:36 Thursday by zhengyuli>

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
;; This file bootstraps use-package and loads all configuration modules.

;;; Code:

;; ==================================================================================
;; Basic check
(unless (version<= "30.2" emacs-version)
  (error "The Emacs version must be >= 30.2 (current: %s)" emacs-version))

;; ==================================================================================
;; Early initialization
;; GC tuning - increase threshold at startup for faster initialization
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent package.el from loading too early
(setq package-enable-at-startup nil)

;; Enable package-quickstart for faster loading (Emacs 27+)
(when (fboundp 'package-quickstart-refresh)
  (setq package-quickstart t))

;; Prevent UI elements from briefly showing (avoid startup flicker)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Native compilation settings (Emacs 29+)
(when (boundp 'native-comp-deferred-compilation)
  (setq native-comp-deferred-compilation t
        native-comp-async-report-warnings-errors 'silent))

;; Use y/n instead of yes/no (Emacs 28+)
(when (boundp 'use-short-answers)
  (setq use-short-answers t))

;; ==================================================================================
;; Global variables
;; Emacs configuration root path (dynamically resolved, supports symlinks)
;; Use file-chase-links to resolve symlinks and get the real file path
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

;; ==================================================================================
;; Helper function for load-path
(defun emacs-add-subdirs-to-load-path (base-dir)
  "Add subdirs to load path.
Look up all subdirs under `BASE-DIR' recursively and add them into load path."
  (let ((default-directory base-dir))
    (add-to-list 'load-path base-dir)
    (normal-top-level-add-subdirs-to-load-path)))

;; ==================================================================================
;; Add custom directories to load-path first
(emacs-add-subdirs-to-load-path emacs-config-lisp-path)
(emacs-add-subdirs-to-load-path emacs-config-packages-path)

;; ==================================================================================
;; Package manager setup (optimized version)
(require 'package)

;; Add package archives (including NonGNU ELPA)
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
;; Package version locking (Emacs 30+)
;; Lock package versions to prevent incompatibility from MELPA rolling updates
;; Lock file located at ~/.emacs.d/package-lock.eld
(when (boundp 'package-lock-file)
  (setq package-lock-file
        (expand-file-name "package-lock.eld" user-emacs-directory))
  ;; Enable package locking for reproducible package versions
  (setq package-lock-locked-package-versions t))

;; Refresh package list asynchronously (only when needed)
;; Install and configure use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-compute-statistics nil   ; Disable for production (re-enable for debugging)
      use-package-verbose nil              ; Disable for production
      use-package-minimum-reported-time 0.5)

;; ==================================================================================
;; GC optimization with gcmh
(use-package gcmh
  :demand t
  :custom
  (gcmh-idle-delay 10)                   ; GC after 10 seconds idle
  (gcmh-high-cons-threshold #x10000000)  ; 256MB
  :config
  (gcmh-mode 1))

;; ==================================================================================
;; Load configuration modules
;; Helper function for safe module loading
(defun require-safe (module)
  "Require MODULE with error protection.
Logs error but continues if module fails to load."
  (condition-case err
      (require module)
    (error
     (message "[Config] Warning: Failed to load %s: %s" module (error-message-string err)))))

;; Load utility functions
(require-safe 'init-functions)

;; Load font configuration (before UI themes)
(require-safe 'init-fonts)

;; Core modules
(dolist (module '(init-ui init-editing init-completion init-projects
                   init-dired init-vc init-terminal init-utilities))
  (require-safe module))

;; Language modules
(dolist (module '(init-prog-base init-text init-elisp init-cc
                   init-python init-go init-shell init-dockerfile
                   init-cmake init-yaml init-markdown))
  (require-safe module))

;; Utility modules
(require-safe 'init-ai)

;; ==================================================================================
;; Startup completion hook
;; GC settings managed automatically by gcmh-mode, no manual restore needed
;; Package update checks handled by package-check-updates in init-utilities.el
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time
                      (time-subtract after-init-time before-init-time))
                     gcs-done)))

;; ==================================================================================
;; Load user custom settings
(let ((custom-file (locate-user-emacs-file "custom_settings.el")))
  (unless (file-exists-p custom-file)
    (with-temp-buffer (write-file custom-file)))
  (load custom-file :noerror :nomessage))

;; Enable HTTP proxy after custom settings are loaded
(enable-http-proxy)

;;; init.el ends here
