;;; init.el --- Emacs configuration entry point -*- lexical-binding:t -*-
;; Time-stamp: <2026-02-27 22:21:24 星期五 by zhengyu.li>

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
;;
;; Module loading order:
;;   1. Core: init-packages, init-funcs, init-base, init-env
;;   2. UI: init-fonts, init-ui
;;   3. Editor: init-completion, init-editing, init-dired, init-projects
;;   4. Tools: init-vc, init-terminal, init-ai, init-auth
;;   5. Languages: init-prog, init-elisp, init-cc, etc.

;;; Code:

;; ==================================================================================
;; Basic check
(unless (version<= "30.2" emacs-version)
  (error "The Emacs version must be >= 30.2 (current: %s)" emacs-version))

;; ==================================================================================
;; Early initialization
;; These settings must be set before any package loading for optimal performance

;; GC tuning - use large threshold during startup for faster initialization
;; gcmh (in init-base.el) will manage GC after startup with reasonable thresholds
(setq gc-cons-threshold (* 100 1024 1024)  ; 100MB during startup
      gc-cons-percentage 0.6)

;; Prevent package.el from loading too early
(setq package-enable-at-startup nil)

;; Enable package-quickstart for faster loading (Emacs 27+)
(when (fboundp 'package-quickstart-refresh)
  (setq package-quickstart t))

;; Native compilation settings (Emacs 29+)
(when (boundp 'native-comp-deferred-compilation)
  (setq native-comp-deferred-compilation t
        native-comp-async-report-warnings-errors 'silent))

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
;; Core modules (foundation - must load first)
(require 'init-packages)
(require 'init-funcs)
(require 'init-base)
(require 'init-env)

;; UI modules
(require 'init-fonts)
(require 'init-ui)

;; Editor modules
(require 'init-completion)
(require 'init-editing)
(require 'init-dired)
(require 'init-projects)

;; Tools modules
(require 'init-vc)
(require 'init-terminal)
(require 'init-ai)
(require 'init-auth)

;; Language modules
(dolist (module '(init-prog init-elisp init-cc init-python init-go
                            init-shell init-dockerfile init-cmake init-yaml init-markdown))
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

;;; init.el ends here
