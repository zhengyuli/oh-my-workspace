;;; init-packages.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-28 17:01:03 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: packages, package-manager
;; Dependencies: (none - must load before other modules)

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
;; Package manager configuration: archives, priorities, version locking,
;; and use-package setup.

;;; Code:

;; ==================================================================================
;; Package archives
(require 'package)

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

;; ==================================================================================
;; use-package setup
;; Note: use-package is critical infrastructure - ensure it's installed via setup.sh
;; or manually run M-x package-refresh-contents then M-x package-install RET use-package
(unless (package-installed-p 'use-package)
  ;; Try to install from cache first (avoids blocking refresh if package is cached)
  (condition-case err
      (progn
        (package-install 'use-package)
        (message "[Config] use-package installed from cache."))
    (error
     ;; Cache miss - provide guidance instead of blocking startup
     (message "[Config] Warning: use-package not in cache.")
     (message "[Config] Please run: M-x package-refresh-contents RET, then restart Emacs")
     (message "[Config] Continuing with reduced functionality..."))))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-compute-statistics nil   ; Disable for production (re-enable for debugging)
      use-package-verbose nil              ; Disable for production
      use-package-minimum-reported-time 0.5)

;; ==================================================================================
;; Auto package update
(use-package auto-package-update
  :defer t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-output t))

;; Check for package updates in background
(defun package-check-updates ()
  "Check for package updates in background and notify if updates available."
  (interactive)
  (message "Checking for package updates...")
  (package-refresh-contents)
  (let ((upgrades (package-menu--find-upgrades)))
    (if upgrades
        (let ((count (length upgrades)))
          (message "")
          (if (yes-or-no-p (format "Found %d package(s) with updates. Upgrade now? " count))
              (auto-package-upgrade-all)
            (message "Run `M-x auto-package-upgrade-all' to upgrade later.")))
      (message "All packages are up to date."))))

;; Check for updates after startup (after 60 seconds idle)
(run-config-timer 60 nil #'package-check-updates)

;; Package upgrade function
(defun auto-package-upgrade-all ()
  "Upgrade all packages installed."
  (interactive)
  (require 'auto-package-update)
  (package-refresh-contents)
  (auto-package-update-now))

;; Convenience aliases
(defalias 'package-upgrade-all 'auto-package-upgrade-all
  "Upgrade all packages interactively.")
(defalias 'upgrade-packages 'auto-package-upgrade-all)

;; ==================================================================================
;;; Provide features
(provide 'init-packages)

;;; init-packages.el ends here
