;;; init-dashboard.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 14:00:00 Thursday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: init-funcs, init-theme, init-tabs

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
;; Dashboard configuration.

;;; Code:

(require 'init-funcs)
(require 'init-theme)
(require 'init-tabs)

;; ==================================================================================
;; Dashboard
(use-package dashboard
  :defer t
  :custom
  (dashboard-center-content t)
  (dashboard-banner-logo-title (format "Welcome to %s's Emacs" emacs-user-name))
  (dashboard-set-heading-icons (display-graphic-p))
  (dashboard-set-file-icons (display-graphic-p))
  (dashboard-set-navigator t)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5)))
  (dashboard-projects-switch-function 'projectile-switch-project)
  :config
  (require 'dashboard-widgets)
  ;; Set random banner before dashboard initializes
  (add-hook 'dashboard-before-initialize-hook
            (lambda ()
              (setq dashboard-startup-banner
                    (or (omw--get-random-banner) 'official))))
  ;; Dashboard mode hook
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (centaur-tabs-local-mode 1)))
  ;; Set dashboard as initial buffer
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  ;; Setup dashboard hook
  (dashboard-setup-startup-hook))

;; Activation via after-init hook (outside use-package block)
;; NOTE: Hook code must be outside :config because deferred packages
;; only execute :config when actually loaded.
(add-hook 'after-init-hook
          (lambda ()
            (require 'dashboard)
            (dashboard-open)))

;; ==================================================================================
;;; Provide features
(provide 'init-dashboard)

;;; init-dashboard.el ends here
