;;; init-projects.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-17 14:00:00 Monday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: project management
;; Dependencies: (none)

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
;; Project management: projectile, and related integrations.

;;; Code:

;; ==================================================================================
;; Projectile - Project management core
(use-package projectile
  :ensure t
  :defer t
  :bind-keymap
  ("C-c p" . projectile-command-map)            ; Key binding immediate, package deferred
  :custom
  (projectile-completion-system 'default)       ; Use default completion system (vertico)
  (projectile-enable-caching t)                 ; Enable caching for speed
  (projectile-indexing-method 'hybrid)          ; Hybrid indexing method
  (projectile-sort-order 'recentf)              ; Sort by recently visited
  (projectile-auto-discover nil)                ; Disable auto project discovery
  (projectile-globally-ignored-directories
   '(".git" ".svn" ".hg" "node_modules" ".venv" "venv" "__pycache__"
     ".pytest_cache" "dist" "build" "target" ".gradle" ".idea" ".vscode"))
  (projectile-globally-ignored-files
   '(".DS_Store" "*.pyc" "*.elc" "*.o" "*.class" "*.jar" "*.log" "*.lock"))
  (projectile-globally-ignored-file-suffixes
   '(".pyc" ".elc" ".o" ".class" ".jar" ".log" ".lock"))
  :config
  (projectile-mode 1)
  ;; Custom projectile-command-map key bindings
  (define-key projectile-command-map (kbd "f") #'projectile-find-file)
  (define-key projectile-command-map (kbd "b") #'projectile-switch-to-buffer)
  (define-key projectile-command-map (kbd "p") #'projectile-switch-project)
  (define-key projectile-command-map (kbd "s") #'projectile-ag)
  (define-key projectile-command-map (kbd "r") #'projectile-replace)
  (define-key projectile-command-map (kbd "k") #'projectile-kill-buffers))

;; ==================================================================================
;;; Provide features
(provide 'init-projects)

;;; init-projects.el ends here
