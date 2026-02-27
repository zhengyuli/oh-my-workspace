;;; init-theme.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 14:00:00 Thursday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: init-funcs

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
;; Theme configuration: random banner, fullscreen, doom-themes,
;; doom-modeline, smooth scrolling.

;;; Code:

(require 'init-funcs)

;; ==================================================================================
;; Random Banner Selection
(defun omw--get-random-banner ()
  "Return a random banner path from banners directory.
Returns nil in terminal mode (uses official banner instead)."
  (when (display-graphic-p)
    (let* ((banners-dir (concat emacs-config-root "/banners"))
           (banners (directory-files banners-dir t "\\.png\\'")))
      (when banners
        (nth (random (length banners)) banners)))))

;; ==================================================================================
;; GUI Frame Settings - fullscreen, size, etc.
(when (display-graphic-p)
  ;; Auto fullscreen on startup (using custom toggle-fullscreen)
  (add-hook 'emacs-startup-hook #'toggle-fullscreen))

;; ==================================================================================
;; Theme - doom-themes
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-xcode t))

;; ==================================================================================
;; Modeline - doom-modeline
(use-package doom-modeline
  :defer t
  :custom
  (doom-modeline-icon (display-graphic-p))  ; Disable icons in terminal
  :hook (after-init . doom-modeline-mode))

;; ==================================================================================
;; Smooth scrolling - use built-in pixel-scroll-precision-mode (Emacs 29+)
(when (>= emacs-major-version 29)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (display-graphic-p)
                (pixel-scroll-precision-mode 1)))))

;; ==================================================================================
;;; Provide features
(provide 'init-theme)

;;; init-theme.el ends here
