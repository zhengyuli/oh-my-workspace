;;; init-fonts.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-28 21:12:16 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: fonts, faces
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
;; Font configuration: default fonts, Chinese fallback, textsize.

;;; Code:

(require 'init-funcs)

;; ==================================================================================
;; Font family definitions (customizable)
(defcustom emacs-font-monospace
  '(".SF NS Mono" "Source Code Pro" "Menlo" "Monospace")
  "Priority list of monospace fonts for code.
First available font will be used."
  :type '(repeat string)
  :group 'omw-emacs-config)

(defcustom emacs-font-variable-pitch
  '(".SF NS" "Helvetica Neue" "Arial" "Sans Serif")
  "Priority list of variable-pitch fonts for prose.
First available font will be used."
  :type '(repeat string)
  :group 'omw-emacs-config)

(defcustom emacs-font-chinese
  '("Hiragino Sans GB" "Heiti SC" "Songti SC")
  "Priority list of Chinese fonts for CJK characters.
First available font will be used."
  :type '(repeat string)
  :group 'omw-emacs-config)

(defcustom emacs-font-size-default 140
  "Default font height (1/10pt). 140 = 14pt."
  :type 'integer
  :group 'omw-emacs-config)

(defcustom emacs-font-size-variable-multiplier 1.0
  "Variable-pitch font size multiplier relative to default."
  :type 'number
  :group 'omw-emacs-config)

;; ==================================================================================
;; Font helper functions
(defun emacs-find-available-font (font-list)
  "Return first available font from FONT-LIST."
  (cl-find-if (lambda (font)
                 (and font
                      (x-list-fonts font)))
               font-list))

(defun emacs-setup-fonts ()
  "Setup all font faces based on available fonts."
  (when (display-graphic-p)
    (let ((mono-font (emacs-find-available-font emacs-font-monospace))
          (var-font (emacs-find-available-font emacs-font-variable-pitch))
          (cjk-font (emacs-find-available-font emacs-font-chinese)))

      ;; Set default face (monospace for code)
      (when mono-font
        (set-face-attribute 'default nil
                            :family mono-font
                            :height emacs-font-size-default)
        (set-face-attribute 'fixed-pitch nil
                            :family mono-font
                            :height emacs-font-size-default))

      ;; Set variable-pitch face
      (when var-font
        (set-face-attribute 'variable-pitch nil
                            :family var-font
                            :height (round (* emacs-font-size-default
                                              emacs-font-size-variable-multiplier))))

      ;; Setup Chinese font fallback via fontset
      (when cjk-font
        ;; CJK Unified Ideographs (common Chinese characters)
        (set-fontset-font t '(#x4e00 . #x9fff) cjk-font nil 'prepend)
        ;; CJK Extension A
        (set-fontset-font t '(#x3400 . #x4dbf) cjk-font nil 'prepend)
        ;; CJK Extension B and beyond
        (set-fontset-font t '(#x20000 . #x2a6df) cjk-font nil 'prepend)
        ;; CJK Symbols and Punctuation
        (set-fontset-font t '(#x3000 . #x303f) cjk-font nil 'prepend))

      ;; Log font setup
      (message "Fonts configured: mono=%s, var=%s, cjk=%s"
               (or mono-font "N/A")
               (or var-font "N/A")
               (or cjk-font "N/A")))))

;; ==================================================================================
;; Initialize fonts (GUI only, deferred for faster startup)
(defvar emacs-fonts-initialized nil
  "Flag to track if fonts have been initialized.")

(defun emacs-setup-fonts-deferred ()
  "Setup fonts after a short idle delay for faster startup."
  (when (and (display-graphic-p)
             (not emacs-fonts-initialized))
    (setq emacs-fonts-initialized t)
    (emacs-setup-fonts)
    ;; Re-apply fonts after theme load (theme may override)
    (with-eval-after-load 'doom-themes
      (add-hook 'doom-load-theme-hook #'emacs-setup-fonts))))

(add-hook 'window-setup-hook
          (lambda ()
            (run-config-timer 0.5 nil #'emacs-setup-fonts-deferred)))

;; ==================================================================================
;; Textsize - automatic font sizing based on screen resolution (GUI only)
(use-package textsize
  :when (display-graphic-p)
  :defer t
  :hook (after-init . textsize-mode)
  :config
  (setq textsize-monitor-size-thresholds
        '((0 . -3) (350 . -1) (500 . 0))
        textsize-pixel-pitch-thresholds
        '((0 . 5) (0.12 . 3) (0.18 . 1) (0.20 . 0) (0.25 . -2))))

;; ==================================================================================
;; Utility functions
(defun emacs-reload-fonts ()
  "Reload font configuration."
  (interactive)
  (emacs-setup-fonts)
  (message "Fonts reloaded"))

(defun emacs-describe-fonts ()
  "Display current font configuration."
  (interactive)
  (message "Current fonts:
  Default:     %s (height: %s)
  Fixed-pitch: %s
  Variable:    %s"
           (face-attribute 'default :family)
           (face-attribute 'default :height)
           (face-attribute 'fixed-pitch :family)
           (face-attribute 'variable-pitch :family)))

;; ==================================================================================
(provide 'init-fonts)
;;; init-fonts.el ends here
