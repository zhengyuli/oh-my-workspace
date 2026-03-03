;;; init-fonts.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 15:19:53 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: fonts, faces
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
;; Font configuration for optimal code editing and CJK support.
;; Features: monospace fonts, Chinese fallback, dynamic text scaling.

;;; Code:

;; ==================================================================================
;; Font family definitions (customizable priority lists)
(defcustom emacs-font-monospace
  '(".SF NS Mono" "Source Code Pro" "Menlo" "Monospace")
  "Priority list of monospace fonts for code editing.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw-emacs-config)

(defcustom emacs-font-variable-pitch
  '(".SF NS" "Helvetica Neue" "Arial" "Sans Serif")
  "Priority list of variable-pitch fonts for prose and UI text.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw-emacs-config)

(defcustom emacs-font-chinese
  '("Hiragino Sans GB" "Heiti SC" "Songti SC")
  "Priority list of Chinese fonts for CJK character display.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw-emacs-config)

(defcustom emacs-font-size-default 140
  "Default font height in 1/10pt units (140 = 14pt)."
  :type 'integer
  :group 'omw-emacs-config)

(defcustom emacs-font-size-variable-multiplier 1.0
  "Variable-pitch font size multiplier relative to monospace font."
  :type 'number
  :group 'omw-emacs-config)

;; ==================================================================================
;; Font setup functions - configure fonts based on availability
(defun emacs-find-available-font (font-list)
  "Return first available font from FONT-LIST."
  (cl-find-if (lambda (font)
                 (and font
                      (x-list-fonts font)))
               font-list))

(defun emacs-setup-fonts ()
  "Configure all font faces based on available fonts.
Sets up monospace for code, variable-pitch for prose, and CJK fallback."
  (when (display-graphic-p)
    (let ((mono-font (emacs-find-available-font emacs-font-monospace))
          (var-font (emacs-find-available-font emacs-font-variable-pitch))
          (cjk-font (emacs-find-available-font emacs-font-chinese)))

      ;; Configure default and fixed-pitch faces (for code)
      (when mono-font
        (set-face-attribute 'default nil
                            :family mono-font
                            :height emacs-font-size-default)
        (set-face-attribute 'fixed-pitch nil
                            :family mono-font
                            :height emacs-font-size-default))

      ;; Configure variable-pitch face (for prose and UI)
      (when var-font
        (set-face-attribute 'variable-pitch nil
                            :family var-font
                            :height (round (* emacs-font-size-default
                                              emacs-font-size-variable-multiplier))))

      ;; Configure Chinese font fallback for CJK character ranges
      (when cjk-font
        ;; CJK Unified Ideographs (common Chinese characters)
        (set-fontset-font t '(#x4e00 . #x9fff) cjk-font nil 'prepend)
        ;; CJK Extension A
        (set-fontset-font t '(#x3400 . #x4dbf) cjk-font nil 'prepend)
        ;; CJK Extension B and beyond
        (set-fontset-font t '(#x20000 . #x2a6df) cjk-font nil 'prepend)
        ;; CJK Symbols and Punctuation
        (set-fontset-font t '(#x3000 . #x303f) cjk-font nil 'prepend))

      ;; Log font configuration for debugging
      (message "Fonts configured: mono=%s, var=%s, cjk=%s"
               (or mono-font "N/A")
               (or var-font "N/A")
               (or cjk-font "N/A")))))

;; Auto-configure fonts after Emacs initialization (GUI only)
(when (display-graphic-p)
  (add-hook 'after-init-hook #'emacs-setup-fonts))

;; ==================================================================================
;; Textsize - automatic font scaling based on screen resolution
;; Adjusts font size for different display densities (Retina vs standard)
(use-package textsize
  :ensure t
  :when (display-graphic-p)
  :defer t
  :hook (after-init . textsize-mode)
  :config
  (setq textsize-pixel-pitch-thresholds nil    ; Disable automatic thresholds
        textsize-monitor-size-thresholds nil)  ; Use manual configuration
  (set-face-attribute 'default nil :height 140))

;; ==================================================================================
(provide 'init-fonts)
;;; init-fonts.el ends here
