;;; init-fonts.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-05 15:06:53 Thursday by zhengyu.li>

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
(defcustom omw/emacs-font-monospace
  '(".SF NS Mono" "Source Code Pro" "Menlo" "Monospace")
  "Priority list of monospace fonts for code editing.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw/emacs-config)

(defcustom omw/emacs-font-variable-pitch
  '(".SF NS" "Helvetica Neue" "Arial" "Sans Serif")
  "Priority list of variable-pitch fonts for prose and UI text.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw/emacs-config)

(defcustom omw/emacs-font-chinese
  '("Hiragino Sans GB" "Heiti SC" "Songti SC")
  "Priority list of Chinese fonts for CJK character display.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw/emacs-config)

(defcustom omw/emacs-font-size-default 140
  "Default font height in 1/10pt units (140 = 14pt)."
  :type 'integer
  :group 'omw/emacs-config)

(defcustom omw/emacs-font-size-variable-multiplier 1.0
  "Variable-pitch font size multiplier relative to monospace font."
  :type 'number
  :group 'omw/emacs-config)

;; ==================================================================================
(defun omw/emacs-find-available-font (font-list)
  "Return first available font from FONT-LIST."
  (cl-find-if (lambda (font)
                 (and font
                      (x-list-fonts font)))
               font-list))

(defun omw/emacs-setup-fonts ()
  "Configure all font faces based on available fonts.
Sets up monospace for code, variable-pitch for prose, and CJK fallback."
  (when (display-graphic-p)
    (let ((mono-font (omw/emacs-find-available-font omw/emacs-font-monospace))
          (var-font (omw/emacs-find-available-font omw/emacs-font-variable-pitch))
          (cjk-font (omw/emacs-find-available-font omw/emacs-font-chinese)))

      ;; Configure default and fixed-pitch faces (for code)
      (when mono-font
        (set-face-attribute 'default nil
                            :family mono-font
                            :height omw/emacs-font-size-default)
        (set-face-attribute 'fixed-pitch nil
                            :family mono-font
                            :height omw/emacs-font-size-default))

      ;; Configure variable-pitch face (for prose and UI)
      (when var-font
        (set-face-attribute 'variable-pitch nil
                            :family var-font
                            :height (round (* omw/emacs-font-size-default
                                              omw/emacs-font-size-variable-multiplier))))

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

;; ==================================================================================
(use-package textsize
  :ensure t
  :defer t
  :config
  (setq textsize-pixel-pitch-thresholds nil
        textsize-monitor-size-thresholds nil)
  (set-face-attribute 'default nil :height 140))

;; ==================================================================================
(use-package emacs
  :ensure nil
  :when (display-graphic-p)
  :defer t
  :hook ((after-init . omw/emacs-setup-fonts)
         (after-init . textsize-mode)))

;; ==================================================================================
(provide 'init-fonts)
;;; init-fonts.el ends here
