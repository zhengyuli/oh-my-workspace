;;; init-fonts.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-08 11:49:50 Sunday by zhengyuli>

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
(defcustom omw/font-monospace
  '("Source Code Pro" "Menlo" "Monaco" "Monospace")
  "Priority list of monospace fonts for code editing.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw/emacs-config)

(defcustom omw/font-variable-pitch
  '("Helvetica Neue" "Arial" "Sans Serif")
  "Priority list of variable-pitch fonts for prose and UI text.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw/emacs-config)

(defcustom omw/font-chinese
  '("PingFang SC" "Hiragino Sans GB" "Songti SC")
  "Priority list of Chinese fonts for CJK character display.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw/emacs-config)

(defcustom omw/font-size-default 150
  "Default font height in 1/10pt units (150 = 15pt)."
  :type 'integer
  :group 'omw/emacs-config)

(defcustom omw/font-size-variable-multiplier 1.0
  "Variable-pitch font size multiplier relative to monospace font."
  :type 'number
  :group 'omw/emacs-config)

;; ==================================================================================
(defun omw/find-available-font (font-list)
  "Return first available font from FONT-LIST."
  (cl-find-if (lambda (font)
                 (and font
                      (x-list-fonts font)))
               font-list))

(defun omw/setup-fonts ()
  "Configure all font faces based on available fonts.
Sets up monospace for code, variable-pitch for prose, and CJK fallback."
  (when (display-graphic-p)
    (let ((mono-font (omw/find-available-font omw/font-monospace))
          (var-font (omw/find-available-font omw/font-variable-pitch))
          (cjk-font (omw/find-available-font omw/font-chinese)))

      ;; Configure default and fixed-pitch faces (for code)
      (when mono-font
        (set-face-attribute 'default nil
                            :family mono-font
                            :height omw/font-size-default)
        (set-face-attribute 'fixed-pitch nil
                            :family mono-font
                            :height omw/font-size-default))

      ;; Configure variable-pitch face (for prose and UI)
      (when var-font
        (set-face-attribute 'variable-pitch nil
                            :family var-font
                            :height (round (* omw/font-size-default
                                              omw/font-size-variable-multiplier))))

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


(use-package emacs
  :ensure nil
  :when (display-graphic-p)
  :demand t
  :hook (after-init . omw/setup-fonts))

;; ==================================================================================
(use-package textsize
  :ensure t
  :when (display-graphic-p)
  :defer t
  :hook (after-init . textsize-mode)
  :config
  (setq textsize-monitor-size-thresholds nil))

;; ==================================================================================
(use-package mixed-pitch
  :ensure t
  :defer t
  :hook (markdown-mode . mixed-pitch-mode))

;; ==================================================================================
(provide 'init-fonts)
;;; init-fonts.el ends here
