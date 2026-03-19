;;; omw-font.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 22:59:48 Wednesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: font, faces
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Font configuration for optimal code editing and CJK support.
;; Features: monospace fonts, Chinese fallback, dynamic text scaling.

;;; Code:

;; ============================================================================
(defcustom omw/font-monospace-list
  '("SauceCodePro Nerd Font Mono" "Menlo" "Monaco" "Monospace")
  "Priority list of monospace fonts for code editing.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw-emacs)

(defcustom omw/font-variable-pitch-list
  '("Helvetica Neue" "Arial" "Sans Serif")
  "Priority list of variable-pitch fonts for prose and UI text.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw-emacs)

(defcustom omw/font-chinese-list
  '("PingFang SC" "Hiragino Sans GB" "Songti SC")
  "Priority list of Chinese fonts for CJK character display.
First available font in the list will be used."
  :type '(repeat string)
  :group 'omw-emacs)

(defcustom omw/font-size-default 160
  "Default font height in 1/10pt units (160 = 16pt)."
  :type 'integer
  :group 'omw-emacs)

(defcustom omw/font-size-variable-multiplier 1.0
  "Variable-pitch font size multiplier relative to monospace font."
  :type 'number
  :group 'omw-emacs)

;; ============================================================================
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
    (let ((mono-font (omw/find-available-font omw/font-monospace-list))
          (var-font (omw/find-available-font omw/font-variable-pitch-list))
          (cjk-font (omw/find-available-font omw/font-chinese-list)))

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

;; ============================================================================
(use-package textsize
  :ensure t
  :when (display-graphic-p)
  :defer t
  :hook ((after-init . textsize-mode)
         (move-frame-functions . textsize-fix-frame))
  :config
  ;; An offset will be selected from the monitor's pixel pitch in mm
  ;; pitch < 0.12: high-end Retina display
  ;; pitch < 0.18: MacBook Retina baseline
  ;; pitch < 0.25: 2K 27-inch monitor
  ;; pitch >= 0.25: 1080p large screen
  (setq textsize-monitor-size-thresholds nil
        textsize-pixel-pitch-thresholds '((0 . 0) (0.12 . 0) (0.18 . 0) (0.25 . 0))))

;; ============================================================================
(use-package mixed-pitch
  :ensure t
  :when (display-graphic-p)
  :defer t
  :hook (markdown-mode . mixed-pitch-mode))

;; ============================================================================
;;; Provide features
(provide 'omw-font)

;;; omw-font.el ends here
