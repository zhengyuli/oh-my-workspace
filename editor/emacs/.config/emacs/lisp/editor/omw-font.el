;;; omw-font.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 22:59:48 Wednesday by zhengyu.li>
;;
;; ============================================================================
;; omw-font.el - Font configuration with CJK support.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: font, faces
;; Dependencies: (none)
;;
;; Copyright (C) 2026 zhengyu li
;;
;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.
;;
;;; Commentary:
;;
;; Font configuration for optimal code editing and CJK support.
;; Features: monospace fonts, Chinese fallback, dynamic text scaling.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Font Configuration
;; ----------------------------------------------------------------------------

(defvar omw/font-monospace-list
  '("SauceCodePro Nerd Font Mono" "Menlo" "Monaco" "Monospace")
  "Priority list of monospace fonts for code editing.
The first font found installed on the system will be used.
Edit this list in source (or set before loading) to prefer a different font.")

(defvar omw/font-variable-pitch-list
  '("Helvetica Neue" "Arial" "Sans Serif")
  "Priority list of variable-pitch fonts for prose and UI text.
The first font found installed on the system will be used.")

(defvar omw/font-chinese-list
  '("PingFang SC" "Hiragino Sans GB" "Songti SC")
  "Priority list of Chinese fonts for CJK character display.
The first font found installed on the system will be used.")

(defvar omw/font-size-default 160
  "Default font height in 1/10pt units (160 = 16pt).
Used as the base size before textsize applies per-monitor DPI adjustment.")

(defvar omw/font-size-variable-multiplier 1.0
  "Scale factor for variable-pitch font height relative to the monospace size.
1.0 means identical height; increase for visually larger prose text.")

;; --- CJK Code-Point Ranges ---
;; Unicode code-point ranges used when mapping the CJK fallback font.
;; Defined as constants so set-fontset-font call-sites are self-documenting
;; and the ranges can be referenced without repeating raw hex literals.
(defconst omw/font-cjk-unified-range '(#x4e00 . #x9fff)
  "CJK Unified Ideographs: the primary block of common Chinese, Japanese,
and Korean characters used in everyday text.")

(defconst omw/font-cjk-ext-a-range '(#x3400 . #x4dbf)
  "CJK Unified Ideographs Extension A: rare or historic CJK characters
not covered by the main Unified block.")

(defconst omw/font-cjk-ext-b-range '(#x20000 . #x2a6df)
  "CJK Unified Ideographs Extension B and beyond: very rare or archaic
characters, including those used in classical literature.")

(defconst omw/font-cjk-symbols-range '(#x3000 . #x303f)
  "CJK Symbols and Punctuation: ideographic space, corner brackets,
wave dashes, and other CJK-specific punctuation marks.")

;; --- Font Setup ---
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

      ;; fixed-pitch must be set separately; mixed-pitch inherits from it
      (when mono-font
        (set-face-attribute 'default nil
                            :family mono-font
                            :height omw/font-size-default)
        (set-face-attribute 'fixed-pitch nil
                            :family mono-font
                            :height omw/font-size-default))

      ;; Separate height lets prose scale independently from code
      (when var-font
        (set-face-attribute 'variable-pitch nil
                            :family var-font
                            :height (round
                                     (* omw/font-size-default
                                        omw/font-size-variable-multiplier))))

      ;; Four ranges cover everyday, rare, archaic CJK and punctuation
      (when cjk-font
        (set-fontset-font t omw/font-cjk-unified-range cjk-font nil 'prepend)
        (set-fontset-font t omw/font-cjk-ext-a-range   cjk-font nil 'prepend)
        (set-fontset-font t omw/font-cjk-ext-b-range   cjk-font nil 'prepend)
        (set-fontset-font t omw/font-cjk-symbols-range cjk-font nil 'prepend))

      ;; Helps diagnose "wrong font" reports when switching monitors
      (when init-file-debug
        (message "Fonts configured: mono=%s, var=%s, cjk=%s"
                 (or mono-font "N/A")
                 (or var-font "N/A")
                 (or cjk-font "N/A"))))))

;; Deferred to after-init so that frames are already created;
;; setting fonts before the first frame renders can cause flicker.
(use-package emacs
  :ensure nil
  :demand t
  :when (display-graphic-p)
  :hook (after-init . omw/setup-fonts))

;; --- Textsize ---
(use-package textsize
  :ensure t
  :defer t
  :when (display-graphic-p)
  :hook ((after-init . textsize-mode)
         (move-frame-functions . textsize-fix-frame))
  :config
  ;; An offset will be selected from the monitor's pixel pitch in mm
  ;; pitch < 0.12: high-end Retina display
  ;; pitch < 0.18: MacBook Retina baseline
  ;; pitch < 0.25: 2K 27-inch monitor
  ;; pitch >= 0.25: 1080p large screen
  (setq textsize-monitor-size-thresholds nil
        textsize-pixel-pitch-thresholds
        '((0 . 0) (0.12 . 0) (0.18 . 0) (0.25 . 0))))

;; --- Mixed Pitch ---
(use-package mixed-pitch
  :ensure t
  :defer t
  :when (display-graphic-p)
  :hook (markdown-mode . mixed-pitch-mode))

;; ============================================================================
;;; Provide features
(provide 'omw-font)

;;; omw-font.el ends here
