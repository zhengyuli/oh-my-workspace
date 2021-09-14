;;; package --- init-theme.el ---
;; Time-stamp: <2021-09-10 06:00:04 Friday by lizhengyu>

;; Copyright (C) 2021 zhengyu li
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: none

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

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'init-theme)

;;; Require:
(require 'cursor-chg)
(require 'smooth-scrolling)
(require 'centaur-tabs)
(require 'powerline)

;;; Code:
;; ==================================================================================

(defun centaur-tabs-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
	((memq major-mode '(magit-process-mode
						magit-status-mode
						magit-diff-mode
						magit-log-mode
						magit-file-mode
						magit-blob-mode
						magit-blame-mode))
	 "Magit"
	 )
	((memq major-mode
		   '(help-mode
			 apropos-mode
			 Info-mode
			 Man-mode))
	 "Help"
	 )
	((memq major-mode
		   '(rmail-mode
			 rmail-edit-mode
			 vm-summary-mode
			 vm-mode
			 mail-mode
			 mh-letter-mode
			 mh-show-mode
			 mh-folder-mode
			 gnus-summary-mode
			 message-mode
			 gnus-group-mode
			 gnus-article-mode
			 score-mode
			 gnus-browse-killed-mode))
	 "Mail"
	 )
	((memq major-mode '(org-mode
						org-agenda-mode
						diary-mode))
	 "OrgMode"
	 )
	((derived-mode-p 'dired-mode)
	 "Dired"
	 )
	((derived-mode-p 'term-mode)
	 "Terminal"
	 )
	((derived-mode-p 'eshell-mode)
	 "EShell"
	 )
	((derived-mode-p 'w3m-mode)
	 "W3m"
	 )
	((and (not (string-equal "*scratch*" (buffer-name)))
		  (derived-mode-p 'emacs-lisp-mode))
	 "Elisp"
	 )
	((string-equal "*" (substring (buffer-name) 0 1))
	 "Emacs"
	 )
	(t
	 (centaur-tabs-get-group-name (current-buffer))
	 )
	)))

;; ==================================================================================
;; Enable `custom-theme-set-faces' act immediately
(customize-set-variable 'custom--inhibit-theme-enable nil)

;; Define a new custom theme `local-custom-theme'
(deftheme local-custom-theme "Lcoal custom theme.")

;; Customize `local-custom-theme' related faces
(custom-theme-set-faces
 'local-custom-theme
 ;;; Basic faces --------------------------------------------------------------------
 ;; Basic default face.
 `(default ((t :background ,emacs-config-default-background
			   :foreground ,emacs-config-default-foreground
			   :family ,emacs-config-default-fixed-font
			   :height 160
			   :width normal
			   :weight normal)))

 ;; Basic bold face
 '(bold ((t :weight bold)))

 ;; Basic italic face.
 '(italic ((t :slant italic)))

 ;; Basic bold italic face
 '(bold-italic ((t :weight bold :slant italic)))

 ;; Basic underlined face
 '(underline ((t :underline t)))

 ;; The basic fixed pitch face
 '(fixed-pitch ((t :family ,emacs-config-default-fixed-font)))

 ;; The basic fixed pitch face with serifs
 '(fixed-pitch-serif ((t :family ,emacs-config-default-fixed-font)))

 ;; The basic variable-pitch face.
 '(variable-pitch ((t :family ,emacs-config-default-variable-font)))

 ;; Basic face for shadowed text
 '(shadow ((t :foreground "#B3B3B3")))

 ;; Basic face for unvisited links
 '(link ((t :foreground "#00FFFF" :underline t)))

 ;; Basic face for visited links
 '(link-visited ((t :foreground "#EE82EE" :underline t)))

 ;; Basic face for highlighting
 '(highlight ((t :background "#556B2F")))

 ;; Basic face for highlighting the region
 '(region ((t :background "#004855" :distant-foreground "#FFFFFF")))

 ;; Basic face for displaying the secondary selection
 '(secondary-selection ((t :background "#4A708B")))

 ;; Basic face for highlighting trailing whitespace
 '(trailing-whitespace ((t :background "#FF0000")))

 ;; Face for characters displayed as sequences using `^' or `\\'
 '(escape-glyph ((t :foreground "#00FFFF")))

 ;; Face for displaying nobreak space
 '(nobreak-space ((t :inverse-video t)))

 ;; Basic mode line face for selected window
 '(mode-line ((t :height 1.1 :background "#FF0000" :foreground "#FFFFFF")))

 ;; Basic mode line face for non-selected windows
 '(mode-line-inactive ((t :height 1.1 :background "gray" :foreground "black")))

 ;; Basic mode line face for highlighting
 '(mode-line-highlight ((t :background "#FF0000" :foreground "#FFFFFF")))

 ;; Face used to emphasize certain mode line features
 '(mode-line-emphasis ((t :weight bold)))

 ;; Face used for buffer identification parts of the mode line
 '(mode-line-buffer-id ((t :weight bold :slant italic)))

 ;; Basic header-line face
 '(header-line ((t :background "#333333" :foreground "#E5E5E5")))

 ;; Face used for vertical window dividers on ttys
 '(vertical-border ((t nil)))

 ;; Basic face for window dividers
 '(window-divider ((t :foreground "#999999")))

 ;; window-divider-first-pixel
 '(window-divider-first-pixel ((t :foreground "#CCCCCC")))

 ;; Basic face for last pixel line/column of window dividers
 '(window-divider-last-pixel ((t :foreground "#666666")))

 ;; Face for minibuffer prompts
 '(minibuffer-prompt ((t :foreground "#63B8FF")))

 ;; Basic face for the fringes to the left and right of windows under X
 `(fringe ((t :background ,emacs-config-default-background)))

 ;; Basic face for the scroll bar colors under X
 '(scroll-bar ((t nil)))

 ;; Basic face for the frame border under X
 '(border ((t nil)))

 ;; Basic face for the cursor color under X
 '(cursor ((t :background "#FF0000")))

 ;; Basic face for the mouse color under X
 '(mouse ((t nil)))

 ;; Basic tool-bar face
 '(tool-bar ((t :background "#BFBFBF" :foreground "#000000" :box (:line-width 1 :style released-button))))

 ;; Basic face for the font and colors of the menu bar and popup menus
 '(menu ((t :inverse-video t)))

 ;; Face to highlight argument names in *Help* buffers
 '(help-argument-name ((t :inherit italic)))

 ;; Face for displaying non-graphic characters
 '(glyphless-char ((t :height 0.6)))

 ;; Basic face used to highlight errors and to denote failure
 '(error ((t :foreground "#FF0000" :weight bold)))

 ;; Basic face used to highlight warnings
 '(warning ((t :foreground "#FF7F00" :weight bold)))

 ;; Basic face used to indicate successful operation
 '(success ((t :foreground "#00FF00" :weight bold)))

 ;; Face for displaying disabled items in TTY menus
 '(tty-menu-enabled-face ((t :foreground "#00FF00" :weight bold)))

 ;; Face for displaying the currently selected item in TTY menus
 '(tty-menu-selected-face ((t :background "#FF0000")))

 ;; Face used for a matching paren
 '(show-paren-match ((t :background "#FF0000")))

 ;; Face used for a mismatching paren
 '(show-paren-mismatch ((t :background "#9B30FF" :foreground "#FFFFFF")))

 ;;; Isearch faces ------------------------------------------------------------------
 ;; Face for highlighting Isearch matches
 '(isearch ((t :background "#EEEE00" :foreground "#000000")))

 ;; Face for highlighting failed part in Isearch echo-area message
 '(isearch-fail ((t :background "#E5E5E5" :foreground "#EE0000")))

 ;; Face for lazy highlighting of matches other than the current one
 '(lazy-highlight ((t :background "#668B8B")))

 ;;; Replace faces ------------------------------------------------------------------
 ;; Face for highlighting query replacement matches
 '(query-replace ((t :background "#CD00CD" :foreground "#B0E2FF")))

 ;; Face used to highlight matches permanently
 '(match ((t :background "#3A5FCD")))

 ;;; Font lock faces ----------------------------------------------------------------
 ;; Face name to use for comments
 '(font-lock-comment-face ((t :foreground "#FFFFF0" :slant italic)))

 ;; Face name to use for comment delimiters
 '(font-lock-comment-delimiter-face ((t :foreground "#FFFFF0" :slant italic)))

 ;; Face name to use for strings
 '(font-lock-string-face ((t :foreground "#FFD700" :slant italic)))

 ;; Face name to use for documentation
 '(font-lock-doc-face ((t :foreground "#FFFFF0" :slant italic)))

 ;; Face name to use for keywords
 '(font-lock-keyword-face ((t :foreground "#FF337F" :slant italic)))

 ;; Face name to use for builtins
 '(font-lock-builtin-face ((t :foreground "#8ABEB7")))

 ;; Face name to use for function names
 '(font-lock-function-name-face ((t :foreground "#50BB00" :height 1.1)))

 ;; Face name to use for variable names
 '(font-lock-variable-name-face ((t :foreground "#3A5FCD")))

 ;; Face name to use for type and class names
 '(font-lock-type-face ((t :foreground "#00BFFF")))

 ;; Face name to use for constant and label names
 '(font-lock-constant-face ((t :foreground "#FFEEBB")))

 ;; Face name to use for things that should stand out
 '(font-lock-warning-face ((t :foreground "#BB0000")))

 ;; Face name to use for easy to overlook negation
 '(font-lock-negation-char-face ((t :weight bold)))

 ;; Face name to use for preprocessor directives
 '(font-lock-preprocessor-face ((t :foreground "#FF337F" :weight bold)))

 ;; Font Lock mode face for backslashes in Lisp regexp grouping constructs
 '(font-lock-regexp-grouping-backslash ((t :weight bold)))

 ;; Font Lock mode face used to highlight grouping constructs in Lisp regexps
 '(font-lock-regexp-grouping-construct ((t :weight bold)))

 ;;; Tooltip faces ------------------------------------------------------------------
 ;; Face for tooltips
 '(tooltip ((t :foreground "#000000" :background "#FFFFE0"))))

;; ==================================================================================
;; Customize `centaur-tabs' realted faces
(custom-set-faces
 `(centaur-tabs-default ((t :background ,emacs-config-default-background)))
 '(centaur-tabs-unselected ((t :background "#333333" :foreground "#999999")))
 '(centaur-tabs-selected ((t :background "#666666" :foreground "#00BFFF")))
 '(centaur-tabs-unselected-modified	((t :background "#333333" :foreground "#FFD700")))
 '(centaur-tabs-selected-modified ((t :background "#666666" :foreground "#FFD700"))))

;; Customize `powerline' realted faces
(custom-set-faces
 '(powerline-active1 ((t (:background "#FFD700" :foreground "black"))))
 '(powerline-active2 ((t (:background "#63B8FF" :foreground "black"))))
 '(powerline-inactive1 ((t (:background "#777777" :foreground "black"))))
 '(powerline-inactive2 ((t (:background "#999999" :foreground "black"))))
 '(mode-line-buffer-id-inactive ((t :inherit mode-line-buffer-id))))

;; ==================================================================================
;; Customize line spacing
(customize-set-variable 'line-spacing 3)

;; Customize `centaur-tabs' realted variables
(customize-set-variable 'centaur-tabs-height 26)
(customize-set-variable 'centaur-tabs-style "bar")
(customize-set-variable 'centaur-tabs-set-icons t)
(customize-set-variable 'centaur-tabs-plain-icons t)
(customize-set-variable 'centaur-tabs-gray-out-icons 'buffer)
(customize-set-variable 'centaur-tabs-set-close-button nil)
(customize-set-variable 'centaur-tabs-cycle-scope 'tabs)

;; Customize `uniquify' realted variables
(customize-set-variable 'uniquify-separator "/")
(customize-set-variable 'uniquify-buffer-name-style 'forward)

;; ==================================================================================
;; Key bindings for `centaur-tabs'
(lazy-set-key
 '(("M-p" . centaur-tabs-backward)
   ("M-n" . centaur-tabs-forward)
   ("M-P" . centaur-tabs-counsel-switch-group)
   ("M-N" . centaur-tabs-counsel-switch-group))
 centaur-tabs-mode-map)

;; ==================================================================================
;; Move the mouse to the upper-right corner on any keypress
(mouse-avoidance-mode "banish")

;; Disable blink cursor mode
(blink-cursor-mode -1)

;; Disable tool bar mode
(tool-bar-mode -1)

;; Disable scroll bar mode
(scroll-bar-mode -1)

;; Disable global menu bar mode
(menu-bar-mode -1)

;; Enable global column number mode
(column-number-mode 1)

;; Enable global just-in-time lock mode
(jit-lock-mode 1)

;; Enable global change cursor mode
(change-cursor-mode 1)

;; Enable cursor type change when idle
(toggle-cursor-type-when-idle 1)

;; Enable global smooth scrolling mode
(smooth-scrolling-mode 1)

;; Make headline use centaur tabs default face
(centaur-tabs-headline-match)

;; Enable centaur tabs mode
(centaur-tabs-mode 1)

;; Enable powerline default theme
(powerline-default-theme)

;; ==================================================================================
;;; Provide features
(provide 'init-theme)

;;; init-theme.el ends here
