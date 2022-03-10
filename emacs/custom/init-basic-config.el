;;; package --- init-basic-config.el ---
;; Time-stamp: <2022-03-10 16:20:54 Thursday by zhengyu.li>

;; Copyright (C) 2021, 2022 zhengyu li
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
;;   (require 'init-basic-config)

;;; Require:
(require 'cursor-chg-autoloads)
(require 'smooth-scrolling-autoloads)
(require 'ivy-autoloads)
(require 'counsel-autoloads)
(require 'counsel-projectile)
(require 'swiper-autoloads)
(require 'which-key-autoloads)
(require 'ascii-autoloads)
(require 'undo-tree-autoloads)
(require 'browse-kill-ring-autoloads)
(require 'expand-region-autoloads)
(require 'multiple-cursors-autoloads)
(require 'visual-regexp-steroids-autoloads)
(require 'ag-autoloads)
(require 'goto-chg-autoloads)
(require 'avy-autoloads)
(require 'lazy-set-key)

;;; Code:
;; ==================================================================================
(defun get-mode-name ()
  "Display major mode and mode name."
  (interactive)
  (message "major-mode: %s, mode-name: %s" major-mode mode-name))

(defun indent-buffer ()
  "Automatic format current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

(defun smart-indent ()
  "If mark is active, indent region, else indent all buffer."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'indent-region)
      (call-interactively 'indent-buffer))))

(defun copy-region ()
  "Copy region."
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end)))

(defun copy-curr-line ()
  "Copy current line."
  (interactive)
  (let ((end (min (point-max) (line-end-position))))
    (copy-region-as-kill (line-beginning-position) end)))

(defun smart-copy ()
  "If mark is active, copy region, else copy current line."
  (interactive)
  (save-excursion
    (if mark-active
        (call-interactively 'copy-region)
      (call-interactively 'copy-curr-line))))

(defun smart-kill ()
  "If mark is active, kill region, else kill whole line."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'kill-whole-line)))

(defun show-http-proxy ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current http proxy is \"%s\"" (cdr (nth 1 url-proxy-services)))
    (message "No http proxy")))

(defun set-http-proxy (proxy)
  "Set http/https proxy to `PROXY'."
  (interactive "sHTTP Proxy Server:")
  (setq url-proxy-services
        `(("no_proxy" . "^\\(127.0.0.1\\|localhost\\|10\\..*\\|192\\.168\\..*\\)")
          ("http" . ,proxy)
          ("https" . ,proxy)))
  (show-http-proxy))

(defun unset-http-proxy ()
  "Unset http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (show-http-proxy))

;; ==================================================================================
(defun tramp-settings ()
  "Settings for `tramp'."

  ;; ----------------------------------------------------------
  ;; Customize `tramp' related variables
  (customize-set-variable 'tramp-default-method "ssh")
  (customize-set-variable 'password-cache-expiry 3600)
  (customize-set-variable 'tramp-auto-save-directory "~/.emacs.d/tramp-auto-save"))

(eval-after-load "tramp" '(tramp-settings))

(defun undo-tree-settings ()
  "Settings for `undo-tree'."

  ;; ----------------------------------------------------------
  ;; Customize `undo-tree' related variables
  (customize-set-variable 'undo-tree-auto-save-history nil)

  ;; ----------------------------------------------------------
  ;; Key unbindings for `undo-tree'
  (lazy-unset-key
   '("C-x u" "M-_")
   undo-tree-map)

  ;; Key bindings for `undo-tree'
  (lazy-set-key
   '(("M-_"  . undo-tree-visualize))
   undo-tree-map))

(eval-after-load "undo-tree" '(undo-tree-settings))

(defun visual-regexp-steroids-settings ()
  "Settings for `visual-regexp-steroids'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `vr/minibuffer-keymap'
  (lazy-set-key
   '(("C-p" . previous-history-element)
     ("C-n" . next-history-element))
   vr/minibuffer-keymap))

(eval-after-load "visual-regexp-steroids" '(visual-regexp-steroids-settings))

(defun ispell-settings ()
  "Settings for `ispell'."

  ;; ----------------------------------------------------------
  ;; Customize `ispell' related variables
  (customize-set-variable 'ispell-program-name "aspell")
  (customize-set-variable 'ispell-extra-args '("--lang=en" "--reverse"))
  (customize-set-variable 'ispell-silently-savep t)
  (customize-set-variable 'ispell-dictionary "english"))

(eval-after-load "ispell" '(ispell-settings))

(defun ibuffer-settings ()
  "Settings for `ibuffer'."

  ;; Require
  (require 'all-the-icons-ibuffer)

  ;; ----------------------------------------------------------
  ;; Enable all the icons ibuffer mode
  (all-the-icons-ibuffer-mode 1))

(eval-after-load "ibuffer" '(ibuffer-settings))

(defun ivy-settings ()
  "Settings for `ivy'."

  ;; Require
  (require 'ivy-rich)
  (require 'all-the-icons-ivy-rich)

  ;; ----------------------------------------------------------
  ;; Customize `ivy' related variables
  (customize-set-variable 'ivy-use-virtual-buffers t)
  (customize-set-variable 'ivy-count-format "")
  (customize-set-variable 'ivy-initial-inputs-alist nil)
  (customize-set-variable 'ivy-re-builders-alist
                          '((t . ivy--regex-ignore-order)))

  ;; ----------------------------------------------------------
  ;; Enable ivy rich mode
  (ivy-rich-mode 1)

  ;; Enable all the icons ivy rich mode
  (all-the-icons-ivy-rich-mode 1))

(eval-after-load "ivy" '(ivy-settings))

(defun counsel-projectile-settings ()
  "Settings for `counsel-projectile'."

  ;; ----------------------------------------------------------
  ;; Key unbindings for `projectile'
  (lazy-set-key
   '(("s-p" . projectile-command-map)
     ("C-x p" . projectile-command-map))
   projectile-mode-map))

(eval-after-load "counsel-projectile" '(counsel-projectile-settings))

(defun avy-settings ()
  "Settings for `avy'."

  ;; ----------------------------------------------------------
  ;; Customize `avy' related faces
  (custom-set-faces
   '(avy-background-face ((t :foreground "#90EE90")))
   `(avy-lead-face ((t :background "#D2691E"
                       :foreground ,emacs-config-background)))
   '(avy-lead-face-0 ((t :inherit 'avy-lead-face)))
   '(avy-lead-face-1 ((t :inherit 'avy-lead-face)))
   '(avy-lead-face-2 ((t :inherit 'avy-lead-face)))))

(eval-after-load "avy" '(avy-settings))

(defun ag-settings ()
  "Settings for `ag'."

  ;; Require
  (require 'wgrep-ag)

  ;; ----------------------------------------------------------
  ;; Customize `ag' related variables
  (customize-set-variable 'ag-reuse-buffers t)
  (customize-set-variable 'ag-reuse-window t)

  ;; Customize `wgrep' related variables
  (customize-set-variable 'wgrep-enable-key "r")
  (customize-set-variable 'wgrep-auto-save-buffer t)

  ;; ----------------------------------------------------------
  ;; Hooks for `ag'
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  (add-hook 'ag-search-finished-hook
            (lambda ()
              ;; ----------------------------------------------------------
              (select-window
               (get-buffer-window (ag/buffer-name "" "" ""))))))

(eval-after-load "ag" '(ag-settings))

(defun which-key-settings ()
  "settings for `which-key'."

  ;; ----------------------------------------------------------
  (which-key-setup-side-window-right))

(eval-after-load "which-key" '(which-key-settings))

;; ==================================================================================
;; Update timestamp before saving files
(add-hook 'before-save-hook 'time-stamp)

;; Update copyright before saving files
(add-hook 'before-save-hook 'copyright-update)

;; Delete trailing whitespace before saving files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Settings after init
(add-hook 'after-init-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Customize `startup' related variables
            (customize-set-variable 'inhibit-default-init t)
            (customize-set-variable 'inhibit-startup-screen t)
            (customize-set-variable 'inhibit-startup-message t)
            (customize-set-variable 'inhibit-startup-echo-area-message t)

            ;; Customize `time-stamp' related variables
            (customize-set-variable 'time-stamp-format
                                    "%Y-%02m-%02d %02H:%02M:%02S %:a by %u")

            ;; Customize `recentf' related variables
            (customize-set-variable 'recentf-max-saved-items 1000)
            (customize-set-variable 'recentf-exclude '("/tmp/" "ssh:"))

            ;; Customize `files' related variables
            (customize-set-variable 'backup-directory-alist
                                    '((".*" . "~/.emacs.d/backup-files")))
            (customize-set-variable 'backup-by-copying t)
            (customize-set-variable 'delete-old-versions t)
            (customize-set-variable 'version-control t)

            ;; Customize `uniquify' realted variables
            (customize-set-variable 'uniquify-separator "/")
            (customize-set-variable 'uniquify-buffer-name-style 'forward)

            ;; Customize line spacing
            (customize-set-variable 'line-spacing 3)

            ;; Customize user and email
            (customize-set-variable 'user-full-name emacs-config-user)
            (customize-set-variable 'user-mail-address emacs-config-email)

            ;; ----------------------------------------------------------
            ;; Global key bindings for basic config
            (lazy-set-key
             '(("C-x C-b" . ibuffer)
               ("C-x k" . kill-this-buffer)
               ("C-x b" . counsel-ibuffer)
               ("C-x B" . counsel-recentf)
               ("C-x C-f" . counsel-find-file)
               ("M-x" . counsel-M-x)
               ("C-s" . swiper-isearch)
               ("C-r" . swiper-isearch-backward)
               ("C-x <tab>" . smart-indent)
               ("C-x TAB" . smart-indent)
               ("C-x m" . set-rectangular-region-anchor)
               ("C-x M" . mc/mark-all-dwim)
               ("C-x g" . ag)
               ("C-x G" . ag-project)
               ("C-x f" . ag-dired-regexp)
               ("C-x F" . ag-project-dired-regexp)
               ("C-; c" . avy-goto-char)
               ("C-; w" . avy-goto-word-0)
               ("C-; l" . avy-goto-line)
               ("M-w" . smart-copy)
               ("M-k" . smart-kill)
               ("M-g" . goto-line)
               ("M-m" . set-mark-command)
               ("M-M" . er/expand-region)
               ("M-o" . goto-last-change)
               ("M-y" . browse-kill-ring)
               ("M-_" . text-scale-decrease)
               ("M-+" . text-scale-increase)))

            ;; ----------------------------------------------------------
            ;; Replace yes-or-no-p with y-or-no-p
            (fset 'yes-or-no-p 'y-or-n-p)

            ;; Move the mouse to the upper-right corner on any keypress
            (mouse-avoidance-mode "banish")

            ;; Initialize mac system exec path
            (when (memq window-system '(mac ns))
              (require 'exec-path-from-shell)
              (setq mac-command-modifier 'super)
              (setq mac-option-modifier 'meta)
              (exec-path-from-shell-initialize))

            ;; Enable cursor type change when idle
            (toggle-cursor-type-when-idle 1)

            ;; ----------------------------------------------------------
            ;; Disable blink cursor mode
            (blink-cursor-mode -1)

            ;; Disable tool bar mode
            (tool-bar-mode -1)

            ;; Disable scroll bar mode
            (scroll-bar-mode -1)

            ;; Disable global menu bar mode
            (menu-bar-mode -1)

            ;; Enable global auto revert mode
            (global-auto-revert-mode 1)

            ;; Enable global recentf mode
            (recentf-mode 1)

            ;; Enable global column number mode
            (column-number-mode 1)

            ;; Enable global just-in-time lock mode
            (jit-lock-mode 1)

            ;; Enable global change cursor mode
            (change-cursor-mode 1)

            ;; Enable global smooth scrolling mode
            (smooth-scrolling-mode 1)

            ;; Enable global undo tree mode
            (global-undo-tree-mode 1)

            ;; Enable global ivy mode
            (ivy-mode 1)

            ;; Enable global counsel mode
            (counsel-mode 1)

            ;; Enable global counsel projectile mode
            (counsel-projectile-mode 1)

            ;; Enable global which key mode
            (which-key-mode 1)))

;; ==================================================================================
;;; Provide features
(provide 'init-basic-config)

;;; init-basic-config.el ends here
