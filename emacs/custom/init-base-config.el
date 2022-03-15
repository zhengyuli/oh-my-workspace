;;; package --- init-base-config.el ---
;; Time-stamp: <2022-03-15 15:08:48 Tuesday by zhengyuli>

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
;;   (require 'init-base-config)

;;; Require:
(load-library "beacon-autoloads")
(load-library "smooth-scrolling-autoloads")
(load-library "ivy-autoloads")
(load-library "counsel-autoloads")
(load-library "counsel-projectile")
(load-library "swiper-autoloads")
(load-library "which-key-autoloads")
(load-library "visual-ascii-mode-autoloads")
(load-library "undo-tree-autoloads")
(load-library "browse-kill-ring-autoloads")
(load-library "expand-region-autoloads")
(load-library "multiple-cursors-autoloads")
(load-library "visual-regexp-steroids-autoloads")
(load-library "ag-autoloads")
(load-library "goto-chg-autoloads")
(load-library "avy-autoloads")
(load-library "yasnippet-autoloads")
(load-library "company-autoloads")
(load-library "centaur-tabs-autoloads")
(load-library "doom-modeline-autoloads")
(load-library "doom-themes-autoloads")
(load-library "winum-autoloads")
(load-library "zoom-autoloads")
(load-library "workgroups2-autoloads")
(load-library "multi-term-autoloads")

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

(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter
     nil
     'fullscreen
     (if (equal 'fullboth current-value)
         (if (boundp 'old-fullscreen)
             old-fullscreen
           nil)
       (setq old-fullscreen current-value)
       'fullboth))))

;; ==================================================================================
(defun tramp-settings ()
  "Settings for `tramp'."

  ;; ----------------------------------------------------------
  ;; Customize `tramp' related variables
  (customize-set-variable 'tramp-default-method "ssh")
  (customize-set-variable 'password-cache-expiry 3600)
  (customize-set-variable 'tramp-auto-save-directory "~/.emacs.d/tramp-auto-save"))

(eval-after-load "tramp" '(tramp-settings))

;; ==================================================================================
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

;; ==================================================================================
(defun visual-regexp-steroids-settings ()
  "Settings for `visual-regexp-steroids'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `vr/minibuffer-keymap'
  (lazy-set-key
   '(("C-p" . previous-history-element)
     ("C-n" . next-history-element))
   vr/minibuffer-keymap))

(eval-after-load "visual-regexp-steroids" '(visual-regexp-steroids-settings))

;; ==================================================================================
(defun ispell-settings ()
  "Settings for `ispell'."

  ;; ----------------------------------------------------------
  ;; Customize `ispell' related variables
  (customize-set-variable 'ispell-program-name "aspell")
  (customize-set-variable 'ispell-extra-args '("--lang=en" "--reverse"))
  (customize-set-variable 'ispell-silently-savep t)
  (customize-set-variable 'ispell-dictionary "english"))

(eval-after-load "ispell" '(ispell-settings))

;; ==================================================================================
(defun ibuffer-settings ()
  "Settings for `ibuffer'."

  ;; Require
  (require 'all-the-icons-ibuffer)

  ;; ----------------------------------------------------------
  ;; Enable all the icons ibuffer mode
  (all-the-icons-ibuffer-mode 1))

(eval-after-load "ibuffer" '(ibuffer-settings))

;; ==================================================================================
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

  ;; Enable global all the icons ivy rich mode
  (all-the-icons-ivy-rich-mode 1))

(eval-after-load "ivy" '(ivy-settings))

;; ==================================================================================
(defun counsel-projectile-settings ()
  "Settings for `counsel-projectile'."

  ;; ----------------------------------------------------------
  ;; Key unbindings for `projectile'
  (lazy-set-key
   '(("s-p" . projectile-command-map)
     ("C-x p" . projectile-command-map))
   projectile-mode-map))

(eval-after-load "counsel-projectile" '(counsel-projectile-settings))

;; ==================================================================================
(defun ag-settings ()
  "Settings for `ag'."

  ;; Require
  (require 'wgrep-ag)

  ;; ----------------------------------------------------------
  (defadvice ag/next-error-function (after ag/next-error-function-after activate)
    (select-window
     (get-buffer-window (ag/buffer-name "" "" ""))))

  ;; ----------------------------------------------------------
  ;; Customize `ag' related variables
  (customize-set-variable 'ag-reuse-buffers t)

  ;; Customize `wgrep' related variables
  (customize-set-variable 'wgrep-enable-key "r")
  (customize-set-variable 'wgrep-auto-save-buffer t)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)

  (add-hook 'ag-search-finished-hook
            (lambda ()
              ;; ----------------------------------------------------------
              (select-window
               (get-buffer-window (ag/buffer-name "" "" ""))))))

(eval-after-load "ag" '(ag-settings))

;; ==================================================================================
(defun which-key-settings ()
  "settings for `which-key'."

  ;; ----------------------------------------------------------
  (which-key-setup-side-window-right))

(eval-after-load "which-key" '(which-key-settings))

;; ==================================================================================
(defun yasnippet-settings ()
  "Settings for `yasnippet'."

  ;; Require
  (require 'yasnippet-snippets)

  ;; ----------------------------------------------------------
  ;; Key bindings for `yasnippet'
  (lazy-unset-key
   '("<tab>" "TAB")
   yas-minor-mode-map)

  ;; ----------------------------------------------------------
  ;; initialize yasnippet snippets
  (yasnippet-snippets-initialize))

(eval-after-load "yasnippet" '(yasnippet-settings))

;; ==================================================================================
(defun company-settings ()
  "Settings for `company'."

  ;; Require
  (require 'company-box)
  (require 'company-quickhelp)
  (require 'company-quickhelp-terminal)
  (require 'company-yasnippet)

  ;; ----------------------------------------------------------
  (defun append-company-backend-with-yas (backend)
    "Append `BACKEND' with company-yas."
    (if (and (listp backend)
             (member 'company-yasnippet backend))
        backend
      (append (if (consp backend)
                  backend
                (list backend))
              '(:with company-yasnippet))))

  ;; ----------------------------------------------------------
  ;; Customize `company' related variables
  (customize-set-variable 'company-idle-delay 0)
  (customize-set-variable 'company-minimum-prefix-length 1)
  (customize-set-variable 'company-tooltip-limit 15)
  (customize-set-variable 'company-selection-wrap-around t)
  (customize-set-variable 'company-format-margin-function 'company-text-icons-margin)
  (customize-set-variable 'company-transformers '(delete-dups company-sort-by-occurrence))
  (customize-set-variable 'company-backends
                          (mapcar 'append-company-backend-with-yas company-backends))

  ;; customize `company-box' related variables
  (customize-set-variable 'company-box-scrollbar nil)
  (customize-set-variable 'company-box-doc-enable t)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'company-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable quickhelp terminal mode if any
              (if (display-graphic-p)
                  ;; Enable company box mode for graphic mode
                  (company-box-mode 1)
                ;; Enable quickhelp terminal mode for on-graphic mode
                (progn
                  (company-quickhelp-mode 1)
                  (company-quickhelp-terminal-mode 1))))))

(eval-after-load "company" '(company-settings))

;; ==================================================================================
(defun centaur-tabs-settings ()
  "Settings for `centaur-tabs'."

  ;; Require
  (require 'all-the-icons)

  ;; ----------------------------------------------------------
  ;; Customize `centaur-tabs' realted variables
  (customize-set-variable 'centaur-tabs-height 25)
  (customize-set-variable 'centaur-tabs-style "zigzag")
  (if (display-graphic-p)
      (customize-set-variable 'centaur-tabs-set-icons t))
  (customize-set-variable 'centaur-tabs-gray-out-icons 'buffer)
  (customize-set-variable 'centaur-tabs-set-close-button nil)
  (customize-set-variable 'centaur-tabs-cycle-scope 'tabs)

  ;; ----------------------------------------------------------
  ;; Key bindings for `centaur-tabs'
  (lazy-set-key
   '(("M-p" . centaur-tabs-backward)
     ("M-n" . centaur-tabs-forward)
     ("M-P" . centaur-tabs-counsel-switch-group)
     ("M-N" . centaur-tabs-counsel-switch-group))
   centaur-tabs-mode-map))

(eval-after-load "centaur-tabs" '(centaur-tabs-settings))

;; ==================================================================================
(defun winum-settings ()
  "Settings for `winum'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `winum'
  (lazy-set-key
   '(("M-0" . winum-select-window-0-or-10)
     ("M-1" . winum-select-window-1)
     ("M-2" . winum-select-window-2)
     ("M-3" . winum-select-window-3)
     ("M-4" . winum-select-window-4)
     ("M-5" . winum-select-window-5)
     ("M-6" . winum-select-window-6)
     ("M-7" . winum-select-window-7)
     ("M-8" . winum-select-window-8)
     ("M-9" . winum-select-window-9))
   winum-keymap))

(eval-after-load "winum" '(winum-settings))

;; ==================================================================================
(defun zoom-settings ()
  "Settings for `zoom'."

  ;; ----------------------------------------------------------
  (defun zoom-size-callback ()
    "Callback for zoom size."
    (cond ((> (frame-pixel-width) 1280)
           '(90 . 0.75))
          (t
           '(0.5 . 0.5))))

  ;; ----------------------------------------------------------
  ;; Customize `zoom' related variables
  (customize-set-variable 'zoom-size 'zoom-size-callback))

(eval-after-load "zoom" '(zoom-settings))

;; ==================================================================================
(defun workgroups2-settings ()
  "Settings for `workgroups2'."

  ;; ----------------------------------------------------------
  ;; Customize `workgroups2' related variables
  (customize-set-variable 'wg-control-frames nil))

(eval-after-load "workgroups2" '(workgroups2-settings))

;; ==================================================================================
;;Customized settings for `multi-term'
(defun multi-term-settings ()
  "Settings for `multi-term'."

  ;; ----------------------------------------------------------
  ;; Customize `terminal' related variables
  (customize-set-variable 'ring-bell-function 'ignore)

  ;; ----------------------------------------------------------
  ;; Key bindings for `multi-term'
  (customize-set-variable
   'term-unbind-key-list
   '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>" "<M-backspace>" "<M-DEL>"))

  (customize-set-variable
   'term-bind-key-alist
   '(("C-p" . term-send-raw)
     ("C-n" . term-send-raw)
     ("C-r" . term-send-raw)
     ("C-y" . term-paste)
     ("M-f" . term-send-raw-meta)
     ("M-b" . term-send-raw-meta)
     ("M-d" . term-send-raw-meta)
     ("<M-backspace>" . term-send-backward-kill-word)
     ("<M-DEL>" . term-send-backward-kill-word)
     ("M-." . term-send-raw-meta)
     ("M-i" . term-interrupt-subjob)))

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'term-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Disable auto fill mode
              (auto-fill-mode -1)

              ;; Disable yasnippet mode
              (yas-minor-mode -1)

              ;; Disable company mode
              (company-mode -1))))

(eval-after-load "multi-term" '(multi-term-settings))

;; ==================================================================================
;; Customized settings for `doom-themes'
(defun doom-themes-settings ()
  "Settings for `doom-themes'."

  ;; ----------------------------------------------------------
  ;; Customize font settings
  (set-face-attribute 'default nil :font emacs-config-fixed-font :height emacs-config-font-size)
  (set-face-attribute 'fixed-pitch nil :font emacs-config-fixed-font)
  (set-face-attribute 'fixed-pitch-serif nil :font emacs-config-fixed-serif-font)

  ;; ----------------------------------------------------------
  ;; Customize `doom-themes' related variables
  (customize-set-variable 'doom-themes-enable-bold t)
  (customize-set-variable 'doom-themes-enable-italic t))

(eval-after-load "doom-themes" '(doom-themes-settings))

;; ==================================================================================
;; Hooks
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
               ("M--" . text-scale-decrease)
               ("M-=" . text-scale-increase)
               ("C-<f10>" . toggle-fullscreen)
               ("C-<left>" . shrink-window-horizontally)
               ("C-<right>" . enlarge-window-horizontally)
               ("C-<down>" . shrink-window)
               ("C-<up>" . enlarge-window)
               ("<f9>" . multi-term)))

            ;; ----------------------------------------------------------
            ;; Replace yes-or-no-p with y-or-no-p
            (fset 'yes-or-no-p 'y-or-n-p)

            ;; Initialize mac system exec path
            (when (memq window-system '(mac ns))
              (require 'exec-path-from-shell)
              (setq mac-command-modifier 'super)
              (setq mac-option-modifier 'meta)
              (exec-path-from-shell-initialize))

            ;; ----------------------------------------------------------
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

            ;; Enable global beacon mode
            (beacon-mode 1)

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
            (which-key-mode 1)

            ;; Enable global yasnippet mode
            (yas-global-mode 1)

            ;; Enable global company mode
            (global-company-mode 1)

            ;; Enable centaur tabs mode
            (centaur-tabs-mode 1)

            ;; Enable doom modeline
            (doom-modeline-mode 1)

            ;; Enable winum mode
            (winum-mode 1)

            ;; Enable zoom mode
            (zoom-mode 1)

            ;; ----------------------------------------------------------
            ;; Hooks
            (add-hook 'before-save-hook
                      (lambda ()
                        ;; ----------------------------------------------------------
                        ;; Update timestamp
                        (time-stamp)

                        ;; Update copyright
                        (copyright-update)

                        ;; Delete trailing whitespace
                        (delete-trailing-whitespace)))))

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Load doom theme
            (load-theme 'doom-one t)

            ;; Enable global workgroups mode
            (workgroups-mode 1)

            ;; Toggle full screen
            (toggle-fullscreen)))

;; ==================================================================================
;;; Provide features
(provide 'init-base-config)

;;; init-base-config.el ends here
