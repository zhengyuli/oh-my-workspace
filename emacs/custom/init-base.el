;;; package --- init-base.el -*- lexical-binding:t -*-
;; Time-stamp: <2024-12-02 22:11:48 Monday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024 zhengyu li
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
;;   (require 'init-base)

;;; Require:

;;; Code:
;; ==================================================================================
(defun get-mode-name ()
  "Display major mode and mode name."
  (interactive)
  (message "major-mode: %s, mode-name: %s" major-mode (car mode-name)))

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

(defun toggle-buffer-writable ()
    "Toggle buffer writable."
    (interactive)
    (if buffer-read-only
        (read-only-mode -1)
      (read-only-mode 1)))

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

(defun adjust-window-split-thresholds ()
  "Adjust window split thresholds."
  (interactive)
  (if (>= (frame-pixel-width) (frame-pixel-height))
      (progn
        (setq split-height-threshold (frame-height))
        (setq split-width-threshold  (/ (frame-width) 2)))
    (progn
      (setq split-height-threshold (frame-height))
      (setq split-width-threshold  (frame-width)))))

;; ==================================================================================
;; Customized settings for `visual-regexp'
(defun visual-regexp-settings ()
  "Settings for `visual-regexp'."

  ;; Require
  (require 'visual-regexp-steroids)

  ;; ----------------------------------------------------------
  ;; Key bindings for `vr/minibuffer-keymap'
  (lazy-set-key
   '(("C-p" . previous-history-element)
     ("C-n" . next-history-element))
   vr/minibuffer-keymap))

(eval-after-load "visual-regexp" '(visual-regexp-settings))

;; ==================================================================================
;; Customized settings for `which-key'
(defun which-key-settings ()
  "settings for `which-key'."

  ;; ----------------------------------------------------------
  (which-key-setup-side-window-right))

(eval-after-load "which-key" '(which-key-settings))

;; ==================================================================================
;; Customized settings for `undo-tree'
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
;; Customized settings for `ivy'
(defun ivy-settings ()
  "Settings for `ivy'."

  ;; Require
  (require 'ivy-rich)
  (require 'ivy-prescient)
  (require 'all-the-icons-ivy-rich)

  ;; ----------------------------------------------------------
  ;; Customize `ivy' related variables
  (customize-set-variable 'ivy-use-virtual-buffers t)
  (customize-set-variable 'ivy-count-format "(%d/%d) ")
  (customize-set-variable 'ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  ;; ----------------------------------------------------------
  ;; Enable global ivy rich mode
  (ivy-rich-mode 1)

  ;; Enable global ivy prescient mode
  (ivy-prescient-mode 1)

  ;; Enable global all the icons ivy rich mode
  (all-the-icons-ivy-rich-mode 1))

(eval-after-load "ivy" '(ivy-settings))

;; ==================================================================================
;; Customized settings for `counsel-projectile'
(defun counsel-projectile-settings ()
  "Settings for `counsel-projectile'."

  ;; ----------------------------------------------------------
  ;; Key unbindings for `projectile'
  (lazy-set-key
   '(("C-x p" . projectile-command-map))
   projectile-mode-map))

(eval-after-load "counsel-projectile" '(counsel-projectile-settings))

;; ==================================================================================
;; Customized settings for `ag'
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
;; Customized settings for `yasnippet'
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
;; Customized settings for `company'
(defun company-settings ()
  "Settings for `company'."

  ;; Require
  (require 'company-yasnippet)
  (require 'company-prescient)
  (require 'company-quickhelp)
  (require 'company-box)

  ;; ----------------------------------------------------------
  (defun company-mode/backend-with-yas (backend)
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
  (customize-set-variable 'company-require-match 'never)
  (customize-set-variable 'company-tooltip-idle-delay 0)
  (customize-set-variable 'company-tooltip-limit 15)
  (customize-set-variable 'company-selection-wrap-around t)
  (customize-set-variable 'company-format-margin-function 'company-text-icons-margin)
  (customize-set-variable 'company-transformers '(delete-dups company-sort-by-occurrence))
  ;; (customize-set-variable 'company-backends
  ;;                         (mapcar 'company-mode/backend-with-yas company-backends))

  ;; Customize `company-dabbrev' related variables
  (customize-set-variable 'company-dabbrev-downcase nil)

  ;; customize `company-box' related variables
  (customize-set-variable 'company-box-backends-colors nil)
  (customize-set-variable 'company-box-scrollbar nil)
  (customize-set-variable 'company-box-doc-enable t)
  (customize-set-variable 'company-box-doc-delay 0.1)

  ;; ----------------------------------------------------------
  ;; Enable global company prescient mode
  (company-prescient-mode 1)

  ;; Enable global company quickhelp mode
  (company-quickhelp-mode 1)

  ;; Enable global company quickhelp for terminal mode
  (unless (display-graphic-p)
    (company-quickhelp-terminal-mode 1))

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'company-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable company box mode
              (company-box-mode 1))))

(eval-after-load "company" '(company-settings))

;; ==================================================================================
;; Customized settings for `autoinsert'
(defun autoinsert-settings ()
  "Settings for `autoinsert'."

  ;; ----------------------------------------------------------
  (defun define-auto-insert-custom (condition action)
    "Custom implementation of `define-auto-insert'."
    (let ((elt (assoc condition auto-insert-alist)))
      (if elt
          (setcdr elt action)
        (add-to-list 'auto-insert-alist (cons condition action)))))

  (defun autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  ;; ----------------------------------------------------------
  ;; Customize `autoinsert' related variables
  (customize-set-variable 'auto-insert 'other)
  (customize-set-variable 'auto-insert-directory
                          (concat emacs-config-root-path "templates/"))

  ;; ----------------------------------------------------------
  ;; Templates
  (define-auto-insert-custom
    '("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C / C++ header")
    ["template.h" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'" . "C / C++ program")
    ["template.c" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.py\\'" . "Python header")
    ["template.py" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.go\\'" . "Golang header")
    ["template.go" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.el\\'" . "Emacs Lisp header")
    ["template.el" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.hs\\'" . "Haskell header")
    ["template.hs" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'" . "Scala header")
    ["template.scala" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . "Groovy header")
    ["template.groovy" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.sh\\'" . "Shell script header")
    ["template.sh" autoinsert-yas-expand])

  (define-auto-insert-custom
    '("\\.org\\'" . "Org header")
    ["template.org" autoinsert-yas-expand]))

(eval-after-load "autoinsert" '(autoinsert-settings))

;; ==================================================================================
;; Customized settings for `flyspell'
(defun flyspell-settings ()
  "Settings for `flyspell'."

  ;; ----------------------------------------------------------
  ;; Customize `ispell' realted variables
  (customize-set-variable 'ispell-program-name "aspell")
  (customize-set-variable 'ispell-dictionary "american")

  ;; Customize `flyspell' realted variables
  (customize-set-variable 'flyspell-issue-message-flag nil)

  ;; ----------------------------------------------------------
  ;; Key unbindings for `undo-tree'
  (lazy-unset-key
   '("C-," "C-." "C-;")
   flyspell-mode-map))

(eval-after-load "flyspell" '(flyspell-settings))

;; ==================================================================================
;; Customized settings for `flyspell-correct'
(defun flyspell-correct-settings ()
  "Settings for `flyspell-correct'."

  ;; Require
  (require 'flyspell-correct-avy-menu))

(eval-after-load "flyspell-correct" '(flyspell-correct-settings))

;; ==================================================================================
;; Customized settings for `winum'
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
;; Customized settings for `dimmer'
(defun dimmer-settings ()
  "Settings for `dimmer'."

  ;; ----------------------------------------------------------
  ;; Customize `dimmer' realted variables
  (customize-set-variable 'dimmer-fraction 0.30))

(eval-after-load "dimmer" '(dimmer-settings))

;; ==================================================================================
;; Customized settings for `centaur-tabs'
(defun centaur-tabs-settings ()
  "Settings for `centaur-tabs'."

  ;; Require
  (require 'centaur-tabs-elements)
  (require 'centaur-tabs-functions)

  ;; ----------------------------------------------------------
  ;; Redefinition of `centaur-tabs-buffer-groups'
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules."
    (list
     (cond
      ((derived-mode-p 'dashboard-mode)
       "Dashboard")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((derived-mode-p 'vterm-mode)
       "Vterm")
      ((derived-mode-p 'eww-mode)
       "Eww")
      ((derived-mode-p 'xwidget-webkit-mode)
       "Xwidget Webkit")
      ((derived-mode-p 'hackernews-mode)
       "Hacker News")
      ((string-prefix-p "*ChatGPT*" (buffer-name))
       "GPT")
      ((or (string-prefix-p "*" (buffer-name))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode)))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "ProgMode")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  ;; ----------------------------------------------------------
  ;; Customized `centaur-tabs-elements' related faces
  (custom-set-faces
   '(centaur-tabs-selected ((t (:bold t :foreground "#28cd41"))))
   '(centaur-tabs-selected-modified ((t (:bold t :foreground "#ff9300"))))
   '(centaur-tabs-unselected ((t (:bold t :foreground "grey"))))
   '(centaur-tabs-unselected-modified ((t (:bold t :foreground "#ff9300")))))

  (set-face-attribute centaur-tabs-display-line nil
                      :inherit 'default
                      :box nil :overline nil :underline nil)

  ;; ----------------------------------------------------------
  ;; Customize `centaur-tabs-elements' realted variables
  (customize-set-variable 'centaur-tabs-height 25)
  (customize-set-variable 'centaur-tabs-style "bar")
  (customize-set-variable 'centaur-tabs-set-close-button nil)
  (customize-set-variable 'centaur-tabs-set-icons t)
  (customize-set-variable 'centaur-tabs-gray-out-icons 'buffer)
  (customize-set-variable 'centaur-tabs-show-count t)
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
;; Customized settings for `zoom'
(defun zoom-settings ()
  "Settings for `zoom'."

  (defun size-callback ()
    (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
          (t '(0.5 . 0.5))))

  ;; ----------------------------------------------------------
  ;; Customize `zoom' related variables
  (customize-set-variable 'zoom-size 'size-callback))

(eval-after-load "zoom" '(zoom-settings))

;; ==================================================================================
;; Customized settings for `doom-modeline'
(defun doom-modeline-settings ()
  "Settings for `doom-modeline'."

  ;; Require
  (require 'doom-modeline-core)

  ;; ----------------------------------------------------------
  ;; Customize `doom-modeline-core' related variables
  (customize-set-variable 'doom-modeline-mu4e t))

(eval-after-load "doom-modeline" '(doom-modeline-settings))

;; ==================================================================================
;; Customized settings for `doom-themes'
(defun doom-themes-settings ()
  "Settings for `doom-themes'."

  ;; Require
  (require 'faces)

  ;; ----------------------------------------------------------
  ;; Customize builtin `faces' related faces
  (set-face-attribute 'default nil :font emacs-config-fixed-font)
  (set-face-attribute 'fixed-pitch nil :font emacs-config-fixed-font)
  (set-face-attribute 'fixed-pitch-serif nil :font emacs-config-fixed-serif-font)
  (set-face-attribute 'variable-pitch nil :font emacs-config-variable-font)

  ;; ----------------------------------------------------------
  ;; Customize `doom-themes' related variables
  (customize-set-variable 'doom-themes-enable-bold t)
  (customize-set-variable 'doom-themes-enable-italic t))

(eval-after-load "doom-themes" '(doom-themes-settings))

;; ==================================================================================
;; Customized settings for `dashboard'
(defun dashboard-settings ()
  "Settings for `dashboard'."

  (require 'dashboard-widgets)

  ;; ----------------------------------------------------------
  ;; Customize `dashboard' related variables
  (customize-set-variable 'dashboard-center-content t)

  ;; Customize `dashboard-widgets' related variables
  (customize-set-variable 'dashboard-startup-banner
                          (concat emacs-config-root-path "banners/totoro.png"))
  (customize-set-variable 'dashboard-banner-logo-title
                          (format "Welcome to %s\'s Emacs" emacs-config-user))
  (customize-set-variable 'dashboard-set-heading-icons t)
  (customize-set-variable 'dashboard-set-file-icons t)
  (customize-set-variable 'dashboard-set-navigator t)
  (customize-set-variable 'dashboard-items
                          '((recents  . 5)
                            (bookmarks . 5)
                            (projects . 5)
                            (agenda . 5)
                            (registers . 5)))
  (customize-set-variable 'dashboard-projects-switch-function
                          'counsel-projectile-switch-project-by-name)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'dashboard-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable local centaur tabs mode
              (centaur-tabs-local-mode 1))))

(eval-after-load "dashboard" '(dashboard-settings))

;; ==================================================================================
;; Customized settings for `textsize'
(defun textsize-settings ()
  "Settings for `textsize'."

  ;; ----------------------------------------------------------
  ;; Customize `textsize' related variables
  (customize-set-variable 'textsize-monitor-size-thresholds
                          '((0 . -2) (350 . 0) (500 . 1)))
  (customize-set-variable 'textsize-pixel-pitch-thresholds
                          '((0 . 6) (0.12 . 4) (0.18 . 2) (0.20 . 1) (0.25 . -1))))

(eval-after-load "textsize" '(textsize-settings))

;; ==================================================================================
;; Customized settings for `epa'
(defun epg-config-settings ()
  "Settings for `epg-config'."

  ;; ----------------------------------------------------------
  ;; Customize `epg-config' related variables
  (customize-set-variable 'epg-pinentry-mode 'loopback)
  (customize-set-variable 'epg-debug t))

(eval-after-load "epg-config" '(epg-config-settings))

;; ==================================================================================
;; Customized settings for `dired'
(defun dired-settings ()
  "Settings for `dired'."

  ;; Require
  (require 'ztree)
  (require 'ztree-view)
  (require 'epa-dired)
  (require 'dired-x)
  (require 'dired-filter)
  (require 'dired-async)
  (require 'dired-hacks-utils)
  (require 'dired-filetype-face)
  (require 'all-the-icons-dired)
  (require 'dired-copy-paste)
  (require 'dired-custom-extension)

  ;; ----------------------------------------------------------
  ;; Customized `dired' related faces
  (custom-set-faces
   '(dired-header ((t (:foreground "#EE82EE" :height 1.1))))
   '(dired-directory ((t (:foreground "#51AFEF" :height 1.1))))
   '(dired-mark ((t (:foreground "#FF1493" :inverse-video nil))))
   '(dired-marked ((t (:foreground "#FFFF00" :inverse-video nil)))))

  ;; ----------------------------------------------------------
  ;; Customize `dired' related variables
  (customize-set-variable 'dired-dwim-target t)
  (customize-set-variable 'dired-recursive-copies 'always)
  (customize-set-variable 'dired-recursive-deletes 'always)

  ;; Customize `dired-x' related variables
  (customize-set-variable 'dired-bind-info nil)
  (customize-set-variable 'dired-omit-extensions
                          (append dired-omit-extensions '(".cache")))
  (customize-set-variable 'dired-omit-files (concat dired-omit-files
                                                    "\\|^\\.\\|^semantic.cache$\\|^CVS$"))

  ;; Customize `dired-filter' related variables
  (customize-set-variable 'dired-filter-stack '())

  ;; ----------------------------------------------------------
  ;; Key bindings for `ztree-view'
  (lazy-set-key
   '(("<return>" . ztree-perform-soft-action)
     ("RET" . ztree-perform-soft-action)
     ("o" . other-window)
     ("n" . next-line)
     ("p" . previous-line))
   ztree-mode-map)

  ;; Key bindings for `dired'
  (lazy-set-key
   '(("<return>" . dired-single-buffer)
     ("RET" . dired-single-buffer)
     ("p" . dired-hacks-previous-file)
     ("n" . dired-hacks-next-file)
     ("M-," . dired-goto-first-line)
     ("M-." . dired-goto-last-line)
     ("h" . dired-up-directory-single)
     ("C-s" . isearch-forward)
     ("C-r" . isearch-backward)
     ("C-k" . dired-do-delete)
     ("k" . kill-this-buffer)
     ("r" . wdired-change-to-wdired-mode)
     ("M-o" . dired-omit-mode)
     ("E" . dired-do-touch)
     ("B" . dired-backup-file)
     ("?" . dired-get-size)
     ("d" . dired-diff)
     ("D" . ztree-diff)
     ("z" . dired-do-compress)
     ("Z" . dired-do-compress)
     (": e" . epa-dired-do-encrypt)
     (": d" . epa-dired-do-decrypt)
     (": s" . epa-dired-do-sign)
     (": v" . epa-dired-do-verify)
     ("M-w" . dired-copy-paste-do-copy)
     ("M-k" . dired-copy-paste-do-cut)
     ("C-y" . dired-copy-paste-do-paste)
     ("/m" . dired-mark-files-regexp)
     ("/*" . dired-filter-by-regexp)
     ("/." . dired-filter-by-extension)
     ("; n" . dired-get-file-name-without-path)
     ("; N" . dired-get-file-name-with-path)
     ("; p" . dired-get-file-name-only-path))
   dired-mode-map)

  ;; ----------------------------------------------------------
  ;; Enable global dired async mode
  (dired-async-mode 1)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'dired-after-readin-hook 'dired-custom-sort)

  (add-hook 'dired-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable dired omit mode
              (dired-omit-mode 1)

              ;; Enable all the icons dired mode
              (when (display-graphic-p)
                (all-the-icons-dired-mode 1)))))

(eval-after-load "dired" '(dired-settings))

;; ==================================================================================
;;Customized settings for `vterm'
(defun vterm-settings ()
  "Settings for `vterm'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `vterm'
  (lazy-set-key
   '(("C-g" . vterm--self-insert))
   vterm-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Disable auto fill mode
              (auto-fill-mode -1)

              ;; Disable yasnippet mode
              (yas-minor-mode -1)

              ;; Disable company mode
              (company-mode -1))))

(eval-after-load "vterm" '(vterm-settings))

;; ==================================================================================
(defun git-user-name ()
  "Get git user name."
  (interactive)
  (message (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.name"))))

(defun git-user-email ()
  "Get git user email."
  (interactive)
  (message (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.email"))))

;; Customized settings for `magit'
(defun magit-settings ()
  "Settings for `magit'."

  ;; Require
  (require 'magit-diff)

  ;; ----------------------------------------------------------
  ;; Customized `magit-diff' related faces
  (custom-set-faces
   '(magit-diff-added ((t (:background "#919191" :foreground "white"))))
   '(magit-diff-removed ((t (:background "#474747" :foreground "white"))))
   '(magit-diff-added-highlight ((t (:background "#B22222" :foreground "white"))))
   '(magit-diff-removed-highlight ((t (:background "#98FB98" :foreground "black"))))
   '(magit-diff-hunk-heading-highlight ((t (:background "#383838" :foreground "white"))))))

(eval-after-load "magit" '(magit-settings))

;; ==================================================================================
;; Customized settings for `go-translate'
(defun go-translate-settings ()
  "Settings for `go-translate'."

  ;; Require
  (require 'gts-core)
  (require 'go-translate)

  ;; ----------------------------------------------------------
  ;; Customize `gts-core' related variables
  (customize-set-variable 'gts-translate-list '(("en" "zh")))

  ;; Customize `go-translate' related variables
  (customize-set-variable 'gts-default-translator
                          (gts-translator
                           :picker (gts-prompt-picker)
                           :engines (list (gts-bing-engine) (gts-google-engine))
                           :render (gts-buffer-render))))

(eval-after-load "go-translate" '(go-translate-settings))

;; ==================================================================================
;; Customize settings for `org-roam'
(defun org-roam-settings ()
  "Settings for `org-roam'."

  ;; Require
  (require 'org-roam-node)
  (require 'org-roam-protocol)

  ;; ----------------------------------------------------------
  ;; Customize `org-roam' related variables
  (customize-set-variable 'org-roam-directory "~/OrgRoamNotes")

  ;; Customize `org-roam-node' related variables
  (customize-set-variable 'org-roam-node-display-template
                          (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

(eval-after-load "org-roam" '(org-roam-settings))

;; ==================================================================================
;; Settings for xwidget
(defun xwidget-settings ()
  "Settings for `xwidget'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `xwidget-webkit-mode'
  (lazy-set-key
   '(("n" . xwidget-webkit-scroll-up-line)
     ("p" . xwidget-webkit-scroll-down-line))
   xwidget-webkit-mode-map))

(eval-after-load "xwidget" '(xwidget-settings))

;; ==================================================================================
(defun eww-bing/search ()
  "Search the web for the text in the region or at the point by
bing search engine."
  (interactive)
  (let ((eww-search-prefix "https://cn.bing.com/search?q="))
    (call-interactively 'eww-search-words)))

(defun eww-google/search ()
  "Search the web for the text in the region or at the point by
google search engine."
  (interactive)
  (let ((eww-search-prefix "https://www.google.com/search?q="))
    (call-interactively 'eww-search-words)))

(defun eww-github/search ()
  "Search the web for the text in the region or at the point by
github search engine."
  (interactive)
  (let ((eww-search-prefix "https://github.com/search?q="))
    (call-interactively 'eww-search-words)))

(defun eww-wiki/search ()
  "Search the web for the text in the region or at the point by
wiki search engine."
  (interactive)
  (let ((eww-search-prefix "https://en.wikipedia.org/wiki/"))
    (call-interactively 'eww-search-words)))

(defun eww-ebook/search ()
  "Search the ebook the text in the region or at the point by
z-lib search engine."
  (interactive)
  (let ((eww-search-prefix "https://z-lib.is/s?q="))
    (call-interactively 'eww-search-words)))

(defun eww-quit ()
  "Quit and kill current eww buffer."
  (interactive)
  (unless (derived-mode-p 'eww-mode)
    (error "Not a eww buffer."))
  (kill-this-buffer))

;; Customized settings for `eww'
(defun eww-settings ()
  "Settings for `eww'."

  ;; Require
  (require 'browse-url)
  (require 'shr)
  (require 'eww-lnum)
  (require 'mixed-pitch)

  ;; ----------------------------------------------------------
  (defun eww-rename-buffer ()
    "Rename `eww-mode' buffer so sites open in new page."
    (let ((title (plist-get eww-data :title)))
      (when (eq major-mode 'eww-mode )
        (if title
            (rename-buffer (concat "EWW " title ) t)
          (rename-buffer "eww" t)))))

  ;; ----------------------------------------------------------
  ;; Customize `shr' related variables
  (customize-set-variable 'shr-use-fonts nil)
  (customize-set-variable 'shr-width 120)

  ;; Customize `eww-lnum' related variables
  (customize-set-variable 'eww-lnum-quick-browsing nil)

  ;; Customize `eww' related variables
  (customize-set-variable 'eww-search-prefix "https://cn.bing.com/search?q=")

  ;; ----------------------------------------------------------
  ;; Key bindings for `eww'
  (lazy-set-key
   '(("r" . eww-reload)
     ("b" . eww-add-bookmark)
     ("B" . eww-list-bookmarks)
     ("n" . next-line)
     ("p" . previous-line)
     ("N" . eww-forward-url)
     ("P" . eww-back-url)
     ("h" . eww-list-histories)
     ("j" . eww-open-in-new-buffer)
     ("d" . eww-download)
     ("o" . eww-lnum-follow)
     ("s" . eww-search-words)
     ("F" . eww-toggle-fonts)
     ("C" . eww-toggle-colors)
     ("e" . eww-browse-with-external-browser)
     ("x" . eww-quit))
   eww-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'eww-mode-hook
            (lambda ()
              ;; -----------------------------------------------
              ;; Enable mixed pitch mode
              (mixed-pitch-mode 1)))

  (add-hook 'eww-after-render-hook 'eww-rename-buffer))

(eval-after-load "eww" '(eww-settings))

;; ==================================================================================
;; Customized settings for `auth-source'
(defun auth-source-settings ()
  "Settings for `auth-source'."

  ;; Require
  (require 'auth-source-pass)

  ;; ----------------------------------------------------------
  ;; Enable auth source pass
  (auth-source-pass-enable))

(eval-after-load "auth-source" '(auth-source-settings))

;; ==================================================================================
;; Customized settings for `mu4e'
(defun mu4e-settings ()
  "Settings for `mu4e'."

  ;; Require
  (require 'message)
  (require 'mm-encode)
  (require 'simple)
  (require 'smtpmail)
  (require 'mu4e-vars)
  (require 'mu4e-headers)
  (require 'mu4e-view)
  (require 'mu4e-compose)
  (require 'mu4e-context)
  (require 'mu4e-alert)

  ;; ----------------------------------------------------------
  ;; Customize `mu4e-vars' related faces
  (custom-set-faces
   '(mu4e-unread-face ((t (:foreground "#54ff9f"))))
   '(mu4e-header-highlight-face ((t :foreground "#ff83fa" :underline t))))

  ;; ----------------------------------------------------------
  ;; Customize `message' related variables
  (customize-set-variable 'message-kill-buffer-on-exit t)
  (customize-set-variable 'message-citation-line-function
                          'message-insert-formatted-citation-line)
  (customize-set-variable 'message-send-mail-function 'smtpmail-send-it)

  ;; Customize `mm-encode' related variables
  (customize-set-variable 'mm-sign-option 'guided)

  ;; Customize `simple' related composition package
  (customize-set-variable 'mail-user-agent 'mu4e-user-agent)

  ;; Customize `smtpmail' related variables
  (customize-set-variable 'smtpmail-debug-info t)
  (customize-set-variable 'smtpmail-stream-type 'starttls)

  ;; Customize `mu4e-vars' related variables
  (customize-set-variable 'mu4e-get-mail-command
                          (format "INSIDE_EMACS=%s mbsync -a" emacs-version))
  (customize-set-variable 'mu4e-update-interval 300)
  (customize-set-variable 'mu4e-completing-read-function 'completing-read)
  (customize-set-variable 'mu4e-change-filenames-when-moving t)
  (customize-set-variable 'mu4e-context-policy 'pick-first)
  (customize-set-variable 'mu4e-display-update-status-in-modeline t)
  (customize-set-variable 'mu4e-compose-complete-addresses t)
  (customize-set-variable 'mu4e-index-update-error-warning nil)

  (customize-set-variable 'mu4e-attachment-dir "~/Downloads")
  (customize-set-variable 'mu4e-refile-folder "/Archive")
  (customize-set-variable 'mu4e-sent-folder "/Sent")
  (customize-set-variable 'mu4e-drafts-folder "/Drafts")
  (customize-set-variable 'mu4e-trash-folder "/Trash")

  (customize-set-variable 'mu4e-headers-fields '((:human-date . 20)
                                                 (:flags . 6)
                                                 (:mailing-list . 10)
                                                 (:from . 22)
                                                 (:subject . nil)))
  (customize-set-variable 'mu4e-headers-date-format "%d-%m-%Y %H:%M")
  (customize-set-variable 'mu4e-headers-include-related t)

  (customize-set-variable 'mu4e-view-show-addresses t)
  (customize-set-variable 'mu4e-view-show-images t)
  (customize-set-variable 'mu4e-use-fancy-chars t)

  (customize-set-variable 'mu4e-compose-context-policy nil)
  (customize-set-variable 'mu4e-compose-dont-reply-to-self t)
  (customize-set-variable 'mu4e-compose-keep-self-cc nil)

  ;; Customize `mu4e-context' related variables
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Personal"
            :enter-func (lambda ()
                          (mu4e-message "Entering personal context")
                          (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                            (revert-buffer)))
            :leave-func (lambda ()
                          (mu4e-message "Leaving personal context")
                          (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                            (revert-buffer)))
            :match-func (lambda (msg)
                          (when msg
                            (or (mu4e-message-contact-field-matches msg :to "lizhengyu419@outlook")
                                (mu4e-message-contact-field-matches msg :from "lizhengyu419@outlook")
                                (mu4e-message-contact-field-matches msg :cc "lizhengyu419@outlook")
                                (mu4e-message-contact-field-matches msg :bcc "lizhengyu419@outlook"))))
            :vars '((user-mail-address . "lizhengyu419@outlook.com")
                    (smtpmail-smtp-user . "lizhengyu419@outlook.com")
                    (smtpmail-smtp-server . "smtp-mail.outlook.com")
                    (smtpmail-smtp-service . 587 )
                    (mu4e-compose-signature . "Best Wishes\nZhengyu Li")))))

  ;; ----------------------------------------------------------
  ;; Enable desktop notifications for unread emails
  (mu4e-alert-enable-notifications)

  ;; Enable display of unread emails in mode-line
  (mu4e-alert-enable-mode-line-display)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Set buffer column width
              (set-fill-column 75))))

(eval-after-load "mu4e" '(mu4e-settings))

(autoload 'mu4e "mu4e" "start mu4e, then show the main view" t)

;; ==================================================================================
;; Require
(require 'awesome-chatgpt-prompts)

(defun gpt-new ()
  "Create a new gptel session."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'gptel)))

;; Settings for `gptel'
(defun gptel-settings ()
  "Settings for `gptel'."

  ;; ----------------------------------------------------------
  ;; Customize `gptel' related variables
  (customize-set-variable 'gptel-default-mode 'markdown-mode)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'gptel-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Set buffer column width to 1000
              (setq-local fill-column 1000)

              ;; Disable yasnippet mode
              (yas-minor-mode -1)

              ;; Disable company mode
              (company-mode -1))))

(eval-after-load "gptel" '(gptel-settings))

(autoload 'gptel-request "gptel" "Request a response from ChatGPT for PROMPT.")

;; ==================================================================================
;; Settings for `hackernews'
(defun hackernews-settings()
  "Settings for `hackernews'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `hackernews'
  (lazy-unset-key
   '("g")
   hackernews-mode-map))

(eval-after-load "hackernews" '(hackernews-settings))

;; ==================================================================================
(defun auto-package-upgrade-all ()
  "Upgrade all packages installed."
  (interactive)
  (package-refresh-contents)
  (auto-package-update-now))

;; Settings for `auto-package-update'
(defun auto-package-update-settings ()
  "Settings for `auto-package-update'."

  ;; ----------------------------------------------------------
  ;; Customize `auto-package-update' related variables
  (customize-set-variable 'auto-package-update-delete-old-versions t))

(eval-after-load "auto-package-update" '(auto-package-update-settings))

;; ==================================================================================
;; Alias
(defalias 'git-status 'magit-status)
(defalias 'git-log 'magit-log-all)

(defalias 'email 'mu4e)

(defalias 'gpt 'gptel)

(defalias 'refresh-auth-cache 'auth-source-forget-all-cached)

(defalias 'upgrade-packages 'auto-package-upgrade-all)

(defalias 'increase-text 'text-scale-increase)
(defalias 'decrease-text 'text-scale-decrease)

;; ==================================================================================
;; Hooks
(add-hook 'before-save-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Update timestamp
            (time-stamp)

            ;; Update copyright
            (copyright-update)

            ;; Delete trailing white space
            (delete-trailing-whitespace)))

;; Setup dashboard startup hook
;; Be careful, `dashboard-setup-startup-hook' will update `after-init-hook' and
;; `emacs-startup-hook' hooks, so, if you want the dashboard to take affection at
;; the end of startup, you should place the following line configuration before
;; all other `after-init-hook' configurations.
(dashboard-setup-startup-hook)

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
            (customize-set-variable 'recentf-max-saved-items 200)
            (customize-set-variable 'recentf-exclude '((expand-file-name package-user-dir)
                                                       ".cache"
                                                       ".cask"
                                                       ".elfeed"
                                                       "bookmarks"
                                                       "cache"
                                                       "ido.*"
                                                       "persp-confs"
                                                       "recentf"
                                                       "undo-tree-hist"
                                                       "url"
                                                       "COMMIT_EDITMSG\\'"))

            ;; Customize `files' related variables
            (customize-set-variable 'backup-directory-alist
                                    '((".*" . "~/.emacs.d/backup-files")))
            (customize-set-variable 'backup-by-copying t)
            (customize-set-variable 'delete-old-versions t)
            (customize-set-variable 'version-control t)

            ;; Customize `uniquify' realted variables
            (customize-set-variable 'uniquify-separator "/")
            (customize-set-variable 'uniquify-buffer-name-style 'forward)

            ;; Disable ring bell
            (customize-set-variable 'ring-bell-function 'ignore)

            ;; Customize user and email
            (customize-set-variable 'user-full-name emacs-config-user)
            (customize-set-variable 'user-mail-address emacs-config-email)

            ;; Replace yes-or-no-p with y-or-no-p
            (fset 'yes-or-no-p 'y-or-n-p)

            (when (featurep 'xwidget-internal)
              (customize-set-variable 'browse-url-browser-function 'xwidget-webkit-browse-url))

            ;; Customize `mac' system realted variables
            (when (memq window-system '(mac ns))
              (customize-set-variable 'mac-command-modifier 'super)
              (customize-set-variable 'mac-option-modifier 'meta))

            ;; ----------------------------------------------------------
            ;; Remap '<return>' to 'RET'
            (define-key key-translation-map (kbd "<return>") (kbd "RET"))

            ;; Global key bindings for basic config
            (lazy-set-key
             '(;; Kill current buffer
               ("C-x k" . kill-this-buffer)
               ;; Activate mark
               ("M-m" . set-mark-command)
               ;; Ibuffer
               ("C-x C-b" . ibuffer)
               ;; Smart edit
               ("C-x <tab>" . smart-indent)
               ("C-x TAB" . smart-indent)
               ("M-w" . smart-copy)
               ("M-k" . smart-kill)
               ;; Expand region
               ("M-M" . er/expand-region)
               ;; Move text
               ("C-S-p" . move-text-up)
               ("C-S-n" . move-text-down)
               ;; Multi cursors
               ("C-x m" . set-rectangular-region-anchor)
               ("C-x M" . mc/mark-all-dwim)
               ;; Browse kill ring
               ("M-Y" . browse-kill-ring)
               ;; Goto last change
               ("M-o" . goto-last-change)
               ("M-g g" . goto-line-preview)
               ("M-g M-g" . goto-line-preview)
               ;; Swiper
               ("C-s" . swiper-isearch)
               ("C-r" . swiper-isearch-backward)
               ;; Counsel
               ("M-x" . counsel-M-x)
               ("C-x b" . counsel-switch-buffer)
               ("C-x B" . counsel-recentf)
               ("C-x C-f" . counsel-find-file)
               ("C-h f" . counsel-describe-function)
               ("C-h v" . counsel-describe-variable)
               ("C-h o" . counsel-describe-symbol)
               ("C-h b" . counsel-descbinds)
               ;; Color moccur
               ("C-x C-u" . occur-by-moccur)
               ;; Ag
               ("C-x g" . ag)
               ("C-x G" . ag-project)
               ("C-x f" . ag-dired-regexp)
               ("C-x F" . ag-project-dired-regexp)
               ;; Avy
               ("C-; c" . avy-goto-char)
               ("C-; w" . avy-goto-word-0)
               ("C-; l" . avy-goto-line)
               ;; Flyspell correct
               ("C-: c" . flyspell-correct-wrapper)
               ("C-: p" . flyspell-correct-previous)
               ("C-: n" . flyspell-correct-next)
               ;; Switch window
               ("C-x o" . switch-window)
               ;; Scale text
               ("C-x =" . text-scale-increase)
               ("C-x _" . text-scale-decrease)
               ("C-x +" . text-scale-increase)
               ("C-x -" . text-scale-decrease)
               ;; Undo window layout
               ("C-_" . winner-undo)
               ;; Multi vterm
               ("C-x C-t" . multi-vterm)
               ;; Dired
               ("C-x C-d" . counsel-dired)
               ("C-x d" . counsel-dired-jump)
               ;; Go translate
               ("C-x C-p" . gts-do-translate)
               ;; Org roam
               ("C-c n f" . org-roam-node-find)
               ("C-c n i" . org-roam-node-insert)
               ("C-c n g" . org-roam-graph)
               ("C-c n c" . org-roam-capture)
               ("C-c n l" . org-roam-buffer-toggle)
               ("C-c n j" . org-roam-dailies-capture-today)
               ;; Eww
               ("C-x C-g" . eww-search-words)
               ;; ChatGPT
               ("C-RET" . gptel-send)
               ("C-<return>" . gptel-send)
               ("C-S-RET" . gptel-menu)
               ("C-S-<return>" . gptel-menu)))

            ;; ----------------------------------------------------------
            ;; Initialize mac system exec path
            (when (memq window-system '(mac ns))
              (require 'exec-path-from-shell)
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

            ;; Enable save place mode
            (save-place-mode 1)

            ;; Enable global column number mode
            (column-number-mode 1)

            ;; Enable global just-in-time lock mode
            (jit-lock-mode 1)

            ;; Enable global beacon mode
            (beacon-mode 1)

            ;; Enable global smooth scrolling mode
            (smooth-scrolling-mode 1)

            ;; Enable global which key mode
            (which-key-mode 1)

            ;; Enable global undo tree mode
            (global-undo-tree-mode 1)

            ;; Enable global ivy mode
            (ivy-mode 1)

            ;; Enable global counsel mode
            (counsel-mode 1)

            ;; Enable global counsel projectile mode
            (counsel-projectile-mode 1)

            ;; Enable global yasnippet mode
            (yas-global-mode 1)

            ;; Enable global company mode
            (global-company-mode 1)

            ;; Enable global autoinsert mode
            (auto-insert-mode 1)

            ;; Enable winum mode
            (winum-mode 1)

            ;; Enable winner mode
            (winner-mode 1)

            ;; Enable centaur tabs mode
            (centaur-tabs-mode 1)

            ;; Enable zoom mode
            ;; (zoom-mode 1)

            ;; Enable dimmer mode
            ;; (dimmer-mode 1)

            ;; Enable global emojify mode
            (global-emojify-mode 1)

            ;; Enable doom modeline
            (doom-modeline-mode 1)

            ;; Enable global emojify modeline mode
            (global-emojify-mode-line-mode 1)

            ;; Enable modeline display mode
            (display-time-mode 1)

            ;; Enable org roam db auto sync mode
            ;; (org-roam-db-autosync-mode 1)

            ;; Start pinentry server
            (pinentry-start)))

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Enable global textsize mode
            (textsize-mode 1)

            ;; Load doom theme
            (load-theme 'doom-xcode t)

            ;; Toggle fullscreen
            (toggle-fullscreen)))

(add-hook 'window-configuration-change-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Adjust window split thresholds
            (adjust-window-split-thresholds)))

;; ==================================================================================
;;; Provide features
(provide 'init-base)

;;; init-base.el ends here
