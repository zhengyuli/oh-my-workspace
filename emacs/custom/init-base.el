;;; package --- init-base.el -*- lexical-binding:t -*-
;; Time-stamp: <2022-03-23 10:10:54 Wednesday by zhengyuli>

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
;;   (require 'init-base)

;;; Require:

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
;; Customized settings for `visual-regexp-steroids'
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
  (customize-set-variable 'ivy-count-format "")
  (customize-set-variable 'ivy-initial-inputs-alist nil)
  (customize-set-variable 'ivy-re-builders-alist
                          '((t . ivy--regex-ignore-order)))

  ;; ----------------------------------------------------------
  ;; Enable ivy rich mode
  (ivy-rich-mode 1)

  ;; Enable ivy prescient mode
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
              (if (display-graphic-p)
                  (progn
                    (require 'company-box)

                    ;; Enable company box mode for graphic mode
                    (company-box-mode 1))
                (progn
                  (require 'company-quickhelp)
                  (require 'company-quickhelp-terminal)

                  ;; Enable company quickhelp mode
                  (company-quickhelp-mode 1)

                  ;; Enable company quickhelp terminal mode
                  (company-quickhelp-terminal-mode 1)))

              ;; Enable company prescient mode
              (company-prescient-mode 1))))

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
;; Customized settings for `centaur-tabs'
(defun centaur-tabs-settings ()
  "Settings for `centaur-tabs'."

  ;; Require
  (require 'all-the-icons)

  ;; ----------------------------------------------------------
  ;; Customized groups policy
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules."
    (list
     (cond
      ((derived-mode-p 'term-mode)
       "Term")
      ((derived-mode-p 'vterm-mode)
       "VTerm")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((derived-mode-p 'eww-mode)
       "Eww")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       mode-name)
      ((memq major-mode '(org-mode org-agenda-mode diary-mode))
       "Org")
      ((derived-mode-p 'markdown-mode)
       "Markdown")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  ;; ----------------------------------------------------------
  ;; Customize `centaur-tabs' realted variables
  (customize-set-variable 'centaur-tabs-height 25)
  (customize-set-variable 'centaur-tabs-style "bar")
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
;; Customized settings for `dashboard'
(defun dashboard-settings ()
  "Settings for `dashboard'."

  ;; ----------------------------------------------------------
  ;; Customize `dashboard' related variables
  (customize-set-variable 'dashboard-banner-logo-title
                          (format "Welcome to %s\'s Emacs" emacs-config-user))
  (customize-set-variable 'dashboard-set-heading-icons t)
  (customize-set-variable 'dashboard-set-file-icons t)
  (customize-set-variable 'dashboard-set-navigator t)
  (customize-set-variable 'dashboard-center-content t)
  (customize-set-variable 'dashboard-items
                          '((recents  . 5)
                            (bookmarks . 5)
                            (projects . 5)
                            (agenda . 5)
                            (registers . 5)))
  (customize-set-variable 'dashboard-projects-switch-function
                          'counsel-projectile-switch-project-by-name))

(eval-after-load "dashboard" '(dashboard-settings))

;; ==================================================================================
;;Customized settings for `vterm'
(defun vterm-settings ()
  "Settings for `vterm'."

  ;; ----------------------------------------------------------
  ;; Customize `terminal' related variables
  (customize-set-variable 'ring-bell-function 'ignore)

  ;; ----------------------------------------------------------
  ;; Key bindings for `vterm'
  (lazy-set-key
   '(("C-g" . vterm-send-C-g)
     ("C-u" . vterm-send-C-u))
   vterm-mode-map)

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

(eval-after-load "vterm" '(vterm-settings))

;; ==================================================================================
;; Customized settings for `dired'
(defun dired-settings ()
  "Settings for `dired'."

  ;; Require
  (require 'ztree)
  (require 'ztree-view)
  (require 'epg-config)
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
  ;; Customized magit related faces
  (custom-set-faces
   '(dired-header ((t (:foreground "#EE0000" :height 1.1))))
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
  (customize-set-variable 'dired-omit-files
                          (concat dired-omit-files
                                  "\\|^\\.\\|^semantic.cache$\\|^CVS$"))

  ;; Customize `epg-config' related variables
  (customize-set-variable 'epg-pinentry-mode 'loopback)

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
     ("C-c n" . dired-get-file-name-without-path)
     ("C-c N" . dired-get-file-name-with-path)
     ("C-c p" . dired-get-file-name-only-path))
   dired-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'dired-after-readin-hook 'dired-custom-sort)

  (add-hook 'dired-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable dired omit mode
              (dired-omit-mode 1)

              ;; Enable dired async mode
              (dired-async-mode 1)

              ;; Enable all the icons dired mode
              (when (display-graphic-p)
                (all-the-icons-dired-mode 1)))))

(eval-after-load "dired" '(dired-settings))

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

;; ==================================================================================
(defun get-git-user-name ()
  "Get git user name."
  (interactive)
  (print (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.name"))))

(defun get-git-user-email ()
  "Get git user email."
  (interactive)
  (print (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.email"))))

;; Customized settings for `magit'
(defun magit-settings ()
  "Settings for `magit'."

  ;; ----------------------------------------------------------
  ;; Customized magit related faces
  (custom-set-faces
   '(magit-diff-added ((t (:background "#919191" :foreground "white"))))
   '(magit-diff-removed ((t (:background "#474747" :foreground "white"))))
   '(magit-diff-added-highlight ((t (:background "#B22222" :foreground "white"))))
   '(magit-diff-removed-highlight ((t (:background "#FFE4C4" :foreground "black"))))))

(eval-after-load "magit" '(magit-settings))

;; ==================================================================================
;; Customized settings for `go-translate'
(defun go-translate-settings ()
  "Settings for `go-translate'."

  ;; ----------------------------------------------------------
  ;; Customize `go-translate' related variables
  (customize-set-variable 'gts-translate-list '(("en" "zh")))
  (customize-set-variable 'gts-default-translator
                          (gts-translator
                           :picker (gts-prompt-picker)
                           :engines (list (gts-bing-engine) (gts-google-engine))
                           :render (gts-buffer-render))))

(eval-after-load "go-translate" '(go-translate-settings))

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

  ;; ----------------------------------------------------------
  ;; Customize `browse-url' related variables
  (customize-set-variable 'browse-url-browser-function 'eww-browse-url)

  ;; Customize `shr' related variables
  (customize-set-variable 'shr-use-fonts nil)
  (customize-set-variable 'shr-width 120)

  ;; Customize `eww' related variables
  (customize-set-variable 'eww-search-prefix "https://www.google.com/search?q=")

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
   eww-mode-map))

(eval-after-load "eww" '(eww-settings))

;; ==================================================================================
;; Alias
(defalias 'git-status 'magit-status)
(defalias 'git-log 'magit-log-all)

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

            ;; Delete trailing whitespace
            (delete-trailing-whitespace)))

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

            ;; Replace yes-or-no-p with y-or-no-p
            (fset 'yes-or-no-p 'y-or-n-p)

            ;; Customize `mac' system realted variables
            (when (memq window-system '(mac ns))
              (customize-set-variable 'mac-command-modifier 'super)
              (customize-set-variable 'mac-option-modifier 'meta))

            ;; ----------------------------------------------------------
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
               ;; Multi cursors
               ("C-x m" . set-rectangular-region-anchor)
               ("C-x M" . mc/mark-all-dwim)
               ;; Browse kill ring
               ("M-Y" . browse-kill-ring)
               ;; Goto last change
               ("M-o" . goto-last-change)
               ;; Counsel
               ("M-x" . counsel-M-x)
               ("C-x b" . counsel-switch-buffer)
               ("C-x B" . counsel-recentf)
               ("C-x C-f" . counsel-find-file)
               ;; Swiper
               ("C-s" . swiper-isearch)
               ("C-r" . swiper-isearch-backward)
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
               ;; Switch window
               ("C-x o" . switch-window)
               ;; Helpful
               ("C-h f" . helpful-callable)
               ("C-h v" . helpful-variable)
               ("C-h k" . helpful-key)
               ;; Scale text
               ("C-x =" . text-scale-increase)
               ("C-x _" . text-scale-decrease)
               ("C-x +" . text-scale-increase)
               ("C-x -" . text-scale-decrease)
               ;; Vterm
               ("C-x C-t" . vterm)
               ;; Dired
               ("C-x C-d" . dired)
               ("C-x d" . dired-jump)
               ;; Go translate
               ("C-x C-p" . gts-do-translate)
               ;; Eww
               ("C-x C-g" . eww-search-words)))

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

            ;; Enable global column number mode
            (column-number-mode 1)

            ;; Enable global just-in-time lock mode
            (jit-lock-mode 1)

            ;; Enable prettify symbol mode
            (prettify-symbols-mode 1)

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

            ;; Enable centaur tabs mode
            (centaur-tabs-mode 1)

            ;; Enable doom modeline
            (doom-modeline-mode 1)))

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Load doom theme
            (load-theme 'doom-xcode t)

            ;; Toggle fullscreen
            (toggle-fullscreen)))

;; Setup `'dashboard' startup hook
(dashboard-setup-startup-hook)

;; ==================================================================================
;;; Provide features
(provide 'init-base)

;;; init-base.el ends here
