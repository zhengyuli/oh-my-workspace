;;; package --- init.el ---
;; Time-stamp: <2022-03-16 13:44:00 Wednesday by zhengyuli>

;; Copyright (C) 2021, 2022 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
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
;;   (require 'init)

;;; Require:
(load-library "package")

;;; Code:
;; ==================================================================================
;; Emacs configuration root path
(defvar emacs-config-root-path "_EMACS_CONFIG_ROOT_PATH_"
  "Emacs configuration root path.")

;; Emacs configuration custom path to load custom settings
(defvar emacs-config-custom-path (concat emacs-config-root-path "custom/")
  "Emacs configuration custom path.")

;; Emacs configuration site packages path to load 3rd party settings
(defvar emacs-config-site-packages-path (concat emacs-config-root-path "site-packages/")
  "Emacs configuration site packages path.")

;; Emacs configuration fixed font
(defvar emacs-config-fixed-font "Source Code Pro" "Emacs configuration fixed font.")

;; Emacs configuration fixed serif font
(defvar emacs-config-fixed-serif-font "Source Serif Pro" "Emacs configuration fixed serif font.")

;; Emacs configuration font size
(defvar emacs-config-font-size 150 "Emacs configuration font size.")

;; Emacs configuration user
(defvar emacs-config-user "_EMACS_CONFIG_USER_" "Emacs configuration user.")

;; Emacs configuration email
(defvar emacs-config-email "_EMACS_CONFIG_EMAIL_" "Emacs configuration email.")

;; ==================================================================================
(defun font-exists-p (font)
  "Check if font exists."
  (if (display-graphic-p)
      (if (null (x-list-fonts font)) nil t)
    t))

(defun add-subdirs-to-load-path (base-dir)
  "Add subdirs to load path.
Look up all subdirs under `BASE-DIR' recrusively and add them into load path."
  (let ((default-directory base-dir))
    (add-to-list 'load-path base-dir)
    (normal-top-level-add-subdirs-to-load-path)))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (package-install package)))
   packages))

(defun lazy-set-key (key-alist &optional keymap key-prefix)
  "This function is to little type when define key binding.
`KEYMAP' is a add keymap for some binding, default is `current-global-map'.
`KEY-ALIST' is a alist contain main-key and command.
`KEY-PREFIX' is a add prefix for some binding, default is nil."
  (let (key def)
    (or keymap (setq keymap (current-global-map)))
    (if key-prefix
        (setq key-prefix (concat key-prefix " "))
      (setq key-prefix ""))
    (dolist (element key-alist)
      (setq key (car element))
      (setq def (cdr element))
      (cond ((stringp key) (setq key (read-kbd-macro (concat key-prefix key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key def))))

(defun lazy-unset-key (key-list &optional keymap)
  "This function is to little type when unset key binding.
`KEYMAP' is add keymap for some binding, default is `current-global-map'
`KEY-LIST' is list contain key."
  (let (key)
    (or keymap (setq keymap (current-global-map)))
    (dolist (key key-list)
      (cond ((stringp key) (setq key (read-kbd-macro (concat key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key nil))))

;; ==================================================================================
;; Basic check
(unless (>= (string-to-number emacs-version) 27.1)
  (error "The Emacs version must be >= 27.1."))

(unless (font-exists-p "Source Code Pro")
  (error "Missing \"Source Code Pro\" font, please install."))

(unless (font-exists-p "Source Serif Pro")
  (error "Missing \"Source Serif Pro\" font, please install."))

;; ==================================================================================
;; Add custom directory to load-path
(add-subdirs-to-load-path emacs-config-custom-path)
(add-subdirs-to-load-path emacs-config-site-packages-path)

;; Initialize package manager
;; Add package archives
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize packages
(package-initialize)

;; Refresh package list if any
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install packages
(ensure-package-installed
 ;; ==============================
 ;; init-base.el
 'beacon
 'smooth-scrolling
 ;; ******************************
 'exec-path-from-shell
 ;; ******************************
 'expand-region
 'multiple-cursors
 'visual-regexp-steroids
 ;; ******************************
 'which-key
 'undo-tree
 'browse-kill-ring
 'goto-chg
 ;; ******************************
 'all-the-icons
 ;; ******************************
 'ivy
 'ivy-rich
 'ivy-prescient
 'all-the-icons-ivy-rich
 ;; ******************************
 'counsel
 'counsel-projectile
 ;; ******************************
 'swiper
 ;; ******************************
 'color-moccur
 ;; ******************************
 'ag
 'wgrep-ag
 ;; ******************************
 'avy
 ;; ******************************
 'popup
 ;; ******************************
 'yasnippet
 ;; ******************************
 'company
 'company-prescient
 'company-box
 'company-quickhelp
 'company-quickhelp-terminal
 ;; ******************************
 'winum
 'zoom
 'centaur-tabs
 'doom-modeline
 'doom-themes
 ;; ******************************
 'workgroups2
 ;; ******************************
 'multi-term
 ;; ******************************
 'async
 'ztree
 ;; ******************************
 'dired-single
 'dired-filter
 'dired-subtree
 'dired-hacks-utils
 'dired-filetype-face
 'all-the-icons-dired
 ;; ******************************
 'magit
 ;; ******************************
 'w3m
 ;; ==============================
 ;; init-prog-mode.el
 'phi-autopair
 'rainbow-delimiters
 'flycheck
 'flycheck-clang-tidy
 'whitespace-cleanup-mode
 'quickrun
 'dumb-jump
 ;; ******************************
 'lsp-mode
 'lsp-ui
 ;; ==============================
 ;; init-c&c++-mode.el
 'google-c-style
 ;; ==============================
 ;; init-python-mode.el
 'sphinx-doc
 'python-docstring
 'pyvenv
 ;; ==============================
 ;; init-go-mode.el
 'go-mode
 'go-eldoc
 ;; ==============================
 ;; init-elisp-mode.el
 'elisp-slime-nav
 'lisp-extra-font-lock
 'rainbow-mode
 ;; ==============================
 ;; init-haskell-mode.el
 'haskell-mode
 ;; ==============================
 ;; init-scala-mode.el
 'scala-mode
 ;; ==============================
 ;; init-groovy-mode.el
 'groovy-mode
 ;; ==============================
 ;; init-cmake-mode.el
 'cmake-mode
 ;; ==============================
 ;; init-dockerfile-mode.el
 'dockerfile-mode
 ;; ==============================
 ;; init-yaml-mode.el
 'yaml-mode
 ;; ==============================
 ;; init-markdown-mode.el
 'markdown-mode
 'markdownfmt)

;; ==================================================================================
;; Load librares
(load-library "init-base")
(load-library "init-prog-mode")
(load-library "init-cc-mode")
(load-library "init-c&c++-mode")
(load-library "init-python-mode")
(load-library "init-go-mode")
(load-library "init-elisp-mode")
(load-library "init-haskell-mode")
(load-library "init-scala-mode")
(load-library "init-groovy-mode")
(load-library "init-sh-script-mode")
(load-library "init-dockerfile-mode")
(load-library "init-cmake-mode")
(load-library "init-yaml-mode")
(load-library "init-markdown-mode")

;; Load user custom settings
(if (not (file-exists-p "~/.emacs.d/custom.el"))
    (shell-command "touch ~/.emacs.d/custom.el"))
(load-file "~/.emacs.d/custom.el")

;;; init.el ends here
