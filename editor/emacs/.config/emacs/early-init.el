;;; early-init.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>
;;
;; ============================================================================
;; early-init.el - Early initialization for XDG paths and package dirs.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: emacs, config
;; Dependencies: (none)
;;
;; Copyright (C) 2026 zhengyu li
;;
;;; History:
;;
;; 2026-03-16 11:30 zhengyu li <lizhengyu419@outlook.com> created.
;;
;;; Commentary:
;;
;; Early initialization file for Emacs 27+.
;; This file is loaded before the package system is initialized,
;; making it the correct place to set package-user-dir
;; and user-emacs-directory.
;;
;; XDG variables defined here can be used directly in init.el.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Early Initialization
;; ----------------------------------------------------------------------------

;; --- XDG Base Directory Configuration ---
;; Must be set before package system initializes (Emacs 27+ auto-initializes)
;; Evaluated once from the environment at startup; never change mid-session.
(defconst omw/xdg-data-home (or (getenv "XDG_DATA_HOME")
                                (expand-file-name "~/.local/share/"))
  "XDG data home directory ($XDG_DATA_HOME, defaults to ~/.local/share/).")

(defconst omw/xdg-cache-home (or (getenv "XDG_CACHE_HOME")
                                 (expand-file-name "~/.cache/"))
  "XDG cache home directory ($XDG_CACHE_HOME, defaults to ~/.cache/).")

(defconst omw/xdg-state-home (or (getenv "XDG_STATE_HOME")
                                 (expand-file-name "~/.local/state/"))
  "XDG state home directory ($XDG_STATE_HOME, defaults to ~/.local/state/).")

;; Emacs directory configuration
(setq user-emacs-directory (expand-file-name "emacs/" omw/xdg-data-home))

;; Package installation directory
(setq package-user-dir (expand-file-name "emacs/elpa/" omw/xdg-data-home))

;; --- Frame UI Suppression ---
;; Suppress UI elements before first frame is displayed
(dolist (mode '(tool-bar-mode
                scroll-bar-mode
                menu-bar-mode
                tooltip-mode))
  (funcall mode -1))

;; ============================================================================
;;; Provide features
(provide 'early-init)

;;; early-init.el ends here
