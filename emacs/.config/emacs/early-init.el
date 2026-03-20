;;; early-init.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: emacs, config
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
;; 2026-03-16 11:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Early initialization file for Emacs 27+.
;; This file is loaded before the package system is initialized,
;; making it the correct place to set package-user-dir and user-emacs-directory.
;;
;; XDG variables defined here can be used directly in init.el.

;;; Code:

;; ============================================================================
;; XDG Base Directory Configuration
;; Must be set before package system initializes (Emacs 27+ auto-initializes)

;; Define XDG directories as global variables for use in init.el
(defvar omw/xdg-data-home (or (getenv "XDG_DATA_HOME")
                              (expand-file-name "~/.local/share/"))
  "XDG data home directory.")

(defvar omw/xdg-cache-home (or (getenv "XDG_CACHE_HOME")
                               (expand-file-name "~/.cache/"))
  "XDG cache home directory.")

(defvar omw/xdg-state-home (or (getenv "XDG_STATE_HOME")
                               (expand-file-name "~/.local/state/"))
  "XDG state home directory.")

;; Emacs directory configuration
(setq user-emacs-directory (expand-file-name "emacs/" omw/xdg-data-home))

;; Package installation directory
(setq package-user-dir (expand-file-name "emacs/elpa/" omw/xdg-data-home))

;; ============================================================================
;;; Provide features
(provide 'early-init)

;;; early-init.el ends here
