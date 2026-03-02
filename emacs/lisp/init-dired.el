;;; init-dired.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 22:36:29 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: dired, dirvish, file manager
;; Dependencies: init-funcs, dired-custom-extension

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

;;; Code:

;; ==================================================================================
(use-package dirvish
  :ensure t
  :defer t)

;; ==================================================================================
;; Core Dired Configuration
;; Built-in directory editor with optimized settings for Dirvish compatibility
(use-package dired
  :ensure nil
  :defer t
  :hook ((dired-mode . dired-omit-mode)
         (dired-mode . dired-async-mode))
  :bind
  (("C-x j" . dired-jump)
   (:map dired-mode-map
         ;; --------------------------------------------------------------------------
         ;; Navigation keys
         ;; Open file or directory in current buffer (single buffer mode)
         ("<return>" . dired-single-buffer)
         ("RET" . dired-single-buffer)
         ;; Go to parent directory in single buffer mode
         ("h" . dired-up-directory-single)
         ;; Navigate to previous/next file
         ("p" . dired-hacks-previous-file)
         ("n" . dired-hacks-next-file)
         ;; Jump to first/last line in directory listing
         ("M-{" . dired-goto-first-line)
         ("M-}" . dired-goto-last-line)
         ;; Toggle omit mode
         ("M-o" . dired-omit-mode)

         ;; --------------------------------------------------------------------------
         ;; File operations
         ;; View file in read-only mode
         ("v" . dired-view-file)
         ;; Delete marked files
         ("C-k" . dired-do-delete)
         ;; Edit filenames in-place (wdired mode)
         ("r" . wdired-change-to-wdired-mode)
         ;; Update file timestamp (touch)
         ("E" . dired-do-touch)
         ;; Create backup of marked files
         ("B" . dired-backup-file)
         ;; Diff file changes
         ("d" . dired-diff)
         ;; Diff two directories
         ("D" . ediff-directories)
         ;; Compress files
         ("z" . dired-do-compress)
         ("Z" . dired-do-compress)

         ;; --------------------------------------------------------------------------
         ;; Cryptography (GPG/PGP operations)
         ;; Encrypt file with GPG
         (": e" . epa-dired-do-encrypt)
         ;; Decrypt GPG encrypted file
         (": d" . epa-dired-do-decrypt)
         ;; Sign file with GPG
         (": s" . epa-dired-do-sign)
         ;; Verify GPG signature
         (": v" . epa-dired-do-verify)

         ;; --------------------------------------------------------------------------
         ;; Copy/Cut/Paste (clipboard operations)
         ;; Copy marked files to clipboard
         ("M-w" . dired-copy-files)
         ;; Cut marked files to clipboard
         ("M-k" . dired-cut-files)
         ;; Paste files from clipboard
         ("C-y" . dired-paste-files)

         ;; --------------------------------------------------------------------------
         ;; File path utilities (copy to clipboard)
         ;; Copy filename without path to clipboard
         ("; n" . dired-get-file-name-without-path)
         ;; Copy full file path to clipboard
         ("; N" . dired-get-file-name-with-path)
         ;; Copy directory path only to clipboard
         ("; p" . dired-get-file-name-only-path)))

  :config
  ;; Load custom Dired extensions (with safety check)
  (require 'dired-custom-extension)

  (setq
   ;; --------------------------------------------------------------------------
   ;; Configure copy/move/delete behavior and confirmation prompts
   ;; Smart target selection: use other dired buffer as target for copy/move operations
   dired-dwim-target t
   ;; Allow recursive copy operations without additional confirmation prompts
   dired-recursive-copies 'always
   ;; Allow recursive delete operations without additional confirmation prompts
   dired-recursive-deletes 'always
   ;; Use simple y/n confirmation for deletion (instead of full yes/no prompts)
   dired-deletion-confirmer 'y-or-n-p

   ;; --------------------------------------------------------------------------
   ;; Auto-revert dired buffer only for local directories (performance optimization for remote files)
   dired-auto-revert-buffer (not (file-remote-p default-directory))

   ;; --------------------------------------------------------------------------
   ;; Customize which files/directories are hidden in Dired buffer
   ;; Core omit filter rules (fixed regex with exact matching)
   dired-omit-files
   (concat
    ;; Hide all hidden files/directories starting with . (e.g., .git, .env)
    "^\\.\\|"
    ;; Hide compiled Python/Emacs Lisp files (exact suffix match)
    "\\.pyc\\'\\|\\.elc\\'\\|"
    ;; Hide compiled object/class files
    "\\.o\\'\\|\\.class\\'\\|"
    ;; Hide jar, log, and lock files (exact match)
    "\\.jar\\'\\|\\.log\\'\\|\\.lock\\'\\|"
    ;; Hide specified directories (exact name match to avoid false positives)
    "\\(node_modules\\|__pycache__\\|.venv\\|venv\\|dist\\|build\\|target\\)\\'\\|"
    ;; Hide macOS special file (exact match)
    "\\.DS_Store\\'")
   ;; Supplementary file extension filter (aligned with omit-files, redundant but as fallback)
   dired-omit-extensions '(".pyc" ".elc" ".o" ".class" ".jar" ".log" ".lock")
   ;; Enable toggle shortcut to show/hide omitted files
   dired-omit-toggle-show-hidden t
   ;; Hide ellipsis marker (...) in dired buffer for cleaner interface
   dired-omit-verbose nil)

  ;; --------------------------------------------------------------------------
  ;; Configure how directories/files are displayed in Dired buffer
  ;; Default ls switches: show all files, long format, human-readable sizes, directories first
  (if (executable-find "gls")
      (setq insert-directory-program "gls"
            dired-listing-switches "-alh --group-directories-first")
    (setq dired-listing-switches "-alh"))

  (dirvish-override-dired-mode 1))

;; ==================================================================================
;;; Provide features
(provide 'init-dired)

;;; init-dired.el ends here
