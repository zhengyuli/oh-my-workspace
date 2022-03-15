;;; package --- init-dired.el ---
;; Time-stamp: <2022-03-15 14:33:42 Tuesday by zhengyuli>

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
;;   (require 'init-dired)

;;; Require:

;;; Code:
;; ==================================================================================
(defun dired-settings ()
  "Settings for `dired'."

  ;; Require
  (require 'dired-x)
  (require 'dired-async)
  (require 'dired-single)
  (require 'dired-filter)
  (require 'dired-subtree)
  (require 'dired-isearch)
  (require 'dired-copy-paste)
  (require 'dired-hacks-utils)
  (require 'dired-filetype-face)
  (require 'all-the-icons-dired)
  (require 'ztree)
  (require 'ztree-view)
  (require 'epg-config)
  (require 'epa-dired)

  ;; ----------------------------------------------------------
  (defun dired-goto-first-line ()
    "Dired go to the first line."
    (interactive)
    (dired-revert)
    (beginning-of-buffer)
    (dired-hacks-next-file))

  (defun dired-goto-last-line ()
    "Dired go to the first line."
    (interactive)
    (dired-revert)
    (end-of-buffer)
    (dired-hacks-previous-file))

  (defun dired-up-directory-single ()
    "Return up directory in single window."
    (interactive)
	(let ((dir (dired-current-directory)))
	  (dired-single-buffer "..")
	  (dired-goto-file dir)))

  (defun dired-backup-file ()
    "Backup file in current directory."
    (interactive)
    (let* ((origin-files (dired-get-marked-files)))
      (dolist (origin-file origin-files)
        (condition-case nil
            (if (file-directory-p origin-file)
                (copy-directory origin-file (concat origin-file ".back") 1)
              (copy-file origin-file (concat origin-file ".back") 1))
          (error nil)))
      (dired-revert)))

  (defun dired-get-size ()
    "Get total size of marked files."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
        (message "Size of all marked files: %s"
                 (progn
                   (re-search-backward "^\\s-*\\([0-9.,]+[A-Za-z]+\\).*\\(total\\|总用量\\)$")
                   (match-string 1))))))

  (defun dired-get-file-name (with-full-path only-path)
    "Dired get file name.
If `WITH-FULL-PATH' is nil and `ONLY-PATH' is nil, return only file name,
If `WITH-FULL-PATH' is t and `ONLY-PATH' is nil, return full file path,
If `WITH-FULL-PATH' is nil and `ONLY-PATH' is t, return only file path,
If `WITH-FULL-PATH' is t and `ONLY-PATH' is t, return only file path."
    (let ((clipboard))
      (if only-path
          (if (equal 'windows-nt system-type)
              (setq clipboard (dired-replace-in-string "/" "\\" (dired-current-directory)))
            (setq clipboard (dired-current-directory)))
        (let ((file (dired-get-file-for-visit)))
          (if with-full-path
              (progn
                (if (file-directory-p file)
                    (setq file (concat file "/")))
                (if(equal 'windows-nt system-type)
                    (setq clipboard (dired-replace-in-string "/" "\\" file))
                  (setq clipboard file)))
            (if (file-directory-p file)
                (setq clipboard (file-name-nondirectory file))
              (setq clipboard (file-name-nondirectory file))))))

      (kill-new clipboard)
      (message "copy string \"%s\" to clipboard" clipboard)))

  (defun dired-get-file-name-with-path ()
    "Dired get file name with path."
    (interactive)
    (dired-get-file-name t nil))

  (defun dired-get-file-name-without-path ()
    "Dired get file name without path."
    (interactive)
    (dired-get-file-name nil nil))

  (defun dired-get-file-name-only-path ()
    "Dired get file name only path."
    (interactive)
    (dired-get-file-name t t))

  (defun dired-custom-sort ()
    "Show directories first in dired mode."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2)
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
    (set-buffer-modified-p nil))

  ;; ----------------------------------------------------------
  ;; Customize `dired' related variables
  (customize-set-variable 'dired-dwim-target t)
  (customize-set-variable 'dired-recursive-copies 'always)
  (customize-set-variable 'dired-recursive-deletes 'always)
  (customize-set-variable 'dired-bind-info nil)
  (customize-set-variable 'dired-omit-extensions (append dired-omit-extensions '(".cache")))
  (customize-set-variable 'dired-omit-files (concat dired-omit-files "\\|^\\.\\|^semantic.cache$\\|^CVS$"))
  (customize-set-variable 'wget-download-directory "./")

  ;; Customize `epg-config' related variables
  (customize-set-variable 'epg-pinentry-mode 'loopback)

  ;; ----------------------------------------------------------
  ;; Define `dired' related aliases
  (defalias 'dired-encrypt 'epa-dired-do-encrypt)
  (defalias 'dired-decrypt 'epa-dired-do-decrypt)
  (defalias 'dired-sign 'epa-dired-do-sign)
  (defalias 'dired-verify 'epa-dired-do-verify)

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
     ("<up>" . dired-hacks-previous-file)
     ("n" . dired-hacks-next-file)
     ("<down>" . dired-hacks-next-file)
     ("M-," . dired-goto-first-line)
     ("M-." . dired-goto-last-line)
     ("h" . dired-up-directory-single)
     ("j" . dired-find-file)
     ("C-s" . dired-isearch-forward)
     ("C-r" . dired-isearch-backward)
     ("M-s" . dired-isearch-forward-regexp)
     ("M-r" . dired-isearch-backward-regexp)
     ("C-k" . dired-do-delete)
     ("k" . kill-this-buffer)
     ("r" . wdired-change-to-wdired-mode)
     ("M-o" . dired-omit-mode)
     ("E" . dired-do-touch)
     ("B" . dired-backup-file)
     ("?" . dired-get-size)
     ("=" . dired-diff)
     ("D" . ztree-diff)
	 ("z" . dired-do-compress)
     ("Z" . dired-do-compress)
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
              (all-the-icons-dired-mode 1))))

(eval-after-load "dired" '(dired-settings))

;; ==================================================================================
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

;; ==================================================================================
;; Hooks
(add-hook 'after-init-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Global key bindings for `dired'
            (lazy-set-key
             '(("C-x C-d" . dired)
               ("C-x d" . dired-jump)))))

;; ==================================================================================
;;; Provide features
(provide 'init-dired)

;;; init-dired.el ends here
