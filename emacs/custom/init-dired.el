;;; package --- init-dired.el ---
;; Time-stamp: <2021-06-26 05:50:47 Saturday by lizhengyu>

;; Copyright (C) 2013 zhengyu li
;;
;; Author: zhengyu li <lizhengyu419@gmail.com>
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
(require 'lazy-set-key)

;;; Code:
;; ==================================================================================
(defun dired-settings ()
  "Settings for `dired'."

  ;; Require
  (require 'epa)
  (require 'wget)
  (require 'ztree)
  (require 'dired-x)
  (require 'dired-async)
  (require 'dired-single)
  (require 'dired-filter)
  (require 'dired-subtree)
  (require 'dired-isearch)
  (require 'dired-copy-paste)
  (require 'dired-hacks-utils)
  (require 'dired-filetype-face)

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
                   (re-search-backward "^\\s-*\\([0-9.,]+[A-Za-z]+\\).*\\(total\\|?????????\\)$")
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
  ;; Customize dired related faces
  (custom-set-faces
   '(dired-directory ((t :foreground "#00AAFF" :height 1.1)))
   '(dired-flagged ((t (:foreground "#FF0000" :height 1.1))))
   '(dired-header ((t (:foreground "#EE0000" :height 1.1))))
   '(dired-ignored ((t (:foreground "#FFFFFF" :height 1.1))))
   '(dired-mark ((t (:foreground "#FF1493" :height 1.1))))
   '(dired-marked ((t (:foreground "#FFFF00" :height 1.1))))
   '(dired-perm-write ((t (:foreground "#FF0000" :height 1.1))))
   '(dired-symlink ((t (:foreground "#999999" :height 1.1))))
   '(dired-warning ((t (:foreground "#FFD700" :height 1.1)))))

  ;; Customize dired filetype related faces
  (custom-set-faces
   '(dired-filetype-js ((t (:foreground "#00FF00" :height 1.1))))
   '(dired-filetype-xml ((t (:foreground "#00FF00" :height 1.1))))
   '(dired-filetype-omit ((t (:foreground "#B3B3B3" :height 1.1))))
   '(dired-filetype-link ((t (:foreground "#FFFF00" :height 1.1))))
   '(dired-filetype-music ((t (:foreground "#00FFFF" :height 1.1))))
   '(dired-filetype-video ((t (:foreground "#FF337F" :height 1.1))))
   '(dired-filetype-plain ((t (:foreground "#FFFFFF" :height 1.1))))
   '(dired-filetype-image ((t (:foreground "#BB0000" :height 1.1))))
   '(dired-filetype-source ((t (:foreground "#00FF00" :height 1.1))))
   '(dired-filetype-common ((t (:foreground "#FFC0CB" :height 1.1))))
   '(dired-filetype-execute ((t (:foreground "#FF0000" :height 1.1))))
   '(dired-filetype-program ((t (:foreground "#EE0000" :height 1.1))))
   '(dired-filetype-document ((t (:foreground "#AAAAFF" :height 1.1))))
   '(dired-filetype-compress ((t (:foreground "#FFD700" :height 1.1)))))

  ;; Customize dired related variables
  (customize-set-variable 'dired-dwim-target t)
  (customize-set-variable 'dired-recursive-copies 'always)
  (customize-set-variable 'dired-recursive-deletes 'always)
  (customize-set-variable 'dired-bind-info nil)
  (customize-set-variable 'dired-omit-extensions (append dired-omit-extensions '(".cache")))
  (customize-set-variable 'dired-omit-files (concat dired-omit-files "\\|^\\.\\|^semantic.cache$\\|^CVS$"))
  (customize-set-variable 'wget-download-directory ".")

  ;; ----------------------------------------------------------
  ;; Key bindings for `ztree-mode-map'
  (lazy-set-key
   '(("<return>" . ztree-perform-soft-action)
     ("RET" . ztree-perform-soft-action)
     ("o" . other-window)
     ("n" . next-line)
     ("p" . previous-line))
   ztree-mode-map)

  ;; Key bindings for `dired-mode-map'
  (lazy-set-key
   '(("0" . dired-smart-shell-command)
     ("1" . delete-other-windows)
     ("2" . split-window-vertically)
     ("3" . split-window-horizontally)
     ("o" . other-window)
	 ("w" . wget)
     ("r" . wdired-change-to-wdired-mode)
     ("D" . ztree-diff)
     ("<return>" . dired-single-buffer)
     ("RET" . dired-single-buffer)
     ("h" . dired-up-directory-single)
     ("v" . dired-view-file)
     ("i" . dired-subtree-insert)
     ("I" . dired-subtree-remove)
     ("C-s" . dired-isearch-forward)
     ("C-r" . dired-isearch-backward)
     ("M-s" . dired-isearch-forward-regexp)
     ("M-r" . dired-isearch-backward-regexp)
     ("M-o" . dired-omit-mode)
     ("C-k" . dired-do-delete)
     ("k" . kill-this-buffer)
     ("B" . dired-backup-file)
     ("E" . dired-do-touch)
     ("j" . dired-find-file)
     ("?" . dired-get-size)
     ("M-," . dired-goto-first-line)
     ("M-." . dired-goto-last-line)
     ("p" . dired-hacks-previous-file)
     ("<up>" . dired-hacks-previous-file)
     ("n" . dired-hacks-next-file)
     ("<down>" . dired-hacks-next-file)
	 ("z" . dired-do-compress)
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
  ;; Hooks for `dired'
  (add-hook 'dired-after-readin-hook 'dired-custom-sort)
  (add-hook 'dired-mode-hook (lambda ()
                               (dired-omit-mode 1)
                               (dired-async-mode 1))))

(eval-after-load "dired" '(dired-settings))

;; ==================================================================================
;; Global key bindings for `dired'
(lazy-set-key
 '(("C-x C-d" . dired)
   ("C-x d" . dired-jump)
   ("C-x f" . find-name-dired)
   ("C-x F" . find-grep-dired)))

;; ==================================================================================
;;; Provide features
(provide 'init-dired)

;;; init-dired.el ends here
