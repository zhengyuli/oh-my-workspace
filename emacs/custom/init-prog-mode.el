;;; package --- init-prog-mode.el ---
;; Time-stamp: <2021-06-26 05:42:10 Saturday by lizhengyu>

;; Copyright (C) 2018 zhengyu li
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
;;   (require 'init-prog-mode)

;;; Require:

;;; Code:
;; ==================================================================================
(defun prog-mode-settings ()
  "Settings for `prog-mode'."

  ;; Require
  (require 'linum)
  (require 'paren)
  (require 'autopair)
  (require 'rainbow-delimiters)
  (require 'flycheck)
  (require 'xref)
  (require 'etags)
  (require 'dumb-jump)
  (require 'quickrun)
  (require 'aggressive-indent)
  (require 'whitespace-cleanup-mode)
  (require 'company-tabnine)
  (require 'lazy-set-key)

  ;; ----------------------------------------------------------
  (defun smart-comment ()
    "If mark is active, comment region, else comment current line."
    (interactive)
    (if mark-active
        (comment-region (region-beginning) (region-end))
      (comment-region (line-beginning-position) (line-end-position))))

  (defun smart-uncomment ()
    "If region is active, uncomment region else uncomment current line."
    (interactive)
    (if mark-active
        (uncomment-region (region-beginning) (region-end))
      (uncomment-region (line-beginning-position) (line-end-position))))

  (defun jump-to-matched-paren ()
    "Jump to the matched parenthese."
    (interactive)
    (cond ((looking-at "[ \t]*[[\"({]")
           (forward-sexp)
           (backward-char))
          ((or (looking-at "[]\")}]")
               (looking-back "[]\")}][ \t]*" nil))
           (if (< (point) (point-max))
               (forward-char))
           (backward-sexp))
          (t (message "couldn't find matched paren"))))

  (defun generate-tag-table (&optional tags-target-directory tags-storage-directory tags-suffix)
    "Generate tag table for `TAGS-TARGET-DIRECTORY' in `TAGS-STORAGE-DIRECTORY' by `TAGS-SUFFIX'."
    (interactive)
    (or tags-target-directory
        (setq tags-target-directory (read-directory-name "Target directory: ")))
    (or tags-storage-directory
        (setq tags-storage-directory (read-directory-name "Storage directory: ")))
    (or tags-suffix
        (setq tags-suffix (read-string "Regexp:")))
    (if (equal tags-suffix "")
        (message "tags suffix is null")
      (progn
        (setq tags-suffix (replace-regexp-in-string "[ ]+" "\" -o -name \"" tags-suffix)))
      (with-temp-buffer
        (cd tags-storage-directory)
		(message "indexing tags ... .. .")
        (shell-command
         (format "find %s -name \"%s\" | xargs etags" tags-target-directory tags-suffix))
        (message "tags index done!"))))

  ;; ----------------------------------------------------------
  ;; Key bindings for `prog-mode-map'
  (lazy-set-key
   '(("C-j" . newline-and-indent)
	 ("<return>" . newline-and-indent)
	 ("RET" . newline-and-indent)
	 ("C-c C-c" . smart-comment)
     ("C-c k" . smart-uncomment)
     ("C-c M-a" . beginning-of-defun)
     ("C-c M-e" . end-of-defun)
     ("C-]" . jump-to-matched-paren)
     ("<f7>" . visit-tags-table)
     ("C-<f7>" . generate-tag-table)
	 ("M-r" . xref-find-references)
     ("M-." . xref-find-definitions)
     ("M-," . xref-pop-marker-stack)
     ("C-0" . quickrun))
   prog-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks for `xref'
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  ;; Hooks for `prog-mode'
  (add-hook 'prog-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends (append-backend-with-yas 'company-tabnine))
              (linum-mode 1)
              (show-paren-mode 1)
              (autopair-mode 1)
              (rainbow-delimiters-mode 1)
              (whitespace-cleanup-mode 1)
              (flycheck-mode 1))))

(eval-after-load "prog-mode" '(prog-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-prog-mode)

;;; init-prog-mode.el ends here
