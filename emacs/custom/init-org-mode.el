;;; package --- init-org-mode.el -*- lexical-binding:t -*-
;; Time-stamp: <2022-05-12 13:04:25 Thursday by zhengyuli>

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
;;   (require 'init-org-mode)

;;; Require:

;;; Code:
;; ==================================================================================
;; Customized settings for `org-mode'
(defun org-mode-settings ()
  "Settings for `org-mode'."

  ;; Require
  (require 'org-tempo)
  (require 'org-bullets)
  (require 'org-appear)
  (require 'valign)

  ;; ----------------------------------------------------------
  ;; Redefinition of `org-babel-confirm-evaluate' to load babel
  ;; library dynamically.
  (defun org-babel-confirm-evaluate (info)
    "Confirm evaluation of the code block INFO."
    (let* ((evalp (org-babel-check-confirm-evaluate info))
           (lang (nth 0 info))
           (name (nth 4 info))
           (name-string (if name (format " (%s) " name) " ")))
      (pcase evalp
        (`nil nil)
        (`t (progn
              (require (intern (concat "ob-" lang)))
              t))
        (`query (or
                 (and (not (bound-and-true-p
                            org-babel-confirm-evaluate-answer-no))
                      (if (yes-or-no-p
                           (format "Evaluate this %s code block%son your system? "
                                   lang name-string))
                          (progn
                            (require (intern (concat "ob-" lang)))
                            t)
                        nil))
                 (progn
                   (message "Evaluation of this %s code block%sis aborted."
                            lang name-string)
                   nil)))
        (x (error "Unexpected value `%s' from `org-babel-check-confirm-evaluate'" x)))))


  ;; Redefinition of `org-tempo-add-block' with uppercase keyword support
  (defun org-tempo-add-block (entry)
    "Add block entry from `org-structure-template-alist'."
    (let* ((key (format "<%s" (car entry)))
           (name (cdr entry))
           (name_prefix (car (split-string name " ")))
           (name_post (string-join (cdr (split-string name " ")) " "))
           (special (member name '("src" "export"))))
      (tempo-define-template (format "org-%s" (replace-regexp-in-string " " "-" name))
                             `(,(format "#+BEGIN_%s%s"
                                        (if (equal "" name_post)
                                            (upcase name_prefix)
                                          (string-join (list (upcase name_prefix) name_post) " "))
                                        (if special " " ""))
                               ,(when special 'p) '> n ,(unless special 'p) n
                               ,(format "#+END_%s" (upcase name_prefix))
                               >)
                             key
                             (format "Insert a %s block" name)
                             'org-tempo-tags)))

  ;; Redefinition of `org-tempo-add-keyword' with uppercase keyword support
  (defun org-tempo-add-keyword (entry)
    "Add keyword entry from `org-tempo-keywords-alist'."
    (let* ((key (format "<%s" (car entry)))
           (name (cdr entry)))
      (tempo-define-template (format "org-%s" (replace-regexp-in-string " " "-" name))
                             `(,(format "#+%s: " (upcase name)) p '>)
                             key
                             (format "Insert a %s keyword" name)
                             'org-tempo-tags)))

  ;; Redefinition of `org-tempo--include-file' with uppercase keyword support
  (defun org-tempo--include-file ()
    "Add #+include: and a file name."
    (let ((inhibit-quit t))
      (unless (with-local-quit
                (prog1 t
                  (insert
                   (format "#+INCLUDE: %S "
                           (file-relative-name
                            (read-file-name "Include file: "))))))
        (insert "<I")
        (setq quit-flag nil))))

  ;; ----------------------------------------------------------
  ;; Customize `org-mode' related variables
  (customize-set-variable 'org-startup-indented t)
  (customize-set-variable 'org-startup-truncated t)
  (customize-set-variable 'org-startup-with-inline-images t)
  (customize-set-variable 'org-image-actual-width '(300))
  (customize-set-variable 'org-pretty-entities t)
  (customize-set-variable 'org-hide-emphasis-markers t)
  (customize-set-variable 'org-catch-invisible-edits t)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'org-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Set buffer column width to 120
              (setq-local fill-column 120)

              ;; Enable auto fill mode
              (auto-fill-mode 1)

              ;; Enable org bullets mode
              (org-bullets-mode 1)

              ;; Enable org appear mode
              (org-appear-mode 1)

              ;; enable valign mode
              (valign-mode 1))))

(eval-after-load "org" '(org-mode-settings))

;; ==================================================================================
;; Customized settings for `org-tree-slide'
(defun org-tree-slide-mode-settings ()
  "Settings for `org-tree-slide-mode'."

  ;; ----------------------------------------------------------
  (defun exit-org-tree-slide-mode ()
    "Exit `org-tree-slide-mode'."
    (interactive)
    (org-tree-slide-mode -1)
    (text-scale-mode -1)
    (read-only-mode -1))

  ;; ----------------------------------------------------------
  ;; Key unbindings for `org-tree-slide-mode'
  (lazy-set-key
   '(("M-p"  . org-tree-slide-move-previous-tree)
     ("M-n"  . org-tree-slide-move-next-tree)
     ("<up>"  . org-tree-slide-move-previous-tree)
     ("<down>"  . org-tree-slide-move-next-tree)
     ("p"  . org-tree-slide-move-previous-tree)
     ("n"  . org-tree-slide-move-next-tree)
     ("q" . exit-org-tree-slide-mode)
     ("Q" . exit-org-tree-slide-mode)
     ("M-w" . toggle-buffer-writable))
   org-tree-slide-mode-map)

  (add-hook 'org-tree-slide-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable read only mode
              (read-only-mode 1)

              ;; Increase font scale
              (text-scale-increase 1))))

(eval-after-load "org-tree-slide" '(org-tree-slide-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-org-mode)

;;; init-org-mode.el ends here
