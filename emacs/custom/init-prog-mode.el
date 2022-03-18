;;; package --- init-prog-mode.el -*- lexical-binding:t -*-
;; Time-stamp: <2022-03-18 09:30:05 Friday by zhengyuli>

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
;;   (require 'init-prog-mode)

;;; Require:

;;; Code:
;; ==================================================================================
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

;; ==================================================================================
;; Customized settings for `prog-mode'
(defun prog-mode-settings ()
  "Settings for `prog-mode'."

  ;; Require
  (require 'linum)
  (require 'xref)
  (require 'paren)
  (require 'smartparens-config)
  (require 'rainbow-delimiters)
  (require 'flycheck)
  (require 'whitespace-cleanup-mode)
  (require 'dumb-jump)
  (require 'quickrun)

  ;; ----------------------------------------------------------
  ;; Customize `flycheck' related variables
  (customize-set-variable 'flycheck-indication-mode 'left-margin)

  ;; ----------------------------------------------------------
  ;; Key bindings for `prog-mode'
  (lazy-set-key
   '(("<return>" . newline-and-indent)
	 ("RET" . newline-and-indent)
	 ("C-c C-c" . smart-comment)
     ("C-c k" . smart-uncomment)
     ("C-c M-a" . beginning-of-defun)
     ("C-c M-e" . end-of-defun)
     ("C-]" . jump-to-matched-paren)
	 ("M-r" . xref-find-references)
     ("M-." . xref-find-definitions)
     ("M-," . xref-pop-marker-stack)
     ("C-0" . quickrun))
   prog-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

  (add-hook 'prog-mode-hook
            (lambda ()
              ;; -----------------------------------------------
              ;; Set tab width with 4 white spaces
			  (setq tab-width 4)

              ;; Disable tab characters for indentation
			  (setq indent-tabs-mode nil)

              ;; -----------------------------------------------
              ;; Enable linum node
              (linum-mode 1)

              ;; Enable show paren mode
              (show-paren-mode 1)

              ;; Enable smart parens mode
              (smartparens-mode 1)

              ;; Enable rainbow delimiters mode
              (rainbow-delimiters-mode 1)

              ;; Enable whitespace cleanup mode
              (whitespace-cleanup-mode 1)

              ;; Enable flycheck mode
              (flycheck-mode 1))))

(eval-after-load "prog-mode" '(prog-mode-settings))

;; ==================================================================================
;; Customized settings for `lsp-mode'
(defun lsp-mode-settings ()
  "Settings for `lsp-mode'."

  ;; Require
  (require 'lsp-modeline)

  ;; ----------------------------------------------------------
  ;; Customize `gc-cons-threshold' to 100MB
  (customize-set-variable 'gc-cons-threshold (* 100 1024 1024))

  ;; Customize `read-process-output-max' to 3MB, default 4k is too low
  (customize-set-variable 'read-process-output-max (* 3 1024 1024))

  ;; Customize `lsp-mode' related variables
  (customize-set-variable 'lsp-headerline-breadcrumb-enable nil))

(eval-after-load "lsp-mode" '(lsp-mode-settings))

;; ==================================================================================
;; Customized settings for `dap-mode'
(defun dap-mode-settings ()
  "Settings for `dap-mode'."

  ;; Require
  (require 'dap-hydra)

  ;; ----------------------------------------------------------
  (defun dap-go-to-server-log-buffer (&optional no-select)
    "Go to server log buffer."
    (interactive)
    (let* ((buffer (process-buffer
                    (dap--debug-session-program-proc
                     (dap--cur-session-or-die))))
           (old-win (get-buffer-window buffer))
           (new-win (display-buffer-in-side-window
                     buffer
                     `((side . bottom) (slot . 5) (window-width . 0.20)))))
      (when old-win
        (delete-window old-win))
      (set-window-dedicated-p new-win t)
      (unless no-select (select-window new-win))
      (fit-window-to-buffer new-win 20 10)))

  ;; ----------------------------------------------------------
  ;; Customize `dap-mode' related variables
  (customize-set-variable 'dap-auto-configure-features
                          '(sessions locals breakpoints expressions))

  ;; ----------------------------------------------------------
  (add-hook 'dap-session-created-hook
            (lambda (arg)
              ;; ----------------------------------------------------------
              ;; Go to debug session server log buffer
              (dap-go-to-server-log-buffer)))

  (add-hook 'dap-stopped-hook
            (lambda (arg)
              ;; ----------------------------------------------------------
              (call-interactively #'dap-hydra))))

(eval-after-load "dap-mode" '(dap-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-prog-mode)

;;; init-prog-mode.el ends here
