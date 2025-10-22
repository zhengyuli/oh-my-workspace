;;; package --- init-programming-base.el -*- lexical-binding:t -*-
;; Time-stamp: <2025-10-18 19:55:22 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025 zhengyu li
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
;;   (require 'init-programming-base)

;;; Require:

;;; Code:
;; ==================================================================================
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

  ;; Core packages
  (require 'xref)
  (require 'paren)
  (require 'smartparens-config)
  (require 'hungry-delete)
  (require 'hideshow)
  (require 'rainbow-delimiters)
  (require 'hl-todo)
  (require 'flycheck)
  (require 'whitespace-cleanup-mode)
  (require 'dumb-jump)
  (require 'quickrun)

  ;; Load only when needed
  (require 'format-all)
  (require 'devdocs)
  (require 'emojify)

  ;; ----------------------------------------------------------
  ;; Customize `flycheck' related variables
  (customize-set-variable 'flycheck-indication-mode 'left-margin)

  ;; ----------------------------------------------------------
  ;; Key bindings for `prog-mode'
  (lazy-set-key
   '(("<return>" . newline-and-indent)
     ("RET" . newline-and-indent)
     ("C-c M-a" . beginning-of-defun)
     ("C-c M-e" . end-of-defun)
     ("C-]" . jump-to-matched-paren)
     ("C-c C-c" . comment-line)
     ("M-r" . xref-find-references)
     ("M-." . xref-find-definitions)
     ("M-," . xref-pop-marker-stack)
     ("C-x C-;" . quickrun)
     ("C-h C-d" . devdocs-lookup)
     ("C-h C-s" . devdocs-search))
   prog-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

  (add-hook 'prog-mode-hook
            (lambda ()
              ;; -----------------------------------------------
              ;; Set tab width with 4 white spaces
              (setq-local tab-width 4)

              ;; Disable tab characters for indentation
              (setq-local indent-tabs-mode nil)

              ;; -----------------------------------------------
              ;; Enable linum node
              (display-line-numbers-mode 1)

              ;; Enable show paren mode
              (show-paren-local-mode 1)

              ;; Enable smart parens mode
              (smartparens-mode 1)

              ;; Enable hungry delete mode
              (hungry-delete-mode 1)

              ;; Enable prettify symbol mode
              (prettify-symbols-mode 1)

              ;; Enable hide show mode
              (hs-minor-mode 1)

              ;; Enable rainbow delimiters mode
              (rainbow-delimiters-mode 1)

              ;; enable highlight todo mode
              (hl-todo-mode 1)

              ;; Enable whitespace cleanup mode
              (whitespace-cleanup-mode 1)

              ;; Enable flycheck mode
              (flycheck-mode 1)

              ;; Enable format all mode
              (format-all-mode 1)

              ;; Disable emojify mode
              (emojify-mode -1))))

(eval-after-load "prog-mode" '(prog-mode-settings))

;; ==================================================================================
;; Customized settings for `cc-mode'
(defun cc-mode-settings ()
  "settings for `cc-mode'."

  ;; ----------------------------------------------------------
  ;; Key bindings for `cc-mode'
  (lazy-set-key
   '(("C-c C-c" . comment-line))
   c-mode-base-map))

(eval-after-load "cc-mode" '(cc-mode-settings))

;; ==================================================================================
;; Customized settings for `lsp-mode'
(defun lsp-mode-settings ()
  "Settings for `lsp-mode'."

  ;; Conditional requires - only load when LSP is actually used
  (when (featurep 'lsp-mode)
    (require 'lsp-modeline))

  ;; Customize `read-process-output-max' to 3MB, default 4k is too low
  (customize-set-variable 'read-process-output-max (* 3 1024 1024))

  ;; Customize `lsp-mode' related variables
  (customize-set-variable 'lsp-headerline-breadcrumb-enable nil)
  (customize-set-variable 'lsp-enable-indentation nil)

  ;; ----------------------------------------------------------
  ;; Key bindings for `prog-mode'
  (lazy-set-key
   '(("M-r" . xref-find-references)
     ("M-." . xref-find-definitions)
     ("M-," . xref-pop-marker-stack))
   prog-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'lsp-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable which key integration
              (lsp-enable-which-key-integration))))

(eval-after-load "lsp-mode" '(lsp-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-programming-base)

;;; init-programming-base.el ends here
