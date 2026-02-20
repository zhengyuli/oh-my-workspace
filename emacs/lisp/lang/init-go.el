;;; init-go.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: init-functions, init-prog-base

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
;; Go mode configuration.
;; Use eglot + gopls for LSP support, navigation via xref system.

;;; Code:

;; ==================================================================================
;; Go mode
(use-package go-mode
  :defer t
  :hook (go-mode . go-mode-setup)
  :config
  ;; Globally set gofmt command
  (setq gofmt-command "gofumpt")

  (defun go-mode-setup ()
    "Setup Go mode environment."
    ;; Load golang related envs (macOS)
    (when (memq window-system '(mac ns))
      (require 'exec-path-from-shell)
      (exec-path-from-shell-copy-env "GOROOT")
      (exec-path-from-shell-copy-env "GOPATH"))
    ;; Auto format on save
    (add-hook 'before-save-hook #'gofmt-before-save nil t)))

;; ==================================================================================
;; Go mode keybindings
;; Note: M-. and M-, already bound in prog-mode-map to xref-find-definitions/xref-pop-marker-stack
;; Navigation via eglot + gopls, no need for godef
(with-eval-after-load 'go-mode
  (lazy-set-key
   '(;; Go specific navigation
     ("C-c C-j" . go-goto-imports)
     ("C-c C-k" . godoc)
     ;; Import management
     ("C-c C-a" . go-import-add)
     ("C-c C-r" . go-remove-unused-imports))
   go-mode-map))

;; ==================================================================================
;; Go Tools Validation
;; Go development tools validation
(defvar required-go-tools
  '((gopls . "go install golang.org/x/tools/gopls@latest")
    (gofumpt . "go install mvdan.cc/gofumpt@latest"))
  "List of Go development tools.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'go-tools
 (lambda () (config-dependency-validate-executables required-go-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-go)

;;; init-go.el ends here
