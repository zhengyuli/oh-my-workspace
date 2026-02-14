;;; init-go.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
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
;; Go mode configuration.

;;; Code:

;; ==================================================================================
;; Go mode
(use-package go-mode
  :defer t
  :hook (go-mode . go-mode-setup)
  :config
  (defun go-mode-setup ()
    ;; Load golang related envs
    (when (memq window-system '(mac ns))
      (require 'exec-path-from-shell)
      (exec-path-from-shell-copy-env "GOROOT")
      (exec-path-from-shell-copy-env "GOPATH"))
    ;; Setup go eldoc
    (go-eldoc-setup)
    ;; Enable lsp mode
    (lsp-deferred)))

;; ==================================================================================
;; Go eldoc
(use-package go-eldoc
  :defer t)

;; ==================================================================================
;; Go mode keybindings
(with-eval-after-load 'go-mode
  (lazy-set-key
   '(("M-." . godef-jump))
   go-mode-map))

;; ==================================================================================
;;; Provide features
(provide 'init-go)

;;; init-go.el ends here
