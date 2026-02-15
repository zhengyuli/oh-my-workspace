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
;; 使用 eglot + gopls 进行 LSP 支持，跳转等功能通过 xrf 体系实现。

;;; Code:

;; ==================================================================================
;; Go mode
(use-package go-mode
  :defer t
  :hook (go-mode . go-mode-setup)
  :config
  ;; 全局设置 gofmt 命令
  (setq gofmt-command "gofumpt")

  (defun go-mode-setup ()
    "Setup Go mode environment."
    ;; Load golang related envs (macOS)
    (when (memq window-system '(mac ns))
      (require 'exec-path-from-shell)
      (exec-path-from-shell-copy-env "GOROOT")
      (exec-path-from-shell-copy-env "GOPATH"))
    ;; 保存时自动格式化
    (add-hook 'before-save-hook #'gofmt-before-save nil t)))

;; ==================================================================================
;; Go mode keybindings
;; 注意: M-. 和 M-, 已在 prog-mode-map 中绑定到 xref-find-definitions/xref-pop-marker-stack
;; 通过 eglot + gopls 提供跳转功能，无需 godef
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
;;; Provide features
(provide 'init-go)

;;; init-go.el ends here
