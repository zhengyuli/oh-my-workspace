;;; init-terminal.el -*- lexical-binding: t; -*-
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
;; Terminal configuration: vterm, multi-vterm.

;;; Code:

;; ==================================================================================
;; Vterm - 现代终端模拟器
(use-package vterm
  :commands (vterm)
  :custom
  (vterm-max-scrollback 10000)           ; 最大滚动历史
  (vterm-scroll-enable-emacs-bar t)      ; Emacs 风格滚动条
  (vterm-kill-buffer-on-exit t)          ; 退出时关闭 buffer
  (vterm-enable-manipulate-selection-data t) ; 允许操作选中内容
  :config
  ;; Key bindings
  (lazy-set-key
   '(("C-g" . vterm--self-insert)
     ("M-<backspace>" . vterm-send-meta-backspace))
   vterm-mode-map)

  ;; Hooks
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Disable auto fill mode
              (auto-fill-mode -1)
              ;; Disable yasnippet mode
              (yas-minor-mode -1)
              ;; Disable corfu mode
              (corfu-mode -1)
              ;; Unbind M-0~9 to allow winnum to work
              (lazy-unset-key '("M-0" "M-1" "M-2" "M-3" "M-4"
                                "M-5" "M-6" "M-7" "M-8" "M-9")
                              vterm-mode-map)
              ;; Set buffer-local variables
              (setq-local global-hl-line-mode nil
                          truncate-lines t))))

;; ==================================================================================
;; Multi-vterm
(use-package multi-vterm
  :commands (multi-vterm))

;; ==================================================================================
;; Terminal keybinding
(add-hook 'after-init-hook
          (lambda ()
            (lazy-set-key
             '(("C-x C-t" . multi-vterm)))))

;; ==================================================================================
;;; Provide features
(provide 'init-terminal)

;;; init-terminal.el ends here
