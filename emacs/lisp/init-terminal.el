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
;; Vterm
(use-package vterm
  :commands (vterm)
  :config
  ;; Key bindings
  (lazy-set-key
   '(("C-g" . vterm--self-insert))
   vterm-mode-map)

  ;; Hooks
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Disable auto fill mode
              (auto-fill-mode -1)
              ;; Disable yasnippet mode
              (yas-minor-mode -1)
              ;; Disable company mode
              (company-mode -1))))

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
