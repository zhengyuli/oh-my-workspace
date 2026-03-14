;;; init-cc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-06 19:10:29 Friday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: c, cpp
;; Dependencies: init-prog

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
;; C/C++ mode configuration with clangd LSP and Google code style.
;; LSP server (clangd) is configured in init-prog.el.

;;; Code:

;; ==================================================================================
(use-package google-c-style
  :ensure t
  :defer t
  :hook ((c-mode . google-set-c-style)
         (c++-mode . google-set-c-style)))

;; ==================================================================================
;;; Provide features
(provide 'init-cc)

;;; init-cc.el ends here
