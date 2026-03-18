;;; omw-cc.el -*- lexical-binding: t; -*-

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: c, cpp
;; Dependencies: omw-prog

;; Copyright (C) 2026 zhengyu li

;; Licensed under the GPL License version 3.0

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; C/C++ mode configuration with clangd LSP and Google code style.
;; LSP server (clangd) is configured in prog.el.

;;; Code:

;; ==================================================================================
(defvar omw/cc-tool-specs
  '(("clangd" "brew install llvm" "brew"))
  "Tool specs for C/C++ development.")

(defun omw/install-cc-tools ()
  "Install C/C++ development tools (clangd) via Homebrew if not present."
  (interactive)
  (require 'omw-utils)
  (apply #'omw/tools-install omw/cc-tool-specs))

;; ==================================================================================
(defun omw/cc-mode-setup ()
  "Apply custom settings for C/C++ mode."
  (require 'omw-utils)
  (apply #'omw/tools-check-and-prompt omw/cc-tool-specs))

(use-package cc-mode
  :ensure nil
  :defer t
  :hook ((c-mode   . omw/cc-mode-setup)
         (c++-mode . omw/cc-mode-setup)))

;; ==================================================================================
(use-package google-c-style
  :ensure t
  :defer t
  :hook ((c-mode   . google-set-c-style)
         (c++-mode . google-set-c-style)))

;; ==================================================================================
;;; Provide features
(provide 'omw-cc)

;;; omw-cc.el ends here
