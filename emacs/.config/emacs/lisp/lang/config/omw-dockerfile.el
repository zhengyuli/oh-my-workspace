;;; omw-dockerfile.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: dockerfile, docker
;; Dependencies: omw-prog, omw-utils

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
;; Dockerfile mode configuration with LSP support.
;; LSP server (docker-langserver) is configured in omw-prog.el.

;;; Code:

;; ============================================================================
(defvar omw/dockerfile-tool-specs
  '(("docker-langserver" "bun install -g dockerfile-language-server-nodejs" "bun"))
  "Tool specs for Dockerfile development.")

(defun omw/install-dockerfile-tools ()
  "Install Dockerfile LSP tools (docker-langserver) via bun if not present."
  (interactive)
  (require 'omw-utils)
  (apply #'omw/tools-install omw/dockerfile-tool-specs))

;; ============================================================================
(defun omw/dockerfile-mode-setup ()
  "Apply custom settings for dockerfile mode."
  (require 'omw-utils)
  (setq-local dockerfile-indent-offset 2)
  (apply #'omw/tools-check-and-prompt omw/dockerfile-tool-specs))

(use-package dockerfile-mode
  :ensure t
  :defer t
  :hook (dockerfile-mode . omw/dockerfile-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-dockerfile)

;;; omw-dockerfile.el ends here
