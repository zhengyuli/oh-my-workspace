;;; omw-term.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: terminal, vterm, eshell
;; Dependencies: (none)

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
;; Terminal emulation configuration with vterm for fast, native PTY support.

;;; Code:

;; ============================================================================
(defun omw/vterm-mode-setup ()
  "Apply custom settings for vterm mode."
  (setq-local truncate-lines t)
  (hl-line-mode -1)
  (corfu-mode -1))

(use-package vterm
  :ensure t
  :defer t
  :hook (vterm-mode . omw/vterm-mode-setup)
  :bind (:map vterm-mode-map
              ("M-1" . nil)
              ("M-2" . nil)
              ("M-3" . nil)
              ("M-4" . nil)
              ("M-5" . nil)
              ("M-6" . nil)
              ("M-7" . nil)
              ("M-8" . nil)
              ("M-9" . nil)
              ("C-g" . vterm--self-insert)
              ("M-<backspace>" . vterm-send-meta-backspace)))

;; ============================================================================
(use-package multi-vterm
  :ensure t
  :defer t
  :bind ("C-x C-t" . multi-vterm))

;; ============================================================================
;;; Provide features
(provide 'omw-term)

;;; omw-term.el ends here
