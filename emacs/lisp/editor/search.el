;;; search.el -*- lexical-binding: t; -*-

;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: search, consult, embark, xref
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
;; 2026-03-14 19:15 chieftain <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Search and navigation enhancements with consult, embark, wgrep, and xref integration.

;;; Code:

;; ==================================================================================
(use-package wgrep
  :ensure t
  :defer t
  :bind (:map grep-mode-map
              ("r" . wgrep-change-to-wgrep-mode))
  :config
  (setq wgrep-auto-save-buffer t))

;; ==================================================================================
(use-package dumb-jump
  :ensure t
  :defer t
  :commands (dumb-jump-xref-activate))

;; ==================================================================================
(use-package xref
  :ensure nil
  :defer t
  :config
  (setq xref-search-program 'ripgrep
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;; ==================================================================================
(use-package consult
  :ensure t
  :defer t
  :bind (("C-s" . consult-line)
         ("C-r" . consult-line-multi)
         ("C-x b" . consult-buffer)
         ("C-x B" . consult-recent-file)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-x g" . consult-ripgrep)
         ("C-x f" . consult-find))
  :config
  (setq consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-preview-key '(:debounce 0.25 any)))

;; ==================================================================================
;; Will be loaded by embark automatically.
(use-package embark-consult
  :ensure t
  :defer t)

(use-package embark
  :ensure t
  :defer t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

;; ==================================================================================
;;; Provide features
(provide 'search)

;;; search.el ends here
