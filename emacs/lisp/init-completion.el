;;; init-completion.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-01 21:42:35 Sunday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: init-funcs

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
;; Completion system: vertico, marginalia, consult, corfu, yasnippet, which-key, avy, ag.

;;; Code:

;; ==================================================================================
;; Orderless - fuzzy matching
(use-package orderless
  :ensure t
  :demand t)

;; ==================================================================================
;; Marginalia - completion annotations
(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

;; ==================================================================================
;; Vertico - vertical completion UI
(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode))

;; ==================================================================================
;; Embark - context actions
(use-package embark
  :ensure t
  :defer t)

;; ==================================================================================
;; Consult - enhanced commands
(use-package consult
  :ensure t
  :defer t)

;; ==================================================================================
;; Embark-consult - Embark and Consult integration
(use-package embark-consult
  :ensure t
  :defer t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :config
  (setq embark-collect-use-consult-preview t))

;; ==================================================================================
;; Corfu - completion framework
(use-package corfu
  :ensure t
  :defer t
  :hook ((after-init . global-corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :config
  (setq corfu-auto t))

;; ==================================================================================
(use-package cape
  :ensure t
  :defer t
  :after corfu
  :hook
  (prog-mode . my/cape-setup)
  :config
  (defun my/cape-setup ()
    (add-to-list 'completion-at-point-functions #'cape-file t)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
    (add-to-list 'completion-at-point-functions #'cape-symbol t)))

;; ==================================================================================
(use-package yasnippet
  :ensure t
  :defer t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  ;; Prevent TAB conflicts with Corfu
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)

  ;; Modern snippet behavior
  (setq yas-triggers-in-field t
        yas-verbosity 1))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet
  :hook (yas-minor-mode . yasnippet-snippets-initialize))
;; ==================================================================================
;;; Provide features
(provide 'init-completion)

;;; init-completion.el ends here
