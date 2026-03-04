;;; init-completion.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-04 13:44:35 Wednesday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: completion, corfu, vertico, consult
;; Dependencies: (none)

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
;; Modern completion framework with vertico (UI), orderless (matching),
;; marginalia (annotations), consult (commands), and embark (actions).

;;; Code:

;; ==================================================================================
;; Orderless - space-separated pattern matching for completion
;; Supports multiple patterns like 'flex regexp' for powerful filtering
(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic)))

;; ==================================================================================
;; Marginalia - rich annotations in completion UI
;; Displays file info, command descriptions, and keybindings
(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

;; ==================================================================================
;; Vertico - vertical completion UI with incremental narrowing
;; Replaces default completion with more intuitive vertical interface
(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode))

;; ==================================================================================
;; Consult - enhanced commands
;; Consult provides many enhanced versions of built-in Emacs commands, especially
;; for searching, switching buffers, and finding files, with incremental completion.
(use-package consult
  :ensure t
  :defer t  ;; Load lazily when first needed to improve startup time
  :bind (;; Search for a string in the current buffer using incremental search
         ("C-s" . consult-line)
         ;; Switch to another buffer (enhanced interface with completion)
         ("C-x b" . consult-buffer)
         ;; Open a recently visited file
         ("C-x B" . consult-recent-file)
         ;; Show and select from the kill ring (yank history)
         ("M-y" . consult-yank-pop)
         ;; Go to a specific line in the current buffer
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)  ;; Same as above, for convenience
         ;; Search for a string in files using grep
         ("C-x g" . consult-grep)
         ;; Search in a git repository using git grep
         ("C-x G" . consult-git-grep)
         ;; Find a file in the current directory tree
         ("C-x f" . consult-find)))

;; ==================================================================================
;; Embark - context actions
(use-package embark
  :ensure t
  :defer t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

;; ==================================================================================
;; Embark-consult - Embark and Consult integration
(use-package embark-consult
  :ensure t
  :after (consult embark)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :config
  (setq embark-collect-use-consult-preview t))

;; ==================================================================================
;; Corfu - completion framework
(use-package corfu
  :ensure t
  :defer t
  :hook (after-init . global-corfu-mode)
  :config
  (setq corfu-auto t)
  (corfu-popupinfo-mode 1))

;; ==================================================================================
;; Corfu-terminal - completion framework
(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; ==================================================================================
;; Cape - completion at point extensions
(use-package cape
  :ensure t
  :after corfu
  :config
  (defun my/cape-setup ()
    (add-to-list 'completion-at-point-functions #'cape-file t)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol t))
  (add-hook 'prog-mode-hook #'my/cape-setup))

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
  :after yasnippet
  :hook (yas-minor-mode . yasnippet-snippets-initialize))
;; ==================================================================================
;;; Provide features
(provide 'init-completion)

;;; init-completion.el ends here
