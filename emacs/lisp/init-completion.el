;;; init-completion.el -*- lexical-binding: t; -*-
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
;; Completion system: ivy, swiper, counsel, company, yasnippet, which-key, avy, ag.

;;; Code:

;; ==================================================================================
;; Which-key
(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-right)
  (which-key-mode 1))

;; ==================================================================================
;; Ivy
(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :ensure t
  :after ivy-rich
  :config
  (ivy-prescient-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))

;; ==================================================================================
;; Swiper
(use-package swiper
  :ensure t
  :after ivy)

;; ==================================================================================
;; Counsel
(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :ensure t
  :after counsel
  :config
  (counsel-projectile-mode 1)
  (with-eval-after-load 'projectile
    (lazy-set-key
     '(("C-x p" . projectile-command-map))
     projectile-mode-map)))

;; ==================================================================================
;; Company
(use-package company
  :ensure t
  :config
  (require 'company-yasnippet)

  (defun company-mode/backend-with-yas (backend)
    "Append `BACKEND' with company-yas."
    (if (and (listp backend)
             (member 'company-yasnippet backend))
        backend
      (append (if (consp backend)
                  backend
                (list backend))
              '(:with company-yasnippet))))

  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-require-match 'never
        company-tooltip-idle-delay 0
        company-tooltip-limit 15
        company-selection-wrap-around t
        company-format-margin-function 'company-text-icons-margin
        company-transformers '(delete-dups company-sort-by-occurrence)
        company-dabbrev-downcase nil)

  (global-company-mode 1))

(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode 1))

(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-scrollbar nil
        company-box-doc-enable t
        company-box-doc-delay 0.1))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode 1))

(use-package company-quickhelp-terminal
  :ensure t
  :after company-quickhelp
  :config
  (unless (display-graphic-p)
    (company-quickhelp-terminal-mode 1)))

;; ==================================================================================
;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (require 'yasnippet-snippets)
  ;; Key unbindings
  (lazy-unset-key '("<tab>" "TAB") yas-minor-mode-map)
  ;; Initialize yasnippet snippets
  (yasnippet-snippets-initialize)
  (yas-global-mode 1))

;; ==================================================================================
;; Color moccur
(use-package color-moccur
  :ensure t
  :defer t)

;; ==================================================================================
;; Ag - The Silver Searcher
(use-package ag
  :ensure t
  :commands (ag ag-project ag-dired-regexp)
  :config
  (require 'wgrep-ag)

  (defun my/ag-next-error-function-after (&rest _)
    (select-window
     (get-buffer-window (ag/buffer-name "" "" ""))))
  (advice-add 'ag/next-error-function :after #'my/ag-next-error-function-after)

  (setq ag-reuse-buffers t
        wgrep-enable-key "r"
        wgrep-auto-save-buffer t)

  ;; Hooks
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)

  (add-hook 'ag-search-finished-hook
            (lambda ()
              (select-window
               (get-buffer-window (ag/buffer-name "" "" ""))))))

;; ==================================================================================
;; Avy
(use-package avy
  :ensure t
  :defer t)

;; ==================================================================================
;; Popup
(use-package popup
  :ensure t
  :defer t)

;; ==================================================================================
;; Completion keybindings
(lazy-set-key
 '(;; Swiper
   ("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch-backward)
   ;; Counsel
   ("M-x" . counsel-M-x)
   ("C-x b" . counsel-switch-buffer)
   ("C-x B" . counsel-recentf)
   ("C-x C-f" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h o" . counsel-describe-symbol)
   ("C-h b" . counsel-descbinds)
   ;; Color moccur
   ("C-x C-u" . occur-by-moccur)
   ;; Ag
   ("C-x g" . ag)
   ("C-x G" . ag-project)
   ("C-x f" . ag-dired-regexp)
   ("C-x F" . ag-project-dired-regexp)
   ;; Avy
   ("C-; c" . avy-goto-char)
   ("C-; w" . avy-goto-word-0)
   ("C-; l" . avy-goto-line)))

;; ==================================================================================
;;; Provide features
(provide 'init-completion)

;;; init-completion.el ends here
