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
;; Completion system: vertico, marginalia, consult, corfu, yasnippet, which-key, avy, ag.

;;; Code:

;; ==================================================================================
;; Which-key
(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-right)
  (which-key-mode 1))

;; ==================================================================================
;; Vertico - 垂直补全 UI
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode))

;; ==================================================================================
;; Orderless - 模糊匹配
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; ==================================================================================
;; Marginalia - 补全注解
(use-package marginalia
  :ensure t
  :after vertico
  :config
  (marginalia-mode))

;; ==================================================================================
;; Consult - 增强命令
(use-package consult
  :ensure t
  :bind
  (;; Search
   ("C-s" . consult-line)
   ("C-r" . consult-line-backward)
   ;; Buffer and file navigation
   ("C-x b" . consult-buffer)
   ("C-x B" . consult-recent-file)
   ;; Help commands
   ("C-h f" . consult-apropos)
   ;; Yank
   ("M-y" . consult-yank-pop)
   ;; Goto line
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ;; Search in project
   ("C-x g" . consult-grep)
   ("C-x G" . consult-git-grep)
   ("C-x f" . consult-find)
   ("C-x F" . consult-locate)))

;; ==================================================================================
;; Consult-projectile - 项目集成
(use-package consult-projectile
  :ensure t
  :after consult
  :bind
  (:map projectile-command-map
        ("p" . consult-projectile-find-file)
        ("b" . consult-projectile-switch-project-buffer))
  :config
  (with-eval-after-load 'projectile
    (lazy-set-key
     '(("C-x p" . projectile-command-map))
     projectile-mode-map)))

;; ==================================================================================
;; Prescient - 智能排序
(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode))

(use-package vertico-prescient
  :ensure t
  :after (vertico prescient)
  :config
  (vertico-prescient-mode))

;; ==================================================================================
;; Corfu - 补全框架
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-min-width 40)
  (corfu-max-width 80)
  :config
  (global-corfu-mode))

;; Corfu 终端支持
(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode)))

;; Corfu prescient 集成
(use-package corfu-prescient
  :ensure t
  :after (corfu prescient)
  :config
  (corfu-prescient-mode))

;; ==================================================================================
;; Cape - 补全后端
(use-package cape
  :ensure t
  :after corfu
  :config
  ;; 添加补全后端
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; ==================================================================================
;; Yasnippet - 延迟加载
(use-package yasnippet
  :ensure t
  :defer t
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)
         (markdown-mode . yas-minor-mode))
  :config
  (require 'yasnippet-snippets)
  ;; Key unbindings
  (lazy-unset-key '("<tab>" "TAB") yas-minor-mode-map)
  ;; Initialize yasnippet snippets
  (yasnippet-snippets-initialize))

;; ==================================================================================
;; Ag - The Silver Searcher (保留用于 wgrep 集成)
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
;; Nerd-icons for completion
(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ==================================================================================
;; Completion keybindings
(lazy-set-key
 '(;; Avy
   ("C-; c" . avy-goto-char)
   ("C-; w" . avy-goto-word-0)
   ("C-; l" . avy-goto-line)))

;; ==================================================================================
;;; Provide features
(provide 'init-completion)

;;; init-completion.el ends here
