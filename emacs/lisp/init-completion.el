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
;; Which-key - 按键提示，延迟加载优化启动时间
(use-package which-key
  :ensure t
  :defer 1                                   ; 延迟 1 秒加载
  :custom
  (which-key-idle-delay 0.5)                 ; 按键后 0.5 秒显示提示
  (which-key-idle-secondary-delay 0.05)      ; 后续提示延迟
  (which-key-sort-order 'which-key-key-order-alpha)  ; 按字母排序
  (which-key-show-remaining-keys t)          ; 显示剩余按键
  :config
  (which-key-setup-side-window-right)
  (which-key-mode 1))

;; ==================================================================================
;; Vertico - 垂直补全 UI
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)                       ; 循环选择
  (vertico-count 15)                      ; 显示 15 个候选
  (vertico-resize t)                      ; 自适应高度
  (vertico-scroll-margin 4)               ; 滚动边距
  :config
  (vertico-mode)
  ;; 按目录分组显示文件
  (vertico-mouse-mode))

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
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy  ; 详细注解
                           marginalia-annotators-light)) ; 轻量注解
  (marginalia-align-offset 15)            ; 对齐偏移
  :config
  (marginalia-mode))

;; ==================================================================================
;; Consult - 增强命令
(use-package consult
  :ensure t
  :custom
  (consult-line-numbers-widen t)          ; 自动扩展行号
  (consult-async-min-input 2)             ; 异步搜索最少输入
  (consult-async-refresh-delay 0.15)      ; 刷新延迟
  (consult-async-input-throttle 0.2)      ; 输入节流
  (consult-preview-key 'any)              ; 任意键触发预览
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
;; Embark - 上下文操作
(use-package embark
  :ensure t
  :defer t
  :custom
  (embark-prompter 'embark-keymap-prompter)    ; 使用 keymap 提示器
  (embark-cycle-key nil)                       ; 禁用循环键
  (embark-help-key "C-h")                      ; 帮助键
  :bind
  (;; 上下文操作
   ("C-." . embark-act)                        ; 对当前目标执行操作
   ("C-," . embark-dwim)                       ; 智能默认操作
   ("C-h B" . embark-bindings)                 ; 查看所有按键绑定
   :map embark-command-map
   ("C-." . embark-act)))

;; ==================================================================================
;; Embark-consult - Embark 与 Consult 集成
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;; ==================================================================================
;; Consult-projectile - 项目集成
(use-package consult-projectile
  :ensure t
  :after (consult projectile)
  :config
  (with-eval-after-load 'projectile
    (bind-key "f" #'consult-projectile-find-file projectile-command-map)
    (bind-key "b" #'consult-projectile-switch-project-buffer projectile-command-map)))

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
  (corfu-cycle t)                      ; 循环选择候选
  (corfu-auto t)                       ; 自动补全
  (corfu-auto-delay 0.2)               ; 输入后延迟 0.2 秒触发，避免输入卡顿
  (corfu-auto-prefix 2)                ; 至少输入 2 个字符才触发补全
  (corfu-min-width 40)                 ; 弹窗最小宽度
  (corfu-max-width 80)                 ; 弹窗最大宽度
  (corfu-count 12)                     ; 显示 12 个候选
  (corfu-scroll-margin 4)              ; 滚动边距
  (corfu-echo-documentation 0.25)      ; 0.25 秒后显示文档
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
;; 顺序很重要: file > keyword > dabbrev (dabbrev 最慢，放最后)
(use-package cape
  :ensure t
  :after corfu
  :config
  ;; 设置补全后端顺序（从高到低优先级）
  (setq completion-at-point-functions
        (list #'cape-file           ; 文件路径补全（最快）
              #'cape-keyword        ; 关键字补全
              #'cape-dabbrev        ; 动态缩写补全（最慢）
              #'cape-line)))        ; 行补全

;; ==================================================================================
;; Yasnippet - 延迟加载
(use-package yasnippet
  :ensure t
  :defer t
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)
         (markdown-mode . yas-minor-mode))
  :config
  ;; Key unbindings
  (lazy-unset-key '("<tab>" "TAB") yas-minor-mode-map))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

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
;; Avy - 快速跳转
(use-package avy
  :ensure t
  :defer t
  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p))
  (avy-background t)                      ; 背景模式，非目标变暗
  (avy-all-windows nil)                   ; 仅当前窗口
  (avy-timeout-seconds 0.3)               ; 超时时间
  (avy-style 'pre))                       ; 标签风格

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
