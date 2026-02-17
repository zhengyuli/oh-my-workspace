;;; init-dired.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-17 20:46:08 Tuesday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
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
;; Dired configuration and extensions.

;;; Code:

;; ==================================================================================
;; Dired packages - 保留的工具包
(use-package dired-filter
  :defer t)

(use-package dired-hacks-utils
  :defer t)

(use-package async
  :defer t)

;; ==================================================================================
;; Dirvish - 现代化文件管理器
;; 替代: diredfl, nerd-icons-dired, dired-preview, dired-collapse
;; 文档: https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org
(use-package dirvish
  :ensure t
  :init
  ;; 启用 Dirvish 覆盖 dired 模式 (C-x d 自动使用 dirvish)
  (dirvish-override-dired-mode)
  :config
  ;; 快速访问目录
  (setq dirvish-quick-access-entries
        '(("h" "~/"                           "Home")
          ("d" "~/Downloads/"                 "Downloads")
          ("w" "~/oh-my-workspace/"           "Workspace")
          ("e" "~/.emacs.d/"                  "Emacs Config")))

  ;; 外观属性: 顺序对某些属性很重要
  ;; vc-state, subtree-state, nerd-icons 位置固定，顺序不重要
  ;; git-msg, file-modes, file-time, file-size 按列表顺序显示
  (setq dirvish-attributes
        (append (when (display-graphic-p) '(nerd-icons))
                '(vc-state subtree-state
                  git-msg file-time file-size)))

  ;; Side 模式属性 (用于 dirvish-side)
  (setq dirvish-side-attributes
        (append (when (display-graphic-p) '(nerd-icons))
                '(vc-state collapse file-size)))

  ;; Header line 配置 - 显示路径和剩余空间
  (setq dirvish-use-header-line 'global   ; header line 跨所有窗格
        dirvish-header-line-height 25      ; 高度
        dirvish-header-line-format
        '(:left (path) :right (free-space))
        dirvish-mode-line-bar-image-width 0) ; 隐藏前导条形图

  ;; Mode line 格式
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index))
        dirvish-mode-line-height 25)

  ;; 预览延迟
  (setq dirvish-preview-delay 0.1)

  ;; 大目录异步处理: 超过 20000 文件时使用 fd 异步加载
  (setq dirvish-large-directory-threshold 20000)

  ;; 快捷键
  (with-eval-after-load 'dirvish
    (bind-keys :map dirvish-mode-map
               ;; 帮助和菜单
               ("?"   . dirvish-dispatch)          ; 帮助菜单 (cheatsheet)
               ("a"   . dirvish-setup-menu)        ; 属性设置菜单
               ("o"   . dirvish-quick-access)      ; 快速访问
               ;; 文件操作
               ("f"   . dirvish-fd)                ; fd 搜索
               ("s"   . dirvish-quicksort)         ; 快速排序
               ("l"   . dirvish-ls-switches-menu)  ; ls 开关菜单
               ("v"   . dirvish-vc-menu)           ; 版本控制菜单
               ("*"   . dirvish-mark-menu)         ; 标记菜单
               ("y"   . dirvish-yank-menu)         ; 复制/粘贴菜单
               ;; 导航
               ("TAB" . dirvish-subtree-toggle)    ; 折叠/展开子目录
               ("N"   . dirvish-narrow)            ; 窄化搜索
               ("M-t" . dirvish-layout-toggle)     ; 切换布局
               ("r"   . dirvish-history-jump)      ; 最近访问
               ("^"   . dirvish-history-last)      ; 上一个历史
               ("M-f" . dirvish-history-go-forward)    ; 历史前进
               ("M-b" . dirvish-history-go-backward))  ; 历史后退
    )
  ;; Dirvish 当前行高亮
  :custom-face
  (dirvish-hl-line ((t (:background "#6495ED" :foreground "white")))))

;; ==================================================================================
;; Dired configuration - 基础设置 (与 Dirvish 兼容)
(use-package dired
  :ensure nil
  :hook (dired-mode . my/dired-setup)
  :config
  (require 'epa-dired)
  (require 'dired-x)
  (require 'dired-async)
  (require 'dired-custom-extension)

  ;; Customize variables
  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-deletion-confirmer 'y-or-n-p  ; 使用 y/n 确认删除
        dired-bind-info nil
        dired-omit-extensions (append dired-omit-extensions '(".cache"))
        dired-omit-files (concat dired-omit-files
                                 "\\|^\\.\\|^semantic.cache$\\|^CVS$")
        dired-filter-stack '())

  ;; macOS 兼容性: BSD ls 不支持 --dired 和 --group-directories-first
  ;; 如果安装了 coreutils (gls)，使用 GNU ls；否则禁用 ls-dired
  ;; GNU ls 使用长选项名称以兼容 dirvish-ls 扩展
  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls"
              dired-listing-switches
              "-l --almost-all --human-readable --group-directories-first --no-group"))
    ;; BSD ls 回退: 使用短选项 (BSD ls 不支持长选项)
    (setq dired-use-ls-dired nil
          dired-listing-switches "-alh"))

  ;; Key bindings (这些会被 Dirvish 继承)
  (lazy-set-key
   '(("<return>" . dired-single-buffer)
     ("RET" . dired-single-buffer)
     ("h" . dired-up-directory-single)      ; 上级目录
     ("p" . dired-hacks-previous-file)
     ("n" . dired-hacks-next-file)
     ("M-," . dired-goto-first-line)
     ("M-." . dired-goto-last-line)
     ("C-s" . isearch-forward)
     ("C-r" . isearch-backward)
     ("C-k" . dired-do-delete)
     ("r" . wdired-change-to-wdired-mode)
     ("M-o" . dired-omit-mode)
     ("E" . dired-do-touch)
     ("B" . dired-backup-file)
     ("d" . dired-diff)
     ("D" . ediff-directories)
     ("z" . dired-do-compress)
     ("Z" . dired-do-compress)
     (": e" . epa-dired-do-encrypt)
     (": d" . epa-dired-do-decrypt)
     (": s" . epa-dired-do-sign)
     (": v" . epa-dired-do-verify)
     ("M-w" . dired-copy-files)
     ("M-k" . dired-cut-files)
     ("C-y" . dired-paste-files)
     ("/m" . dired-mark-files-regexp)
     ("/*" . dired-filter-by-regexp)
     ("/." . dired-filter-by-extension)
     ("/ /" . dired-filter-pop)        ; 移除最后一个过滤器
     ("/c" . dired-filter-pop-all)     ; 清除所有过滤器
     ("/p" . dired-filter-pop)
     ("; n" . dired-get-file-name-without-path)
     ("; N" . dired-get-file-name-with-path)
     ("; p" . dired-get-file-name-only-path))
   dired-mode-map)

  ;; Enable global dired async mode
  (dired-async-mode 1)

  ;; Hooks
  (add-hook 'dired-after-readin-hook 'dired-custom-sort)

  (defun my/dired-setup ()
    "Setup dired mode."
    ;; Enable dired omit mode
    (dired-omit-mode 1)))

;; ==================================================================================
;; View-mode vim-style navigation (for viewing files from dired)
(with-eval-after-load 'view
  (lazy-set-key
   '(;; Basic movement
     ("j" . next-line)
     ("k" . previous-line)
     ("h" . backward-char)
     ("l" . forward-char)
     ;; Word movement
     ("w" . forward-word)
     ("b" . backward-word)
     ("e" . forward-word)
     ;; Line movement
     ("0" . beginning-of-line)
     ("^" . beginning-of-line)
     ("$" . end-of-line)
     ;; Page movement (vim-style: C-f/C-b for full page, C-d/C-u for half page)
     ("C-f" . View-scroll-page-forward)
     ("C-b" . View-scroll-page-backward)
     ("C-d" . View-scroll-half-page-forward)
     ("C-u" . View-scroll-half-page-backward)
     ;; Alternative: f/d for page down, b/u for page up
     ("f" . View-scroll-page-forward)
     ("d" . View-scroll-half-page-forward)
     ("u" . View-scroll-half-page-backward)
     ;; Buffer navigation
     ("g" . beginning-of-buffer)
     ("G" . end-of-buffer)
     ("%" . goto-percent)
     ;; Search
     ("/" . isearch-forward)
     ("?" . isearch-backward)
     ("n" . isearch-repeat-forward)
     ("N" . isearch-repeat-backward))
   view-mode-map)

  ;; Helper function for percentage jump
  (defun goto-percent (percent)
    "Go to PERCENT of the buffer."
    (interactive "nGo to percent: ")
    (goto-char (+ (point-min)
                  (/ (* percent (- (point-max) (point-min))) 100)))))

;; ==================================================================================
;; Dired keybindings
(add-hook 'after-init-hook
          (lambda ()
            (lazy-set-key
             '(("C-x C-d" . dired)
               ("C-x d" . dired-jump)))))

;; ==================================================================================
;;; Provide features
(provide 'init-dired)

;;; init-dired.el ends here
