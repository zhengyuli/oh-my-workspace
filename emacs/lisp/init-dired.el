;;; init-dired.el -*- lexical-binding: t; -*-
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

  ;; 外观属性: 图标（仅GUI）、文件时间、大小、子目录状态、git 状态
  (setq dirvish-attributes
        (append (when (display-graphic-p) '(nerd-icons))
                '(file-time file-size subtree-state vc-state)))

  ;; Mode line 格式
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))

  ;; 预览延迟
  (setq dirvish-preview-delay 0.1)

  ;; 快捷键
  (with-eval-after-load 'dirvish
    (bind-keys :map dirvish-mode-map
               ("?"   . dirvish-dispatch)          ; 帮助菜单
               ("a"   . dirvish-quick-access)      ; 快速访问
               ("f"   . dirvish-fd)                ; fd 搜索
               ("s"   . dirvish-quicksort)         ; 快速排序
               ("TAB" . dirvish-subtree-toggle)    ; 折叠/展开子目录
               ("M-n" . dirvish-narrow)            ; 窄化搜索
               ("M-t" . dirvish-layout-toggle))    ; 切换布局
    ))

;; ==================================================================================
;; 以下包已被 Dirvish 内置功能替代，已禁用

;; diredfl - 被 Dirvish 内置着色替代
;; (use-package diredfl
;;   :ensure t
;;   :hook (dired-mode . diredfl-mode))

;; nerd-icons-dired - 被 Dirvish 的 nerd-icons 属性替代
;; (use-package nerd-icons-dired
;;   :ensure t
;;   :defer t)

;; dired-preview - 被 Dirvish 内置预览替代
;; (use-package dired-preview
;;   :ensure t
;;   :defer t)

;; dired-collapse - 被 Dirvish 的 collapse 属性替代
;; (use-package dired-collapse
;;   :ensure t
;;   :defer t)

;; dired-narrow - 被 Dirvish 的 dirvish-narrow 替代
;; (use-package dired-narrow
;;   :ensure t
;;   :commands (dired-narrow)
;;   :init
;;   (with-eval-after-load 'dired
;;     (bind-key "//" #'dired-narrow dired-mode-map)))

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
  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls"
              dired-listing-switches "-alh --group-directories-first"))
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
