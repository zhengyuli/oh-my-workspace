;;; init-dired.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-01 23:14:49 Sunday by zhengyuli>

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
;; Editing enhancements: buffer utilities, vundo, move-text, expand-region,
;; multiple-cursors, smart copy/kill, etc.

;;; Code:

;; ==================================================================================
;;; init-dired.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Modern Dired + Dirvish configuration

;;; Code:

;; ==================================================================================
(use-package nerd-icons-dired
  :ensure t
  :defer t
  :hook (dired-mode . nerd-icons-dired-mode))

;; ==================================================================================
(use-package dired
  :ensure nil
  :defer t
  :init

  ;; ;; 稳定排序
  ;; (setq dired-listing-switches
  ;;       "-alh --group-directories-first")

  ;; ;; 自动刷新
  (setq dired-auto-revert-buffer t)

  ;; 递归操作
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)

  ;; 智能目标
  (setq dired-dwim-target t)

  ;; 提升性能
  (setq dired-kill-when-opening-new-dired-buffer t)

  :config
  (require 'dired-x)
  (require 'epa-dired)

  ;; omit 设置
  (setq dired-omit-extensions
        (append dired-omit-extensions '(".cache")))

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^\\.\\|^semantic.cache$\\|^CVS$")))

;; ==================================================================================
;; Dirvish（核心）
;; ==================================================================================

(use-package dirvish
  :ensure t
  :defer t

  ;; ⭐ override 必须放 init
  :init
  (dirvish-override-dired-mode)

  :config
  ;; UI
  (setq dirvish-attributes
        '(vc-state
          subtree-state
          nerd-icons
          collapse
          file-time
          file-size))

  ;; 布局
  (setq dirvish-default-layout '(0 0 0.4))

  ;; preview
  (setq dirvish-preview-delay 0.1)

  (setq dirvish-preview-dispatchers
        '(image video audio pdf archive))

  ;; 打开文件
  (setq dirvish-open-with-programs
        '(("pdf" . ("open"))
          ("mp4" . ("mpv"))
          ("mkv" . ("mpv"))))

  ;; UI 清爽
  (setq dirvish-use-mode-line nil)

  ;; 侧边栏
  (setq dirvish-side-follow-mode t)

  ;; override dired
  (dirvish-override-dired-mode))

;; ==================================================================================
;; Dirvish extras（可选）
;; ==================================================================================

(use-package dirvish-extras
  :after dirvish
  :defer t)

;; ==================================================================================
;; Keybindings（保留原始习惯 + modern）
;; ==================================================================================

(with-eval-after-load 'dired

  ;; Vim 风格导航
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map (kbd "l") #'dired-find-file)
  (define-key dired-mode-map (kbd "j") #'dired-next-line)
  (define-key dired-mode-map (kbd "k") #'dired-previous-line)

  ;; modern navigation
  (define-key dired-mode-map (kbd "TAB") #'dirvish-subtree-toggle)

  ;; dirvish menus
  (define-key dired-mode-map (kbd "C-c C-m") #'dirvish-mark-menu)
  (define-key dired-mode-map (kbd "C-c C-e") #'dirvish-emerge)
  (define-key dired-mode-map (kbd "C-c C-f") #'dirvish-file-info-menu))

;; ==================================================================================
;; 快捷入口
;; ==================================================================================

(global-set-key (kbd "C-x C-d") #'dired)
(global-set-key (kbd "C-x d") #'dired-jump)

;; ==================================================================================
;; macOS 依赖检测
;; ==================================================================================

(defvar required-dired-tools
  '((gls . "brew install coreutils")
    (fd . "brew install fd")))

;; ==================================================================================
;;; Provide features
(provide 'init-dired)

;;; init-dired.el ends here
