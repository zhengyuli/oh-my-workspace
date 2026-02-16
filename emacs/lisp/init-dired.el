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
;; Dired packages
(use-package dired-filter
  :defer t)

(use-package dired-hacks-utils
  :defer t)

;; dired-narrow - 实时过滤文件
;; 按 // 开始输入，实时缩小文件范围
(use-package dired-narrow
  :ensure t
  :commands (dired-narrow)
  :init
  (with-eval-after-load 'dired
    (bind-key "//" #'dired-narrow dired-mode-map)))

;; dired-collapse - 折叠嵌套空目录
;; 将 a/b/c/file.txt 折叠显示为单行
;; 已禁用：如需启用，手动 M-x dired-collapse-mode
(use-package dired-collapse
  :ensure t
  :defer t)

;; diredfl - 替换过时的 dired-filetype-face
;; 从 Dired+ 提取的 fontification 规则，更活跃维护
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :ensure t
  :defer t)

;; dired-preview - 自动预览文件 (默认禁用，手动 M-x dired-preview-mode 启用)
(use-package dired-preview
  :ensure t
  :defer t)

(use-package async
  :defer t)

;; ==================================================================================
;; Dired configuration
(use-package dired
  :ensure nil
  :hook (dired-mode . my/dired-setup)
  :config
  (require 'epa-dired)
  (require 'dired-x)
  (require 'dired-async)
  (require 'dired-custom-extension)

  ;; Customized faces
  (custom-set-faces
   '(dired-header ((t (:foreground "#EE82EE" :height 1.1))))
   '(dired-directory ((t (:foreground "#51AFEF" :height 1.1))))
   '(dired-mark ((t (:foreground "#FF1493" :inverse-video nil))))
   '(dired-marked ((t (:foreground "#FFFF00" :inverse-video nil)))))

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

  ;; Key bindings
  (lazy-set-key
   '(("<return>" . dired-single-buffer)
     ("RET" . dired-single-buffer)
     ("p" . dired-hacks-previous-file)
     ("n" . dired-hacks-next-file)
     ("M-," . dired-goto-first-line)
     ("M-." . dired-goto-last-line)
     ("h" . dired-up-directory-single)
     ("C-s" . isearch-forward)
     ("C-r" . isearch-backward)
     ("C-k" . dired-do-delete)
     ("k" . kill-this-buffer)
     ("r" . wdired-change-to-wdired-mode)
     ("M-o" . dired-omit-mode)
     ("E" . dired-do-touch)
     ("B" . dired-backup-file)
     ("?" . dired-get-size)
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
     ("/ /" . dired-filter-remove)      ; 移除最后一个过滤器
     ("/c" . dired-filter-clear)        ; 清除所有过滤器
     ("/p" . dired-filter-pop)          ; 弹出最后一个过滤器
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
    (dired-omit-mode 1)
    ;; Enable nerd icons dired mode
    (when (display-graphic-p)
      (nerd-icons-dired-mode 1))))

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
