;;; package --- init.el -*- lexical-binding:t -*-
;; Time-stamp: <2026-02-16 21:05:38 zhengyuli>

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
;; Emacs configuration entry point.
;; This file bootstraps use-package and loads all configuration modules.

;;; Code:

;; ==================================================================================
;; Basic check
(unless (>= (string-to-number emacs-version) 30.2)
  (error "The Emacs version must be >= 30.2."))

;; ==================================================================================
;; Early initialization
;; GC 调优 - 启动时增大阈值，加快启动速度
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; 阻止 package.el 过早加载
(setq package-enable-at-startup nil)

;; 启用 package-quickstart 加速（Emacs 27+）
(when (fboundp 'package-quickstart-refresh)
  (setq package-quickstart t))

;; 阻止 UI 元素短暂显示 (避免启动时闪烁)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; 原生编译设置 (Emacs 29+)
(when (boundp 'native-comp-deferred-compilation)
  (setq native-comp-deferred-compilation t
        native-comp-async-report-warnings-errors 'silent))

;; 用 y/n 替代 yes/no (Emacs 28+)
(when (boundp 'use-short-answers)
  (setq use-short-answers t))

;; ==================================================================================
;; Global variables
;; Emacs configuration root path (动态获取，支持符号链接)
;; 使用 file-chase-links 解析符号链接，获取真实文件路径
(defvar emacs-config-root-path
  (let ((config-file (or load-file-name buffer-file-name)))
    (if config-file
        (file-name-directory (file-chase-links config-file))
      default-directory))
  "Emacs configuration root path.
Automatically resolves symlinks to find the actual configuration directory.")

(defvar emacs-config-custom-settings-path (expand-file-name "lisp/" emacs-config-root-path)
  "Emacs configuration custom settings path.")

(defvar emacs-config-custom-site-packages-path (expand-file-name "site-packages/" emacs-config-root-path)
  "Emacs configuration custom site packages path.")

;; Fonts
(defvar emacs-config-fixed-font "Source Code Pro"
  "Emacs configuration fixed font.")
(defvar emacs-config-fixed-serif-font "Source Serif Pro"
  "Emacs configuration fixed serif font.")
(defvar emacs-config-variable-font "Times New Roman"
  "Emacs configuration variable font.")

;; User info
(defvar emacs-config-user "Zhengyu Li"
  "Emacs configuration user.")
(defvar emacs-config-email "lizhengyu419@outlook.com"
  "Emacs configuration email.")

;; Proxy
(defvar emacs-http-proxy nil
  "Emacs configuration http proxy, default is nil.")

;; GC optimization - save original values
(defvar gc-cons-threshold-original gc-cons-threshold
  "Original value of gc-cons-threshold.")
(defvar gc-cons-percentage-original gc-cons-percentage
  "Original value of gc-cons-percentage.")

;; ==================================================================================
;; Helper function for load-path
(defun add-subdirs-to-load-path (base-dir)
  "Add subdirs to load path.
Look up all subdirs under `BASE-DIR' recursively and add them into load path."
  (let ((default-directory base-dir))
    (add-to-list 'load-path base-dir)
    (normal-top-level-add-subdirs-to-load-path)))

;; ==================================================================================
;; Add custom directories to load-path first
(add-subdirs-to-load-path emacs-config-custom-settings-path)
(add-subdirs-to-load-path emacs-config-custom-site-packages-path)

;; ==================================================================================
;; Package manager setup (优化版)
(require 'package)

;; Add package archives (添加 NonGNU ELPA)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; 设置优先级：melpa > nongnu > gnu
(setq package-archive-priorities
      '(("melpa" . 10)
        ("nongnu" . 5)
        ("gnu" . 0)))

;; Initialize packages
(package-initialize)

;; 异步刷新包列表（仅当需要时）
(defun my/package-refresh-contents-async ()
  "Asynchronously refresh package contents if needed."
  (unless package-archive-contents
    (message "Refreshing package archives...")
    (package-refresh-contents)))

;; Install and configure use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-compute-statistics t
      use-package-verbose t
      use-package-minimum-reported-time 0.1)

;; ==================================================================================
;; GC optimization with gcmh
(use-package gcmh
  :ensure t
  :demand t
  :custom
  (gcmh-idle-delay 10)                   ; 空闲 10 秒后 GC
  (gcmh-high-cons-threshold #x10000000)  ; 256MB
  :config
  (gcmh-mode 1))

;; ==================================================================================
;; Load utility functions
(require 'init-functions)

;; ==================================================================================
;; Font verification (仅 GUI 模式)
(when (display-graphic-p)
  (ensure-font-installed emacs-config-fixed-font)
  (ensure-font-installed emacs-config-fixed-serif-font)
  (ensure-font-installed emacs-config-variable-font))

;; ==================================================================================
;; Try to enable HTTP proxy
(enable-http-proxy)

;; ==================================================================================
;; Load configuration modules
;; Core modules
(require 'init-ui)
(require 'init-editing)
(require 'init-completion)
(require 'init-dired)
(require 'init-vc)
(require 'init-terminal)
(require 'init-utilities)

;; Language modules
(require 'init-prog-base)
(require 'init-text)
(require 'init-elisp)
(require 'init-cc)
(require 'init-python)
(require 'init-go)
(require 'init-haskell)
(require 'init-shell)
(require 'init-dockerfile)
(require 'init-cmake)
(require 'init-yaml)
(require 'init-markdown)

;; Tool modules
(require 'init-ai)

;; ==================================================================================
;; Restore GC settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold gc-cons-threshold-original
                  gc-cons-percentage gc-cons-percentage-original)
            ;; 后台刷新包列表（下次启动更快）
            (run-with-idle-timer 60 nil #'my/package-refresh-contents-async)
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time
                      (time-subtract after-init-time before-init-time))
                     gcs-done)))

;; ==================================================================================
;; Load user custom settings
(let ((custom-settings-file (expand-file-name "~/.emacs.d/custom_settings.el")))
  (unless (file-exists-p custom-settings-file)
    (make-directory (file-name-directory custom-settings-file) t)
    (write-region "" nil custom-settings-file))
  (load-file custom-settings-file))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((claude-code-ide :url
		      "https://github.com/manzaltu/claude-code-ide.el"))))
;; 注: custom-set-faces 已移至各模块文件中
;; - centaur-tabs 面部在 init-ui.el 中定义
;; - dired 面部在 init-dired.el 中定义
