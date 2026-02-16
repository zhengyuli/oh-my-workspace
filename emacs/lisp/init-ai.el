;;; init-ai.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-16 21:00:00 Monday by zhengyuli>

;; Copyright (C) 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: ai, claude, coding-assistant, copilot, gptel

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
;; AI coding assistants configuration.
;; Currently includes: Claude Code IDE
;; Future: GitHub Copilot, GPTel, Aider, etc.

;;; Code:

;; ==================================================================================
;; Claude Code IDE - AI 编程助手
;; Repository: https://github.com/manzaltu/claude-code-ide.el

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el"
      :rev :newest)
  :commands (claude-code-ide
             claude-code-ide-resume
             claude-code-ide-continue
             claude-code-ide-toggle
             claude-code-ide-send-prompt)
  :custom
  ;; CLI 配置
  (claude-code-ide-cli-path "claude")        ; Claude Code CLI 路径
  (claude-code-ide-cli-extra-flags "")       ; 额外 CLI 参数
  ;; 终端配置
  (claude-code-ide-terminal-backend 'vterm)  ; 使用 vterm 后端
  (claude-code-ide-vterm-anti-flicker t)     ; 减少闪烁
  ;; 调试
  (claude-code-ide-debug nil)                ; 关闭调试模式
  (claude-code-ide-cli-debug nil))           ; 关闭 CLI 调试

;; ==================================================================================
;; Claude Code 终端快捷键增强
;; 在 Claude Code buffer 中使用 C-p/C-n 上下选择选项
(declare-function vterm-send-up "vterm" ())
(declare-function vterm-send-down "vterm" ())
(defvar vterm-mode-map)

;; 为 byte-compiler 声明本地函数（在 with-eval-after-load 中定义）
(declare-function my/claude-code-vterm-send-up "init-ai.el" ())
(declare-function my/claude-code-vterm-send-down "init-ai.el" ())

(with-eval-after-load 'vterm
  ;; 定义 vterm 导航函数
  (defun my/claude-code-vterm-send-up ()
    "Send Up arrow to vterm for navigating options."
    (interactive)
    (vterm-send-up))

  (defun my/claude-code-vterm-send-down ()
    "Send Down arrow to vterm for navigating options."
    (interactive)
    (vterm-send-down))

  ;; 直接在 vterm-mode-map 中绑定按键
  (define-key vterm-mode-map (kbd "C-p") #'my/claude-code-vterm-send-up)
  (define-key vterm-mode-map (kbd "C-n") #'my/claude-code-vterm-send-down))

;; ==================================================================================
;; 未来可添加的 AI 工具：
;; - GitHub Copilot (copilot.el)
;; - GPTel (gptel)
;; - Aider (aider.el)
;; - Ellama (ellama)

;; ==================================================================================
;;; Provide features
(provide 'init-ai)

;;; init-ai.el ends here
