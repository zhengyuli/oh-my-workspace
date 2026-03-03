;;; init-ai.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 17:39:08 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: ai, claude, coding-assistant, copilot, gptel
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
;; AI coding assistants: Claude Code IDE integration.
;; Future: GitHub Copilot, GPTel, Aider support.

;;; Code:

;; ==================================================================================
;; Claude Code IDE - AI-powered coding assistance
;; Integrates Anthropic's Claude into Emacs for code completion, debugging, and chat
(use-package claude-code-ide
  ;; Install from GitHub (not yet in ELPA/MELPA)
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el"
       :rev :newest)
  :defer t
  :commands (claude-code-ide
             claude-code-ide-resume
             claude-code-ide-continue
             claude-code-ide-toggle
             claude-code-ide-send-prompt)
  :config
  (setq
   ;; CLI Configuration
   claude-code-ide-cli-path "claude"              ; Path to Claude Code CLI
   claude-code-ide-cli-extra-flags ""            ; Extra CLI flags

   ;; Terminal Configuration
   claude-code-ide-terminal-backend 'vterm       ; Use vterm for performance
   claude-code-ide-vterm-anti-flicker t         ; Reduce vterm flicker

   ;; ------------------------------- Debug Configuration ---------------------------
   ;; Disable debug logging (set to t for troubleshooting issues)
   claude-code-ide-debug nil
   ;; Disable CLI-level debug output (avoids cluttering terminal with verbose logs)
   claude-code-ide-cli-debug nil)

  ;; Optional: Verify CLI path exists (adds robustness)
  (when (not (executable-find claude-code-ide-cli-path))
    (message "WARNING: Claude Code CLI not found at '%s' - check path configuration"
             claude-code-ide-cli-path)))

;; ==================================================================================
;; Future AI tools that can be added:
;; - GitHub Copilot (copilot.el)
;; - GPTel (gptel)
;; - Aider (aider.el)
;; - Ellama (ellama)

;; ==================================================================================
;;; Provide features
(provide 'init-ai)

;;; init-ai.el ends here
