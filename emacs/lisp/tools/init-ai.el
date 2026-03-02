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
;; AI coding assistants configuration.
;; Currently includes: Claude Code IDE
;; Future: GitHub Copilot, GPTel, Aider, etc.

;;; Code:

;; ==================================================================================
;; Claude Code IDE - AI-Powered Code Assistance
;; claude-code-ide integrates Anthropic's Claude AI into Emacs, providing code completion,
;; debugging, and natural language code assistance via the Claude Code CLI.
(use-package claude-code-ide
  :defer t
  ;; Install from GitHub (package not yet published to ELPA/MELPA)
  ;; TODO: Replace :vc with :ensure t once package is available in official repos
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el"
       :rev :newest)
  :commands (claude-code-ide
             claude-code-ide-resume
             claude-code-ide-continue
             claude-code-ide-toggle
             claude-code-ide-send-prompt)
  :config
  (setq
   ;; ------------------------------ CLI Configuration ------------------------------
   ;; Path to Claude Code CLI executable (assumes "claude" is in $PATH)
   claude-code-ide-cli-path "claude"
   ;; Extra command-line flags passed to the Claude Code CLI (e.g., "--model claude-3-5-sonnet")
   claude-code-ide-cli-extra-flags ""

   ;; ---------------------------- Terminal Configuration ---------------------------
   ;; Terminal backend for Claude Code IDE (vterm = fast, native PTY support)
   claude-code-ide-terminal-backend 'vterm
   ;; Reduce visual flicker in vterm when interacting with Claude AI
   claude-code-ide-vterm-anti-flicker t

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
