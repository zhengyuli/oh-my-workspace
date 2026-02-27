;;; init-ai.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-20 21:50:18 Friday by zhengyuli>

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
;; Claude Code IDE - AI coding assistant
;; Repository: https://github.com/manzaltu/claude-code-ide.el

(use-package claude-code-ide
  ;; Use :vc because claude-code-ide is not yet available in ELPA/MELPA
  ;; TODO: Switch to :ensure t when package is published
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el"
      :rev :newest)
  :commands (claude-code-ide
             claude-code-ide-resume
             claude-code-ide-continue
             claude-code-ide-toggle
             claude-code-ide-send-prompt)
  :custom
  ;; CLI configuration
  (claude-code-ide-cli-path "claude")        ; Claude Code CLI path
  (claude-code-ide-cli-extra-flags "")       ; Extra CLI flags
  ;; Terminal configuration
  (claude-code-ide-terminal-backend 'vterm)  ; Use vterm backend
  (claude-code-ide-vterm-anti-flicker t)     ; Reduce flicker
  ;; Debug
  (claude-code-ide-debug nil)                ; Disable debug mode
  (claude-code-ide-cli-debug nil))           ; Disable CLI debug

;; ==================================================================================
;; Claude Code terminal keybinding enhancements
;; Use C-p/C-n to navigate options up/down in Claude Code buffer
(declare-function vterm-send-up "vterm" ())
(declare-function vterm-send-down "vterm" ())
(defvar vterm-mode-map)

;; Declare local functions for byte-compiler (defined in with-eval-after-load)
(declare-function claude-code-vterm-send-up "init-ai.el" ())
(declare-function claude-code-vterm-send-down "init-ai.el" ())

(with-eval-after-load 'vterm
  ;; Define vterm navigation functions
  (defun claude-code-vterm-send-up ()
    "Send Up arrow to vterm for navigating options."
    (interactive)
    (vterm-send-up))

  (defun claude-code-vterm-send-down ()
    "Send Down arrow to vterm for navigating options."
    (interactive)
    (vterm-send-down))

  ;; Bind keys directly in vterm-mode-map
  (define-key vterm-mode-map (kbd "C-p") #'claude-code-vterm-send-up)
  (define-key vterm-mode-map (kbd "C-n") #'claude-code-vterm-send-down))

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
