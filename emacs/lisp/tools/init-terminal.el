;;; init-terminal.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 22:16:13 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: terminal, vterm, eshell
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

;;; Code:

;; ==================================================================================
;; Vterm - Emacs Terminal Emulator
;; Vterm provides a fast, feature-rich terminal emulator for Emacs with native PTY support,
;; offering better performance than built-in shell modes like eshell or term.
(use-package vterm
  :ensure t
  :defer t
  :commands (vterm)
  :bind
  (:map vterm-mode-map
        ;; Fix C-g to insert literal C-g instead of interrupting vterm
        ("C-g" . vterm--self-insert)
        ;; Send meta-backspace (Alt+Backspace) to terminal (delete word left)
        ("M-<backspace>" . vterm-send-meta-backspace))
  :hook
  (vterm-mode . my/vterm-mode-setup)
  :config
  (setq
   ;; Maximum scrollback history lines (10k for sufficient history)
   vterm-max-scrollback 10000
   ;; Use Emacs-style scrollbar instead of terminal scrollbar
   vterm-scroll-enable-emacs-bar t
   ;; Auto-kill vterm buffer when terminal exits (cleanup)
   vterm-kill-buffer-on-exit t
   ;; Allow Emacs to manipulate terminal selection data
   vterm-enable-manipulate-selection-data t)

  ;; --------------------------------------------------------------------------
  ;; Custom Vterm Mode Setup (Consolidated Hook Function)
  (defun my/vterm-mode-setup ()
    "Custom initialization for vterm-mode (buffer-local settings & mode disabling)."
    ;; Disable auto-fill mode (prevents line wrapping in terminal)
    (auto-fill-mode -1)

    ;; Disable line highlighting (avoids visual clutter in terminal)
    (when (fboundp 'hl-line-mode)
      (hl-line-mode -1))

    ;; Disable snippet/completion modes (avoid conflicts with terminal input)
    ;; Disable yasnippet if available
    (when (fboundp 'yas-minor-mode)
      (yas-minor-mode -1))
    ;; Disable corfu completion if available
    (when (fboundp 'corfu-mode)
      (corfu-mode -1))

    ;; Unbind M-0~9 to allow winnum package to control window switching
    (dolist (key '("M-0" "M-1" "M-2" "M-3" "M-4"
                   "M-5" "M-6" "M-7" "M-8" "M-9"))
      (define-key vterm-mode-map (kbd key) nil))

    ;; Buffer-local settings (only apply to vterm buffers)
    ;; Disable line wrapping (use terminal's own wrapping)
    (setq-local truncate-lines t)))

;; ==================================================================================
;; Multi-vterm
(use-package multi-vterm
  :ensure t
  :defer t
  :commands (multi-vterm)
  :bind ("C-x C-t" . multi-vterm))

;; ==================================================================================
;;; Provide features
(provide 'init-terminal)

;;; init-terminal.el ends here
