;;; init-terminal.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-21 21:33:12 Saturday by zhengyuli>

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
;; Terminal configuration: vterm, multi-vterm.

;;; Code:

(require 'init-funcs)

;; ==================================================================================
;; Vterm - modern terminal emulator
(use-package vterm
  :commands (vterm)
  :custom
  (vterm-max-scrollback 10000)           ; Maximum scrollback history
  (vterm-scroll-enable-emacs-bar t)      ; Emacs-style scrollbar
  (vterm-kill-buffer-on-exit t)          ; Kill buffer on exit
  (vterm-enable-manipulate-selection-data t) ; Allow selection manipulation
  :config
  ;; Key bindings
  (lazy-set-key
   '(("C-g" . vterm--self-insert)
     ("M-<backspace>" . vterm-send-meta-backspace))
   vterm-mode-map)

  ;; Hooks
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Disable auto fill mode
              (auto-fill-mode -1)
              ;; Disable yasnippet mode (check availability first)
              (when (fboundp 'yas-minor-mode)
                (yas-minor-mode -1))
              ;; Disable corfu mode (check availability first)
              (when (fboundp 'corfu-mode)
                (corfu-mode -1))
              ;; Disable line highlighting (use hl-line-mode, not global-hl-line-mode)
              (when (fboundp 'hl-line-mode)
                (hl-line-mode -1))
              ;; Unbind M-0~9 to allow winnum to work
              (lazy-unset-key '("M-0" "M-1" "M-2" "M-3" "M-4"
                                "M-5" "M-6" "M-7" "M-8" "M-9")
                              vterm-mode-map)
              ;; Set buffer-local variables
              (setq-local truncate-lines t))))

;; ==================================================================================
;; Multi-vterm
(use-package multi-vterm
  :commands (multi-vterm))

;; ==================================================================================
;; Terminal keybinding
(add-hook 'after-init-hook
          (lambda ()
            (lazy-set-key
             '(("C-x C-t" . multi-vterm)))))

;; ==================================================================================
;; Terminal Tools Validation
;; Terminal tools validation (vterm module requires libvterm library compilation)
(defvar required-terminal-tools
  '((vterm-dump . "brew install libvterm (vterm compilation dependency)"))
  "List of terminal tools.
vterm-dump is provided by libvterm homebrew package.")

(config-dependency-register
 'terminal-tools
 (lambda () (config-dependency-validate-executables required-terminal-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-terminal)

;;; init-terminal.el ends here
