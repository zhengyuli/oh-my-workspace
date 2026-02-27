;;; init-highlight.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 14:00:00 Thursday by zhengyuli>

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
;; Visual highlights: pulsar, emojify, winner-mode.

;;; Code:

(require 'init-funcs)

;; ==================================================================================
;; Pulsar - cursor highlighting (replaces beacon)
(use-package pulsar
  :defer t
  :hook (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse-functions '(recenter-top-bottom
                                  move-to-window-line-top-bottom
                                  reposition-window
                                  bookmark-jump
                                  other-window
                                  delete-other-windows
                                  forward-page
                                  backward-page
                                  scroll-up-command
                                  scroll-down-command
                                  windmove-right
                                  windmove-left
                                  windmove-up
                                  windmove-down
                                  tab-new
                                  tab-close
                                  tab-next
                                  tab-previous)
        pulsar-delay 0.055))

;; ==================================================================================
;; Emojify - enable only in specific modes
(use-package emojify
  :defer t
  :hook ((org-mode . emojify-mode)
         (markdown-mode . emojify-mode)
         (text-mode . emojify-mode)))

;; ==================================================================================
;; Winner mode - undo/redo window layout
(winner-mode 1)

;; ==================================================================================
;; Highlight keybindings
(add-hook 'after-init-hook
          (lambda ()
            (lazy-set-key
             '(;; Undo window layout (use C-c w u to avoid overriding C-/ undo)
               ("C-c w u" . winner-undo)
               ("C-c w r" . winner-redo)))))

;; ==================================================================================
;;; Provide features
(provide 'init-highlight)

;;; init-highlight.el ends here
