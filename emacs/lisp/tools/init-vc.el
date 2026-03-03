;;; init-vc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 22:16:07 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: vc, git, diff, merge
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
;; Magit - Git Integration for Emacs
;; Magit provides a powerful, intuitive interface for Git operations within Emacs,
;; replacing the need for most command-line Git interactions with a visual workflow.
(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-log-all)
  :bind
  ((;; Global keybinding for Magit status (core entry point to Git workflow)
    "C-c g s" . magit-status)
   ;; Global keybinding for viewing full Git commit log (all branches)
   ("C-c g l" . magit-log-all))
  :config
  ;; Load magit-diff
  (require 'magit-diff)

  ;; --------------------------------------------------------------------------
  ;; Core Magit Configuration (Optional Enhancements)
  ;; Improve diff performance and readability
  (setq
   ;; Show word-level diffs in hunks (more precise)
   magit-diff-refine-hunk 'all
   magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; --------------------------------------------------------------------------
  ;; Custom Magit Diff Faces (Semantic Coloring)
  ;; Semantic color scheme: green for added content, red for removed content
  (custom-set-faces
   ;; Basic added line (light green background, black text for contrast)
   '(magit-diff-added ((t (:background "#98FB98" :foreground "black"))))
   ;; Basic removed line (light pink background, black text for contrast)
   '(magit-diff-removed ((t (:background "#FFB6C1" :foreground "black"))))
   ;; Highlighted added line (darker green for active hunk)
   '(magit-diff-added-highlight ((t (:background "#90EE90" :foreground "black"))))
   ;; Highlighted removed line (darker red for active hunk)
   '(magit-diff-removed-highlight ((t (:background "#F08080" :foreground "black"))))
   ;; Highlighted diff hunk heading (dark gray background, white text)
   '(magit-diff-hunk-heading-highlight ((t (:background "#383838" :foreground "white"))))))

;; ==================================================================================
;;; Provide features
(provide 'init-vc)

;;; init-vc.el ends here
