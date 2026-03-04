;;; init-vc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-04 13:41:54 Wednesday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: vc, git, diff, merge
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
;; Version control integration with Magit for powerful Git operations.

;;; Code:

;; ==================================================================================
;; Magit - complete Git interface for Emacs
;; Replaces command-line Git with intuitive visual workflow
(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-log-all)
  :bind (("C-c g s" . magit-status)    ; Open Magit status
         ("C-c g l" . magit-log-all))   ; View commit log
  :config
  (require 'magit-diff)

  ;; Core Magit configuration
  (setq magit-diff-refine-hunk 'all               ; Show word-level diffs
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; Custom diff faces: green for additions, red for deletions
  (custom-set-faces
   '(magit-diff-added ((t (:background "#98FB98" :foreground "black"))))
   '(magit-diff-removed ((t (:background "#FFB6C1" :foreground "black"))))
   '(magit-diff-added-highlight ((t (:background "#90EE90" :foreground "black"))))
   '(magit-diff-removed-highlight ((t (:background "#F08080" :foreground "black"))))
   '(magit-diff-hunk-heading-highlight ((t (:background "#383838" :foreground "white"))))))

;; ==================================================================================
;;; Provide features
(provide 'init-vc)

;;; init-vc.el ends here
