;;; init-vc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: init-functions

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
;; Version control configuration: magit.

;;; Code:

;; ==================================================================================
;; Magit - Git version control interface
(use-package magit
  :commands (magit-status magit-log-all)
  :custom
  (magit-diff-refine-hunk t)             ; Show character-level differences
  (magit-revert-buffers 'silent)         ; Silently revert files
  (magit-no-message '("Turning on magit-auto-revert-mode"))
  :config
  (require 'magit-diff)

  ;; Customized faces
  ;; Semantic: added = green, removed = red
  (custom-set-faces
   '(magit-diff-added ((t (:background "#98FB98" :foreground "black"))))              ; light green
   '(magit-diff-removed ((t (:background "#FFB6C1" :foreground "black"))))            ; light red
   '(magit-diff-added-highlight ((t (:background "#90EE90" :foreground "black"))))    ; highlighted green
   '(magit-diff-removed-highlight ((t (:background "#F08080" :foreground "black"))))  ; highlighted red
   '(magit-diff-hunk-heading-highlight ((t (:background "#383838" :foreground "white"))))))

;; ==================================================================================
;; Function aliases
(defalias 'git-status 'magit-status)
(defalias 'git-log 'magit-log-all)

;; ==================================================================================
;; VC Tools Validation
;; Version control tools validation
(defvar required-vc-tools
  '((git . "brew install git"))
  "List of required version control tools.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'vc-tools
 (lambda () (config-dependency-validate-executables required-vc-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-vc)

;;; init-vc.el ends here
