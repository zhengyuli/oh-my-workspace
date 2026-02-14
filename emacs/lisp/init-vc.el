;;; init-vc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none

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
;; Magit
(use-package magit
  :commands (magit-status magit-log-all)
  :config
  (require 'magit-diff)

  ;; Customized faces
  (custom-set-faces
   '(magit-diff-added ((t (:background "#919191" :foreground "white"))))
   '(magit-diff-removed ((t (:background "#474747" :foreground "white"))))
   '(magit-diff-added-highlight ((t (:background "#B22222" :foreground "white"))))
   '(magit-diff-removed-highlight ((t (:background "#98FB98" :foreground "black"))))
   '(magit-diff-hunk-heading-highlight ((t (:background "#383838" :foreground "white"))))))

;; ==================================================================================
;; Aliases
(defalias 'git-status 'magit-status)
(defalias 'git-log 'magit-log-all)

;; ==================================================================================
;;; Provide features
(provide 'init-vc)

;;; init-vc.el ends here
