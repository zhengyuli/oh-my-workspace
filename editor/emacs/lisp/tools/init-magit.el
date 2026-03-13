;;; init-magit.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-05 17:07:53 Thursday by zhengyu.li>

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
(use-package magit
  :ensure t
  :defer t
  :bind (("C-c g s" . magit-status)
         ("C-c g l" . magit-log-all)))

;; ==================================================================================
;;; Provide features
(provide 'init-magit)

;;; init-magit.el ends here
