;;; init-python.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 21:46:48 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: python, pyvenv, poetry
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
;; Python mode configuration.

;;; Code:

;; ==================================================================================
;; Pyvenv
(use-package pyvenv
  :ensure t
  :defer t
  :hook (python-mode . pyvenv-tracking-mode))

;; ==================================================================================
;; Poetry
(use-package poetry
  :ensure t
  :defer t)

;; ==================================================================================
;; Python mode keybindings
(use-package python
  :ensure nil
  :defer t
  :bind
  (:map python-mode-map
        ("C-c C-c" . comment-line))
  :config
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil
        python-indent-guess-indent-offset-verbose nil))

;; ==================================================================================
;;; Provide features
(provide 'init-python)

;;; init-python.el ends here
