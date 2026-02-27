;;; init-env.el -*- lexical-binding: t; -*-
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
;; Environment and platform settings: exec-path-from-shell, macOS settings.

;;; Code:

(require 'init-funcs)

;; ==================================================================================
;; Exec path from shell (macOS)
;; Optimization: use -l instead of -i to avoid slow shell startup
(use-package exec-path-from-shell
  :when (memq window-system '(mac ns))
  :defer t)

;; Initialize exec-path from shell (deferred)
;; Note: Must be outside use-package :config because :defer t means :config
;; only runs when the package is loaded. Without any autoload trigger,
;; the package never loads and initialize is never called.
(when (memq window-system '(mac ns))
  (run-config-timer 1 nil
    (lambda ()
      (require 'exec-path-from-shell)
      (exec-path-from-shell-initialize))))

;; ==================================================================================
;; Mac system settings
(add-hook 'after-init-hook
          (lambda ()
            (when (memq window-system '(mac ns))
              (setq mac-command-modifier 'super
                    mac-option-modifier 'meta))))

;; ==================================================================================
;;; Provide features
(provide 'init-env)

;;; init-env.el ends here
