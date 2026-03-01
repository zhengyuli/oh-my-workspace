;;; init-auth.el -*- lexical-binding: t; -*-
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
;; Security and authentication: pinentry, EPG config, auth-source-pass,
;; password-store.

;;; Code:

(require 'init-funcs)

;; ==================================================================================
;; Pinentry
(use-package pinentry
  :defer t)

;; Start pinentry after idle (deferred)
;; Note: Must be outside use-package :config because :defer t means :config
;; only runs when the package is loaded. pinentry-start is NOT autoloaded,
;; so we must explicitly require the package before calling it.
(run-with-idle-timer 2 nil
  (lambda ()
    (require 'pinentry)
    (pinentry-start)))

;; ==================================================================================
;; EPG config
(use-package epg-config
  :ensure nil
  :config
  (setq epg-pinentry-mode 'loopback
        epg-debug t))

;; ==================================================================================
;; Auth source pass
(use-package auth-source
  :ensure nil
  :config
  (require 'auth-source-pass)
  (auth-source-pass-enable))

;; ==================================================================================
;; Password store
(use-package password-store
  :defer t)

(use-package pass
  :defer t)

;; ==================================================================================
;; Aliases
(defalias 'refresh-auth-cache 'auth-source-forget-all-cached)

;; ==================================================================================
;; Utility Tools Validation
;; Password management tools validation
(defvar required-utility-tools
  '((pass . "brew install pass"))
  "List of utility tools.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'utility-tools
 (lambda () (config-dependency-validate-executables required-utility-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-auth)

;;; init-auth.el ends here
