;;; init-auth.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-02 22:16:32 星期一 by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: auth, gpg, pass, pinentry
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
;; Pinentry
(use-package pinentry
  :ensure t
  :defer t
  :hook (after-init . pinentry-start))

;; ==================================================================================
;; EPG config
(use-package epg-config
  :ensure nil
  :demand t
  :config
  (setq epg-pinentry-mode 'loopback
        epg-debug t))

;; ==================================================================================
;; Auth source pass
(use-package auth-source
  :ensure nil
  :demand t
  :config
  (require 'auth-source-pass)
  (auth-source-pass-enable))

;; ==================================================================================
;; Password store
(use-package password-store
  :ensure t
  :defer t)

(use-package pass
  :ensure t
  :defer t)

;; ==================================================================================
;;; Provide features
(provide 'init-auth)

;;; init-auth.el ends here
