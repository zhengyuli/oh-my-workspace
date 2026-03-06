;;; init-auth.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-05 13:27:58 Thursday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: auth, gpg, pass, pinentry
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
;; Authentication and credential management configuration.
;; Features: GPG integration, pass password store.

;;; Code:

;; ==================================================================================
(use-package epg-config
  :ensure nil
  :demand t
  :config
  ;; Use loopback pinentry mode, make sure pinentry
  ;; program is installed.
  (setq epg-pinentry-mode 'loopback))

;; ==================================================================================
(use-package auth-source-pass
  :ensure nil
  :demand t
  :config
  (auth-source-pass-enable))

;; ==================================================================================
(use-package pass
  :ensure t
  :defer t)

;; ==================================================================================
;;; Provide features
(provide 'init-auth)

;;; init-auth.el ends here
