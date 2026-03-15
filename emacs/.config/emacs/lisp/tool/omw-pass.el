;;; omw-pass.el -*- lexical-binding: t; -*-

;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: auth, gpg, pass, pinentry
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;; Licensed under the GPL License version 3.0

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; History:
;;
;; 2026-03-14 15:30 chieftain <lizhengyu419@outlook.com> created.

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
  ;; Use loopback mode for PIN entry (allows Emacs to prompt for GPG passphrase)
  (setq epg-pinentry-mode 'loopback))

;; ==================================================================================
(use-package auth-source-pass
  :ensure nil
  :demand t
  :config
  ;; Enable pass (password-store.org) as auth source backend
  (auth-source-pass-enable))

;; ==================================================================================
(use-package pass
  :ensure t
  :defer t)

;; ==================================================================================
;;; Provide features
(provide 'omw-pass)

;;; omw-pass.el ends here
