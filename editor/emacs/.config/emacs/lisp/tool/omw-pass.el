;;; omw-pass.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: auth, gpg, pass, pinentry
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Authentication and credential management configuration.
;; Features: GPG integration, pass password store.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Authentication & Credentials
;; ----------------------------------------------------------------------------

;; --- EPG Configuration ---
(use-package epg-config
  :ensure nil
  :demand t
  :config
  ;; Use loopback mode for PIN entry (allows Emacs to prompt for
  ;; GPG passphrase)
  (setq epg-pinentry-mode 'loopback))

;; --- auth-source-pass ---
(use-package auth-source-pass
  :ensure nil
  :demand t
  :config
  ;; Enable pass (password-store.org) as auth source backend
  (auth-source-pass-enable))

;; --- pass ---
(use-package pass
  :ensure t
  :defer t)

;; ============================================================================
;;; Provide features
(provide 'omw-pass)

;;; omw-pass.el ends here
