;;; omw-elisp.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: elisp, emacs-lisp, lisp
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Emacs Lisp mode with enhanced navigation and syntax highlighting.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Emacs Lisp
;; ----------------------------------------------------------------------------

;; --- elisp-slime-nav ---
(use-package elisp-slime-nav
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; --- lisp-extra-font-lock ---
(use-package lisp-extra-font-lock
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))

;; --- rainbow-mode ---
(use-package rainbow-mode
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . rainbow-mode))

;; --- Elisp Mode Setup ---
(defun omw/elisp-mode-setup ()
  "Apply custom settings for Emacs Lisp mode."
  (eldoc-mode 1))

(use-package elisp-mode
  :ensure nil
  :defer t
  :hook (emacs-lisp-mode . omw/elisp-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-elisp)

;;; omw-elisp.el ends here
