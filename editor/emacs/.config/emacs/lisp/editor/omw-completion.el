;;; omw-completion.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>
;;
;; ============================================================================
;; omw-completion.el - Completion framework with vertico, corfu, and cape.
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: completion, corfu, vertico, cape, orderless
;; Dependencies: (none)
;;
;; Copyright (C) 2026 zhengyu li
;;
;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.
;;
;;; Commentary:
;;
;; Modern completion framework with vertico for UI, orderless for matching,
;; marginalia for annotations, cape for completion extensions, and corfu popup.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Completion Framework
;; ----------------------------------------------------------------------------

;; --- Orderless ---
(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; --- Vertico ---
(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode))

;; --- Marginalia ---
(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

;; --- Cape ---
(use-package cape
  :ensure t
  :demand t
  :config
  (push (cape-capf-super #'yasnippet-capf
                         #'cape-file
                         #'cape-dabbrev)
        completion-at-point-functions))

;; --- Corfu ---
(defun omw/corfu-init ()
  "Enable corfu auto-completion with documentation popup."
  (setq corfu-auto t)
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

(use-package corfu
  :ensure t
  :defer t
  :hook (after-init . omw/corfu-init))

(use-package corfu-terminal
  :ensure t
  :when (not (display-graphic-p))
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; ============================================================================
;;; Provide features
(provide 'omw-completion)

;;; omw-completion.el ends here
