;;; omw-template.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-04-02 21:21:54 Thursday by zhengyu.li>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: template, yasnippet, snippet
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-14 19:10 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Template system using yasnippet for file header generation.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Template System
;; ----------------------------------------------------------------------------

(use-package yasnippet
  :ensure t
  :defer t
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil))
  :config
  (setq yas-triggers-in-field t))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yasnippet-snippets-initialize))

(use-package yasnippet-capf
  :ensure t
  :defer t)

;; ============================================================================
;;; Provide features
(provide 'omw-template)

;;; omw-template.el ends here
