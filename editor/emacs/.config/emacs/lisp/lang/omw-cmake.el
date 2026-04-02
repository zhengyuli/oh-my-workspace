;;; omw-cmake.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 10:16:24 Friday by zhengyu.li>

;; ============================================================================
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: cmake
;; Dependencies: omw-prog, omw-utils

;; Copyright (C) 2026 zhengyu li

;;; History:
;;
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; CMake mode configuration with LSP support.
;; LSP server (cmake-language-server) is configured in omw-prog.el.
;; ============================================================================

;; ----------------------------------------------------------------------------
;; CMake Configuration
;; ----------------------------------------------------------------------------

;; --- Cmake Tool Specs ---
(defconst omw/cmake-tool-specs
  '(("cmake-language-server" "uv tool install cmake-language-server" "uv"))
  "Tool specs for CMake development.")

(defun omw/install-cmake-tools ()
  "Install CMake LSP tools (cmake-language-server) via uv if not present."
  (interactive)
  (apply #'omw/tools-install omw/cmake-tool-specs))

;; --- Cmake Mode Setup ---
(defun omw/cmake-mode-setup ()
  "Apply custom settings for cmake mode."
  (setq-local cmake-tab-width 2)
  (apply #'omw/tools-check-and-prompt omw/cmake-tool-specs))

(use-package cmake-mode
  :ensure t
  :defer t
  :hook (cmake-mode . omw/cmake-mode-setup))

;; ============================================================================
;;; Provide features
(provide 'omw-cmake)

;;; omw-cmake.el ends here
