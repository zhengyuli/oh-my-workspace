;;; omw-utils.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-23 12:11:14 Monday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: utils, tools
;; Dependencies: (none)

;; Copyright (C) 2026 zhengyu li

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; History:
;;
;; 2026-03-18 00:00 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Shared utility functions for omw Emacs configuration.
;; Provides tool installation helpers used by language modules.

;;; Code:

;; ============================================================================
(defun omw/--install-tool-spec (spec)
  "Install a single tool described by SPEC (TOOL INSTALL-CMD PACKAGE-MANAGER).
Skips silently if TOOL is already installed; warns if
PACKAGE-MANAGER is absent."
  (let* ((tool (nth 0 spec))
         (install-cmd (nth 1 spec))
         (pm (nth 2 spec)))
    (if (executable-find tool)
        (message "%s is already installed." tool)
      (if (executable-find pm)
          (progn
            (message "Installing %s via: %s" tool install-cmd)
            (shell-command install-cmd))
        (message "Cannot install %s: %s not found." tool pm)))))

;; ============================================================================
(defun omw/tools-install (&rest tool-specs)
  "Install missing development tools.
Each element of TOOL-SPECS is a list (TOOL INSTALL-CMD PACKAGE-MANAGER).
TOOL is the executable name checked via `executable-find', INSTALL-CMD is
the shell command to install it, and PACKAGE-MANAGER is the installer
executable that must be available."
  (dolist (spec tool-specs)
    (omw/--install-tool-spec spec)))

;; ============================================================================
(defun omw/tools-check-and-prompt (&rest tool-specs)
  "Check for missing tools and prompt user to install each one.
Each element of TOOL-SPECS is a list (TOOL INSTALL-CMD PACKAGE-MANAGER).
For each missing tool, asks whether to install it now.
No-op in batch/non-interactive mode."
  (unless noninteractive
    (dolist (spec tool-specs)
      (let ((tool (nth 0 spec)))
        (unless (executable-find tool)
          (when (yes-or-no-p (format "%s not found. Install now? " tool))
            (omw/--install-tool-spec spec)))))))

;; ============================================================================
;;; Provide features
(provide 'omw-utils)

;;; omw-utils.el ends here
