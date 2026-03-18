;;; omw-utils.el -*- lexical-binding: t; -*-

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: utils, tools
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
;; 2026-03-18 00:00 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Shared utility functions for omw Emacs configuration.
;; Provides tool installation helpers used by language modules.

;;; Code:

;; ==================================================================================
(defun omw/tools-install (&rest tool-specs)
  "Install missing development tools.
Each element of TOOL-SPECS is a list (TOOL INSTALL-CMD PACKAGE-MANAGER) where:
- TOOL is the executable name checked via `executable-find'
- INSTALL-CMD is the shell command string to install it
- PACKAGE-MANAGER is the installer executable that must be available"
  (dolist (spec tool-specs)
    (let* ((tool (nth 0 spec))
           (install-cmd (nth 1 spec))
           (pm (nth 2 spec)))
      (if (executable-find tool)
          (message "%s is already installed." tool)
        (if (executable-find pm)
            (progn
              (message "Installing %s via: %s" tool install-cmd)
              (shell-command install-cmd))
          (message "Cannot install %s: %s not found." tool pm))))))

;; ==================================================================================
(defun omw/tools-check-and-prompt (&rest tool-specs)
  "Check for missing tools and prompt user to install each one.
Each element of TOOL-SPECS is a list (TOOL INSTALL-CMD PACKAGE-MANAGER).
For each missing tool, asks whether to install it now.
No-op in batch/non-interactive mode."
  (when (not noninteractive)
    (dolist (spec tool-specs)
      (let ((tool (nth 0 spec)))
        (unless (executable-find tool)
          (when (yes-or-no-p (format "%s not found. Install now? " tool))
            (omw/tools-install spec)))))))

;; ==================================================================================
;;; Provide features
(provide 'omw-utils)

;;; omw-utils.el ends here
