;;; omw-pdf.el -*- lexical-binding: t; -*-

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: pdf, pdf-tools, document-viewer
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
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; PDF viewing and navigation configuration using pdf-tools.
;; Features: PDF viewing, editing, navigation, and state restoration.

;;; Code:

;; ==================================================================================
(use-package pdf-tools
  :ensure t
  :when (display-graphic-p)
  :defer t)

;; ==================================================================================
(use-package pdf-view
  :ensure nil
  :when (display-graphic-p)
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-fit-height-to-window)
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("+" . pdf-view-enlarge)
              ("-" . pdf-view-shrink))
  :config
  (pdf-tools-install :no-query))

;; ==================================================================================
(defvar omw/pdf-view-restore-path (expand-file-name "pdf-view-restore" user-emacs-directory)
  "Pdf view restore file path, which will be used to store pdf view state.")

(use-package pdf-view-restore
  :ensure t
  :when (display-graphic-p)
  :defer t
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename omw/pdf-view-restore-path))

;; ==================================================================================
;;; Provide features
(provide 'omw-pdf)

;;; omw-pdf.el ends here
