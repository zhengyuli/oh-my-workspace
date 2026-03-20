;;; omw-pdf.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: pdf, pdf-tools, document-viewer
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
;; 2026-03-14 15:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; PDF viewing and navigation configuration using pdf-tools.
;; Features: PDF viewing, editing, navigation, and state restoration.

;;; Code:

;; ============================================================================
(use-package pdf-tools
  :ensure t
  :when (display-graphic-p)
  :defer t)

;; ============================================================================
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

;; ============================================================================
(defvar omw/pdf-view-restore-path
  (expand-file-name "pdf-view-restore" user-emacs-directory)
  "Pdf view restore file path, which will be used to store pdf view state.")

(use-package pdf-view-restore
  :ensure t
  :when (display-graphic-p)
  :defer t
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename omw/pdf-view-restore-path))

;; ============================================================================
;;; Provide features
(provide 'omw-pdf)

;;; omw-pdf.el ends here
