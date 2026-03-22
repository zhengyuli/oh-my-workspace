;;; omw-git.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-22 13:51:42 Sunday by zhengyuli>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: vc, git, diff, merge
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
;; Version control integration with Magit for powerful Git operations.

;;; Code:

;; ============================================================================
(use-package magit
  :ensure t
  :defer t
  :bind (("C-c g s" . magit-status)
         ("C-c g S" . magit-status-here)
         ("C-c g l" . magit-log-all)
         ("C-c g b" . magit-blame)
         ("C-c g d" . magit-dispatch)
         :map magit-status-mode-map
         ("q" . magit-kill-this-buffer)
         :map magit-section-mode-map
         ("M-1" . nil)
         ("M-2" . nil)
         ("M-3" . nil)
         ("M-4" . nil))
  :config
  ;; Show word-level diff refinement in all hunks
  (setq magit-diff-refine-hunk 'all))

;; ============================================================================
;;; Provide features
(provide 'omw-git)

;;; omw-git.el ends here
