;;; omw-gitconfig.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-20 11:10:00 Friday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: git, gitconfig, gitignore, configuration
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
;; 2026-03-19 16:30 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Git configuration file modes setup.
;; Covers: gitconfig, gitignore, and related files.

;;; Code:

;; ============================================================================
(use-package git-modes
  :ensure t
  :defer t
  :mode (("/git/config\\'" . gitconfig-mode)
         ("/git/config\\.local\\'" . gitconfig-mode)
         ("/git/config\\.local\\.example\\'" . gitconfig-mode)
         ("/git/ignore\\'" . gitignore-mode)
         ("\\.gitignore_global\\'" . gitignore-mode)
         ("\\.npmignore\\'" . gitignore-mode)
         ("\\.dockerignore\\'" . gitignore-mode)
         ("\\.stow-local-ignore\\'" . gitignore-mode)))

;; ============================================================================
;;; Provide features
(provide 'omw-gitconfig)

;;; omw-gitconfig.el ends here
