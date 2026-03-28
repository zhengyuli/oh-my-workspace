;;; omw-template.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-18 00:00:00 Tuesday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: template, yasnippet, autoinsert, snippet
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
;; 2026-03-14 19:10 zhengyu li <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Template system using yasnippet and autoinsert for file header generation.

;;; Code:

;; ============================================================================
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
(defun omw/define-auto-insert-custom (condition action)
  "Add or update auto-insert rule for CONDITION with ACTION.
CONDITION is a regex matching file names.
ACTION is a template file or function to insert."
  ;; Check if rule exists and update, otherwise add new rule
  (let ((elt (assoc condition auto-insert-alist)))
    (if elt
        (setcdr elt action)
      (push (cons condition action) auto-insert-alist))))

(defun omw/autoinsert-yas-expand ()
  "Expand YASnippet template in current buffer."
  (require 'yasnippet)
  ;; Use buffer content as snippet template and expand
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;; Configure auto-insert to use templates directory
(use-package autoinsert
  :ensure nil
  :defer t
  :hook (after-init . auto-insert-mode)
  :config
  (setq auto-insert 'other
        auto-insert-directory (concat omw/emacs-config-root-path "/templates/"))

  (omw/define-auto-insert-custom
    '("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C/C++ header")
    ["template.h" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'" . "C/C++ source")
    ["template.c" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.py\\'" . "Python header")
    ["template.py" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.go\\'" . "Golang header")
    ["template.go" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.el\\'" . "Emacs Lisp header")
    ["template.el" omw/autoinsert-yas-expand])
  (omw/define-auto-insert-custom
    '("\\.sh\\'" . "Shell script header")
    ["template.sh" omw/autoinsert-yas-expand]))

;; ============================================================================
;;; Provide features
(provide 'omw-template)

;;; omw-template.el ends here
