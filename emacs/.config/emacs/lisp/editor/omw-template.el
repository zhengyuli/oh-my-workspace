;;; omw-template.el -*- lexical-binding: t; -*-

;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: template, yasnippet, autoinsert, snippet
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
;; 2026-03-14 19:10 chieftain <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Template system using yasnippet and autoinsert for automatic file header generation.

;;; Code:

;; ==================================================================================
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
  :defer t
  :after yasnippet
  :config
  (yasnippet-snippets-initialize))

(use-package yasnippet-capf
  :ensure t
  :defer t)

;; ==================================================================================
(defun omw/define-auto-insert-custom (condition action)
  "Add or update auto-insert rule for CONDITION with ACTION.
CONDITION is a regex matching file names.
ACTION is a template file or function to insert."
  (require 'autoinsert)
  ;; Check if rule exists and update, otherwise add new rule
  (let ((elt (assoc condition auto-insert-alist)))
    (if elt
        (setcdr elt action)
      (add-to-list 'auto-insert-alist (cons condition action)))))

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

;; ==================================================================================
;;; Provide features
(provide 'omw-template)

;;; omw-template.el ends here
