;;; init-completion.el -*- lexical-binding: t; -*-

;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: completion, corfu, vertico, consult
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
;; 2026-03-14 15:30 chieftain <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; Modern completion framework with vertico (UI), orderless (matching),
;; marginalia (annotations), consult (commands), and embark (actions).

;;; Code:

;; ==================================================================================
(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; ==================================================================================
(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

;; ==================================================================================
(use-package consult
  :ensure t
  :defer t
  :bind (("C-s" . consult-line)
         ("C-r" . consult-line-multi)
         ("C-x b" . consult-buffer)
         ("C-x B" . consult-recent-file)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-x g" . consult-ripgrep)
         ("C-x f" . consult-find)))

;; ==================================================================================
;; Will be loaded by embark automatically.
(use-package embark-consult
  :ensure t
  :defer t)

(use-package embark
  :ensure t
  :defer t
  :bind (("C-." . embark-act)
         ("C-;" . embar-dwim)))

;; ==================================================================================
(defun omw/corfu-mode-setup ()
  "Configure corfu and corfu-popupinfo."
  (require 'corfu)
  (require 'corfu-popupinfo)

  (setq corfu-auto t)
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

(use-package corfu
  :ensure t
  :defer t
  :hook (after-init . omw/corfu-mode-setup))

(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

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
  :after yasnippet
  :config
  (yasnippet-snippets-initialize))

(use-package yasnippet-capf
  :ensure t
  :defer t)

;; ==================================================================================
(use-package cape
  :ensure t
  :demand t
  :config
  (add-to-list 'completion-at-point-functions (cape-capf-super
                                               #'yasnippet-capf
                                               #'cape-file
                                               #'cape-dabbrev)))

;; ==================================================================================
;;; Provide features
(provide 'init-completion)

;;; init-completion.el ends here
