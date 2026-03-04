;;; init-completion.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-04 13:44:35 Wednesday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: completion, corfu, vertico, consult
;; Dependencies: (none)

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Modern completion framework with vertico (UI), orderless (matching),
;; marginalia (annotations), consult (commands), and embark (actions).

;;; Code:

;; ==================================================================================
(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic)))

;; ==================================================================================
(use-package marginalia
  :ensure t
  :defer t)

;; ==================================================================================
(use-package vertico
  :ensure t
  :defer t)

;; ==================================================================================
(use-package consult
  :ensure t
  :defer t
  :bind* (("C-s" . consult-line)
          ("C-x b" . consult-buffer)
          ("C-x B" . consult-recent-file)
          ("M-y" . consult-yank-pop)
          ("M-g g" . consult-goto-line)
          ("M-g M-g" . consult-goto-line)
          ("C-x g" . consult-grep)
          ("C-x G" . consult-git-grep)
          ("C-x f" . consult-find)))

;; ==================================================================================
(use-package embark
  :ensure t
  :defer t
  :bind* (("C-." . embark-act)
          ("C-;" . embark-dwim)))

;; ==================================================================================
(use-package embark-consult
  :ensure t
  :after (consult embark)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :config
  (setq embark-collect-use-consult-preview t))

;; ==================================================================================
(use-package corfu
  :ensure t
  :defer t
  :config
  (setq corfu-auto t)
  (corfu-popupinfo-mode 1))

;; ==================================================================================
(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; ==================================================================================
(use-package cape
  :ensure t
  :defer t)

;; ==================================================================================
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yasnippet-snippets-initialize))

(use-package yasnippet
  :ensure t
  :defer t
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil))
  :config
  (setq yas-triggers-in-field t))

(use-package yasnippet-capf
  :ensure t
  :defer t)

;; ==================================================================================
(use-package emacs
  :ensure nil
  :demand t
  :hook ((after-init . marginalia-mode)
         (after-init . vertico-mode)
         (after-init . global-corfu-mode)))

;; ==================================================================================
;;; Provide features
(provide 'init-completion)

;;; init-completion.el ends here
