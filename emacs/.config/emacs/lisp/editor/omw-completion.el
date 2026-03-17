;;; omw-completion.el -*- lexical-binding: t; -*-

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: completion, corfu, vertico, cape, orderless
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
;; Modern completion framework with vertico (UI), orderless (matching),
;; marginalia (annotations), cape (completion extensions), and corfu (popup).

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

;; ==================================================================================
(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

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
  :when (not (display-graphic-p))
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; ==================================================================================
;;; Provide features
(provide 'omw-completion)

;;; omw-completion.el ends here
