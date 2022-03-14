;;; package --- init-company.el ---
;; Time-stamp: <2022-03-14 14:45:13 Monday by zhengyuli>

;; Copyright (C) 2021, 2022 zhengyu li
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: none

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

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'init-company)

;;; Require:
(require 'company-autoloads)

;;; Code:
;; ==================================================================================
(defun company-settings ()
  "Settings for `company'."

  ;; Require
  (require 'company-box)
  (require 'company-quickhelp)
  (require 'company-quickhelp-terminal)
  (require 'company-yasnippet)
  (require 'company-jedi)

  ;; ----------------------------------------------------------
  (defun append-company-backend-with-yas (backend)
    "Append `BACKEND' with company-yas."
    (if (and (listp backend)
		     (member 'company-yasnippet backend))
        backend
      (append (if (consp backend)
				  backend
			    (list backend))
              '(:with company-yasnippet))))

  ;; ----------------------------------------------------------
  (defun python-mode-company-settings ()
    "Settings for `python-mode' company backends."

    ;; ----------------------------------------------------------
    ;; Hooks for `python-mode'
    (add-hook 'python-mode-hook
              (lambda ()
                ;; ----------------------------------------------------------
                ;; Add `company-jedi' backend
                (make-local-variable 'company-backends)
                (add-to-list 'company-backends
                             (append-company-backend-with-yas 'company-jedi)))))

  (eval-after-load "python" '(python-mode-company-settings))

  ;; ----------------------------------------------------------
  ;; Customize `company' related faces
  (custom-set-faces
   '(company-tooltip ((t :background "#4D4D4D" )))
   '(company-tooltip-selection ((t :background "#8B0000")))
   '(company-tooltip-common ((t :bold t :foreground "#CAFF70")))
   '(company-scrollbar-fg ((t :inherit company-tooltip)))
   '(company-scrollbar-bg ((t :inherit company-tooltip))))

  ;; Customize `popup' related faces
  (custom-set-faces
   '(popup-tip-face ((t :foreground "#000000" :background "#FFF8DC"))))

  ;; Customize `company-quickhelp' related colors
  (customize-set-variable 'company-quickhelp-color-background "#FFF8DC")
  (customize-set-variable 'company-quickhelp-color-foreground "#000000")

  ;; ----------------------------------------------------------
  ;; Customize `company' related variables
  (customize-set-variable 'company-idle-delay 0)
  (customize-set-variable 'company-minimum-prefix-length 1)
  (customize-set-variable 'company-tooltip-limit 15)
  (customize-set-variable 'company-selection-wrap-around t)
  (customize-set-variable 'company-format-margin-function 'company-text-icons-margin)
  (customize-set-variable 'company-transformers '(delete-dups company-sort-by-occurrence))
  (customize-set-variable 'company-backends
                          (mapcar 'append-company-backend-with-yas company-backends))

  ;; customize `company-box' related variables
  (customize-set-variable 'company-box-scrollbar nil)
  (customize-set-variable 'company-box-doc-enable t)

  ;; ----------------------------------------------------------
  ;; Hooks for `company'
  (add-hook 'company-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable quickhelp terminal mode if any
              (if (display-graphic-p)
                  ;; Enable company box mode for graphic mode
                  (company-box-mode 1)
                ;; Enable quickhelp terminal mode for on-graphic mode
                (progn
                  (company-quickhelp-mode 1)
                  (company-quickhelp-terminal-mode 1))))))

(eval-after-load "company" '(company-settings))

;; ==================================================================================
;; Settings after init
(add-hook 'after-init-hook
          (lambda ()
            ;; ----------------------------------------------------------
            ;; Enable global company mode
            (global-company-mode 1)))

;; ==================================================================================
;;; Provide features
(provide 'init-company)

;;; init-company.el ends here
