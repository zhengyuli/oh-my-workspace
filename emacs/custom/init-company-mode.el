;;; package --- init-company-mode.el ---
;; Time-stamp: <2020-12-01 14:56:11 Tuesday by lizhengyu>

;; Copyright (C) 2018 zhengyu li
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
;;   (require 'init-company-mode)

;;; Require:
(require 'company)
(require 'company-quickhelp)
(require 'company-quickhelp-terminal)
(require 'company-yasnippet)

;;; Code:
;; ==================================================================================
(defun append-backend-with-yas (backend)
  "Append `BACKEND' with company-yas."
  (if (and (listp backend)
		   (member 'company-yasnippet backend))
      backend
    (append (if (consp backend)
				backend
			  (list backend))
            '(:with company-yasnippet))))

;; ==================================================================================
;; Customize company related faces
(custom-set-faces
 '(company-tooltip ((t :background "#4D4D4D" )))
 '(company-tooltip-selection ((t :background "#8b0000")))
 '(company-tooltip-common ((t :foreground "#0000FF")))
 '(company-scrollbar-fg ((t :inherit company-tooltip)))
 '(company-scrollbar-bg ((t :inherit company-tooltip))))

;; Customize company related variables
(customize-set-variable 'company-idle-delay 0)
(customize-set-variable 'company-minimum-prefix-length 1)
(customize-set-variable 'company-tooltip-limit 15)
(customize-set-variable 'company-transformers '(company-sort-by-occurrence))
(customize-set-variable 'company-selection-wrap-around t)
(customize-set-variable 'company-backends (mapcar 'append-backend-with-yas company-backends))

;; ==================================================================================
;; Enable global company mode
(global-company-mode 1)

;; Enable global company quickhelp mode
(if (display-graphic-p)
	(company-quickhelp-mode 1)
  (company-quickhelp-terminal-mode 1))


;; ==================================================================================
;;; Provide features
(provide 'init-company-mode)

;;; init-company-mode.el ends here
