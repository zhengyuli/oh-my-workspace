;;; package --- init-go-mode.el ---
;; Time-stamp: <2022-03-09 23:25:13 Wednesday by zhengyu.li>

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
;;   (require 'init-go-mode)

;;; Require:
(require 'go-mode-autoloads)

;;; Code:
;; ==================================================================================
(defun go-mode-settings ()
  "Settings for `go-mode'."

  ;; Require
  (require 'go-eldoc)

  ;; ----------------------------------------------------------
  ;; Load golang related envs
  (when (memq window-system '(mac ns))
	(require 'exec-path-from-shell)
    (exec-path-from-shell-copy-env "GOROOT")
    (exec-path-from-shell-copy-env "GOPATH"))

  ;; ----------------------------------------------------------
  ;; Set `prog-mode-map' as the parent of `go-mode-map'
  (set-keymap-parent go-mode-map prog-mode-map)

  ;; ----------------------------------------------------------
  ;; Key bindings for `go-mode'
  (lazy-set-key
   '(("M-." . godef-jump))
   go-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks for `go-mode'
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda ()
                            ;; ----------------------------------------------------------
							(go-eldoc-setup))))

(eval-after-load "go-mode" '(go-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-go-mode)

;;; init-go-mode.el ends here
