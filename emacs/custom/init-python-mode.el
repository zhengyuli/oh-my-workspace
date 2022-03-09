;;; package --- init-python-mode.el ---
;; Time-stamp: <2022-03-09 23:22:19 Wednesday by zhengyu.li>

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
;;   (require 'init-python-mode)

;;; Require:

;;; Code:
;; ==================================================================================
;; Customized settings for `python-mode'
(defun python-mode-settings ()
  "Settings for `python-mode'."

  ;; Require
  (require 'flycheck)
  (require 'py-yapf)
  (require 'sphinx-doc)
  (require 'python-docstring)
  (require 'jedi-core)
  (require 'anaconda-mode)
  (require 'virtualenvwrapper)

  ;; ----------------------------------------------------------
  (defun sphinx-doc-format ()
    "Sphinx documentation format."
    (interactive)
    (sphinx-doc)
    (python-docstring-fill))

  (defun flycheck-virtualenv-set-python-executables ()
	(let ((exec-path (python-shell-calculate-exec-path)))
	  (setq-local flycheck-python-pylint-executable
				  (executable-find "pylint"))
	  (setq-local flycheck-python-flake8-executable
				  (executable-find "flake8"))))

  (defadvice venv-workon (after venv-workon-after activate)
    (jedi:stop-server)
	(flycheck-virtualenv-set-python-executables))

  (defadvice venv-deactivate (after venv-deactivate-after activate)
    (jedi:stop-server)
	(flycheck-virtualenv-set-python-executables))

  ;; ----------------------------------------------------------
  ;; Customize `python-mode' related variables
  (customize-set-variable 'python-indent-guess-indent-offset-verbose nil)
  (customize-set-variable 'python-indent-offset 4)

  ;; ----------------------------------------------------------
  ;; Define `python-mode' related aliases
  (defalias 'python-format 'py-yapf-buffer)

  ;; ----------------------------------------------------------
  ;; Key bindings for `python-mode'
  (lazy-set-key
   '(("C-c C-c" . smart-comment)
     ("C-c k" . smart-uncomment)
	 ("M-r" . anaconda-mode-find-references)
     ("M-." . jedi:goto-definition)
     ("M-," . jedi:goto-definition-pop-marker)
     ("M-*" . jedi:show-doc)
     ("C-c d f" . sphinx-doc-format))
   python-mode-map)

  ;; ----------------------------------------------------------
  ;; Hooks for `python-mode'
  (add-hook 'python-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable sphinx doc mode
              (sphinx-doc-mode 1)

              ;; Enable python docstring mode
              (python-docstring-mode 1)

              ;; Initialize python virtual env interactive shells
              (venv-initialize-interactive-shells)

              ;; Setup python virtual env flycheck checker
			  (flycheck-virtualenv-set-python-executables))))

(eval-after-load "python" '(python-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-python-mode)

;;; init-python-mode.el ends here
