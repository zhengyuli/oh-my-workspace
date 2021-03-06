;;; package --- init-proxy.el ---
;; Time-stamp: <2020-11-25 17:53:07 Wednesday by lizhengyu>

;; Copyright (C) 2013 zhengyu li
;;
;; Author: zhengyu li <lizhengyu419@gmail.com>
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
;;   (require 'init-proxy)

;;; Require:
(require 'url-vars)

;;; Code:
;; ==================================================================================
(defun show-proxy ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
	  (message "Current proxy is \"%s\"" (cdr (nth 1 url-proxy-services)))
	(message "No proxy")))

(defun set-proxy (proxy)
  "Set http/https proxy to `PROXY'."
  (interactive "sProxy Server:")
  (setq url-proxy-services
		`(("no_proxy" . "^\\(127.0.0.1\\|localhost\\|10\\..*\\|192\\.168\\..*\\)")
		  ("http" . ,proxy)
		  ("https" . ,proxy)))
  (show-proxy))

(defun unset-proxy ()
  "Unset http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (show-proxy))

;; ==================================================================================
;;; Provide features
(provide 'init-proxy)

;;; init-proxy.el ends here
