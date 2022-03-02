;;; package --- init-netease-cloud-music.el ---
;; Time-stamp: <2022-03-02 13:18:59 星期三 by zhengyli>

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
;;   (require 'init-netease-cloud-music)

;;; Require:
(require 'netease-cloud-music-autoloads)

;;; Code:
;; ==================================================================================
;; Backward compatibility, if emacs version < 27.1
(unless (fboundp 'make-empty-file)
  (defun make-empty-file (filename)
    "Create an empty file."
    (with-temp-buffer
      (write-file filename))))

;; ==================================================================================
;;; Provide features
(provide 'init-netease-cloud-music)

;;; init-netease-cloud-music.el ends here
