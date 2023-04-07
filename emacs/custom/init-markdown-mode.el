;;; package --- init-markdown-mode.el -*- lexical-binding:t -*-
;; Time-stamp: <2023-04-07 09:11:40 Friday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023 zhengyu li
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
;;   (require 'init-markdown-mode)

;;; Require:

;;; Code:
;; ==================================================================================
;; Customized settings for `markdown-mode'
(defun markdown-mode-settings ()
  "Settings for `markdown-mode'."

  ;; Require
  (require 'valign)
  (require 'markdownfmt)
  (require 'impatient-mode)

  ;; ----------------------------------------------------------
  (defun markdown-impatient-mode-filter (buffer)
    "Markdown filter for impatient-mode"
    (princ
     (with-temp-buffer
       (let ((tmpname (buffer-name)))
         (set-buffer buffer)
         (set-buffer (markdown tmpname))
         (format "
<!DOCTYPE html>
<html>
  <head>
    <title>Markdown Preview</title>
    <meta name='viewport' content='width=device-width, initial-scale=1'>
    <link rel='stylesheet'
          href='https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/4.0.0/github-markdown.min.css'
          integrity='sha512-Oy18vBnbSJkXTndr2n6lDMO5NN31UljR8e/ICzVPrGpSud4Gkckb8yUpqhKuUNoE+o9gAb4O/rAxxw1ojyUVzg=='
          crossorigin='anonymous'>
    <!-- https://github.com/sindresorhus/github-markdown-css -->
    <link rel='stylesheet' href=
          'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.2.0/styles/github.min.css'>
    <!-- https://highlightjs.org -->

    <style>
      .markdown-body {
          box-sizing: border-box;
          margin: 0 auto;
          max-width: 980px;
          min-width: 200px;
          padding: 45px;
      }

      @media (max-width: 767px) {
          .markdown-body {
              padding: 15px;
          }
      }
    </style>
  </head>
  <body>
    <article class='markdown-body'>
      %s
    </article>
    <script src='https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.2.0/highlight.min.js'></script>
    <script id='MathJax-script' src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>
    <script>
      hljs.highlightAll();
    </script>
  </body>
</html>" (buffer-string))))
     (current-buffer)))

  (defun markdown-live-preview ()
    "Markdown live preview."
    (interactive)
    (let ((browse-url-browser-function 'xwidget-webkit-browse-url))
      (delete-other-windows)
      (split-window-right)
      (other-window 1)
      (imp-visit-buffer)
      (switch-to-buffer xwidget-webkit-last-session-buffer)))

  ;; ----------------------------------------------------------
  ;; Customize `markdown-mode' related variables
  (customize-set-variable 'markdown-live-preview-window-function 'pop-to-buffer)
  (customize-set-variable 'markdown-command "pandoc -s --mathjax")
  (customize-set-variable 'markdown-enable-math t)
  (customize-set-variable 'markdown-display-remote-images t)

  ;; ----------------------------------------------------------
  ;; Hooks
  (add-hook 'markdown-mode-hook
            (lambda ()
              ;; ----------------------------------------------------------
              ;; Enable impatient mode
              (impatient-mode 1)

              ;; Setup user defined impatient mode filter
              (imp-set-user-filter 'markdown-impatient-mode-filter)

              ;; Set buffer column width to 120
              (setq-local fill-column 120)

              ;; Enable auto fill mode
              (auto-fill-mode 1)

              ;; enable valign mode
              (valign-mode 1)

              ;; Copy the global before-save-hook to a local hook
              (setq-local before-save-hook (default-value 'before-save-hook))

              ;; Format buffer before save
              (add-hook 'before-save-hook 'markdownfmt-format-buffer nil t))))

(eval-after-load "markdown-mode" '(markdown-mode-settings))

;; ==================================================================================
;;; Provide features
(provide 'init-markdown-mode)

;;; init-markdown-mode.el ends here
