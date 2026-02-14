;;; init-markdown.el -*- lexical-binding: t; -*-
;; Time-stamp: <2025-10-18 20:05:59 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
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
;; Markdown mode configuration.

;;; Code:

;; ==================================================================================
;; Valign
(use-package valign
  :defer t)

;; ==================================================================================
;; Markdownfmt
(use-package markdownfmt
  :defer t)

;; ==================================================================================
;; Impatient mode
(use-package impatient-mode
  :defer t)

;; ==================================================================================
;; Markdown mode
(use-package markdown-mode
  :defer t
  :hook (markdown-mode . markdown-mode-setup)
  :config
  (defun markdown-impatient-mode-filter (buffer)
    "Markdown filter for impatient-mode"
    (princ
     (with-temp-buffer
       (let ((tmpname (buffer-name)))
         (set-buffer buffer)
         (set-buffer (markdown tmpname))
         (format
          "<!DOCTYPE html>
           <html>
             <head>
               <title>Markdown Preview</title>
               <meta name='viewport' content='width=device-width, initial-scale=1'>
               <link rel='stylesheet'
                     href='https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/4.0.0/github-markdown.min.css'
                     integrity='sha512-Oy18vBnbSJkXTndr2n6lDMO5NN31UljR8e/ICzVPrGpSud4Gkckb8yUpqhKuUNoE+o9gAb4O/rAxxw1ojyUVzg=='
                     crossorigin='anonymous'>
               <link rel='stylesheet' href=
                     'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.2.0/styles/github.min.css'>

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
    (imp-visit-buffer))

  (defun markdown-mode-setup ()
    "Setup markdown mode."
    ;; Enable impatient mode
    (impatient-mode 1)
    ;; Setup user defined impatient mode filter
    (imp-set-user-filter 'markdown-impatient-mode-filter)
    ;; Set buffer column width to 120
    (setq-local fill-column 120)
    ;; Enable auto fill mode
    (auto-fill-mode 1)
    ;; Enable valign mode
    (valign-mode 1))

  ;; Customize variables
  (setq markdown-live-preview-window-function 'pop-to-buffer
        markdown-command "pandoc -s --mathjax"
        markdown-enable-math t
        markdown-display-remote-images t))

;; ==================================================================================
;;; Provide features
(provide 'init-markdown)

;;; init-markdown.el ends here
