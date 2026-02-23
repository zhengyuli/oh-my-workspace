;;; init-completion.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-21 21:32:00 Saturday by zhengyuli>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: none
;; Dependencies: init-functions

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
;; Completion system: vertico, marginalia, consult, corfu, yasnippet, which-key, avy, ag.

;;; Code:

;; ==================================================================================
;; Which-key - key hints, deferred loading for faster startup
(use-package which-key
  :defer t
  :custom
  (which-key-idle-delay 0.5)                 ; Show hints 0.5s after keystroke
  (which-key-idle-secondary-delay 0.05)      ; Subsequent hint delay
  (which-key-sort-order 'which-key-key-order-alpha)  ; Sort alphabetically
  (which-key-show-remaining-keys t)          ; Show remaining keys
  :init
  ;; Start which-key after 2 seconds idle (triggers lazy loading)
  (run-with-idle-timer 2 nil #'which-key-mode)
  :config
  (which-key-setup-side-window-right))

;; ==================================================================================
;; Vertico - vertical completion UI
(use-package vertico
  :custom
  (vertico-cycle t)                       ; Cycle through candidates
  (vertico-count 15)                      ; Show 15 candidates
  (vertico-resize t)                      ; Adaptive height
  (vertico-scroll-margin 4)               ; Scroll margin
  :config
  (vertico-mode)
  ;; Enable mouse support
  (vertico-mouse-mode))

;; ==================================================================================
;; Orderless - fuzzy matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; ==================================================================================
;; Marginalia - completion annotations
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy  ; Detailed annotations
                           marginalia-annotators-light)) ; Lightweight annotations
  (marginalia-align-offset 15)            ; Alignment offset
  :config
  (marginalia-mode))

;; ==================================================================================
;; Consult - enhanced commands
(use-package consult
  :custom
  (consult-line-numbers-widen t)          ; Auto-widen line numbers
  (consult-async-min-input 2)             ; Minimum input for async search
  (consult-async-refresh-delay 0.15)      ; Refresh delay
  (consult-async-input-throttle 0.2)      ; Input throttle
  ;; Preview optimization: use delayed preview to avoid frequent triggers
  ;; 'any = trigger on any key (may be too frequent)
  ;; "M-." = preview only on M-. (on-demand)
  ;; (:keys "any" :delay 0.3) = any key + 0.3s delay
  (consult-preview-key 'any)              ; Trigger preview on any key
  :bind
  (;; Search
   ("C-s" . consult-line)
   ("C-r" . isearch-backward)             ; Backward search
   ;; Buffer and file navigation
   ("C-x b" . consult-buffer)
   ("C-x B" . consult-recent-file)
   ;; Help commands - use built-in apropos
   ("C-h f" . apropos-command)
   ;; Yank
   ("M-y" . consult-yank-pop)
   ;; Goto line
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ;; Search in project
   ("C-x g" . consult-grep)
   ("C-x G" . consult-git-grep)
   ("C-x f" . consult-find)
   ("C-x F" . consult-locate)))

;; ==================================================================================
;; Embark - context actions
(use-package embark
  :defer t
  :custom
  (embark-prompter 'embark-keymap-prompter)    ; Use keymap prompter
  (embark-cycle-key nil)                       ; Disable cycle key
  (embark-help-key "C-h")                      ; Help key
  :bind
  (;; Context actions
   ("C-." . embark-act)                        ; Act on current target
   ("C-," . embark-dwim)                       ; Smart default action
   ("C-h B" . embark-bindings)                 ; View all key bindings
   :map embark-command-map
   ("C-." . embark-act)))

;; ==================================================================================
;; Embark-consult - Embark and Consult integration
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;; ==================================================================================
;; Consult-projectile - project integration
(use-package consult-projectile
  :after (consult projectile)
  :config
  (with-eval-after-load 'projectile
    (bind-key "f" #'consult-projectile-find-file projectile-command-map)
    (bind-key "b" #'consult-projectile-switch-project-buffer projectile-command-map)))

;; ==================================================================================
;; Prescient - smart sorting
(use-package prescient
  :config
  (prescient-persist-mode))

(use-package vertico-prescient
  :after (vertico prescient)
  :config
  (vertico-prescient-mode))

;; ==================================================================================
;; Corfu - completion framework
(use-package corfu
  :custom
  (corfu-cycle t)                      ; Cycle through candidates
  (corfu-auto t)                       ; Auto completion
  (corfu-auto-delay 0.2)               ; Trigger 0.2s after input to avoid lag
  (corfu-auto-prefix 2)                ; At least 2 characters to trigger completion
  (corfu-min-width 40)                 ; Popup minimum width
  (corfu-max-width 80)                 ; Popup maximum width
  (corfu-count 12)                     ; Show 12 candidates
  (corfu-scroll-margin 4)              ; Scroll margin
  (corfu-echo-documentation 0.25)      ; Show documentation after 0.25s
  :config
  (global-corfu-mode))

;; Corfu terminal support
(use-package corfu-terminal
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode)))

;; Corfu prescient integration
(use-package corfu-prescient
  :after (corfu prescient)
  :config
  (corfu-prescient-mode))

;; ==================================================================================
;; Cape - completion backends
;; Order matters: file > keyword > dabbrev > line
(use-package cape
  :after corfu
  :config
  ;; Set completion backend order (highest to lowest priority)
  (setq completion-at-point-functions
        (list #'cape-file           ; File path completion (fastest)
              #'cape-keyword        ; Keyword completion
              #'cape-dabbrev        ; Dynamic abbreviation completion
              #'cape-line)))        ; Line completion

;; ==================================================================================
;; Yasnippet - deferred loading
(use-package yasnippet
  :defer t
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)
         (markdown-mode . yas-minor-mode))
  :config
  ;; Key unbindings
  (lazy-unset-key '("<tab>" "TAB") yas-minor-mode-map))

(use-package yasnippet-snippets
  :after yasnippet)

;; ==================================================================================
;; Ag - The Silver Searcher (retained for wgrep integration)
(use-package ag
  :commands (ag ag-project ag-dired-regexp)
  :config
  (require 'wgrep-ag)

  ;; Advice for selecting window after next-error - check before adding to prevent accumulation
  (defun ag-next-error-function-after (&rest _)
    "Select ag buffer window after next-error."
    (select-window
     (get-buffer-window (ag/buffer-name "" "" ""))))

  ;; Only add advice if not already present (prevents accumulation on config reload)
  (unless (advice-member-p #'ag-next-error-function-after 'ag/next-error-function)
    (advice-add 'ag/next-error-function :after #'ag-next-error-function-after))

  (setq ag-reuse-buffers t
        wgrep-enable-key "r"
        wgrep-auto-save-buffer t)

  ;; Hooks
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)

  (add-hook 'ag-search-finished-hook
            (lambda ()
              (select-window
               (get-buffer-window (ag/buffer-name "" "" ""))))))

;; ==================================================================================
;; Avy - fast jumping
(use-package avy
  :defer t
  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p))
  (avy-background t)                      ; Background mode, dim non-targets
  (avy-all-windows nil)                   ; Current window only
  (avy-timeout-seconds 0.3)               ; Timeout
  (avy-style 'pre))                       ; Label style

;; ==================================================================================
;; Nerd-icons for completion
(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ==================================================================================
;; Search tools validation
(defvar required-search-tools
  '((rg . "brew install ripgrep")
    (ag . "brew install the_silver_searcher"))
  "List of required search tools.
Each element is (EXECUTABLE . INSTALL-INSTRUCTIONS).")

(config-dependency-register
 'search-tools
 (lambda () (config-dependency-validate-executables required-search-tools)))

;; ==================================================================================
;;; Provide features
(provide 'init-completion)

;;; init-completion.el ends here
