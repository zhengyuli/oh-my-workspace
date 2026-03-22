---
paths:
  - "**/*.el"
  - "**/emacs/**"
---

# Emacs Lisp Patterns

> This file extends [../common/patterns.md](../common/patterns.md) with Emacs Lisp specifics.

## use-package Patterns (MANDATORY)

**Keywords MUST appear in this exact order:**

```
1. :ensure or :vc          # Package source
2. :when or :if            # Conditional loading (optional)
3. :defer or :demand       # Loading strategy - OMIT when :after is present
4. :after                  # Dependencies (optional) - implies deferral
5. :hook                   # Mode hooks
6. :bind                   # Keybindings
7. :custom-face            # Face customization (optional)
8. :config                 # Configuration
```

### Critical: `:after` Implies Deferral

**CRITICAL: `:after` implies deferral - never use `:defer t` together with `:after`.**

`:after` waits for the dependency to load before loading this package, which already defers loading. Adding `:defer t` is redundant and incorrect.

```elisp
;; CORRECT - :after provides deferral, no :defer t needed
(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; WRONG - :defer t and :after together
(use-package corfu-terminal
  :ensure t
  :defer t
  :after corfu
  :config
  (corfu-terminal-mode 1))
```

### Package Source Patterns

```elisp
;; From ELPA/MELPA
(use-package magit
  :ensure t)

;; From VC (Git)
(use-package my-package
  :vc (:url "https://github.com/user/my-package.git"
       :rev :newest))

;; Built-in (no :ensure)
(use-package dired
  :config
  (setq dired-dwim-target t))
```

### Conditional Loading

```elisp
;; Load only on specific systems
(use-package exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; Load only when feature available
(use-package treesit-auto
  :ensure t
  :if (treesit-available-p))
```

### Hook Patterns

```elisp
;; Single hook
(use-package eglot
  :ensure t
  :hook (python-mode . eglot-ensure))

;; Multiple hooks
(use-package eglot
  :ensure t
  :hook ((c-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (go-mode . eglot-ensure)))
```

### Binding Patterns

```elisp
;; Global bindings
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch)))

;; Mode-specific bindings
(use-package python-mode
  :bind (:map python-mode-map
              ("C-c C-c" . python-shell-send-buffer)))
```

### Configuration Patterns

```elisp
;; With custom variables
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (global-corfu-mode 1))

;; With custom faces
(use-package doom-themes
  :ensure t
  :custom-face
  (doom-modeline-bar '((t (:background "#51afef")))))
```

## Module Structure

### Standard Module Layout

```elisp
;;; omw-module.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-22 10:00:00 Sunday by zhengyu.li>

;; Author: Zhengyu Li <zhengyu.li@example.com>
;; Keywords: convenience, tools
;; Dependencies: (none)

;; Copyright (C) 2026 Zhengyu Li

;; MIT License text...

;;; History:
;; 2026-03-22: Initial creation

;;; Commentary:
;; Brief description of this module.

;;; Code:

;; ============================================================================
;; Dependencies
(require 'cl-lib)

;; ============================================================================
;; Customization
(defgroup omw-module nil
  "Module customization group."
  :group 'omw-emacs
  :prefix "omw-module-")

(defcustom omw-module-option nil
  "Description of option."
  :type 'boolean
  :group 'omw-module)

;; ============================================================================
;; Functions
(defun omw-module-main ()
  "Main function description."
  ...)

;; ============================================================================
;;; Provide features
(provide 'omw-module)

;;; omw-module.el ends here
```

### Provide Pattern

Always use `(provide 'omw-*)` for consistency:

```elisp
;; CORRECT
(provide 'omw-editor)
(provide 'omw-python)
(provide 'omw-completion)

;; WRONG - inconsistent naming
(provide 'my-editor)
(provide 'editor-config)
```

## Defcustom Patterns

```elisp
;; Boolean option
(defcustom omw-feature-enabled t
  "Whether the feature is enabled."
  :type 'boolean
  :group 'omw-emacs)

;; String option
(defcustom omw-default-name "default"
  "Default name for the feature."
  :type 'string
  :group 'omw-emacs)

;; Choice option
(defcustom omw-completion-style 'corfu
  "Completion style to use."
  :type '(choice (const :tag "Corfu" corfu)
                 (const :tag "Company" company))
  :group 'omw-emacs)

;; List option
(defcustom omw-excluded-modes '(fundamental-mode special-mode)
  "Modes to exclude from feature."
  :type '(repeat symbol)
  :group 'omw-emacs)
```

## Advice Patterns

```elisp
;; Add advice before function
(advice-add 'function :before #'omw/before-function)

;; Add advice after function
(advice-add 'function :after #'omw/after-function)

;; Around advice for wrapping
(defun omw/around-function (orig-fun &rest args)
  "Advice around ORIG-FUN with ARGS."
  (let ((result (apply orig-fun args)))
    (modify result)))
(advice-add 'function :around #'omw/around-function)
```

## Lazy Loading Patterns

```elisp
;; Autoload for on-demand loading
;;;###autoload
(defun omw/feature-command ()
  "Command that triggers package loading."
  ...)

;; With use-package :commands
(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch))
```
