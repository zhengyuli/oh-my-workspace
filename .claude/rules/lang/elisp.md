---
globs:
  - "**/*.el"
---

# Emacs Lisp Conventions

Coding standards for Emacs Lisp in oh-my-workspace.

;; ============================================================================
## File Header
;; ============================================================================

```elisp
;;; filename.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: keyword1, keyword2
;; Dependencies: (none) or list of packages

;; Copyright (C) 2026 zhengyu li

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;;; Commentary:
;;
;; Brief description of what this module does.

;;; Code:

;; ============================================================================
```

**Requirements:**
- First line: `;;; -*- lexical-binding: t; -*-`
- Last lines: `(provide 'module-name)` and `;;; module-name.el ends here`

## Delimiter Hierarchy

**Level 0** (File Header): `;; ============...` (79 chars)
**Level 1** (Primary Section): `;; -----------...` (79 chars)

**Semicolon convention:**
- `;;;` — file-level (headers, `provide`, `ends here`)
- `;;` — section-level (delimiters, major comments)
- `;` — inline (line-level only)

;; ----------------------------------------------------------------------------
## Error Handling
;; ----------------------------------------------------------------------------

### ignore-errors

Use only for side effects that must not interrupt the calling operation.

```elisp
;; Pre-save hooks must not break the save on failure
(ignore-errors (copyright-update))
(ignore-errors (time-stamp))
```

### condition-case

Use to recover from or log specific failures — never silently discard errors on critical paths.

```elisp
(condition-case err
    (load "omw-local")
  (error (message "omw-local not found: %s"
                  (error-message-string err))))
```

### unwind-protect

Use when cleanup code must run regardless of success/failure.

```elisp
(unwind-protect
    (risky-operation)
  (set-window-configuration saved-config))
```

;; ----------------------------------------------------------------------------
## Code Patterns
;; ----------------------------------------------------------------------------

### Comments

Explain WHY, not WHAT. Use separate comment lines; never inline explanations.

```elisp
;; Validate package exists before stow operations
;; Returns nil if package directory not found
(defun omw-validate-package (pkg)
  (file-exists-p (expand-file-name pkg omw-script-dir)))
```

### Docstrings

Every public function requires a docstring. First line must be a complete
sentence; document all parameters and return value.

```elisp
(defun omw-find-config (name &optional directory)
  "Find config file with NAME in DIRECTORY or default location.
NAME should be a string without extension.
DIRECTORY defaults to `omw-config-directory'.

Returns the full path to the config file, or nil if not found."
  ...)
```

### Naming

kebab-case only: `omw-buffer-empty-p`, never `omwBufferEmptyP`. All
symbols must carry the `omw-` package prefix. Predicate names must end
with `-p`.

### Buffer-local Variables

Always declare with `defvar-local`; never use `make-local-variable`.

```elisp
(defvar-local omw-shell-buffer-name nil
  "Name of the current shell buffer.")
```

### Key Binding Syntax

Always use `kbd` macro; never use raw escape strings.

```elisp
;; CORRECT
(define-key shell-mode-map (kbd "C-c C-s") 'omw-shell-sync)
```

### Immutability

Never mutate shared lists in-place; always produce a new list via `cons`.

```elisp
;; CORRECT
(setq auto-mode-alist
      (cons '("\\.py\\'" . python-mode) auto-mode-alist))
```

;; ----------------------------------------------------------------------------
## use-package Declaration
;; ----------------------------------------------------------------------------

All package configuration must use `use-package`. Follow this keyword order:

1. `:ensure` / `:ensure nil`
2. `:demand t` / `:defer t`
3. `:when` / `:if`
4. `:after`
5. `:requires`
6. `:mode` / `:interpreter`
7. `:magic` / `:magic-fallback`
8. `:hook`
9. `:bind` / `:bind*`
10. `:bind-keymap` / `:bind-keymap*`
11. `:chords`
12. `:init`
13. `:config`

```elisp
(use-package magit
  :ensure t
  :defer t
  :bind ("C-c g" . magit-status)
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))
```

### Lazy Loading

Default to `:defer t` to minimize startup time; use `:mode`, `:hook`,
and `:bind` where possible as they imply deferral.

;; ----------------------------------------------------------------------------
## Anti-Patterns
;; ----------------------------------------------------------------------------

### Don't: Mutate Shared Lists

```elisp
;; WRONG
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; CORRECT
(setq auto-mode-alist
      (cons '("\\.py\\'" . python-mode) auto-mode-alist))
```

### Don't: Raw Key Strings

```elisp
;; WRONG
(define-key shell-mode-map "\C-c\C-s" 'omw-shell-sync)

;; CORRECT
(define-key shell-mode-map (kbd "C-c C-s") 'omw-shell-sync)
```

### Don't: Inline Explanations

```elisp
;; WRONG
(defvar config-dir nil)  ; Store config directory path

;; CORRECT
;; Store config directory path
(defvar config-dir nil
  "Config directory")
```

;; ----------------------------------------------------------------------------
## Functions
;; ----------------------------------------------------------------------------

### Interactive Declaration

All user-facing commands (key-bound or M-x callable) must declare
`(interactive)`; internal helpers must not.

### Parameter Validation

Validate all required parameters at the start of the function body.

```elisp
(defun omw-load-module (name)
  "Load module with NAME."
  (unless (stringp name)
    (error "omw-load-module: NAME must be a string, got %S" name))
  ...)
```

### Optional Dependency Guard

Always check existence before calling into optional features.

```elisp
(when (featurep 'magit)
  (magit-auto-revert-mode -1))

(when (fboundp 'eglot-ensure)
  (eglot-ensure))
```

;; ----------------------------------------------------------------------------
## Security
;; ----------------------------------------------------------------------------

### Package Sources

Only install packages from GNU ELPA or MELPA Stable; never from unverified
third-party repositories.

### Bytecompile Artifacts

Never commit `.elc` files; always delete them before committing.

;; ----------------------------------------------------------------------------
## References
;; ----------------------------------------------------------------------------

1. [Emacs Lisp Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
2. [use-package Documentation](https://github.com/jwiegley/use-package)

;; ----------------------------------------------------------------------------
## Validation
;; ----------------------------------------------------------------------------

```bash
# Single file
emacs --batch -f batch-byte-compile omw-module.el

# Directory
emacs --batch --eval \
  "(byte-recompile-directory \"~/.config/emacs/lisp\" 0)"

# Pre-commit cleanup
find emacs/ -name '*.elc' -delete
```
