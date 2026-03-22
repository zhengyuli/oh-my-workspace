---
paths:
  - "**/*.el"
  - "**/emacs/**"
---

# Emacs Lisp Hooks

> This file extends [../common/hooks.md](../common/hooks.md) with Emacs Lisp specifics.

## Pre-Commit Hooks

### Byte Compilation Check

Run byte compilation to catch errors:

```bash
# Check for byte-compile errors (without writing .elc files)
emacs --batch --eval '(progn
  (setq byte-compile-error-on-warn t)
  (byte-compile-file "module.el"))'
```

### Package-Lint

Use package-lint for elisp files:

```bash
# Run package-lint on elisp files
emacs --batch -l package-lint.el -f package-lint-batch-and-exit *.el
```

### Common Lint Issues

| Issue | Detection | Fix |
|-------|-----------|-----|
| Missing `omw/` prefix | `grep -rn "defun (?!omw/)"` | Rename with prefix |
| Missing `:group 'omw-emacs` | `grep -rn "defcustom.*:group"` | Add `:group 'omw-emacs` |
| Unquoted symbols | package-lint | Quote with `'` |
| Missing lexical binding | Check line 1 | Add `-*- lexical-binding: t; -*-` |

## File Loading Hooks

### after-init-hook

For configuration that must run after Emacs is fully initialized:

```elisp
(add-hook 'after-init-hook #'omw/after-init-setup)
```

### prog-mode-hook

For programming mode setup:

```elisp
(add-hook 'prog-mode-hook #'omw/prog-mode-setup)
```

### Mode-Specific Hooks

```elisp
;; Python mode
(add-hook 'python-mode-hook #'omw/python-mode-setup)

;; Markdown mode
(add-hook 'markdown-mode-hook #'omw/markdown-mode-setup)
```

## Save Hooks

### before-save-hook

For actions before saving:

```elisp
;; Format on save
(add-hook 'before-save-hook #'omw/format-on-save)

;; Delete trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)
```

### after-save-hook

For actions after saving:

```elisp
;; Auto-compile on save (for .el files)
(add-hook 'after-save-hook #'omw/auto-compile-elisp)
```

## Hook Best Practices

### Use Named Functions

```elisp
;; CORRECT - named function for debugging and removal
(defun omw/prog-mode-setup ()
  "Set up programming mode."
  (display-line-numbers-mode 1)
  (hl-line-mode 1))
(add-hook 'prog-mode-hook #'omw/prog-mode-setup)

;; AVOID - lambda functions are harder to debug
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)
            (hl-line-mode 1)))
```

### Conditional Hook Registration

```elisp
;; Only add if not already present
(unless (memq #'omw/prog-mode-setup prog-mode-hook)
  (add-hook 'prog-mode-hook #'omw/prog-mode-setup))
```

### Hook Depth Control

```elisp
;; Add with specific depth (lower runs first)
(add-hook 'prog-mode-hook #'omw/prog-mode-setup 50)

;; Add to local hook (buffer-specific)
(add-hook 'prog-mode-hook #'omw/buffer-local-setup nil t)
```

## use-package Hook Integration

```elisp
(use-package eglot
  :ensure t
  :hook ((c-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (go-mode . eglot-ensure)))
```

This is equivalent to:

```elisp
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'go-mode-hook #'eglot-ensure)
```

## Hook Verification

### List Active Hooks

```elisp
;; List all hooks for a mode
M-x describe-variable RET prog-mode-hook
```

### Debug Hook Execution

```elisp
;; Trace hook execution
(setq debug-on-error t)

;; Or use toggle-debug-on-error
M-x toggle-debug-on-error
```
