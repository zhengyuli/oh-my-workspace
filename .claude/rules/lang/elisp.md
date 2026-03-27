---
globs:
  - "**/*.el"
---

# Emacs Lisp Conventions

Coding standards for Emacs Lisp in oh-my-workspace.

## File Header (MANDATORY)

```elisp
;;; filename.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>

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
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; History:
;;
;; 2026-03-14 15:30 user <user@outlook.com> created.

;;; Commentary:
;;
;; Brief description of what this module does.
;; Additional context about features and usage.

;;; Code:

;; ============================================================================
```

**Lexical Binding**: Always use `;;; -*- lexical-binding: t; -*-`

**Tail Pattern (MANDATORY)**:

Every Emacs Lisp file MUST end with:

```elisp
;; ============================================================================
(provide 'module-name)
;;; module-name.el ends here
```

This follows Emacs convention and helps tools identify file boundaries.

## Delimiter Hierarchy (MANDATORY)

**Level 0** (File Header): `;; ===` * 76 (79 chars)
**Level 1** (Primary Section): `;; ---` * 76 (79 chars)

**Example:**
```elisp
;; ============================================================================
;; Customization
;; ============================================================================
(defgroup omw-git nil
  "Git integration settings.")

;; ----------------------------------------------------------------------------
;; Customization - Variables
;; ----------------------------------------------------------------------------
(defcustom omw-git-auto-revert t
  "Automatically revert buffers."
  :type 'boolean
  :group 'omw-git)

;; ----------------------------------------------------------------------------
;; Customization - Faces
;; ----------------------------------------------------------------------------
(defface omw-git-highlight-face
  '((t :inherit highlight))
  "Face for git highlights."
  :group 'omw-git)
```

**Semicolon Convention:**
- `;;;` (triple) - File-level (headers, provide, ends here)
- `;;` (double) - Section level (delimiters, major sections)
- `;` (single) - Line level (inline comments)

## Error Handling

Use `ignore-errors` only for side effects that must not interrupt the
calling operation on failure:

```elisp
;; Pre-save hooks must not break the save on failure
(ignore-errors (copyright-update))
(ignore-errors (time-stamp))
```

Use `condition-case` to recover from or log specific failures — never
silently discard errors on critical paths:

```elisp
;; Recover gracefully when loading optional local config
(condition-case err
    (load "omw-local")
  (error (message "omw-local not found: %s"
                  (error-message-string err))))
```

Use `unwind-protect` when cleanup code must run regardless of whether
the body succeeds or fails:

```elisp
;; Always restore window configuration even if body errors
(unwind-protect
    (risky-operation)
  (set-window-configuration saved-config))
```

## Documentation & Code Patterns

**Comment Philosophy**:
- Explain rationale (WHY), not mechanics (WHAT)
- Document non-obvious design decisions and constraints
- Use separate comment lines for clarity

```elisp
;; Validate package exists before stow operations
;; Returns nil if package directory not found
(defun omw-validate-package (pkg)
  (file-exists-p (expand-file-name pkg omw-script-dir)))
```

**Docstring Standards**: Every public function requires a docstring

```elisp
(defun omw-find-config (name &optional directory)
  "Find config file with NAME in DIRECTORY or default location.
NAME should be a string without extension.
DIRECTORY defaults to `omw-config-directory'.

Returns the full path to the config file, or nil if not found."
  ...)
```

**First Line**: Complete sentence, not terse

```elisp
;; Good
"Return the absolute path to the workspace directory."

;; Bad
"Returns absolute path"  ;; Not a sentence
"Get workspace dir"       ;; Too terse
```

**Naming**:
- Use kebab-case: `omw-buffer-empty-p`, not `omwBufferEmptyP`
- Package prefix: use `omw-` for oh-my-workspace
- Predicates: end with `-p`: `omw-buffer-empty-p`

```elisp
;; Good
(defun omw-buffer-empty-p ()
  "Return t if buffer is empty."
  (zerop (buffer-size)))

;; Bad
(defun omwBufferEmptyP ()
  ...)
```

**Buffer-local Variables**: Prefer `defvar-local` over `make-local-variable`

```elisp
(defvar-local omw-shell-buffer-name nil
  "Name of the current shell buffer.")
```

**Key Binding Syntax**: Use `kbd` macro for readability

```elisp
;; Good - readable
(define-key shell-mode-map (kbd "C-c C-s") 'omw-shell-sync)

;; Bad - hard to read
(define-key shell-mode-map "\C-c\C-s" 'omw-shell-sync)
```

**Immutability**: Prefer creating new lists over mutating existing ones

```elisp
;; WRONG: Mutates the existing list in-place
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; CORRECT: Creates a new list — safe, side-effect-free
(setq auto-mode-alist
      (cons '("\\.py\\'" . python-mode) auto-mode-alist))
```

**use-package (MANDATORY)**: Always use `use-package` for package
configuration. Follow this keyword order within each declaration:

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

**Prefer lazy loading**: Default to `:defer t` to minimize startup
time. Prefer `:mode`, `:hook`, and `:bind` which imply deferral:

```elisp
(use-package magit
  :defer t
  :bind ("C-c g" . magit-status))

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode))
```

**Formatting Rules:**
- Never align values with spaces
- Never use inline comments for explanations

```elisp
;; WRONG - aligned
(defvar config-dir nil
    "Config directory")
(defvar data-dir  nil
    "Data directory")

;; WRONG - inline comment for explanation
(defvar config-dir nil)  ; Store config directory path

;; CORRECT
;; Store config directory path
(defvar config-dir nil
  "Config directory")
(defvar data-dir nil
  "Data directory")
```

## Functions

**Single Responsibility**: One thing per function

```elisp
(defun omw-validate-package (pkg) ...)  ; Validation only
(defun omw-install-package (pkg) ...)   ; Installation only
```

**`interactive` Declaration**: Required for all user-facing commands

```elisp
;; Command bound to a key or callable via M-x — must declare interactive
(defun omw/jump-to-matched-paren ()
  "Jump to the matched delimiter at point."
  (interactive)
  ...)

;; Internal helper — no interactive declaration
(defun omw-find-config (name)
  "Return path to config NAME, or nil if not found."
  ...)
```

**Parameter Validation**: Guard at the start of the function

```elisp
(defun omw-load-module (name)
  "Load module with NAME."
  (unless (stringp name)
    (error "omw-load-module: NAME must be a string, got %S" name))
  ...)
```

**Feature Existence Check**: Use `featurep` / `fboundp` / `boundp` before
depending on optional features

```elisp
;; Check feature is loaded before using it
(when (featurep 'magit)
  (magit-auto-revert-mode -1))

;; Check function exists before calling it
(when (fboundp 'eglot-ensure)
  (eglot-ensure))
```

## Security

### Package Security

**Principles**:
- Verify package sources before installation
- Prefer GNU ELPA and MELPA Stable over unstable repositories
- Review package signatures when available

### Local Configuration Strategy

**Split-file Pattern**: Separate sensitive configuration from version control

```elisp
;; Main config (committed)
(setq user-full-name "Your Name")

;; Local overrides (not committed, in .gitignore)
;; File: ~/.config/emacs/lisp/omw-local.el
;; (setq user-mail-address "your.email@company.com")
;; (setq smtpmail-smtp-server "smtp.company.com")

;; Load local overrides if available
(require 'omw-local nil t)
```

### Byte Compilation Security

**Stale .elc Files**: Emacs always prefers `foo.elc` over `foo.el`

**Risk**: Editing `.el` files without removing stale `.elc` files causes
Emacs to load outdated compiled versions.

**Prevention**:
```bash
# Remove all .elc files before committing
find ~/.config/emacs/ -name '*.elc' -delete
```

## Validation

**Byte Compilation**: Always ensure clean compilation

```bash
# Compile a single file
emacs --batch -f batch-byte-compile omw-module.el

# Compile all files in a directory
emacs --batch --eval \
  "(byte-recompile-directory \"~/.config/emacs/lisp\" 0)"
```

A clean compile produces no warnings. Treat warnings as errors.

**Stale .elc Files — Critical Pitfall**:

Emacs always prefers `foo.elc` over `foo.el`. Editing `foo.el` without
removing stale `foo.elc` means Emacs loads the old compiled version.

```bash
# Remove all .elc from repo
find emacs/ -name '*.elc' -delete
```
