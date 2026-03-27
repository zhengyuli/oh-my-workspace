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

Every file must declare `;;; -*- lexical-binding: t; -*-` on the first
line.

Every file must end with:

```elisp
;; ============================================================================
(provide 'module-name)
;;; module-name.el ends here
```

## Delimiter Hierarchy (MANDATORY)

**Level 0** (File Header): `;; ===` × 76 (79 chars total)
**Level 1** (Primary Section): `;; ---` × 76 (79 chars total)

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

**Semicolon convention:**
- `;;;` — file-level (headers, `provide`, `ends here`)
- `;;` — section-level (delimiters, major comments)
- `;` — inline (line-level only)

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

**Comments:**
- Explain rationale (WHY), not mechanics (WHAT)
- Document non-obvious decisions and constraints
- Use separate comment lines; never inline explanations

```elisp
;; Validate package exists before stow operations
;; Returns nil if package directory not found
(defun omw-validate-package (pkg)
  (file-exists-p (expand-file-name pkg omw-script-dir)))
```

**Docstrings**: Every public function requires a docstring. First line
must be a complete sentence; document all parameters and return value:

```elisp
(defun omw-find-config (name &optional directory)
  "Find config file with NAME in DIRECTORY or default location.
NAME should be a string without extension.
DIRECTORY defaults to `omw-config-directory'.

Returns the full path to the config file, or nil if not found."
  ...)
```

```elisp
;; CORRECT
"Return the absolute path to the workspace directory."

;; WRONG — not a sentence / too terse
"Returns absolute path"
"Get workspace dir"
```

**Naming:**
- kebab-case only: `omw-buffer-empty-p`, never `omwBufferEmptyP`
- All symbols must carry the `omw-` package prefix
- Predicate names must end with `-p`

```elisp
;; CORRECT
(defun omw-buffer-empty-p ()
  "Return t if buffer is empty."
  (zerop (buffer-size)))

;; WRONG
(defun omwBufferEmptyP () ...)
```

**Buffer-local variables**: Always declare with `defvar-local`; never
use `make-local-variable`:

```elisp
(defvar-local omw-shell-buffer-name nil
  "Name of the current shell buffer.")
```

**Key binding syntax**: Always use `kbd` macro; never use raw escape
strings:

```elisp
;; CORRECT
(define-key shell-mode-map (kbd "C-c C-s") 'omw-shell-sync)

;; WRONG
(define-key shell-mode-map "\C-c\C-s" 'omw-shell-sync)
```

**Immutability**: Never mutate shared lists in-place; always produce a
new list via `cons`:

```elisp
;; WRONG
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; CORRECT
(setq auto-mode-alist
      (cons '("\\.py\\'" . python-mode) auto-mode-alist))
```

**use-package (MANDATORY)**: All package configuration must use
`use-package`. Follow this keyword order within each declaration:

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

**Lazy loading**: Default to `:defer t` to minimize startup time; use
`:mode`, `:hook`, and `:bind` where possible as they imply deferral:

```elisp
(use-package magit
  :defer t
  :bind ("C-c g" . magit-status))

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode))
```

**Formatting:**
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

**Single responsibility**: Each function does exactly one thing.

**`interactive` declaration**: All user-facing commands (key-bound or
M-x callable) must declare `(interactive)`; internal helpers must not:

```elisp
;; CORRECT — user command
(defun omw/jump-to-matched-paren ()
  "Jump to the matched delimiter at point."
  (interactive)
  ...)

;; CORRECT — internal helper
(defun omw-find-config (name)
  "Return path to config NAME, or nil if not found."
  ...)
```

**Parameter validation**: Validate all required parameters at the start
of the function body:

```elisp
(defun omw-load-module (name)
  "Load module with NAME."
  (unless (stringp name)
    (error "omw-load-module: NAME must be a string, got %S" name))
  ...)
```

**Optional dependency guard**: Always check existence before calling
into optional features:

```elisp
(when (featurep 'magit)
  (magit-auto-revert-mode -1))

(when (fboundp 'eglot-ensure)
  (eglot-ensure))
```

## Security

**Package sources**: Only install packages from GNU ELPA or MELPA
Stable; never from unverified third-party repositories.

**Local overrides**: Keep machine-specific or sensitive settings in
`omw-local.el`, excluded from version control via `.gitignore`:

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

**Stale `.elc` files**: Never commit `.elc` files; always delete them
before committing.

## Validation

**Byte compilation**: Zero warnings required — treat every warning as
an error:

```bash
# Single file
emacs --batch -f batch-byte-compile omw-module.el

# Directory
emacs --batch --eval \
  "(byte-recompile-directory \"~/.config/emacs/lisp\" 0)"
```

**Before committing**: After verification passes, delete all `.elc`
files:

```bash
find emacs/ -name '*.elc' -delete
```
