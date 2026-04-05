---
paths:
  - "editor/emacs/.config/emacs/**/*.el"
---

# Emacs Lisp Conventions

Coding standards for Emacs Lisp in oh-my-workspace.

## File Header

```elisp
;;; filename.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
;;
;; ============================================================================
;; filename.el - Brief description
;;
;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: keyword1, keyword2
;; Dependencies: (none) or list of packages
;;
;; Copyright (C) 2026 zhengyu li
;;
;;; History:
;;
;; 2026-MM-DD HH:MM zhengyu li <lizhengyu419@outlook.com> created.
;;
;;; Commentary:
;;
;; Detailed description of what this module does.
;; ============================================================================
```

## File Tail

```elisp
;; ============================================================================
;;; Provide features
(provide 'omw-module)

;;; omw-module.el ends here
```

Feature names use hyphenated form matching the file base name: file
`omw-font.el` provides `'omw-font`, file `omw-python.el` provides
`'omw-python`.  The `omw/` prefix is for code symbols only — never for
feature names or file names.

## Module Loading

Modules are loaded via `require` in `init.el`.  Each module file must
`(provide 'omw-module-name)` at its tail so that `require` can find it.

The `init.el` adds all subdirectories under `lisp/` to `load-path`
recursively, so `require` resolves module names without directory prefixes.

Do not use `use-package` for local modules — `use-package` is for
third-party packages only.  Local modules use plain `require`:

```elisp
;; In init.el — local module loading
(require 'omw-utils)
(require 'omw-font)

;; In module files — third-party packages via use-package
(use-package eglot
  :ensure t
  ...)
```

## Delimiter Hierarchy

File headers (`;;; Commentary:`, `;;; History:`) follow the standard Emacs
Lisp header convention recognized by `outline-minor-mode` and `imenu`.  The
`;;;` semicolons there are not part of the delimiter hierarchy below.

The delimiter hierarchy applies to project-internal logical sections within
the file body:

**Level 0** (File Header): `;; ============...` (79 chars)
**Level 1** (Primary Section): `;; -----------...` (79 chars)
**Level 2** (Subsection): `;; --- Title ---`

**Title Case required**: capitalize the first letter of every word in both
Section Title and Subsection Title (e.g., `GC Tuning`, `Doom Modeline`).
Abbreviations follow their established convention: ALL CAPS for standard
abbreviations (e.g., `FZF Preview`, `PDF Tools`, `JSON Mode`, `VTERM`),
lowercase for established lowercase names (e.g., `cc Mode`, `sh Mode`, `xref`).

### Blank Lines

Blank lines mark boundaries between delimiter levels and top-level forms.

**Around delimiters** — one blank line before Level 1 opening, one after
Level 1 closing.  Level 2 has no trailing blank line — code follows directly.

```elisp
;; ----------------------------------------------------------------------------
;; Font Configuration
;; ----------------------------------------------------------------------------

;; --- Base Font ---
(set-face-attribute 'default nil :height 120)
(set-face-attribute 'default nil :width 'normal)

;; --- Fallback Font ---
(set-face-attribute 'variable-pitch nil :family "Serif")
```

**Between top-level forms within the same subsection** — one blank line.
Related forms (e.g., consecutive `setq`) are not separated.

```elisp
;; --- Hook Setup ---
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

(defvar omw/prog-margin 80
  "Right margin for prog buffers.")

(make-variable-buffer-local 'omw/prog-margin)
```

**Inside function bodies** — one blank line between logical paragraphs.
Single-expression bodies have no extra blank lines.

```elisp
;; Multi-paragraph body
(defun omw/setup-theme ()
  "Apply theme and adjust faces."
  (load-theme 'modus-vivendi t)
  (set-face-attribute 'mode-line nil :box nil)

  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic))

;; Single-expression body — no extra blank lines
(defun omw/buffer-empty-p ()
  "Return non-nil if buffer is empty."
  (= (buffer-size) 0))
```

**Prohibited**: two or more consecutive blank lines anywhere in the file.

```elisp
;; Level 1 / Level 2 — no trailing blank line
;; --- Subsection Title ---
(code-here)
```

## Line Length

79 characters maximum.

Exceptions:

- URLs and file paths that cannot be wrapped
- Symbol names and docstrings that cannot be meaningfully split

## Error Handling

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

## Code Patterns

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

kebab-case only: `omw/buffer-empty-p`, never `omwBufferEmptyP`.

All project symbols use `omw/` prefix: `omw/setup-fonts`,
`omw/http-proxy`, `omw/gc-startup-threshold`.

File names use `omw-` with hyphen: `omw-font.el`, `omw-proxy.el`.  The
`omw/` prefix is for code symbols only — never for file names.

Predicate names must end with `-p`: `omw/buffer-empty-p`.

### Buffer-local Variables

Always declare with `defvar-local`; never use `make-local-variable`.

```elisp
(defvar-local omw-shell-buffer-name nil
  "Name of the current shell buffer.")
```

### Customizable Variables

Use `defcustom` for user-facing options that benefit from type checking and
the Customize UI.  Use `defvar` for internal state that should not be exposed
to users.

```elisp
;; User-facing option — appears in Customize
(defcustom omw/emacs-user-name ""
  "Emacs configuration user name."
  :type 'string
  :group 'omw-emacs)

;; User-facing option — appears in Customize
(defcustom omw/http-proxy nil
  "HTTP proxy URL for Emacs network operations."
  :type '(choice (const :tag "No proxy" nil)
                 (string :tag "Proxy URL"))
  :group 'omw-emacs)

;; Internal state — not for Customize
(defvar omw/some-internal-state nil
  "Internal buffer-local state.")
```

### Key Binding Syntax

Always use `kbd` macro; never use raw escape strings.

```elisp
;; CORRECT
(define-key shell-mode-map (kbd "C-c C-s") #'omw-shell-sync)
```

### Immutability

Use `#'` (sharp quote) for all function references so the byte-compiler can
verify the function exists at compile time.  Use `'` (plain quote) only for
symbols that are not function names (e.g., `'forward` as a direction argument).

```elisp
;; CORRECT
(add-hook 'eglot-mode-hook #'eglot-ensure)
(define-key map (kbd "C-c s") #'omw-shell-sync)

;; WRONG — loses compile-time checking
(add-hook 'eglot-mode-hook 'eglot-ensure)
```

### Lists

Prefer `push` over `add-to-list` in init files.  `push` is a macro that
prepends in O(1) without duplicate checking; `add-to-list` is a function
that traverses the entire list in O(n) for a redundant membership test.
Under `lexical-binding: t`, `push` also behaves predictably with lexical
variables, whereas `add-to-list` requires a quoted symbol referencing a
global variable.

```elisp
;; CORRECT — direct prepend, no redundant traversal
(push '("\\.py\\'" . python-mode) auto-mode-alist)
```

### Validation at Boundaries

Validate all function parameters at entry.

```elisp
(defun omw-load-module (name)
  "Load module with NAME."
  (unless (stringp name)
    (error "omw-load-module: NAME must be a string, got %S" name))
  ...)
```

### Nesting Limit

Max 3 levels. Use early-return guards to flatten conditional structures.

```elisp
;; CORRECT — flattened with early return
(defun omw-process-file (file)
  (unless (file-exists-p file)
    (error "File not found: %s" file))
  (unless (file-readable-p file)
    (error "File not readable: %s" file))
  (omw-read-file file))
```

### No Magic Numbers

Use `defconst` for named constants.

```elisp
;; WRONG
(if (> length 80)

;; CORRECT
(defconst omw-max-line-length 80
  "Maximum line length for Emacs Lisp files.")
(if (> length omw-max-line-length)
```

### Variable Assignment

Use `setq` consistently for all variable assignments.  Reserve `setopt`
(Emacs 29+) only for variables where the custom setter side-effect is
explicitly needed (e.g., `setopt indicate-empty-lines t` triggers the
display update that `setq` would skip).  When in doubt, use `setq`.

### Hook Functions

Always use named functions in `add-hook` — they are debuggable, traceable,
and removable via `remove-hook`.  Avoid anonymous lambdas.

```elisp
;; CORRECT
(defun omw/after-init-setup () ...)
(add-hook 'after-init-hook #'omw/after-init-setup)

;; WRONG — cannot remove or debug
(add-hook 'after-init-hook (lambda () ...))
```

When using `use-package`, prefer the `:hook` keyword which generates the
named-function pattern automatically.

### Section Uniqueness

Each section title must be unique within the file at every delimiter level
(Level 1 and Level 2).  Group related settings together — do not create
multiple sections of the same name.

```elisp
;; WRONG — duplicate section at Level 2
;; --- Font Setup ---
(set-face-attribute 'default nil :height 120)
;; --- Other Config ;;
(global-display-line-numbers-mode t)
;; --- Font Setup ---              ← same name reused
(set-face-attribute 'default nil :width 'normal)

;; CORRECT
;; --- Font Setup ---
(set-face-attribute 'default nil :height 120)
(set-face-attribute 'default nil :width 'normal)
;; --- Other Config ;;
(global-display-line-numbers-mode t)
```

## use-package Declaration

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
12. `:custom-face`
13. `:init`
14. `:config`

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

### `:after` Implies `:defer`

Do not combine `:after` with `:defer t`.  The `:after` keyword already
defers loading until the named package is available; adding `:defer t`
is redundant.

```elisp
;; WRONG
(use-package corfu-terminal
  :ensure t
  :defer t
  :after corfu
  :config ...)

;; CORRECT
(use-package corfu-terminal
  :ensure t
  :after corfu
  :config ...)
```

## Comments

Comments explain *why*, not *what*. The code itself should be readable
enough to show what it does. Only add comments when the reasoning is
non-obvious from the code.

```elisp
;; WRONG — restates the code
;; Set tab width to 4
(setq tab-width 4)

;; CORRECT — explains the reasoning
;; Match project convention (Go, Python default indent)
(setq tab-width 4)
```

Docstrings serve a different purpose — they describe public API contracts
(see Docstrings under Code Patterns). Do not duplicate docstring content
in inline comments.

## Anti-Patterns

### Don't: add-to-list in Init Files

`add-to-list` performs an O(n) duplicate check on every call — unnecessary
overhead when you control the config and know the entry is new.  Under
`lexical-binding: t` it also requires a quoted global-variable symbol,
making it fragile with lexical bindings.

```elisp
;; WRONG — O(n) duplicate check, unnecessary in config
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; CORRECT — O(1) prepend, lexical-binding safe
(push '("\\.py\\'" . python-mode) auto-mode-alist)
```

### Don't: Raw Key Strings

```elisp
;; WRONG
(define-key shell-mode-map "\C-c\C-s" #'omw-shell-sync)

;; CORRECT
(define-key shell-mode-map (kbd "C-c C-s") #'omw-shell-sync)
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

### Don't: Align Values

Do not pad `setq` arguments with extra spaces to create visual alignment —
it produces noisy diffs when variable names or values change.

```elisp
;; WRONG — alignment breaks on first rename
(setq-default tab-width    4
              fill-column 80)

;; CORRECT
(setq-default tab-width 4
              fill-column 80)
```

## Functions

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

## Security

### Package Sources

Install packages from GNU ELPA or MELPA by default.  Use `:vc` to fetch
directly from a Git repository only when the package is not yet available on
ELPA/MELPA.  Always add a comment explaining why `:vc` is required.

```elisp
;; Not yet published to ELPA/MELPA; install directly from upstream.
(use-package some-package
  :vc (:url "https://github.com/author/some-package.el" :rev :newest)
  ...)
```

### Bytecompile Artifacts

Never commit `.elc` files; always delete them before committing.

### Secrets Management

Never hardcode API keys, tokens, or passwords in Emacs Lisp files.  Read
sensitive values from environment variables via `getenv`.

```elisp
;; WRONG — hardcoded credential
(setq omw/http-proxy "http://user:pass@proxy:8080")

;; CORRECT — read from environment
(defvar omw/http-proxy (getenv "HTTP_PROXY")
  "HTTP proxy URL loaded from environment.")
```

**Sensitive types**: API keys, tokens, passwords, private keys, certificates.

## LSP Configuration (eglot)

This project uses `eglot` as the LSP client — never generate `lsp-mode` code.

### Server Registration

Register language servers via the `:hook` keyword in `use-package eglot`.
Server programs are configured with `eglot-server-programs` in the `:config`
section when the default entry needs customization.

```elisp
(use-package eglot
  :ensure t
  :defer t
  :hook ((python-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :config
  (setq eglot-sync-connect nil
        eglot-autoshutdown t))
```

### Per-language Server Setup

Language-specific server configuration (e.g., `basedpyright` for Python,
`typescript-language-server` for TypeScript) lives in the corresponding
language module (`omw-python.el`, `omw-typescript.el`), not in `omw-prog.el`.
Tool installation is managed via `omw/tools-install` with declarative specs.

## References

1. [Emacs Lisp Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
2. [use-package Documentation](https://github.com/jwiegley/use-package)

## Validation

```bash
# Single file — syntax check
emacs --batch -f batch-byte-compile omw-module.el

# Directory — recompile all
emacs --batch --eval \
  "(byte-recompile-directory \"~/.config/emacs/lisp\" 0)"

# Naming and docstring conventions (install: M-x package-install package-lint)
emacs --batch -f package-lint-batch-and-exit omw-module.el

# Pre-commit cleanup
find emacs/ -name '*.elc' -delete
```
