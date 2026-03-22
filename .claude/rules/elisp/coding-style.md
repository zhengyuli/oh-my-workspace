---
paths:
  - "**/*.el"
  - "**/emacs/**"
---

# Emacs Lisp Coding Style

> This file extends [../common/coding-style.md](../common/coding-style.md) with Emacs Lisp specifics.

## Naming Convention (CRITICAL)

**All custom functions and variables MUST use `omw/` prefix** (not `my/`):

```elisp
;; CORRECT
(defun omw/prog-mode-setup () ...)
(defcustom omw/http-proxy nil ...)

;; WRONG
(defun my/prog-mode-setup () ...)
```

The `omw/` prefix stands for "oh-my-workspace" and is used consistently throughout the codebase.

### Exception: `site-packages/` Directory

The `site-packages/` directory holds custom packages maintained as if they were external packages (potential future MELPA candidates). Functions in this directory follow Emacs ecosystem naming conventions for their respective subsystem (e.g., `dired-` prefix for dired-ecosystem extensions) rather than the `omw/` prefix.

### Function Naming Patterns

| Pattern | Format | Examples |
|---------|--------|----------|
| Setup functions | `omw/<mode>-setup` | `omw/prog-mode-setup`, `omw/markdown-mode-setup` |
| Tool installers | `omw/install-<lang>-tools` | `omw/install-python-tools`, `omw/install-go-tools` |
| Action functions | `omw/<noun>-<verb>` | `omw/indent-entire-buffer`, `omw/set-http-proxy` |
| Mode indicators | `omw/<feature>-mode-line-indicator` | `omw/pyvenv-mode-line-indicator` |

### Variable Naming Patterns

- Format: `omw/<category>-<item>`
- Examples: `omw/font-monospace-list`, `omw/font-size-default`, `omw/http-proxy`
- Must use `defcustom` with `:group 'omw-emacs`

### Minor Mode Naming

- Format: `omw/<feature>-mode`
- Examples: `omw/prog-before-save-mode`

## File Header Format (MANDATORY)

Template file: `templates/template.el` (auto-inserted via auto-insert)

**Structure:**

| Line | Content | Description |
|-----|------|------|
| 1 | `;;; filename.el -*- lexical-binding: t; -*-` | Filename + lexical binding |
| 2 | `;; Time-stamp: <YYYY-MM-DD HH:MM:SS Day by Author>` | Last modification timestamp |
| 4-6 | Author, Keywords, Dependencies | Metadata |
| 8 | Copyright | Current year |
| 10-22 | MIT License | Full license text |
| 24-26 | History | Creation date |
| 28-30 | Commentary | Module description |

**Required fields checklist:**
- `-*- lexical-binding: t; -*-` on line 1
- Time-stamp: `<YYYY-MM-DD HH:MM:SS Day by Author>` on line 2
- Author line with name and email
- Keywords: 2-4 relevant tags
- Dependencies: list if any, otherwise "(none)"
- Copyright with current year
- MIT license full text
- History section with creation date
- Commentary: Brief description (1-2 sentences)

## File Footer (MANDATORY)

```elisp
;; ============================================================================
;;; Provide features
(provide 'omw-module)

;;; omw-module.el ends here
```

## Section Separators

**Required format (with blank lines before/after):**
```elisp
;; ============================================================================

(use-package ...)

;; ============================================================================
```

**Usage:**
- Before each major use-package block
- Before each major function definition (if file has multiple sections)
- Before "Provide features" section
- With blank line before and after

**Width:** 79 characters (matching repository-wide standard)

## Alignment Spaces (CRITICAL)

> See [../common/coding-style.md](../common/coding-style.md) for the repository-wide alignment rule.

Emacs Lisp follows the repository-wide alignment space prohibition.

**AVOID - Alignment spaces in cons cells:**
```elisp
'(("short"   . value1)
  ("medium"  . value2)
  ("long-key" . value3))
```

**CORRECT - Single space before dot:**
```elisp
'(("short" . value1)
  ("medium" . value2)
  ("long-key" . value3))
```

**AVOID - Alignment spaces in :hook/:bind lists:**
```elisp
:hook ((c-mode      . eglot-ensure)
       (python-mode . eglot-ensure))
```

**CORRECT - Single space:**
```elisp
:hook ((c-mode . eglot-ensure)
       (python-mode . eglot-ensure))
```

### Exception: Comments in let bindings

Comments within `let`/`let*` bindings may use `;;` prefix on the same line as the variable they describe:

```elisp
(let* (;; Normalize proxy URL: add http:// prefix if missing
       (proxy-url (if (string-match-p "\\`https?://" proxy)
                      proxy
                    (concat "http://" proxy)))
       ;; Parse proxy URL to extract host and port
       (parsed (url-generic-parse-url proxy-url)))
  ...)
```

## Code Formatting

**Indentation:** 2 spaces (Emacs Lisp default)

**Multi-line setq:**
```elisp
(setq var1 value1
      var2 value2
      var3 value3)
```

### setq grouping and comment rules (CRITICAL)

1. **Group related variables into a single `setq`** - variables that configure the same feature or concern belong together.
2. **Put a single group-level comment above the `setq`** - describe the group as a whole, not individual variables.
3. **Never add per-variable inline comments inside a `setq` body.**

```elisp
;; CORRECT - group comment above, single setq, no per-variable comments
;; Omit filter rules:
;; - dired-omit-files: hidden files/dirs (.), common project dirs
;; - dired-omit-extensions: compiled artifacts and lock files
(setq dired-omit-files (concat "^\\.\\|" "…")
      dired-omit-extensions (append dired-omit-extensions '(".pyc" ".elc")))

;; CORRECT - no comment needed when purpose is obvious
(setq var1 value1
      var2 value2
      var3 value3)

;; WRONG - per-variable inline comments inside setq body
(setq var1 value1
      ;; Set tab width
      var2 4
      ;; Enable this feature
      var3 t)

;; WRONG - split related variables into separate setq blocks needlessly
(setq dired-omit-files "…")
;; Supplementary extension filter
(setq dired-omit-extensions (append …))
```

## Docstring Style

**Use paragraph form, not bullet lists:**

```elisp
;; AVOID: Bullet list in docstrings
"Do X.
- Step 1
- Step 2
- Step 3"

;; CORRECT: Paragraph form
"Do X by first doing step 1, then step 2.
Finally complete step 3."
```

**Why:** Bullet lists are hard to read in docstrings and violate Emacs conventions.
