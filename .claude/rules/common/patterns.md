---
paths:
  - "**/*"
---

# Common Patterns

> This file documents design patterns and conventions used throughout the workspace repository.

## Directory Structure Pattern

### XDG Base Directory

All tools follow XDG Base Directory specification:

```
$HOME/.config/      ← XDG_CONFIG_HOME (configurations)
$HOME/.cache/       ← XDG_CACHE_HOME (cache files)
$HOME/.local/share/ ← XDG_DATA_HOME (data files)
$HOME/.local/state/ ← XDG_STATE_HOME (state files)
```

### Stow Package Layout

Each stow package mirrors the target structure:

```
package/
└── .config/
    └── package/
        └── config-file
```

See [xdg-paths.md](./xdg-paths.md) for full XDG conventions.

## Configuration File Pattern

### Header Structure

All configuration files follow a consistent header pattern:

```
# <filename>[ -*- mode: <mode>; -*-][ Time-stamp: <...>]
# =============================================================================
# <One-line description>
#
# [Location: <path>]
# [XDG: <XDG compliance info>]
# [Loaded via: <loading mechanism>]
# [References:]
#   [1. <doc name>: <url>]
# =============================================================================
```

See [coding-style.md](./coding-style.md) for header field specifications.

### Section Separators

| Type | Format | Width | Usage |
|------|--------|-------|-------|
| Header | `# ===...===` | 79 chars | File header open/close ONLY |
| Major section | `# ---...---` | 79 chars | Top-level content sections |
| Subsection | `# --- Name ---` | Variable | Nested sections within major |

## Shell Script Pattern

### Idempotent Operations

All shell configurations must be safe to source multiple times:

```bash
# PATH deduplication
typeset -U path fpath manpath

# Array assignment preserves existing
path=(
  "$HOME/.local/bin"
  $path  # preserve existing
)

# Conditional hook registration
if (( ! ${+hooks[directory-history-chdir]} )); then
    add-zsh-hook chpwd handler_function
fi
```

### Numeric Prefix Loading

Zsh conf.d files use numeric prefixes for load order:

```
00-env.zsh       # Environment variables first
10-path.zsh      # PATH setup
20-tools.zsh     # Tool configuration
30-aliases.zsh   # Aliases
40-completion.zsh # Completion setup
```

### Error Handling

Use explicit `if` statements, not short-circuit operators:

```bash
# CORRECT
if [[ -f "$file" ]]; then
    source "$file"
fi

# AVOID
[[ -f "$file" ]] && source "$file"
```

## Emacs Lisp Pattern

### Naming Convention

All custom functions use `omw/` prefix:

```elisp
;; Setup functions
(defun omw/prog-mode-setup () ...)

;; Tool installers
(defun omw/install-python-tools () ...)

;; Action functions
(defun omw/indent-entire-buffer () ...)
```

### use-package Keyword Order

Keywords must appear in this exact order:

```
1. :ensure or :vc          # Package source
2. :when or :if            # Conditional loading
3. :defer or :demand       # Loading strategy
4. :after                  # Dependencies
5. :hook                   # Mode hooks
6. :bind                   # Keybindings
7. :custom-face            # Face customization
8. :config                 # Configuration
```

### File Structure

```elisp
;;; filename.el -*- lexical-binding: t; -*-
;; Time-stamp: <...>
;; Header metadata...

;;; Code:
;; Implementation...

;; ============================================================================
;;; Provide features
(provide 'omw-module)

;;; filename.el ends here
```

## Git Workflow Pattern

### Commit Format

Conventional commits with descriptive messages:

```
<type>: <description>

<optional body explaining why>
```

See [git-workflow.md](./git-workflow.md) for full workflow.

### Branch Naming

| Pattern | Example |
|---------|---------|
| Feature | `feat/starship-config` |
| Bugfix | `fix/zsh-path-dedup` |
| Refactor | `refactor/consolidate-hooks` |

## Validation Pattern

### Pre-Commit Checks

Before any commit:

1. Check for cache files (see [xdg-paths.md](./xdg-paths.md))
2. Verify stow symlinks are valid
3. Test shell/emacs loads without errors
4. Verify config headers are compliant
5. Check no alignment spaces violations

Use `/pre-commit-check` command for automated verification.

### Configuration Validation

Use `/validate-config` command to verify:

- File headers compliant
- Inline comments on separate lines
- No alignment spaces
- All comments in English
- Syntax checks pass
