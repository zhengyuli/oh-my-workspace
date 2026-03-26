# Common Patterns

Design patterns and principles for oh-my-workspace configuration code.

## Immutability Principle

**ALWAYS create new configurations, NEVER modify existing ones in-place:**

```bash
# WRONG: Modifies file in-place
sed -i 's/old/new/' ~/.zshrc

# CORRECT: Creates new version with backup
cp ~/.zshrc ~/.zshrc.bak
sed 's/old/new/' ~/.zshrc.bak > ~/.zshrc
```

**For Emacs Lisp:**

```elisp
;; WRONG: Modifies existing list
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; CORRECT: Creates new list with addition
(setq auto-mode-alist
      (cons '("\\.py\\'" . python-mode) auto-mode-alist))
```

**Rationale:**
- Enables easy rollback if changes break
- Prevents unintended side effects
- Makes debugging easier
- Maintains history of changes

## File Organization

**MANY SMALL FILES > FEW LARGE FILES:**

- **Organization:** By feature/domain, not by type
- **Cohesion:** High within files, low between files
- **Size targets by file type:** See `coding-style.md` for authoritative limits

**When to split configuration files:**

- Emacs: Separate major modes into individual files (e.g., `init-python.el`, `init-git.el`)
- Zsh: Split aliases, functions, and options into separate files
- Shell scripts: Extract utilities from main scripts

**Example - Modular Emacs Config:**

```
~/.config/emacs/
├── init.el          # Main entry (< 100 lines)
├── init-base.el     # Base configuration (< 300 lines)
├── init-editor.el   # Editor settings (< 200 lines)
├── init-shell.el    # Shell integration (< 150 lines)
├── init-git.el      # Git configuration (< 200 lines)
└── init-python.el   # Python setup (< 150 lines)
```

**Benefits:**
- Easier to find and modify specific functionality
- Reduces merge conflicts
- Enables selective loading
- Improves readability

## Safe Defaults

**Always provide fallback values:**

```bash
# Good - uses default if not set
EDITOR="${EDITOR:-emacs}"
PAGER="${PAGER:-less}"

# Bad - fails if not set
export EDITOR
```

**For environment variables:**

```bash
# Provide sensible defaults
export PATH="${HOME}/bin:${PATH:-/usr/local/bin:/usr/bin:/bin}"
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
```

## Modular Emacs Configuration

**Structure Emacs configs for maintainability:**

```elisp
;;; init.el -*- lexical-binding: t; -*-
;; Main entry point - load modules

;; Core modules (required)
(require 'init-base)
(require 'init-packages)
(require 'init-keybindings)

;; Feature modules (optional based on use)
(require 'init-shell)
(require 'init-git)
(require 'init-project)

;; Local overrides (not in git)
(require 'init-local nil t)
```

**Each module file should:**
- Be < 400 lines
- Have single responsibility (e.g., `init-git.el` only configures git-related packages)
- Be documented with commentary section
- Handle missing dependencies gracefully

**Module template:**

```elisp
;;; omw-<feature>.el --- Brief description -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Your Name

;;; Commentary:

;; Full description of what this module configures.
;; Dependencies: omw-base.el, package-X

;;; Code:

;; Configuration here — prefix all symbols with omw-

(provide 'omw-<feature>)
;;; omw-<feature>.el ends here
```

## Design Patterns

### Repository Pattern for Configs

Encapsulate config access behind consistent interface:

```bash
# Define standard operations
config_load() { /* load config */ }
config_save() { /* save config */ }
config_validate() { /* check syntax */ }
config_backup() { /* create backup */ }
```

**Benefits:**
- Enables easy swapping of config locations
- Simplifies testing with mock configs
- Centralizes error handling

### Validation Pattern

**Always validate configs before use:**

```bash
_validate_config() {
  local -r config_file="$1"

  # Check file exists
  [[ -f "$config_file" ]] || return 1

  # Check readable
  [[ -r "$config_file" ]] || return 1

  # Validate syntax (shell)
  bash -n "$config_file" 2>/dev/null || return 1

  return 0
}

# Usage
if _validate_config "$config"; then
  source "$config"
else
  printf 'error: invalid config %s\n' "$config" >&2
  return 1
fi
```

## Anti-Patterns to Avoid

### Deep Nesting

**Limit nesting to 4 levels maximum:**

```bash
# WRONG - 5 levels deep
if condition1; then
  if condition2; then
    if condition3; then
      if condition4; then
        if condition5; then
          # Too deep!
        fi
      fi
    fi
  fi
fi

# CORRECT - Refactor to reduce nesting
_validate_and_execute() {
  condition1 || return 1
  condition2 || return 1
  condition3 || return 1
  condition4 || return 1
  # Execute logic here
}
```

### Magic Numbers

**No hardcoded values - use constants or config:**

```bash
# WRONG
sleep 30
timeout 300

# CORRECT
readonly CONNECT_TIMEOUT=30
readonly MAX_RETRIES=3
readonly RETRY_DELAY=5

sleep "$RETRY_DELAY"
timeout "$CONNECT_TIMEOUT"
```

### Hardcoded Paths

**Use variables instead of hardcoded paths:**

```bash
# WRONG
source /home/user/.config/zsh/aliases.zsh

# CORRECT
source "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/aliases.zsh"
```

### Large Monolithic Configs

**Don't create one giant config file:**

```bash
# WRONG - 2000 line .zshrc
# Everything in one file

# CORRECT - Modular structure
source "$ZDOTDIR/aliases.zsh"
source "$ZDOTDIR/functions.zsh"
source "$ZDOTDIR/options.zsh"
source "$ZDOTDIR/completion.zsh"
```
