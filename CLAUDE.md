# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project Overview

This is a **dotfiles repository** providing comprehensive development environment setup for macOS. It uses a unified setup script with a `*.symlink` convention for managing configuration files.

## Repository Structure

```
oh-my-dotfiles/
├── setup.sh              # Unified setup and maintenance utility
├── emacs/                # Emacs configuration (>= 30.2)
│   ├── CLAUDE.md         # Emacs-specific guidance (detailed)
│   ├── emacs.symlink     # Symlinked to ~/.emacs
│   ├── lisp/             # Core and language modules
│   ├── templates/        # File templates (C, Python, Go, etc.)
│   └── banners/          # Startup banners
├── vim/                  # Vim configuration
│   └── vimrc.symlink     # Symlinked to ~/.vimrc
├── zsh/                  # Zsh configuration
│   ├── conf.d/           # Modular zsh configs (aliases, completion, etc.)
│   ├── zshrc.symlink     # Symlinked to ~/.zshrc
│   ├── zshenv.symlink    # Symlinked to ~/.zshenv
│   └── zprofile.symlink  # Symlinked to ~/.zprofile
├── git/                  # Git configuration
│   ├── gitconfig.symlink # Symlinked to ~/.gitconfig
│   └── gitignore.symlink # Symlinked to ~/.gitignore
├── homebrew/
│   └── Brewfile          # Homebrew bundle for all packages
└── macos/                # macOS-specific settings
```

## Setup Commands

```bash
# Full setup (Homebrew, plugins, languages, symlinks, shell)
./setup.sh full-setup

# Re-create symlinks only
./setup.sh create-links

# Remove all managed symlinks
./setup.sh remove-links

# Update zsh plugins
./setup.sh update-plugins

# Check symlink and tool status
./setup.sh show-status

# Silent mode
VERBOSE=0 ./setup.sh full-setup
```

## Symlink System

Files named `*.symlink` are automatically linked to `$HOME`:

| Source                       | Target           |
|------------------------------|------------------|
| `emacs/emacs.symlink`        | `~/.emacs`       |
| `vim/vimrc.symlink`          | `~/.vimrc`       |
| `zsh/zshrc.symlink`          | `~/.zshrc`       |
| `zsh/zshenv.symlink`         | `~/.zshenv`      |
| `zsh/zprofile.symlink`       | `~/.zprofile`    |
| `git/gitconfig.symlink`      | `~/.gitconfig`   |
| `git/gitignore.symlink`      | `~/.gitignore`   |

Existing files are backed up to `~/.dotfiles-backup/` before linking (single backup, no timestamps).

## XDG Base Directory

The setup script exports XDG environment variables:

```bash
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
```

## Zsh Plugin System

Zsh plugins are installed to `${XDG_DATA_HOME}/zsh/plugins/` (not Oh My Zsh dependent):

- zsh-completions
- zsh-autosuggestions
- zsh-history-substring-search
- zsh-syntax-highlighting

## Subdirectory CLAUDE.md Files

Each major component has its own `CLAUDE.md` with detailed guidance:

- **emacs/CLAUDE.md** - Emacs coding standards, module architecture, use-package patterns, naming conventions (`omw/` prefix)

These files are lazy-loaded when working in their respective directories.

## Quick Validation

```bash
# Check setup status
./setup.sh status

# Test Emacs configuration
emacs --debug-init

# Batch test Emacs
emacs --batch --eval '(progn (load-file "emacs/emacs.symlink") (message "OK"))'
```

## Key Conventions

- **macOS-focused**: All configurations assume macOS as primary OS
- **Symlink-based**: Config files are symlinked via `*.symlink` convention
- **Unified setup**: Single `setup.sh` handles all components
- **Emacs prefix**: Uses `omw/` prefix (oh-my-workspace) for custom functions/variables

## Shell Script Coding Standards

These conventions apply to **both bash and zsh scripts** throughout the repository.

### 1. Avoid `[[ cond ]] && cmd || true` Pattern (CRITICAL)

**WRONG** - This pattern is error-prone and hides potential issues:

```bash
# Problem 1: Non-zero exit code from condition causes function to fail under set -e
[[ -n "$var" ]] && log_warn "message"

# Problem 2: Adding || true masks errors and is hard to read
[[ -n "$var" ]] && log_warn "message" || true
```

**CORRECT** - Use explicit `if` statements:

```bash
if [[ -n "$var" ]]; then
    log_warn "message"
fi
```

**Rationale:**
1. With `set -e`, a false condition (`[[ -n "$var" ]]` when var is empty) returns exit code 1, causing unexpected script termination
2. Adding `|| true` masks potential errors in the command itself
3. `if` statements are explicit, readable, and behave predictably

**Acceptable uses of `&&`:**
```bash
# Early return pattern (function continues if condition is false)
[[ -d "$dir" ]] && return

# Conditional execution where false condition is expected and harmless
[[ -n "$value" ]] && git config "$key" "$value"
```

### 2. Use `[[ ]]` for Conditional Tests

Prefer `[[ ]]` over `[ ]` or `test` for condition checks:

```bash
# ✅ CORRECT
[[ -f "$file" ]]
[[ "$var" == "pattern" ]]
[[ -n "$value" ]]

# ❌ AVOID
[ -f "$file" ]
test -f "$file"
```

**Rationale:** `[[ ]]` prevents word-splitting, supports pattern matching, and is more readable.

### 3. Use `(( ))` for Arithmetic

```bash
# ✅ CORRECT
(( count++ ))
(( total = a + b ))
local result=$(( a * b ))

# ❌ AVOID
let count+=1
result=$(expr $a + $b)
```

### 4. Use `$()` for Command Substitution

```bash
# ✅ CORRECT
local dir=$(dirname "$file")

# ❌ AVOID - hard to nest, hard to read
local dir=`dirname "$file"`
```

### 5. Quote All External Data

Any value from files, environment variables, or command output must be quoted:

```bash
# ✅ CORRECT
rm -- "${file}"
grep -- "${pattern}" "${file}"
source "${config_file}"

# ❌ WRONG - word-splitting and glob expansion risks
rm ${file}
grep ${pattern} ${file}
```

### 6. Prefer `printf` Over `echo -e`

```bash
# ✅ CORRECT - portable and predictable
printf '%s\n' "$message"
printf 'Status: %s\n' "$status"

# ❌ AVOID - behavior varies between shells
echo -e "$message"
```

### 7. Use `set -e` with Caution

```bash
# ✅ CORRECT - scoped to specific operations
(
    set -e
    cmd1
    cmd2
)

# ❌ AVOID globally in interactive shells
set -e  # Can cause unexpected exits
```

### 8. Security: Validate Before Sourcing

```bash
# ✅ CORRECT - check before sourcing
if [[ -f "${file}" ]]; then
    source "${file}"
fi

# ❌ WRONG - error if file missing
source "${file}"
```

### 9. Security: Never `eval` Untrusted Input

```bash
# ✅ CORRECT - use allowlist
local -a allowed=(rbenv pyenv nodenv)
if (( ${allowed[(I)${tool}]} )); then
    eval "$(${tool} init -)"
fi

# ❌ DANGEROUS - arbitrary code execution
eval "${user_input}"
```

### 10. Check File Existence Before Operations

```bash
# ✅ CORRECT
[[ -f "$file" ]] && rm -- "$file"

# ❌ WRONG - error if file doesn't exist
rm "$file"
```

For zsh-specific conventions (startup files, setopt, arrays, etc.), see [zsh/CLAUDE.md](zsh/CLAUDE.md).
