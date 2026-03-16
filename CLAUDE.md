# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project Overview

This is a **dotfiles repository** providing comprehensive development environment setup for macOS. It uses **GNU Stow** for symlink management with XDG-compliant directory structure.

## Repository Structure

```
oh-my-dotfiles/
├── .stow-local-ignore    # Files to ignore when stowing
├── emacs/                # Emacs configuration (>= 30.2)
│   ├── CLAUDE.md         # Emacs-specific guidance (detailed)
│   └── .config/emacs/    # Symlinked to ~/.config/emacs/
├── vim/                  # Vim configuration
│   └── .config/vim/      # Symlinked to ~/.config/vim/
├── zsh/                  # Zsh configuration
│   ├── CLAUDE.md         # Zsh-specific guidance
│   ├── .zshenv           # Symlinked to ~/.zshenv (bootstrap file)
│   └── .config/zsh/      # Symlinked to ~/.config/zsh/
├── git/                  # Git configuration
│   └── .config/git/      # Symlinked to ~/.config/git/
├── homebrew/
│   └── Brewfile          # Homebrew bundle for all packages
└── macos/                # macOS-specific settings
```

## Stow Commands

GNU Stow manages symlinks from packages to `$HOME`:

```bash
# Stow all packages
stow -d /path/to/oh-my-dotfiles -t "$HOME" zsh git vim emacs

# Stow specific packages
stow zsh git

# Unstow (remove symlinks)
stow -D zsh git vim emacs

# Restow (refresh symlinks)
stow -R zsh git vim emacs

# Dry-run to see what would happen
stow -n zsh
```

## Stow Package System

| Package  | Contents                                        |
|----------|-------------------------------------------------|
| `zsh/`   | `.zshenv` → `$HOME/.zshenv`, `.config/zsh/` → `$HOME/.config/zsh/` |
| `git/`   | `.config/git/` → `$HOME/.config/git/`           |
| `vim/`   | `.config/vim/` → `$HOME/.config/vim/`           |
| `emacs/` | `.config/emacs/` → `$HOME/.config/emacs/`       |

**Note:** `homebrew/` and `macos/` are NOT stow packages — they provide scripts/utilities only.

## Initial Setup

```bash
# 1. Clone the repository
git clone https://github.com/zhengyuli/oh-my-dotfiles.git ~/oh-my-dotfiles
cd ~/oh-my-dotfiles

# 2. Install Homebrew packages
brew bundle --file homebrew/Brewfile

# 3. Stow configuration packages
stow zsh git vim emacs

# 4. Restart shell or source zshenv
source ~/.zshenv
```

## XDG Base Directory

Shell configuration exports XDG environment variables:

```bash
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
```

## Zsh Plugin System

Zsh plugins are managed by **Zinit** installed at `${XDG_DATA_HOME}/zinit/`. See `zsh/CLAUDE.md` for plugin configuration.

## Subdirectory CLAUDE.md Files

Each major component has its own `CLAUDE.md` with detailed guidance:

- **zsh/CLAUDE.md** - Zsh configuration structure, startup sequence, plugin system
- **emacs/CLAUDE.md** - Emacs coding standards, module architecture, use-package patterns

These files are lazy-loaded when working in their respective directories.

## Quick Validation

```bash
# Verify stow packages are linked
ls -la ~/.zshenv ~/.config/zsh/.zshrc ~/.config/git/config

# Test Zsh configuration
zsh -c 'echo $ZDOTDIR'

# Test Emacs configuration
emacs --debug-init
```

## Key Conventions

- **macOS-focused**: All configurations assume macOS as primary OS
- **Stow-managed**: Config files are symlinked via GNU Stow
- **XDG-compliant**: Follows XDG Base Directory Specification
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
