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
├── ghostty/              # Ghostty terminal configuration
│   └── .config/ghostty/  # Symlinked to ~/.config/ghostty/
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
stow -d /path/to/oh-my-dotfiles -t "$HOME" zsh git vim emacs ripgrep

# Stow specific packages
stow zsh git

# Unstow (remove symlinks)
stow -D zsh git vim emacs ripgrep

# Restow (refresh symlinks)
stow -R zsh git vim emacs ripgrep

# Dry-run to see what would happen
stow -n zsh
```

## Stow Package System

| Package    | Contents                                        |
|------------|-------------------------------------------------|
| `zsh/`     | `.zshenv` → `$HOME/.zshenv`, `.config/zsh/` → `$HOME/.config/zsh/` |
| `git/`     | `.config/git/` → `$HOME/.config/git/`           |
| `vim/`     | `.config/vim/` → `$HOME/.config/vim/`           |
| `emacs/`   | `.config/emacs/` → `$HOME/.config/emacs/`       |
| `ghostty/`  | `.config/ghostty/` → `$HOME/.config/ghostty/`   |
| `ripgrep/` | `.config/ripgrep/` → `$HOME/.config/ripgrep/` |

**Note:** `homebrew/` and `macos/` are NOT stow packages — they provide scripts/utilities only.

## Initial Setup

```bash
# 1. Clone the repository
git clone https://github.com/zhengyuli/oh-my-dotfiles.git ~/oh-my-dotfiles
cd ~/oh-my-dotfiles

# 2. Install Homebrew packages
brew bundle --file homebrew/Brewfile

# 3. Stow configuration packages
stow zsh git vim emacs ghostty ripgrep

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

## Toolchain

Modern development toolchain without traditional version managers:

| Tool | Purpose | Replaces |
|------|---------|----------|
| **bun** | JS/TS runtime + package manager | node, npm, yarn, pnpm, fnm |
| **uv** | Python package manager | pip, pipenv, poetry, pyenv |
| **go** | Go programming language | (standard install) |

### Installation Commands

```bash
# Global LSP Servers (bun/uv managed)

# JS/TS ecosystem
bun install -g typescript-language-server typescript
bun install -g vscode-langservers-extracted   # html/css/json/eslint
bun install -g bash-language-server
bun install -g @tailwindcss/language-server
bun install -g prettier

# Python ecosystem
uv tool install basedpyright
uv tool install ruff

# Go (standard install)
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest

# System-level (via Homebrew)
# clangd, lua-language-server, rust-analyzer
```

## Terminal

- **Ghostty**: Fast, native terminal emulator with XDG-compliant configuration
- **Theme**: Doom One color scheme across all tools

## Theme

Unified **Doom One** theme across:
- **Emacs**: `doom-one` theme via `doom-themes` package
- **Ghostty**: Custom color palette matching Doom One
- **fzf**: Doom One color configuration in `FZF_DEFAULT_OPTS`

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
- **Modern toolchain**: bun for JS/TS, uv for Python, no version managers needed

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

## Cache File Detection

**IMPORTANT**: Before committing, always check for runtime cache files that should not be tracked.

### Files to NEVER Commit

| Tool | Files | Proper Location |
|------|-------|-----------------|
| Zsh | `.zcompdump`, `.zcompdump.*`, `cache/` | `$XDG_CACHE_HOME/zsh/` |
| Vim | `*.swp`, `*.swo`, `*.bak`, `*~`, `undo/` | `$XDG_STATE_HOME/vim/`, `$XDG_DATA_HOME/vim/` |
| Emacs | `*.elc`, `.#*`, `*~`, `#*#` | Generated, should not be in repo |
| Git | `config.local`, `credentials` | Machine-specific |

### Detection Command

Before committing, run:

```bash
# Check for cache files that shouldn't be committed
git status --porcelain | grep -E '\.(zcompdump|swp|swo|bak|elc|pyc)$|cache/|undo/'

# If any files match, either:
# 1. Add them to .gitignore if not already covered
# 2. Remove with: git rm --cached <file>
```

### Why This Matters

If XDG environment variables (`XDG_CACHE_HOME`, `XDG_STATE_HOME`) are not properly set, tools may create cache files in `.config/` directories instead of their proper locations. The `.gitignore` includes fallback patterns to catch these cases.
