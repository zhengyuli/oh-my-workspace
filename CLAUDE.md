# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project Overview

This is a **dotfiles repository** providing comprehensive development environment setup for macOS. It uses **GNU Stow** for symlink management with XDG-compliant directory structure.

## Directory Structure

```
oh-my-dotfiles/
├── CLAUDE.md             # This file - project-wide guidance
├── setup.sh              # Interactive setup script
├── bun/                  # Bun configuration
│   ├── CLAUDE.md         # Bun-specific guidance
│   └── .config/bun/      # Symlinked to ~/.config/bun/
├── emacs/                # Emacs configuration (>= 30.2)
│   ├── CLAUDE.md         # Emacs-specific guidance (detailed)
│   └── .config/emacs/    # Symlinked to ~/.config/emacs/
├── ghostty/              # Ghostty terminal configuration
│   └── .config/ghostty/  # Symlinked to ~/.config/ghostty/
├── git/                  # Git configuration
│   └── .config/git/      # Symlinked to ~/.config/git/
├── homebrew/
│   └── Brewfile          # Homebrew bundle for all packages
├── macos/                # macOS-specific settings
├── ripgrep/              # Ripgrep configuration
│   └── .config/ripgrep/  # Symlinked to ~/.config/ripgrep/
├── starship/             # Starship prompt configuration
│   └── .config/starship.toml  # Symlinked to ~/.config/starship.toml
├── uv/                   # UV package manager configuration
│   └── .config/uv/       # Symlinked to ~/.config/uv/
├── vim/                  # Vim configuration
│   ├── CLAUDE.md         # Vim-specific guidance
│   └── .config/vim/      # Symlinked to ~/.config/vim/
└── zsh/                  # Zsh configuration
    ├── CLAUDE.md         # Zsh-specific guidance
    ├── .zshenv           # Symlinked to ~/.zshenv (bootstrap file)
    └── .config/zsh/      # Symlinked to ~/.config/zsh/
```

## Quick Start

### Setup

```bash
# 1. Clone the repository
git clone https://github.com/zhengyuli/oh-my-dotfiles.git ~/oh-my-dotfiles
cd ~/oh-my-dotfiles

# 2. Install Homebrew packages
brew bundle --file homebrew/Brewfile

# 3. Stow configuration packages
stow bun emacs ghostty git ripgrep starship uv vim zsh

# 4. Restart shell or source zshenv
source ~/.zshenv
```

### Common Commands

| Command | Description |
|---------|-------------|
| `stow zsh git` | Stow specific packages |
| `stow -D zsh git` | Unstow (remove symlinks) |
| `stow -R zsh git` | Restow (refresh symlinks) |
| `stow -n zsh` | Dry-run to preview changes |
| `brew bundle --file homebrew/Brewfile` | Install all Homebrew packages |

### Quick Validation

```bash
# Verify stow packages are linked
ls -la ~/.zshenv ~/.config/zsh/.zshrc ~/.config/git/config

# Test Zsh configuration
zsh -c 'echo $ZDOTDIR'

# Test Emacs configuration
emacs --debug-init
```

## Stow Package System

| Package | Contents |
|---------|----------|
| `bun/` | `.config/bun/` → `$HOME/.config/bun/` |
| `emacs/` | `.config/emacs/` → `$HOME/.config/emacs/` |
| `ghostty/` | `.config/ghostty/` → `$HOME/.config/ghostty/` |
| `git/` | `.config/git/` → `$HOME/.config/git/` |
| `ripgrep/` | `.config/ripgrep/` → `$HOME/.config/ripgrep/` |
| `starship/` | `.config/starship.toml` → `$HOME/.config/starship.toml` |
| `uv/` | `.config/uv/` → `$HOME/.config/uv/` |
| `vim/` | `.config/vim/` → `$HOME/.config/vim/` |
| `zsh/` | `.zshenv` → `$HOME/.zshenv`, `.config/zsh/` → `$HOME/.config/zsh/` |

**Note:** `homebrew/` and `macos/` are NOT stow packages - they provide scripts/utilities only.

## Architecture

### XDG Base Directory

Shell configuration exports XDG environment variables:

```
$HOME/.config/     ← XDG_CONFIG_HOME (configurations)
$HOME/.cache/      ← XDG_CACHE_HOME (cache files)
$HOME/.local/share/ ← XDG_DATA_HOME (data files)
$HOME/.local/state/ ← XDG_STATE_HOME (state files)
```

```bash
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
```

### Toolchain Overview

| Tool | Purpose | Replaces |
|------|---------|----------|
| **bun** | JS/TS runtime + package manager | node, npm, yarn, pnpm, fnm |
| **uv** | Python package manager | pip, pipenv, poetry, pyenv |
| **go** | Go programming language | (standard install) |

### Subdirectory CLAUDE.md Files

Each major component has its own `CLAUDE.md` with detailed guidance:

| File | Content |
|------|---------|
| **zsh/CLAUDE.md** | Zsh configuration structure, startup sequence, plugin system |
| **emacs/CLAUDE.md** | Emacs coding standards, module architecture, use-package patterns |
| **vim/CLAUDE.md** | Vim configuration, XDG paths, alignment exception |
| **bun/CLAUDE.md** | Bun configuration, tool installation |

These files are lazy-loaded when working in their respective directories.

## Toolchain

### LSP Server Installation

```bash
# JS/TS ecosystem (bun)
bun install -g typescript-language-server typescript
bun install -g vscode-langservers-extracted   # html/css/json/eslint
bun install -g bash-language-server
bun install -g @tailwindcss/language-server
bun install -g prettier

# Python ecosystem (uv)
uv tool install basedpyright
uv tool install ruff

# Go (standard install)
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest

# System-level (via Homebrew)
# clangd, lua-language-server, rust-analyzer
```

## Theme

Unified **Doom One** theme across:

| Tool | Configuration |
|------|---------------|
| **Emacs** | `doom-one` theme via `doom-themes` package |
| **Ghostty** | Custom color palette matching Doom One |
| **Starship** | Prompt configuration with Doom One colors |
| **fzf** | Doom One color configuration in `FZF_DEFAULT_OPTS` |

## Terminal

- **Ghostty**: Fast, native terminal emulator with XDG-compliant configuration

## Setup Scripts

### setup.sh

The main setup script provides interactive package management for stow operations.

**Key patterns:**

```bash
# Pure query functions (no side effects)
is_stowed()    # Check if package is stowed
has_stow()     # Check if stow is available
has_homebrew() # Check if homebrew is installed

# Operation functions (have side effects)
stow_package()   # Stow a single package
backup_file()    # Backup existing file
restore_file()   # Restore from backup
```

**Error handling:**
- Use explicit `if` statements (not `[[ ]] && cmd`)
- Use `set -euo pipefail` at script start
- All output via `printf` with helper functions: `log_ok`, `log_err`, `log_warn`

## Coding Standards

See `.claude/rules/dotfiles/` for detailed standards:

- **coding-style.md** - Universal config file standards (headers, comments, alignment)
- **shell-standards.md** - Shell script patterns (extends global rules)
- **xdg-paths.md** - XDG Base Directory conventions

For validation workflows, use:
- `/validate-config` - Configuration validation
- `/pre-commit-check` - Pre-commit verification
- `/stow-management` - Stow package operations

## Code Quality

### Validation Commands

**Verify stow symlinks:**
```bash
ls -la ~/.zshenv ~/.config/zsh/.zshrc ~/.config/git/config
```

**Test shell configuration:**
```bash
zsh -c 'echo $ZDOTDIR'
```

**Check for cache files before committing:**
```bash
git status --porcelain | grep -E '\.(zcompdump|swp|swo|bak|elc|pyc)$|cache/|undo/'
```

### Compliance Checklist

- [ ] All stow packages properly symlinked
- [ ] No cache files committed (`.zcompdump`, `*.swp`, etc.)
- [ ] Configuration files have standardized headers
- [ ] Shell scripts follow coding standards (no `[[ ]] && cmd || true`)
- [ ] No `echo` in scripts (use `printf`)
- [ ] No inline comments in config files (comments on separate lines above code)
- [ ] Time-stamp present in ALL configuration files
- [ ] Section separators use `---` for content, `===` for headers only (79 chars)
- [ ] No alignment spaces (single space only)
- [ ] All comments in English
- [ ] Line length within limits (80 code, 79 comments)

## Best Practices

### 1. Stow Management

**Always use stow for symlink management:**
```bash
# CORRECT - use stow
stow zsh git

# AVOID - manual symlinks break stow tracking
ln -s ~/oh-my-dotfiles/zsh/.zshenv ~/.zshenv
```

### 2. Cache File Prevention

**Never commit runtime cache files:**
```bash
# Check before committing
git status --porcelain | grep -E '\.(zcompdump|swp|swo|bak|elc)$'
```

**Why:** If XDG variables aren't set, tools may create cache files in `.config/` instead of proper locations.

### 3. Configuration File Headers

**Use standardized headers for all configuration files:**
- Level 1: Simple files (ghostty, ripgrep)
- Level 2: Complex files with mode lines (gitconfig)

**Why:** Consistent headers improve discoverability and maintenance.

## Key Conventions

- **macOS-focused**: All configurations assume macOS as primary OS
- **Stow-managed**: Config files are symlinked via GNU Stow
- **XDG-compliant**: Follows XDG Base Directory Specification
- **Emacs prefix**: Uses `omw/` prefix (oh-my-workspace) for custom functions/variables
- **Modern toolchain**: bun for JS/TS, uv for Python, no version managers needed

## Quick Reference Card

### Essential Rules

1. **Stow Only:** Use `stow` for symlink management, never manual `ln -s`
2. **XDG Paths:** All tools must respect `$XDG_*` environment variables
3. **Shell Scripts:** Use `if` statements, avoid `[[ ]] && cmd || true`
4. **Output:** Use `printf`, never `echo`
5. **Config Headers:** Standardized format with `===` for headers, `---` for sections
6. **Time-stamp:** Required for ALL configuration files
7. **Inline Comments:** Prohibited (comments on line above code)
8. **Alignment Spaces:** Prohibited (single space only, including around `=`)
9. **Line Length:** 80 chars for code, 79 for comments
10. **Comment Language:** English only
11. **Cache Files:** Never commit `.zcompdump`, `*.swp`, `*.elc`, etc.
12. **Prefix Convention:** Emacs uses `omw/`, zsh uses no special prefix
13. **Modern Toolchain:** bun for JS/TS, uv for Python, no version managers
14. **Theme:** Doom One across all tools (Emacs, Ghostty, fzf)
15. **Subdirectory CLAUDE.md:** Check zsh/CLAUDE.md and emacs/CLAUDE.md for specific guidance

### Pre-Commit Checklist

- [ ] No cache files: `git status | grep -E '\.(zcompdump|swp|elc)$'`
- [ ] Stow symlinks valid: `ls -la ~/.zshenv ~/.config/git/config`
- [ ] Shell loads: `zsh -c 'source ~/.zshenv'`
- [ ] Config headers compliant: Standardized format with `===`/`---`
- [ ] Time-stamp present: ALL config files have Time-stamp
- [ ] No alignment spaces: Single space between elements (including around `=`)
- [ ] No `echo`: all output via `printf`
- [ ] All comments in English

### Common Patterns

```bash
# Stow package management
stow bun emacs ghostty git ripgrep starship uv vim zsh  # Install all
stow -D zsh                                              # Remove package
stow -R zsh                                              # Refresh package

# XDG environment
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# Shell conditional (correct pattern)
if [[ -n "$var" ]]; then
    cmd
fi

# Config file header
# filename
# =============================================================================
# Description
#
# Location: ~/.config/tool/filename
# =============================================================================
```
