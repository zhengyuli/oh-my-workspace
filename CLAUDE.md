---
lastUpdated: 2026-03-23
maintainer: zhengyu.li
---

# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project Overview

This is a **workspace repository** providing comprehensive development environment setup for macOS. It uses **GNU Stow** for symlink management with XDG-compliant directory structure.

## Directory Structure

Directories are organized by category for better organization and cross-platform extensibility:

```
oh-my-workspace/
├── editor/              # Editor configurations
│   ├── emacs/          # Emacs >= 30.2
│   └── vim/            # Vim
├── shell/              # Shell configurations
│   ├── zsh/            # Zsh
│   └── starship/       # Starship prompt
├── runtime/            # Language runtimes and package managers
│   ├── bun/            # Bun JS/TS runtime
│   └── uv/             # UV Python package manager
├── terminal/           # Terminal emulator configurations
│   └── ghostty/        # Ghostty terminal
├── vcs/                # Version control systems
│   └── git/            # Git
├── tools/              # General CLI tools
│   └── ripgrep/        # Ripgrep
├── packages/           # System package managers
│   └── homebrew/       # Homebrew Brewfile (NOT stowed)
└── platform/           # Operating system configurations
    └── macos/          # macOS settings (NOT stowed)
```

### Category Definitions

| Category | Purpose | Future Extensions |
|----------|---------|-------------------|
| `editor/` | Text editors | neovim, helix |
| `shell/` | Shell and prompt | fish, nushell |
| `runtime/` | Language runtimes | go, rust, deno |
| `terminal/` | Terminal emulators | alacritty, kitty, wezterm |
| `vcs/` | Version control | mercurial |
| `tools/` | CLI utilities | fd, bat, eza |
| `packages/` | System packages | apt, pacman, dnf (Linux) |
| `platform/` | OS configurations | linux |

## Setup

```bash
git clone https://github.com/zhengyuli/oh-my-workspace.git ~/oh-my-workspace
cd ~/oh-my-workspace
./setup.sh install --all
source ~/.zshenv
```

Or manually with stow:
```bash
brew bundle --file package-manager/homebrew/Brewfile
stow -d editor -t $HOME emacs vim
stow -d shell -t $HOME zsh starship
stow -d runtime -t $HOME bun uv
stow -d terminal -t $HOME ghostty
stow -d vcs -t $HOME git
stow -d tools -t $HOME ripgrep
source ~/.zshenv
```

| Command | Description |
|---------|-------------|
| `stow -d shell zsh` | Stow zsh from shell category |
| `stow -D zsh` | Unstow (remove symlinks) |
| `stow -R zsh` | Restow (refresh symlinks) |
| `stow -n zsh` | Dry-run preview |

## Stow Packages

| Package | Category | Symlink Target |
|---------|----------|----------------|
| `emacs` | editor | `$HOME/.config/emacs/` |
| `vim` | editor | `$HOME/.config/vim/` |
| `zsh` | shell | `$HOME/.zshenv`, `$HOME/.config/zsh/` |
| `starship` | shell | `$HOME/.config/starship.toml` |
| `bun` | runtime | `$HOME/.config/bun/` |
| `uv` | runtime | `$HOME/.config/uv/` |
| `ghostty` | terminal | `$HOME/.config/ghostty/` |
| `git` | vcs | `$HOME/.config/git/` |
| `ripgrep` | tools | `$HOME/.config/ripgrep/` |

`homebrew/` and `macos/` are NOT stow packages.

## Architecture

### XDG Base Directory

| Variable | Default | Purpose |
|----------|---------|---------|
| `XDG_CONFIG_HOME` | `~/.config` | Configurations |
| `XDG_CACHE_HOME` | `~/.cache` | Cache files |
| `XDG_DATA_HOME` | `~/.local/share` | Data files |
| `XDG_STATE_HOME` | `~/.local/state` | State files |

### Toolchain

| Tool | Purpose | Replaces |
|------|---------|----------|
| **bun** | JS/TS runtime + package manager | node, npm, yarn, pnpm, fnm |
| **uv** | Python package manager | pip, pipenv, poetry, pyenv |
| **go** | Go programming language | — |

Use `bun` for all JS/TS operations (never npm/yarn/pnpm). Use `uv` for Python.

### LSP Servers

```bash
# JS/TS (bun)
bun install -g typescript-language-server typescript vscode-langservers-extracted \
  bash-language-server @tailwindcss/language-server prettier

# Python (uv)
uv tool install basedpyright ruff

# Go
go install golang.org/x/tools/gopls@latest golang.org/x/tools/cmd/goimports@latest

# System (Homebrew): clangd, lua-language-server, rust-analyzer
```

### Theme

Unified **Doom One** across Emacs, Ghostty, Starship, and fzf.

## Coding Standards

See `.claude/rules/` for all standards:

| Directory | Coverage |
|-----------|---------|
| `rules/common/` | Config file format, headers, timestamps, XDG conventions |
| `rules/shell/` | Shell scripting patterns, Zsh-specific rules |
| `rules/elisp/` | Emacs Lisp standards (omw/ prefix, use-package, etc.) |

## Custom Commands

| Command | Purpose |
|---------|---------|
| `/validate-config` | Validate configuration headers and format |
| `/pre-commit-check` | Run pre-commit verification checks |
| `/stow-management` | Manage stow package symlinks |

## setup.sh

Interactive stow package manager with these key functions:

| Function | Type | Purpose |
|----------|------|---------|
| `is_stowed()` | Query | Check if package is stowed |
| `has_stow()` | Query | Check stow availability |
| `stow_package()` | Operation | Stow a single package |
| `backup_file()` | Operation | Backup existing file |

Error handling: explicit `if` statements, `set -euo pipefail`, all output via `printf`.
