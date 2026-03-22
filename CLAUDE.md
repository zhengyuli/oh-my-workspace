---
lastUpdated: 2026-03-22
maintainer: zhengyu.li
---

# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project Overview

This is a **dotfiles repository** providing comprehensive development environment setup for macOS. It uses **GNU Stow** for symlink management with XDG-compliant directory structure.

## Directory Structure

```
oh-my-dotfiles/
├── bun/          # Bun JS/TS runtime configuration
├── emacs/        # Emacs >= 30.2 — see emacs/CLAUDE.md
├── ghostty/      # Ghostty terminal configuration
├── git/          # Git configuration
├── homebrew/     # Brewfile (NOT a stow package)
├── macos/        # macOS settings (NOT a stow package)
├── ripgrep/      # Ripgrep configuration
├── starship/     # Starship prompt configuration
├── uv/           # UV package manager configuration
├── vim/          # Vim configuration — see vim/CLAUDE.md
└── zsh/          # Zsh configuration — see zsh/CLAUDE.md
```

## Setup

```bash
git clone https://github.com/zhengyuli/oh-my-dotfiles.git ~/oh-my-dotfiles
brew bundle --file homebrew/Brewfile
stow bun emacs ghostty git ripgrep starship uv vim zsh
source ~/.zshenv
```

| Command | Description |
|---------|-------------|
| `stow zsh git` | Stow specific packages |
| `stow -D zsh` | Unstow (remove symlinks) |
| `stow -R zsh` | Restow (refresh symlinks) |
| `stow -n zsh` | Dry-run preview |

## Stow Packages

| Package | Symlink Target |
|---------|----------------|
| `bun/` | `$HOME/.config/bun/` |
| `emacs/` | `$HOME/.config/emacs/` |
| `ghostty/` | `$HOME/.config/ghostty/` |
| `git/` | `$HOME/.config/git/` |
| `ripgrep/` | `$HOME/.config/ripgrep/` |
| `starship/` | `$HOME/.config/starship.toml` |
| `uv/` | `$HOME/.config/uv/` |
| `vim/` | `$HOME/.config/vim/` |
| `zsh/` | `$HOME/.zshenv`, `$HOME/.config/zsh/` |

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
