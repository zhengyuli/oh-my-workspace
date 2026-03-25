# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

oh-my-workspace is a dotfiles repository for macOS that uses GNU Stow to manage configuration files. It follows an XDG Base Directory Specification-compliant structure.

## Key Commands

```bash
# Full setup (prerequisites + brew bundle + stow all)
./setup.sh install --all

# Stow specific packages
./setup.sh install zsh git vim

# Restow to pick up new dotfiles (removes old symlinks, recreates)
./setup.sh install --force zsh

# Preview changes without modifying files
./setup.sh install --dry-run --all

# Unstow all packages
./setup.sh uninstall --all

# Check stow status and symlinks
./setup.sh status
```

## Architecture

### Package System

Packages are organized by category in the repository root:
- `shell/` — Shell and prompt configs (zsh, starship)
- `editor/` — Text editors (vim, emacs)
- `term/` — Terminal emulators (ghostty)
- `tool/` — CLI utilities (git, lazygit, ripgrep, yazi)
- `lang/` — Language runtimes (python/uv, typescript/bun)
- `platform/` — Platform-specific configs (darwin)

Each package directory follows GNU Stow convention: files are placed as they would appear in `$HOME`. For example, `shell/zsh/.config/zsh/.zshrc` becomes `~/.config/zsh/.zshrc` after stowing.

### Zsh Configuration

Uses XDG-compliant two-stage loading:
1. `~/.zshenv` — Bootstrap only: sets XDG paths and `ZDOTDIR`
2. `$ZDOTDIR/conf.d/*.zsh` — Numbered modules loaded in order (00-env, 05-path, ..., 99-local)

The numbered prefix ensures load order. Local overrides go in `99-local.zsh` (not tracked by git).

### Emacs Configuration

Modular structure under `editor/emacs/.config/emacs/`:
- `init.el` — Entry point, sets up load paths
- `early-init.el` — Early initialization
- `lisp/editor/` — Editor features (completion, appearance, etc.)
- `lisp/lang/` — Language-specific modes (python, go, rust, etc.)
- `lisp/tool/` — Tool integrations (git, term, pass, etc.)
- `lisp/lib/` — Shared utilities
- `site-packages/` — Vendored packages

### Stow Workflow

When adding new dotfiles:
1. Create files in the appropriate package directory
2. Test with `./setup.sh install --dry-run <package>`
3. Apply with `./setup.sh install --force <package>`

Conflicts (existing files at target paths) are automatically removed by `_remove_stow_conflicts()` in setup.sh.

## Package Registry

All packages are registered in `PKG_ALL` array in `setup.sh`:
```
shell/zsh shell/starship editor/vim editor/emacs
term/ghostty tool/git tool/lazygit tool/ripgrep tool/yazi
lang/python/uv lang/typescript/bun
```

## Homebrew Dependencies

Managed via `pkg/homebrew/Brewfile`. Run `brew bundle` separately or let `./setup.sh install --all` handle it.

## Configuration Rules

Project-specific coding standards are in `.claude/rules/`:
- `coding-style.md` — Line length, documentation, immutability
- `git-workflow.md` — Conventional Commits format
- `development-workflow.md` — Research-first approach, stow workflows
- `patterns.md` — Design patterns and anti-patterns
- `lang/shell.md`, `lang/bash.md`, `lang/zsh.md` — Shell conventions
- `lang/elisp.md` — Emacs Lisp conventions
- `lang/python.md` — Python conventions
- `testing.md` — Syntax validation and testing strategy
- `hooks.md` — Claude Code hook integration
- `security.md` — Secrets management

These rules are conditionally loaded based on file paths being edited.
