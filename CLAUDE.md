# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project Overview

This is a **dotfiles repository** providing comprehensive development environment setup for macOS. It uses a unified setup script with a `*.symlink` convention for managing configuration files.

## Repository Structure

```
oh-my-dotfiles/
├── setup.sh              # Unified setup and maintenance utility
├── editor/
│   ├── emacs/            # Emacs configuration (>= 30.2)
│   │   ├── CLAUDE.md     # Emacs-specific guidance (detailed)
│   │   ├── init.el       # Main entry point
│   │   └── lisp/         # Core and language modules
│   └── vim/              # Vim configuration
├── homebrew/
│   └── Brewfile          # Homebrew bundle for all packages
├── lang/                 # Language runtime configurations
├── macos/                # macOS-specific settings
├── shell/
│   └── zsh/              # Zsh configuration
├── terminal/
│   └── itermt2/          # iTerm2 configuration
└── vc/
    └── git/              # Git configuration
```

## Setup Commands

```bash
# Full setup (Homebrew, plugins, languages, symlinks, shell)
./setup.sh setup

# Re-create symlinks only
./setup.sh link

# Remove all managed symlinks
./setup.sh unlink

# Update zsh plugins
./setup.sh update

# Check symlink and tool status
./setup.sh status

# Silent mode
VERBOSE=0 ./setup.sh setup
```

## Symlink System

Files named `*.symlink` are automatically linked to `$HOME`:

| Source                              | Target           |
|-------------------------------------|------------------|
| `editor/emacs/emacs.symlink`        | `~/.emacs`       |
| `editor/vim/vimrc.symlink`          | `~/.vimrc`       |
| `shell/zsh/zshrc.symlink`           | `~/.zshrc`       |
| `shell/zsh/zshenv.symlink`          | `~/.zshenv`      |
| `shell/zsh/zprofile.symlink`        | `~/.zprofile`    |
| `vc/git/gitconfig.symlink`          | `~/.gitconfig`   |
| `vc/git/gitignore.symlink`          | `~/.gitignore`   |

Existing files are backed up to `~/.dotfiles-backup-<timestamp>/` before linking.

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

- **editor/emacs/CLAUDE.md** - Emacs coding standards, module architecture, use-package patterns, naming conventions (`omw/` prefix)

These files are lazy-loaded when working in their respective directories.

## Quick Validation

```bash
# Check setup status
./setup.sh status

# Test Emacs configuration
emacs --debug-init

# Batch test Emacs
emacs --batch --eval '(progn (load-file "editor/emacs/init.el") (message "OK"))'
```

## Key Conventions

- **macOS-focused**: All configurations assume macOS as primary OS
- **Symlink-based**: Config files are symlinked via `*.symlink` convention
- **Unified setup**: Single `setup.sh` handles all components
- **Emacs prefix**: Uses `omw/` prefix (oh-my-workspace) for custom functions/variables
