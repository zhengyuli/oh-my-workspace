# CLAUDE.md - Vim Configuration

This file provides guidance to Claude Code when working with the Vim configuration.

## Project Overview

**Minimal, plugin-free Vim configuration** with Doom One theme for basic editing. For advanced editing, use Emacs.

## Directory Structure

```
vim/.config/vim/
├── vimrc        # Main configuration (symlinked to ~/.config/vim/vimrc)
└── undo/        # Persistent undo history (gitignored, created at runtime)
```

## Setup

```bash
stow vim
vim -c 'echo &undodir'   # Verify XDG undo path
```

## Architecture

### XDG Paths

Vim is loaded via `VIMINIT` set in `zsh/.config/zsh/conf.d/00-env.zsh`:

```bash
export VIMINIT="source ${XDG_CONFIG_HOME}/vim/vimrc"
```

| Path | Purpose |
|------|---------|
| `$XDG_CONFIG_HOME/vim/vimrc` | Main configuration |
| `$XDG_DATA_HOME/vim/undo` | Persistent undo history |
| `$XDG_STATE_HOME/vim/viminfo` | Vim state file |

## Key Patterns

1. **No plugins**: intentionally minimal
2. **Doom One theme**: custom color scheme
3. **XDG compliance**: all state/data in XDG paths

## Coding Standards

See `.claude/rules/common/coding-style.md` for: header format (use `"` for comments), section separators, timestamps. The color scheme section of `vimrc` may use alignment spaces for `hi` commands (exception documented in coding-style.md).

## Validation

```bash
vim -c 'source ~/.config/vim/vimrc' -c 'q'   # Syntax check
vim -c 'echo &undodir'                         # Verify XDG undo path
ls -la ~/.config/vim/vimrc                     # Verify symlink
```
