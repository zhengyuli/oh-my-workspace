# CLAUDE.md - Vim Configuration

This file provides guidance for Claude Code when working with the Vim configuration in this directory.

## Project Overview

This is a **minimal, plugin-free Vim configuration** with Doom One theme for basic editing needs.

## Directory Structure

```
vim/.config/vim/
├── vimrc        # Main configuration (symlinked to ~/.config/vim/vimrc)
└── undo/        # Persistent undo history (gitignored, created at runtime)
```

## Quick Start

### Setup

```bash
# Stow the vim package
stow vim

# Test configuration loads
vim -c 'echo "OK"'
```

### Quick Validation

```bash
# Verify vimrc is linked
ls -la ~/.config/vim/vimrc

# Test configuration
vim -c 'echo "Vim loaded successfully"'
```

## Architecture

### XDG Paths

Vim is configured via the `VIMINIT` environment variable set in `zsh/.config/zsh/conf.d/00-env.zsh`:

```bash
export VIMINIT="source ${XDG_CONFIG_HOME}/vim/vimrc"
```

| Path | Purpose |
|------|---------|
| `$XDG_CONFIG_HOME/vim/vimrc` | Main configuration |
| `$XDG_DATA_HOME/vim/undo` | Persistent undo history |
| `$XDG_STATE_HOME/vim/viminfo` | Vim state file |

## Coding Standards

### Header Format

Vim configuration follows the [Root CLAUDE.md - Configuration Comment Standards](../CLAUDE.md#configuration-comment-standards).

Header example:
```vim
" vimrc
" =============================================================================
" Vim Configuration - Minimal, Plugin-Free Setup
"
" Location: ~/.config/vim/vimrc
" XDG: Loaded via VIMINIT environment variable
" =============================================================================
```

### Comment Character

Vim uses `"` for comments:
```vim
" This is a comment
set number  " This is also a comment
```

### Alignment Spaces Exception

Vim color scheme configuration MAY use alignment spaces for readability. This is an exception to the repository-wide alignment space prohibition.

**Scope:** This exception applies ONLY to:
- `vim/.config/vim/vimrc` color scheme section (lines ~286-400)
- Any `*.vim` files defining color schemes

See [Root CLAUDE.md - Alignment Spaces](../CLAUDE.md#alignment-spaces-critical) for details.

### Key Patterns

1. **No plugins**: This configuration is intentionally minimal
2. **Doom One theme**: Custom color scheme matching the repository theme
3. **XDG compliance**: All state/data stored in XDG paths
4. **Sensible defaults**: Modern editing preferences without external dependencies

## Code Quality

### Validation Commands

```bash
# Check vimrc syntax
vim -c 'source ~/.config/vim/vimrc' -c 'q'

# Verify XDG paths are used
vim -c 'echo &undodir'
```

### Compliance Checklist

- [ ] Header follows configuration comment standards
- [ ] Time-stamp present if file has mode line
- [ ] Comments use `"` prefix
- [ ] Color scheme section may use alignment spaces (exception)

## Best Practices

### 1. Keep It Minimal

This configuration intentionally avoids plugins. For advanced editing, use Emacs.

### 2. XDG Compliance

Always use XDG paths for any new settings:
```vim
" ✅ CORRECT
set undodir=$XDG_DATA_HOME/vim/undo

" ❌ AVOID
set undodir=~/.vim/undo
```

### 3. Theme Consistency

Any color changes should align with the Doom One theme used across the repository.

## Quick Reference Card

### Essential Rules

1. **No plugins**: Keep configuration minimal
2. **XDG paths**: Use `$XDG_CONFIG_HOME`, `$XDG_DATA_HOME`, `$XDG_STATE_HOME`
3. **Comment character**: Use `"` for comments
4. **Alignment spaces**: Allowed in color scheme section only
5. **Theme**: Doom One colors

### Pre-Commit Checklist

- [ ] Configuration loads: `vim -c 'q'`
- [ ] Header compliant with comment standards
- [ ] XDG paths used for all state/data
