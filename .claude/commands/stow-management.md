---
name: stow-management
description: Manage stow packages for symlink operations
argument-hint: "<action> [packages...] - actions: install|remove|refresh|status|validate"
allowed-tools:
  - Read
  - Write
  - Grep
  - Glob
  - Bash
  - TodoWrite
  - AskUserQuestion
invocable: true
---

# Stow Package Management Workflow

Usage: `/stow-management <action> [packages...]`

## Actions

| Action | Description |
|--------|-------------|
| `install` | Stow packages (create symlinks) |
| `remove` | Unstow packages (remove symlinks) |
| `refresh` | Restow packages (refresh symlinks) |
| `status` | Check which packages are stowed |
| `validate` | Verify symlinks are correctly created |

## Steps

1. **Check prerequisites**
   - Verify stow is installed: `which stow`
   - Verify package directory exists in `$STOW_DIR`

2. **Verify package directory exists**
   - Check for `bun/`, `emacs/`, `ghostty/`, `git/`, `ripgrep/`, `starship/`, `uv/`, `vim/`, `zsh/`

3. **Execute stow operation**
   - `install`: `stow -d "$STOW_DIR" -t "$HOME" "$pkg"`
   - `remove`: `stow -D -d "$STOW_DIR" -t "$HOME" "$pkg"`
   - `refresh`: `stow -R -d "$STOW_DIR" -t "$HOME" "$pkg"`

4. **Validate symlinks created**
   - Check symlinks point to correct locations
   - Verify no broken symlinks

## Stow Packages

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

## Validation Commands
```bash
# Verify symlinks
ls -la ~/.zshenv ~/.config/zsh/.zshrc ~/.config/git/config

# Check for broken symlinks
find -L ~/.config -type l -xtype l 2>/dev/null | head -20
```

## Common Operations
```bash
# Install all packages
stow bun emacs ghostty git ripgrep starship uv vim zsh

# Install specific packages
stow zsh git

# Remove package
stow -D zsh

# Refresh package (useful after changes)
stow -R zsh

# Dry-run to preview changes
stow -n zsh
```

## Exit Codes
- 0: Operation successful
- 1: Prerequisite not met (stow not installed)
- 2: Package directory not found
- 3: Stow operation failed
