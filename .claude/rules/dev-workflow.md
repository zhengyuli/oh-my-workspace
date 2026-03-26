---
version: "1.0.0"
last-updated: "2026-03-26"
maintainer: "zhengyu.li"
---
# Development Workflow

Research-first approach for configuration changes in oh-my-workspace.

## Research First (Mandatory)

Before making any configuration change, always research:

1. **Check existing implementations** - Search oh-my-workspace for similar configs
2. **Review external references** - Check dotfiles repos, package documentation
3. **Understand dependencies** - What does this config depend on?
4. **Document findings** - Brief notes on what exists and potential conflicts

## Planning Phase

### Document the Change

Answer before starting:
- **What:** What configuration is being changed?
- **Why:** What problem does this solve?
- **Impact:** What files affected? What dependencies exist?

### Create Implementation Plan

1. Files to create/modify
2. Order of operations
3. Backup strategy
4. Rollback plan
5. Testing approach

### Use TodoWrite for Complex Changes

For multi-file updates or breaking changes, use TodoWrite to:
- Track progress across files
- Reveal out-of-order steps
- Identify missing dependencies
- Ensure correct granularity

## Implementation Phase

### Backup Before Modifying

Follow immutability principle - create backups:

```bash
cp ~/.zshrc ~/.zshrc.bak
cp init.el init.el.bak
```

### Make Changes Incrementally

1. One logical change at a time
2. Test after each change
3. Commit frequently
4. Document as you go

### Follow Immutability

- Create new versions instead of modifying in-place
- Use symlinks to switch between versions
- Keep old configs until new ones verified

```bash
cp config.v1 config.v2
# Edit config.v2, test it
ln -sf config.v2 config
```

### Test After Each Change

- New terminal for shell changes
- Restart Emacs for Elisp changes
- Verify symlinks for stow changes

## Verification Phase

### Syntax Validation

```bash
bash -n script.sh      # Shell
zsh -n script.zsh      # Zsh
emacs --batch -f batch-byte-compile init.el  # Emacs Lisp
python -m py_compile script.py  # Python
```

### Functional Testing

1. Shell changes: Open new terminal, test commands
2. Emacs changes: **clean stale `.elc` first** (see `lang/elisp.md` for procedure)
3. Symlink changes: Verify symlinks point correctly
4. Stow changes: Check stow status and target files

### Documentation Check

- [ ] README updated if structure changed
- [ ] Comments explain non-obvious choices
- [ ] File headers present and accurate
- [ ] Dependencies documented

## Rollback Strategy

### Git Rollback

```bash
git log --oneline -10  # View recent changes
git revert <hash>      # Revert specific commit
```

### Backup Restoration

```bash
cp ~/.zshrc.bak ~/.zshrc
git checkout HEAD~1 -- path/to/file
```

### Symlink Switch

```bash
ln -sf config.v1 config  # Switch back to previous version
```

## GNU Stow Operations

### Key Flags

| Flag | Meaning |
|------|---------|
| `-n` / `--no` | Dry-run — simulate without making changes |
| `-v` / `--verbose` | Show each symlink created/removed |
| `-R` / `--restow` | Unstow then re-stow (useful after moving files) |
| `-D` / `--delete` | Remove symlinks for a package |
| `-t DIR` | Override target directory (default: parent of stow dir) |

### Workflow: New Package

```bash
# 1. Create directory tree mirroring target location
mkdir -p shell/zsh/.config/zsh

# 2. Add files with standard headers
# 3. Dry-run to check for conflicts
stow -n -v -t "$HOME" shell/zsh

# 4. Install
stow -v -t "$HOME" shell/zsh

# 5. Verify symlinks
ls -la "$HOME/.config/zsh"

# 6. Update PKG_ALL in setup.sh
```

### Workflow: Remove Package

```bash
# Dry-run first
stow -n -D -v -t "$HOME" shell/zsh

# Remove symlinks
stow -D -v -t "$HOME" shell/zsh
```

### Workflow: Restow After Moving Files

```bash
# After restructuring a package directory
stow -R -v -t "$HOME" shell/zsh
```

### Resolving Conflicts

Stow refuses to create a symlink if the target already exists as a
real file (not a symlink). Resolve before stowing:

```bash
# Inspect conflict
ls -la "$HOME/.zshrc"

# Back up and remove the blocking file
cp "$HOME/.zshrc" "$HOME/.zshrc.bak"
rm "$HOME/.zshrc"

# Now stow succeeds
stow -v -t "$HOME" shell/zsh
```

## Config-Specific Workflows

### Adding New Package

1. Create package directory: `mkdir -p category/package`
2. Add configuration files with headers
3. Test: `stow -n -v -t "$HOME" category/package` (dry-run)
4. Install: `stow -v -t "$HOME" category/package`
5. Verify symlinks in target directory
6. Update PKG_ALL array in setup.sh

### Modifying Existing Config

1. Research: check current implementation
2. Backup: create copy or ensure git clean
3. Plan: document changes
4. Implement: make incremental changes
5. Verify: test syntax and functionality
6. Document: update comments
7. Commit: follow Conventional Commits

### Merging Upstream Changes

1. Fetch: `git fetch upstream`
2. Review: `git log upstream/master..HEAD`
3. Test merge: `git merge --no-commit upstream/master`
4. Resolve conflicts carefully
5. Test affected configurations
6. Commit merge with clear message

## See Also

- `hooks.md` — Claude Code hooks integration for automated validation
- `git-workflow.md` — Commit message conventions
- `coding-style.md` — Code formatting standards
- `lang/elisp.md` — Emacs Lisp byte-compile and `.elc` cleanup
