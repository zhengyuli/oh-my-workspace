---
name: pre-commit-check
description: Run pre-commit verification checks for workspace repository
argument-hint: "[--fix] attempt to fix issues automatically"
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

# Pre-Commit Verification Workflow

Usage: `/pre-commit-check`

## Steps
1. **Check for cache files**
   - Scan for `.zcompdump`, `*.swp`, `*.swo`, `*.bak`, `*.elc`, `*.pyc`
   - Check for `cache/` and `undo/` directories
   - Report any files that should not be committed

2. **Verify stow symlinks are valid**
   - Check `~/.zshenv` symlink
   - Check `~/.config/zsh/.zshrc` symlink
   - Check `~/.config/git/config` symlink
   - Report any broken or missing symlinks

3. **Test shell/emacs loads**
   - Zsh: `zsh -c 'source ~/.zshenv && echo "OK"'`
   - Emacs: `emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") (message "OK"))'`

4. **Check config headers are compliant**
   - Verify standardized header format
   - Check Time-stamp presence
   - Verify section separators are 79 chars

5. **Verify no alignment spaces**
   - Scan for padding spaces in assignment operators
   - Check for aligned array elements
   - Report any violations

6. **Confirm all comments in English**
   - Scan for non-English characters in comments
   - Report any potential issues

## Cache Files to Check

| Tool | Files | Proper Location |
|------|-------|-----------------|
| Zsh | `.zcompdump`, `.zcompdump.*`, `cache/` | `$XDG_CACHE_HOME/zsh/` |
| Vim | `*.swp`, `*.swo`, `*.bak`, `*~`, `undo/` | `$XDG_STATE_HOME/vim/` |
| Emacs | `*.elc`, `.#*`, `*~` | Generated, should not be in repo |
| Git | `config.local`, `credentials` | Machine-specific |

## Detection Commands
```bash
# Check for cache files
git status --porcelain | grep -E '\.(zcompdump|swp|swo|bak|elc|pyc)$|cache/|undo/'

# Verify symlinks
ls -la ~/.zshenv ~/.config/zsh/.zshrc ~/.config/git/config

# Test shell loads
zsh -c 'source ~/.zshenv && echo "OK"'

# Check header format compliance
grep -L "^# ===" */.config/*/* 2>/dev/null
```

## Compliance Checklist

- [ ] No cache files in staging area
- [ ] Stow symlinks valid
- [ ] Shell loads without errors
- [ ] Emacs loads without errors (if modified)
- [ ] Config headers compliant
- [ ] No alignment spaces
- [ ] All comments in English

## Exit Codes
- 0: All checks passed
- 1: Cache files detected
- 2: Broken symlinks
- 3: Configuration load errors
- 4: Header format violations
