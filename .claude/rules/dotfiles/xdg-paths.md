---
paths:
  - "**/*"
---

# XDG Base Directory Conventions

## Environment Variables

Shell configuration exports XDG environment variables:

```bash
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
```

## Standard Paths

```
$HOME/.config/     ← XDG_CONFIG_HOME (configurations)
$HOME/.cache/      ← XDG_CACHE_HOME (cache files)
$HOME/.local/share/ ← XDG_DATA_HOME (data files)
$HOME/.local/state/ ← XDG_STATE_HOME (state files)
```

## Cache File Detection

**IMPORTANT**: Before committing, always check for runtime cache files that should not be tracked.

### Files to NEVER Commit

| Tool | Files | Proper Location |
|------|-------|-----------------|
| Zsh | `.zcompdump`, `.zcompdump.*`, `cache/` | `$XDG_CACHE_HOME/zsh/` |
| Vim | `*.swp`, `*.swo`, `*.bak`, `*~`, `undo/` | `$XDG_STATE_HOME/vim/`, `$XDG_DATA_HOME/vim/` |
| Emacs | `*.elc`, `.#*`, `*~`, `#*#` | Generated, should not be in repo |
| Git | `config.local`, `credentials` | Machine-specific |

### Detection Command

Before committing, run:

```bash
# Check for cache files that shouldn't be committed
git status --porcelain | grep -E '\.(zcompdump|swp|swo|bak|elc|pyc)$|cache/|undo/'

# If any files match, either:
# 1. Add them to .gitignore if not already covered
# 2. Remove with: git rm --cached <file>
```

### Why This Matters

If XDG environment variables (`XDG_CACHE_HOME`, `XDG_STATE_HOME`) are not properly set, tools may create cache files in `.config/` directories instead of their proper locations. The `.gitignore` includes fallback patterns to catch these cases.
