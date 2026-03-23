# Common Patterns

## XDG Base Directory

All tools follow XDG Base Directory specification:

| Variable | Default | Purpose |
|----------|---------|---------|
| `XDG_CONFIG_HOME` | `~/.config` | Configurations |
| `XDG_CACHE_HOME` | `~/.cache` | Cache files |
| `XDG_DATA_HOME` | `~/.local/share` | Data files |
| `XDG_STATE_HOME` | `~/.local/state` | State files |

### Cache File Detection

Before committing, check for runtime cache files:

```bash
git status --porcelain | grep -E '\.(zcompdump|swp|swo|bak|elc|pyc)$|cache/|undo/'
```

### Files to Never Commit

| Tool | Files | Proper Location |
|------|-------|-----------------|
| Zsh | `.zcompdump`, `.zcompdump.*` | `$XDG_CACHE_HOME/zsh/` |
| Vim | `*.swp`, `*.swo`, `*.bak`, `*~` | `$XDG_STATE_HOME/vim/` |
| Emacs | `*.elc`, `.#*`, `*~` | Generated, not in repo |

## Configuration File Header

All configuration files use a standardized header:

```
# <filename>[ -*- mode: <mode>; -*-][ Time-stamp: <YYYY-MM-DD HH:MM:SS Day by Author>]
# =============================================================================
# <One-line description>
#
# [Location: <path>]
# [References:]
#   [1. <doc name>: <url>]
# =============================================================================
```

## Section Separators

| Type | Format | Width | Usage |
|------|--------|-------|-------|
| Header | `# ===...===` | 79 chars | File header open/close ONLY |
| Major section | `# ---...---` | 79 chars | Top-level content sections |
| Subsection | `# --- Name ---` | Variable | Nested sections within major |

**Rules:**
1. No sandwiching: description goes AFTER the closing separator
2. No double separators: exactly ONE opening + ONE closing `---` per section
3. No blank line after closing separator

## Idempotent Operations

All configurations must be safe to reload multiple times:
- Use deduplication where available
- Conditional registration for hooks
- Preserve existing state when appending
