---
globs:
  - "**/config"
  - "**/*.conf"
  - "**/*.cfg"
  - "**/rc"
  - "**/.gitconfig"
  - "**/git/config"
---

# Configuration Files (No Extension)

Standards for configuration files without extensions (config, conf, rc).

## File Header (MANDATORY)

```
# filename -*- mode: xxx; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# Location: $WORKSPACE_DIR/path/to/file
# References:
#   1. Official documentation URL
# Note: Important usage notes, non-obvious behavior
# =============================================================================
```

## Delimiter Hierarchy (MANDATORY)

**Level 0** (File Header): `# ===` * 77 (79 chars)
**Level 1** (Primary Section): `# ---` * 77 (79 chars)
**Level 2** (Subsection): `# --- Title ---` (inline style)

**Example:**
```conf
# =============================================================================
# Ripgrep Configuration
# =============================================================================

# -----------------------------------------------------------------------------
# Search Behavior
# -----------------------------------------------------------------------------
--smart-case

# -----------------------------------------------------------------------------
# Ignore Patterns
# -----------------------------------------------------------------------------

# --- VCS ---
--glob=!.git/*

# --- Build ---
--glob=!target/*
```

## Comments & Patterns

**Comments**: Explain WHY, not WHAT. Use separate lines, never inline.

```conf
# Use delta for syntax-highlighted diffs (falls back to less)
pager = delta

# Warn when line endings change (prevents silent corruption)
safecrlf = warn
```

**Boolean**: `option = true` (explicit, not implicit)
**Paths**: `~/.config/app/file` (XDG-compliant, not relative)
**Includes**: `[include] path = config.local` (machine-specific overrides)

**Formatting Rules:**
- Never align values with spaces
- Never use inline comments

```conf
# WRONG - aligned with spaces
option1  = value1
option10 = value10

# WRONG - inline comment
pager = delta  # syntax highlighting

# CORRECT - no alignment, separate comment
# Use delta for syntax-highlighted diffs
pager = delta

option1 = value1
option10 = value10
```

## Secrets & Sensitive Values

**Never commit**: API keys, tokens, passwords, private keys, certificates, database credentials

**Split strategy**:
```conf
# Main config (committed)
[core]
    editor = emacs
[include]
    path = ~/.config/git/config.local

# config.local (not committed, in .gitignore)
[user]
    email = your.email@company.com
    signingkey = ABC123DEF456
```

**Validate**: `git log -p | grep -E "(password|token|key)" | head -20`
