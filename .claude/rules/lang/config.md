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

## File Header

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

## Delimiter Hierarchy

**Level 0** (File Header):
```
# =============================================================================
```

**Level 1** (Primary Section):
```
# -----------------------------------------------------------------------------
```

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

## Documentation & Code Patterns

### Comments

Explain rationale (WHY), not mechanics (WHAT). Document non-obvious design decisions and constraints. Use separate comment lines for clarity, never inline explanations.

```conf
# Use delta for syntax-highlighted diffs (falls back to less)
pager = delta

# Warn when line endings change (prevents silent corruption)
safecrlf = warn
```

### Booleans

`option = true` (explicit, not `"true"`)

### Paths

`~/.config/app/file` (XDG-compliant, not relative)

### Includes

`[include] path = config.local` (machine-specific overrides)

### Formatting

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

## Security

### Secrets Management

Never commit sensitive data to version control.

**Sensitive Data Types**: API keys, tokens, passwords, private keys, certificates, database credentials

**Split-file Strategy**:
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

**Validation Command**: `git log -p | grep -E "(password|token|key)" | head -20`

## Validation

Verify the config is accepted by its consuming tool before committing:

```bash
# Git config
git config --list

# Ripgrep (no dedicated check — test a real search)
rg --version
```

For other tools, reload or restart the application after editing to confirm the config is valid.
