---
paths:
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
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# References:
#   1. Official documentation URL
# =============================================================================
```

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...` (79 chars)
**Level 1** (Primary Section): `# -----------...` (79 chars)
**Level 2** (Subsection): `# --- Title ---`

```conf
# Level 0 (file header — shown in File Header section above)

# Level 1 (primary section)
# -----------------------------------------------------------------------------
# Section Name
# -----------------------------------------------------------------------------

# Level 2 (subsection)
# --- Subsection Title ---
```

## Line Length 

79 characters maximum.

## Code Patterns

### Comments

Explain WHY, not WHAT. Use separate comment lines.

```conf
# Use delta for syntax-highlighted diffs (falls back to less)
pager = delta
```

### Value Types

- **Booleans**: `option = true` (not `"true"`)
- **Paths**: `~/.config/app/file` (XDG-compliant, not relative)
- **Includes**: `[include] path = config.local` (machine-specific overrides)

### Section Uniqueness

Each section title must be unique within the file at every delimiter level (Level 1 and Level 2). Group related settings together — do not create multiple sections of the same name.

```conf
# WRONG — duplicate section at Level 2
# --- Paths ---
pager = delta
# --- Other Config ---
editor = vim
# --- Paths ---              ← same name reused
default = main

# CORRECT
# --- Paths ---
pager = delta
default = main
# --- Other Config ---
editor = vim
```

## Anti-Patterns

### Don't: Inline Explanations

```conf
# WRONG
pager = delta  # syntax highlighting

# CORRECT
# Use delta for syntax-highlighted diffs
pager = delta
```

### Don't: Align Values

```conf
# WRONG
option1  = value1
option10 = value10

# CORRECT
option1 = value1
option10 = value10
```

## Security

### Secrets Management

Never commit sensitive data. Use split-file strategy

```conf
# Main config (committed)
[core]
    editor = emacs
[include]
    path = ~/.config/git/config.local

# config.local (not committed, in .gitignore)
[user]
    email = your.email@company.com
```

**Sensitive types**: API keys, tokens, passwords, private keys, certificates

## References

1. [Git Configuration](https://git-scm.com/docs/git-config)
2. [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)

## Validation

```bash
# Git config
git config --list

# Ripgrep (no dedicated check — test a real search)
rg --version
```
