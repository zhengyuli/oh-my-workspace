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
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# Location: $WORKSPACE_DIR/path/to/file
# References:
#   1. Official documentation URL
# =============================================================================
```

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...`
**Level 1** (Primary Section): `# -----------...`
**Level 2** (Subsection): `# --- Title ---`

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

### Formatting

- Never align values with spaces
- Never use inline comments for explanations

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
