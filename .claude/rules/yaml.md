---
paths:
  - "**/*.yml"
  - "**/*.yaml"
---

# YAML Configuration Files

Standards for YAML configuration files (lazygit, GitHub Actions, etc.).

## File Header

```yaml
# filename.yml -*- mode: yaml; -*-
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
#
# =============================================================================
# Title - Brief description
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: keyword1, keyword2
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   2026-MM-DD HH:MM zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   Detailed description of what this config does.
# =============================================================================
```

**Schema reference** (optional): `# yaml-language-server: $schema=https://...`

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...` (79 chars)
**Level 1** (Primary Section): `# -----------...` (79 chars)
**Level 2** (Subsection): `# --- Title ---`

Blank line is required after every Level 1 closing line before code.

```yaml
# Level 0 (file header — shown in File Header section above)

# Level 1 (primary section)
# -----------------------------------------------------------------------------
# Section Title
# -----------------------------------------------------------------------------
# ← blank line required here

# Level 2 (subsection)
# --- Subsection Title ---
```

## Line Length

79 characters maximum.

## Code Patterns

### Comments

Explain WHY, not WHAT. Prefer separate lines; inline acceptable for brief notes only.

```yaml
# Catppuccin Mocha Blue theme for visual consistency
theme:
  activeBorderColor:
    - '#89b4fa'
```

### Value Types

- **Strings**: Quote strings containing special characters
- **Booleans**: `enabled: true` (not `"true"`)
- **Lists**: `colors: [red, green]` or multiline
- **Maps**: Nested structure or inline `{x: 1, y: 2}`

### Formatting

### Section Uniqueness

Each section title must be unique within the file at every delimiter level (Level 1 and Level 2). Group related settings together — do not create multiple sections of the same name.

```yaml
# WRONG — duplicate section at Level 2
# --- GUI ---
nerdFontsVersion: "3"
# --- Git ---
paging:
  colorArg: always
# --- GUI ---              ← same name reused
showFileTree: true

# CORRECT
# --- GUI ---
nerdFontsVersion: "3"
showFileTree: true
# --- Git ---
paging:
  colorArg: always
```

## Anti-Patterns

### Don't: Unquoted Special Characters

```yaml
# WRONG
message: error: file not found
pattern: *.txt

# CORRECT
message: "error: file not found"
pattern: "*.txt"
```

### Don't: Aligned Values

```yaml
# WRONG
nerdFontsVersion: "3"
showFileTree:     true

# CORRECT
nerdFontsVersion: "3"
showFileTree: true
```

## Security

### Secrets Management

Never hardcode sensitive data. Use split-file strategy

```yaml
# config.yml (committed)
api:
  key: ""  # Set in config.local.yml

# config.local.yml (not committed, in .gitignore)
api:
  key: "sk-1234567890"
```

Add to `.gitignore`: `*.local.yml`, `*.local.yaml`, `*_secret.yml`

## References

1. [YAML Specification](https://yaml.org/spec/)
2. [YAMLlint Documentation](https://yamllint.readthedocs.io/)

## Validation

```bash
# Python validator
python3 -c "import yaml; yaml.safe_load(open('config.yml'))"

# Yamllint (if installed)
yamllint config.yml
```
