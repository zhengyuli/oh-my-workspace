---
paths:
  - "**/*.toml"
---

# TOML Configuration Files

Standards for TOML configuration files (starship, uv, bun, yazi, etc.).

## File Header

```toml
# filename.toml -*- mode: toml; -*-
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# References:
#   1. Official documentation URL
# =============================================================================
```

**Schema reference** (optional): `"$schema" = 'https://...'`

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...` (79 chars)
**Level 1** (Primary Section): `# -----------...` (79 chars)
**Level 2** (Subsection): `# --- Title ---`

```toml
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

Explain WHY, not WHAT. Prefer separate lines over inline.

```toml
# Use Nerd Font symbols for better visual identification
symbol = " "
```

### Value Types

- **Strings**: Always quoted `"value"`
- **Numbers**: `timeout = 30` (int), `ratio = 0.8` (float)
- **Booleans**: `enabled = true` (not `"true"`)
- **Arrays**: `items = ["a", "b"]` or multiline
- **Tables**: `[section]` or inline `{ x = 1 }`

### Formatting

- Never align values with spaces

## Anti-Patterns

### Don't: Unquoted Strings

```toml
# WRONG
name = value

# CORRECT
name = "value"
```

### Don't: Inline Explanations

```toml
# WRONG
symbol = " "  # Python icon

# CORRECT
# Python language icon
symbol = " "
```

## Security

### Secrets Management

Never hardcode sensitive data. Use split-file strategy

```toml
# config.toml (committed)
[api]
# Set key in config.local.toml or environment

# config.local.toml (not committed, in .gitignore)
[api]
key = "sk-1234567890"
```

Add to `.gitignore`: `*.local.toml`, `*_secret.toml`

## References

1. [TOML Specification](https://toml.io/en/)
2. [TOML GitHub Wiki](https://github.com/toml-lang/toml/wiki)

## Validation

```bash
# Python validator
python3 -c "import toml; toml.load('config.toml')"

# Tool-specific
starship config --help
```
