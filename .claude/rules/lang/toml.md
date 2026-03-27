---
globs:
  - "**/*.toml"
---

# TOML Configuration Files

Standards for TOML configuration files (starship, uv, bun, yazi, etc.).

## File Header (MANDATORY)

```toml
# filename.toml -*- mode: toml; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# Location: $WORKSPACE_DIR/path/to/file.toml
# References:
#   1. Official documentation URL
# =============================================================================
```

**Schema reference** (optional): `"$schema" = 'https://starship.rs/config-schema.json'`

## Delimiter Hierarchy (MANDATORY)

**Level 0** (File Header): `# ===` * 77 (79 chars)
**Level 1** (Primary Section): `# ---` * 77 (79 chars)
**Level 2** (Subsection): `# --- Title ---` (inline style)

**Example:**
```toml
# =============================================================================
# Starship Configuration
# =============================================================================

# -----------------------------------------------------------------------------
# Languages
# -----------------------------------------------------------------------------
[python]
symbol = " "

# -----------------------------------------------------------------------------
# Cloud Services
# -----------------------------------------------------------------------------

# --- AWS ---
[aws]
symbol = " "

# --- GCP ---
[gcloud]
symbol = " "
```

## Comments & Patterns

**Comments**: TOML supports inline comments, but prefer separate lines for clarity.

```toml
# Use Nerd Font symbols for better visual identification
symbol = " "

# Inline comment (acceptable for brief notes)
style = "bg:#00ADD8"  # Go blue
```

**Strings**: Always quote strings, never unquoted
```toml
name = "value"  # Quoted
path = "~/.config/app"  # Quoted
# Avoid: name = value  # Unquoted (parsing errors)
```

**Booleans**: `enabled = true` (explicit, not strings)
**Arrays**: `colors = ["red", "green"]` (short inline, long multiline)
**Tables**: `[section]` for top-level, `[section.sub]` for nested

**Formatting Rules:**
- Never align values with spaces
- Prefer separate-line comments over inline

```toml
# WRONG - aligned with spaces
symbol  = " "
style   = "bg:#00ADD8"

# WRONG - inline comment for explanation
symbol = " "  # Python language icon

# CORRECT - no alignment, separate comment
# Python language icon
symbol = " "
style = "bg:#00ADD8"
```

## Value Types

**Strings**: Always quoted `"value"`
**Numbers**: `timeout = 30` (int), `ratio = 0.8` (float)
**Booleans**: `enabled = true` (not `"true"`)
**Arrays**: `items = ["a", "b"]` or multiline
**Tables**: `[section]` or inline `{ x = 1, y = 2 }`

## Security

### Secrets

Never hardcode API keys, tokens, or credentials in TOML files:

```toml
# Bad — hardcoded secret committed to git
api_key = "sk-1234567890"
```

Use a split-file strategy: commit a `config.toml` with placeholders or
non-sensitive defaults; keep secrets in a `config.local.toml` that is
listed in `.gitignore`:

```toml
# config.toml (committed)
[api]
# Set key in config.local.toml or environment

# config.local.toml (not committed)
[api]
key = "sk-1234567890"
```

Add to `.gitignore`:

```
*.local.toml
*_secret.toml
```

## Validation

```bash
# Python validator
python3 -c "import toml; toml.load('config.toml')"

# Tool-specific
starship config --help
```
