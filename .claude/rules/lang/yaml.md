---
globs:
  - "**/*.yml"
  - "**/*.yaml"
---

# YAML Configuration Files

Standards for YAML configuration files (lazygit, GitHub Actions, etc.).

# =============================================================================
## File Header
# =============================================================================

```yaml
# filename.yml -*- mode: yaml; -*-
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# Location: $WORKSPACE_DIR/path/to/file.yml
# References:
#   1. Official documentation URL
# =============================================================================
```

**Schema reference** (optional): `# yaml-language-server: $schema=https://...`

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...` (79 chars)
**Level 1** (Primary Section): `# -----------...` (79 chars)
**Level 2** (Subsection): `# --- Title ---`

# -----------------------------------------------------------------------------
## Code Patterns
# -----------------------------------------------------------------------------

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

- 2-space indentation (never tabs)
- Never align values with spaces

# -----------------------------------------------------------------------------
## Anti-Patterns
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
## Security
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
## References
# -----------------------------------------------------------------------------

1. [YAML Specification](https://yaml.org/spec/)
2. [YAMLlint Documentation](https://yamllint.readthedocs.io/)

# -----------------------------------------------------------------------------
## Validation
# -----------------------------------------------------------------------------

```bash
# Python validator
python3 -c "import yaml; yaml.safe_load(open('config.yml'))"

# Yamllint (if installed)
yamllint config.yml
```
