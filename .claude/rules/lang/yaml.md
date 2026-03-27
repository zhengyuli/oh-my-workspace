---
globs:
  - "**/*.yml"
  - "**/*.yaml"
---

# YAML Configuration Files

Standards for YAML configuration files (lazygit, GitHub Actions, etc.).

## File Header (MANDATORY)

```yaml
# filename.yml -*- mode: yaml; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# Location: $WORKSPACE_DIR/path/to/file.yml
# References:
#   1. Official documentation URL
# =============================================================================
```

**Schema reference** (optional): `# yaml-language-server: $schema=https://example.com/schema.json`

## Delimiter Hierarchy (MANDATORY)

**Level 0** (File Header): `# ===` * 77 (79 chars)
**Level 1** (Primary Section): `# ---` * 77 (79 chars)
**Level 2** (Subsection): `# --- Title ---` (inline style)

**Example:**
```yaml
# =============================================================================
# Lazygit Configuration
# =============================================================================

# -----------------------------------------------------------------------------
# GUI
# -----------------------------------------------------------------------------
gui:
  nerdFontsVersion: "3"

# -----------------------------------------------------------------------------
# Git
# -----------------------------------------------------------------------------

# --- Pagers ---
git:
  pagers:
    - colorArg: always
```

## Comments & Patterns

**Indentation**: Spaces only, 2-space indent, never tabs

**Comments**: Prefer separate lines, inline acceptable for brief notes

```yaml
# Catppuccin Mocha Blue theme
# Reference: https://github.com/catppuccin/lazygit
theme:
  activeBorderColor:
    - '#89b4fa'

# Inline comment (acceptable for brief notes)
nerdFontsVersion: "3"  # Required for latest symbols
```

**Strings**: Quote special characters
```yaml
name: "value"  # Quoted
message: "error: file not found"  # Quote if contains colon
pattern: "*.txt"  # Quote if contains asterisk
```

**Booleans**: `enabled: true` (not `"true"`)
**Lists**: `colors: [red, green]` or multiline
**Maps**: Nested structure or inline `{x: 1, y: 2}`

**Formatting Rules:**
- Never align values with spaces
- Prefer separate-line comments over inline

```yaml
# WRONG - aligned with spaces
nerdFontsVersion: "3"
showFileTree:     true

# CORRECT - no alignment
# Required for latest symbols
nerdFontsVersion: "3"
showFileTree: true
```

## Validation

```bash
python3 -c "import yaml; yaml.safe_load(open('config.yml'))"  # Python validator
yamllint config.yml  # If installed
```
