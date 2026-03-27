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

All configuration files MUST include a standard header:

```
# filename -*- mode: xxx; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# Location: Full path or XDG path
# XDG/Loaded via: How the file is loaded
# References:
#   1. Official documentation URL
#   2. Related reference URL
# Note: Important usage notes
# =============================================================================
```

### Header Components

**Mode Line**: `-*- mode: xxx; -*-`
- Specifies editor major mode
- Examples: `conf`, `gitconfig`, `sh`

**Timestamp**: `Time-stamp: <...>`
- Format: `<YYYY-MM-DD HH:MM:SS Day by username>`
- Auto-updated by Emacs `time-stamp` package

**Title**: First line after separator
- Brief, descriptive title
- May include purpose clarification

**Location**: Where file resides
- Use `$WORKSPACE_DIR` for repo-relative paths
- Use `~/.config/...` for XDG-compliant paths

**XDG/Loaded via**: How configuration is loaded
- Environment variable (e.g., `RIPGREP_CONFIG_PATH`)
- XDG default path
- Include directive

**References**: External documentation
- Numbered list (1., 2., 3.)
- Include official docs first
- Add helpful references second

**Note**: Important clarifications
- Tools that bypass this config
- Non-obvious behavior
- Dependencies or requirements

## Section Structure

Organize configuration into logical sections with clear delimiters:

```
# -----------------------------------------------------------------------------
# Section Title
# -----------------------------------------------------------------------------
```

### Section Guidelines

- **Delimiter**: 77 characters (80 - 3 for `# `)
- **Format**: `# ` + `-` * 77 + newline + `# Section Title` + newline + `# ` + `-` * 77
- **Grouping**: Related settings in same section
- **Order**: Logical flow (core → advanced → optional)

### Common Sections

Organize by category, not alphabetically:

1. **Core Settings** - Fundamental behavior
2. **UI/Display** - Visual output configuration
3. **Integration** - External tool integration
4. **Performance** - Optimization settings
5. **Advanced** - Expert-level configuration

## Comments

### Principles

- **Explain WHY, not WHAT**: Code shows what; comments explain why
- **Reference external docs**: Link to official documentation
- **Note non-obvious behavior**: Clarify surprising choices
- **Keep updated**: Comments must match code

### Good Examples

```conf
# Use delta for syntax-highlighted diffs
# Falls back to less if delta is not installed
pager = delta

# Warn when line endings change (prevents silent corruption)
safecrlf = warn

# Display non-ASCII filenames correctly (e.g., Chinese characters)
quotepath = false
```

### Bad Examples

```conf
# Set pager to delta
pager = delta

# Enable safecrlf
safecrlf = warn

# Disable quotepath
quotepath = false
```

## Configuration Patterns

### Boolean Settings

Prefer explicit `true`/`false` over implicit:

```conf
# Good - explicit
option = true
feature = false

# Avoid - implicit (tool-specific)
# option
# no-feature
```

### Path Handling

Use absolute paths or XDG variables:

```conf
# Good - XDG-compliant
excludesfile = ~/.config/git/ignore

# Good - absolute path
pager = /usr/local/bin/delta

# Avoid - relative paths (context-dependent)
# excludesfile = .gitignore.global
```

### Environment Variables

Document if environment variables are expanded:

```conf
# IMPORTANT: Environment variables are NOT expanded in this file.
#            All paths must be absolute. XDG expansion is done in .zshenv.
```

### Include Directives

Use includes for machine-specific overrides:

```conf
# Include local config for machine-specific settings (identity, proxy, etc.)
# Uses XDG-compliant path: ~/.config/git/config.local
[include]
    path = ~/.config/git/config.local
```

## Format-Specific Guidelines

### Git Config

- Use tabs for indentation under sections
- Group related settings
- Use includes for local overrides

### Ripgrep Config

- One flag per line
- Group by category (ignore patterns, output, performance)
- Use `--glob=!` for ignore patterns

### Ghostty Config

- Key-value pairs with space separator
- Group by functionality
- Theme settings together

## Validation

Always validate configuration syntax:

```bash
# Git config
git config --global --list

# Ripgrep (check if flags are valid)
rg --debug 2>&1 | grep -i config

# General syntax check (tool-specific)
<tool> --help
```

## Cross-References

Reference related configuration files:

```conf
# Shell integration: See ~/.config/zsh/conf.d/00-env.zsh
# Editor integration: See ~/.config/emacs/lisp/tool/omw-rg.el
```

## See Also

- `lang/toml.md` - TOML configuration files
- `lang/yaml.md` - YAML configuration files
- `coding-style.md` - Universal coding standards
- `patterns.md` - Design patterns and anti-patterns

## Delimiter Hierarchy (MANDATORY)

All configuration files MUST use a three-level delimiter system:

### Level 0: File Header Delimiter

**Format**: `# ` + `=` * 77 (79 total characters)
**Purpose**: Mark file-level metadata area
**Character**: `=` (equals sign - strongest visual emphasis)

```
# filename -*- mode: xxx; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# File Title
#
# Location: ...
# References:
#   1. ...
# =============================================================================
```

**Position**: 
- Immediately after mode line and timestamp
- Surrounds file header metadata

### Level 1: Primary Section Delimiter

**Format**: `# ` + `-` * 77 (79 total characters)
**Purpose**: Mark major functional categories
**Character**: `-` (hyphen - medium visual emphasis)

```
# -----------------------------------------------------------------------------
# Primary Category
# -----------------------------------------------------------------------------
```

**Characteristics**:
- Appears in pairs (top and bottom boundary)
- Single-level category (no hyphen in title)

### Level 2: Subsection Delimiter

**Format**: `# ` + `-` * 77 (79 total characters) + " - " in title
**Purpose**: Mark subcategories within primary categories
**Character**: `-` (hyphen - same as Level 1)
**Distinction**: Title uses " - " (space-hyphen-space) to indicate hierarchy

```
# -----------------------------------------------------------------------------
# Primary Category - Subcategory
# -----------------------------------------------------------------------------
```

**Characteristics**:
- Appears in pairs (top and bottom boundary)
- Title contains " - " to show parent-child relationship
- Uses same delimiter character as Level 1, distinguished by title content

### Hierarchy Example

```
File Header (Level 0)
├── Core Settings (Level 1)
│   ├── Core Settings - Editor (Level 2)
│   └── Core Settings - Performance (Level 2)
├── User Interaction (Level 1)
│   ├── User Interaction - Identity (Level 2)
│   └── User Interaction - Signing (Level 2)
└── Advanced (Level 1)
```

### Complete Example

```conf
# rc -*- mode: conf; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# Ripgrep Configuration
#
# Location: ~/.config/ripgrep/rc
# Loaded via: RIPGREP_CONFIG_PATH environment variable
# =============================================================================

# -----------------------------------------------------------------------------
# Search Behavior
# -----------------------------------------------------------------------------
--smart-case
--hidden

# -----------------------------------------------------------------------------
# Ignore Patterns
# -----------------------------------------------------------------------------
# Define patterns to exclude from search

# -----------------------------------------------------------------------------
# Ignore Patterns - VCS
# -----------------------------------------------------------------------------
--glob=!.git/*
--glob=!.svn/*

# -----------------------------------------------------------------------------
# Ignore Patterns - Build Artifacts
# -----------------------------------------------------------------------------
--glob=!target/*
--glob=!dist/*
--glob=!build/*

# -----------------------------------------------------------------------------
# Output Formatting
# -----------------------------------------------------------------------------
--line-number
--column
--color=always
```

### Delimiter Specifications

**Width Calculation**:
- Total width: 79 characters (80-char line - 1 newline)
- Format: `# ` (2 chars) + delimiter character * 77

**Character Selection**:
- **Level 0**: `=` (equals) - strongest emphasis for file header
- **Level 1 & 2**: `-` (hyphen) - medium emphasis for sections

**Title Format**:
- **Level 1**: `Primary Category` (single phrase)
- **Level 2**: `Primary Category - Subcategory` (connected with " - ")

### Comment Grouping (Optional)

Within a section, use simple blank lines and comments for further grouping:

```conf
# -----------------------------------------------------------------------------
# Core Settings
# -----------------------------------------------------------------------------

# Editor configuration
editor = vim
pager = delta

# Performance settings
compression = 9
preloadindex = true
```

This avoids excessive delimiter usage while maintaining clarity.
