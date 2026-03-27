---
globs:
  - "**/*.toml"
---

# TOML Configuration Files

Standards for TOML configuration files (starship, uv, bun, yazi, etc.).

## File Header (MANDATORY)

All TOML files MUST include a standard header:

```toml
# filename.toml -*- mode: toml; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# Location: $WORKSPACE_DIR/path/to/file.toml
# XDG/Loaded via: How the file is loaded
# References:
#   1. Official documentation URL
#   2. Related reference URL
# =============================================================================
```

### TOML-Specific Headers

Add schema reference when available:

```toml
"$schema" = 'https://starship.rs/config-schema.json'
```

## Section Structure

TOML uses tables (sections) naturally:

```toml
# -----------------------------------------------------------------------------
# Section Category
# -----------------------------------------------------------------------------

[section_name]
option = "value"
```

### Section Guidelines

- **Delimiter**: Same 77-character delimiter as config files
- **Grouping**: Group related tables under category headers
- **Ordering**: Logical organization (core → modules → advanced)

### Example: Starship Prompt

```toml
# -----------------------------------------------------------------------------
# Cloud Services
# -----------------------------------------------------------------------------

[aws]
symbol = " "
style = "bg:#FF9900"

[gcloud]
symbol = " "
style = "bg:#4285F4"

# -----------------------------------------------------------------------------
# Build Tools & Package Managers
# -----------------------------------------------------------------------------

[bun]
symbol = " "

[cargo]
symbol = " "

[npm]
symbol = " "
```

## Comments

### Inline Comments

TOML supports inline comments:

```toml
option = true  # Explain why this setting is used
timeout = 30   # Seconds - based on network latency testing
```

### Block Comments

Use block comments for section explanations:

```toml
# Git module configuration
# Shows current branch, status, and remote info
# Disabled in large repos for performance (see threshold below)
[git_branch]
symbol = " "
style = "bg:#F14E32"
```

## Value Types

### Strings

```toml
# Good - quoted strings
name = "value"
path = "~/.config/app"

# Avoid - unquoted (may cause parsing issues)
# name = value
```

### Numbers

```toml
# Integers
timeout = 30
retry_count = 3

# Floats
version = 1.5
ratio = 0.8
```

### Booleans

```toml
# Good - explicit booleans
enabled = true
disabled = false

# Avoid - string representations
# enabled = "true"
```

### Arrays

```toml
# Inline arrays (short lists)
colors = ["red", "green", "blue"]

# Multiline arrays (long lists)
files = [
  "config.toml",
  "settings.toml",
  "preferences.toml"
]
```

### Tables

```toml
# Top-level table
[section]
option = "value"

# Nested table
[section.subsection]
option = "value"

# Inline table
point = { x = 1, y = 2 }
```

## Configuration Patterns

### Default Values

Document defaults when overriding:

```toml
# Default: 500 (reduced since minified files are filtered)
max_columns = 200

# Default: true (disabled for better performance in large repos)
disabled = false
```

### Conditional Logic

TOML doesn't support conditionals; document alternatives:

```toml
# Theme: Catppuccin Mocha
# Alternative themes: see ~/.config/app/themes/
theme = "catppuccin-mocha"
```

### Environment Variables

Document if tool supports environment variables:

```toml
# Note: Some options can be overridden via environment variables
# APP_TIMEOUT=60 app-command
timeout = 30
```

## Tool-Specific Guidelines

### Starship Prompt

- Group modules by category (languages, cloud, tools)
- Use Nerd Font symbols
- Document symbol sources

```toml
# Symbol Reference: https://www.nerdfonts.com/cheat-sheet
[python]
symbol = " "  # Nerd Font: nf-dev-python
```

### UV Package Manager

- Document version constraints
- Explain dependency sources

```toml
# UV configuration for Python package management
# Version constraints: https://peps.python.org/pep-0440/
```

### Yazi File Manager

- Group by functionality (UI, keymap, theme)
- Cross-reference related files

```toml
# See also: keymap.toml for key bindings
# See also: theme.toml for color schemes
```

## Validation

Always validate TOML syntax:

```bash
# Python validator
python3 -c "import toml; toml.load('config.toml')"

# Online validator
# https://www.toml-lint.com/

# Tool-specific validation (if available)
starship config --help
```

## Common Mistakes

### Unquoted Strings

```toml
# WRONG - may cause parsing errors
path = ~/.config/app

# CORRECT - quoted strings
path = "~/.config/app"
```

### Missing Section Headers

```toml
# WRONG - orphaned options
option = "value"

# CORRECT - in a section
[section]
option = "value"
```

### Inconsistent Formatting

```toml
# WRONG - inconsistent
[section_a]
opt1="val1"
opt2 = "val2"

# CORRECT - consistent
[section_a]
opt1 = "val1"
opt2 = "val2"
```

## Cross-References

Reference related configuration:

```toml
# Related configs:
# - ~/.config/app/config.yml (YAML version)
# - ~/.config/app/rc (no-extension version)
```

## See Also

- `lang/config.md` - Configuration files without extensions
- `lang/yaml.md` - YAML configuration files
- `coding-style.md` - Universal coding standards

## Delimiter Hierarchy (MANDATORY)

All TOML files MUST use a three-level delimiter system:

### Level 0: File Header Delimiter

**Format**: `# ` + `=` * 77 (79 total characters)
**Purpose**: Mark file-level metadata area
**Character**: `=` (equals sign - strongest visual emphasis)

```toml
# filename.toml -*- mode: toml; -*-
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

```toml
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

```toml
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
├── Languages (Level 1)
│   ├── Languages - C/C++ (Level 2)
│   ├── Languages - JVM (Level 2)
│   ├── Languages - Systems (Level 2)
│   └── Languages - Scripting (Level 2)
├── Cloud Services (Level 1)
│   ├── Cloud Services - AWS (Level 2)
│   └── Cloud Services - GCP (Level 2)
└── General Settings (Level 1)
```

### Complete Example (Starship)

```toml
# starship.toml -*- mode: toml; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# Starship Prompt Configuration
#
# Location: ~/.config/starship.toml
# References:
#   1. https://starship.rs/config/
# =============================================================================

"$schema" = 'https://starship.rs/config-schema.json'

# -----------------------------------------------------------------------------
# Cloud Services
# -----------------------------------------------------------------------------

[aws]
symbol = " "
style = "bg:#FF9900"

# -----------------------------------------------------------------------------
# Languages
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Languages - C/C++
# -----------------------------------------------------------------------------

[c]
symbol = " "

[cmake]
symbol = " "

# -----------------------------------------------------------------------------
# Languages - JVM
# -----------------------------------------------------------------------------

[java]
symbol = " "

[gradle]
symbol = " "

# -----------------------------------------------------------------------------
# Languages - Systems
# -----------------------------------------------------------------------------

[golang]
symbol = " "

[rust]
symbol = " "

# -----------------------------------------------------------------------------
# Languages - Scripting
# -----------------------------------------------------------------------------

[python]
symbol = " "

[nodejs]
symbol = " "

# -----------------------------------------------------------------------------
# General Settings
# -----------------------------------------------------------------------------

[directory]
read_only = " 󰌾"

[hostname]
ssh_symbol = "󰣇"
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

### Natural TOML Grouping

TOML tables naturally group related settings:

```toml
# -----------------------------------------------------------------------------
# Languages - Systems
# -----------------------------------------------------------------------------

[golang]
symbol = " "
style = "bg:#00ADD8"

[rust]
symbol = " "
style = "bg:#F74C00"
```

Use delimiters to group related tables, not for every individual table.

### Comment Grouping (Optional)

Within a section, use simple comments to group related settings:

```toml
# -----------------------------------------------------------------------------
# General Settings
# -----------------------------------------------------------------------------

# Directory display
[directory]
truncation_length = 3
truncate_to_repo = true

# Git status
[git_status]
conflicted = "="
ahead = "⇡"
behind = "⇣"
```
