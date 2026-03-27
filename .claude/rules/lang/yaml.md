---
globs:
  - "**/*.yml"
  - "**/*.yaml"
---

# YAML Configuration Files

Standards for YAML configuration files (lazygit, GitHub Actions, etc.).

## File Header (MANDATORY)

All YAML files MUST include a standard header:

```yaml
# filename.yml -*- mode: yaml; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# Location: ~/.config/app/filename.yml
# XDG/Loaded via: How the file is loaded
# References:
#   1. Official documentation URL
#   2. Related reference URL
# =============================================================================
```

### YAML-Specific Headers

Add schema reference for LSP support:

```yaml
# yaml-language-server: $schema=https://example.com/schema.json
```

## Structure and Indentation

### Indentation Rules

- **Spaces only**: Never use tabs
- **2 spaces**: Standard indentation level
- **Consistent**: Maintain throughout file

```yaml
# Good - 2-space indentation
gui:
  theme:
    activeBorderColor:
      - '#89b4fa'
      - bold

# Bad - inconsistent indentation
gui:
    theme:
      activeBorderColor:
    - '#89b4fa'
```

### Section Organization

Use comments to separate major sections:

```yaml
# -----------------------------------------------------------------------------
# GUI Configuration
# -----------------------------------------------------------------------------

gui:
  theme: catppuccin-mocha
  showFileTree: true

# -----------------------------------------------------------------------------
# Git Integration
# -----------------------------------------------------------------------------

git:
  pager: delta --dark --paging=never
```

## Comments

### Block Comments

Explain sections or complex configurations:

```yaml
# Catppuccin Mocha Blue theme
# Reference: https://github.com/catppuccin/lazygit
theme:
  activeBorderColor:
    - '#89b4fa'
    - bold
```

### Inline Comments

Explain specific choices:

```yaml
gui:
  nerdFontsVersion: "3"  # Required for latest symbols
  showFileTree: true     # Better overview in large repos
```

## Value Types

### Strings

```yaml
# Good - quoted strings (prevents parsing issues)
name: "value"
path: "~/.config/app"
regex: "\\d+"

# Acceptable - unquoted simple strings
name: simple-value

# Multiline strings
description: |
  This is a multiline
  description that spans
  multiple lines.
```

### Numbers

```yaml
# Integers
timeout: 30
retry_count: 3

# Floats
version: 1.5
ratio: 0.8
```

### Booleans

```yaml
# Good - explicit booleans
enabled: true
disabled: false

# Avoid - string representations
# enabled: "true"
```

### Lists

```yaml
# Inline lists (short)
colors: [red, green, blue]

# Multiline lists (preferred for readability)
colors:
  - red
  - green
  - blue

# List of objects
themes:
  - name: catppuccin-mocha
    colors:
      primary: '#89b4fa'
  - name: tokyo-night
    colors:
      primary: '#7aa2f7'
```

### Maps

```yaml
# Nested maps
gui:
  theme:
    activeBorderColor:
      - '#89b4fa'
      - bold

# Inline maps (short)
point: {x: 1, y: 2}
```

## Configuration Patterns

### Theme Configuration

Group color settings logically:

```yaml
# Theme: Catppuccin Mocha Blue
# Reference: https://github.com/catppuccin/lazygit
theme:
  activeBorderColor:
    - '#89b4fa'  # Blue
    - bold
  inactiveBorderColor:
    - '#a6adc8'  # Gray
  selectedLineBgColor:
    - '#313244'  # Surface0
```

### Pager Configuration

Document external tool integration:

```yaml
# Delta integration for syntax-highlighted diffs
# Requires: delta (brew install git-delta)
git:
  pagers:
    - colorArg: always
      pager: delta --dark --paging=never
```

### Conditional Behavior

YAML doesn't support conditionals; document alternatives:

```yaml
# Theme options:
# - catppuccin-mocha (dark, recommended)
# - tokyo-night (dark, alternative)
# - gruvbox (dark, high contrast)
# - nord (dark, blue-toned)
theme: catppuccin-mocha
```

## Tool-Specific Guidelines

### Lazygit

- Group by functionality (GUI, Git, Custom Commands)
- Document keybindings in comments
- Reference external themes

```yaml
# Custom commands: https://github.com/jesseduffield/lazygit/wiki/Custom-Commands
customCommands:
  - key: '<c-r>'
    command: 'git rebase -i {{.SelectedLocalCommit.Hash}}'
```

### GitHub Actions

- Document required secrets
- Explain environment variables
- Reference workflow syntax

```yaml
# Required secrets:
# - DOCKER_USERNAME
# - DOCKER_PASSWORD
env:
  REGISTRY: ghcr.io
```

## Validation

Always validate YAML syntax:

```bash
# Python validator
python3 -c "import yaml; yaml.safe_load(open('config.yml'))"

# yamllint (if installed)
yamllint config.yml

# Online validator
# https://www.yamllint.com/

# Tool-specific validation (if available)
lazygit --debug
```

## Common Mistakes

### Inconsistent Indentation

```yaml
# WRONG - mixed indentation
gui:
  theme:
    activeBorderColor:
  - '#89b4fa'

# CORRECT - consistent 2-space
gui:
  theme:
    activeBorderColor:
      - '#89b4fa'
```

### Unquoted Special Characters

```yaml
# WRONG - unquoted colon
message: error: file not found

# CORRECT - quoted
message: "error: file not found"

# WRONG - unquoted asterisk
pattern: *.txt

# CORRECT - quoted
pattern: "*.txt"
```

### Missing Newline at EOF

```yaml
# WRONG - no newline at end
option: value
# EOF marker here (no newline)

# CORRECT - newline at end
option: value
# (blank line here)
```

## Cross-References

Reference related configuration:

```yaml
# Related configs:
# - ~/.config/app/config.toml (TOML version)
# - ~/.config/app/rc (no-extension version)
```

## Anchors and Aliases (Advanced)

Use YAML anchors to avoid repetition:

```yaml
# Define anchor
default_colors: &default_colors
  primary: '#89b4fa'
  secondary: '#a6adc8'

# Use alias
theme1:
  <<: *default_colors
  accent: '#f38ba8'

theme2:
  <<: *default_colors
  accent: '#a6e3a1'
```

## See Also

- `lang/config.md` - Configuration files without extensions
- `lang/toml.md` - TOML configuration files
- `coding-style.md` - Universal coding standards

## Delimiter Hierarchy (MANDATORY)

All YAML files MUST use a three-level delimiter system:

### Level 0: File Header Delimiter

**Format**: `# ` + `=` * 77 (79 total characters)
**Purpose**: Mark file-level metadata area
**Character**: `=` (equals sign - strongest visual emphasis)

```yaml
# filename.yml -*- mode: yaml; -*-
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

```yaml
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

```yaml
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
├── GUI (Level 1)
│   ├── GUI - Theme (Level 2)
│   └── GUI - Display (Level 2)
├── Git (Level 1)
│   ├── Git - Pagers (Level 2)
│   └── Git - Merging (Level 2)
└── Keybindings (Level 1)
```

### Complete Example (Lazygit)

```yaml
# config.yml -*- mode: yaml; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# Lazygit Configuration
#
# Location: ~/.config/lazygit/config.yml
# References:
#   1. https://github.com/jesseduffield/lazygit/blob/master/docs/Config.md
# =============================================================================

# yaml-language-server: $schema=https://raw.githubusercontent.com/jesseduffield/lazygit/master/schema/config.json

# -----------------------------------------------------------------------------
# GUI
# -----------------------------------------------------------------------------

gui:
  nerdFontsVersion: "3"
  showFileTree: true
  showRandomTip: false

# -----------------------------------------------------------------------------
# GUI - Theme
# -----------------------------------------------------------------------------
# Catppuccin Mocha Blue theme
# Reference: https://github.com/catppuccin/lazygit

  theme:
    activeBorderColor:
      - '#89b4fa'
      - bold
    inactiveBorderColor:
      - '#a6adc8'
    selectedLineBgColor:
      - '#313244'

# -----------------------------------------------------------------------------
# Git
# -----------------------------------------------------------------------------

git:
  pagers:
    - colorArg: always
      pager: delta --dark --paging=never

# -----------------------------------------------------------------------------
# OS
# -----------------------------------------------------------------------------

os:
  editPreset: 'nvim'

# -----------------------------------------------------------------------------
# Keybindings
# -----------------------------------------------------------------------------

keybinding:
  universal:
    quit: 'q'
    return: '<esc>'
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

### Natural YAML Grouping

YAML's nested structure naturally groups related settings:

```yaml
# -----------------------------------------------------------------------------
# GUI - Theme
# -----------------------------------------------------------------------------

gui:
  theme:
    activeBorderColor:
      - '#89b4fa'
      - bold
    inactiveBorderColor:
      - '#a6adc8'
```

Use delimiters to mark major sections, not every nested level.

### Comment Grouping (Optional)

Within a section, use simple comments to group related settings:

```yaml
# -----------------------------------------------------------------------------
# GUI
# -----------------------------------------------------------------------------

gui:
  # Display settings
  nerdFontsVersion: "3"
  showFileTree: true
  
  # Theme settings (Catppuccin Mocha)
  theme:
    activeBorderColor:
      - '#89b4fa'
```

### Multi-Level Nesting

For deeply nested configurations, use Level 2 delimiters sparingly:

```yaml
# -----------------------------------------------------------------------------
# Git
# -----------------------------------------------------------------------------

git:
  # Pager configuration
  pagers:
    - colorArg: always
      pager: delta --dark --paging=never

# -----------------------------------------------------------------------------
# Git - Advanced
# -----------------------------------------------------------------------------

git:
  # Merging behavior
  merging:
    manualCommit: false
```
