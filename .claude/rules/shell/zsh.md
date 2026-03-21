---
paths:
  - "**/*.zsh"
  - "**/conf.d/*"
  - "**/.zshenv"
---

# Zsh Configuration Standards

> This file extends [shell.md](./shell.md) with zsh-specific standards.

## File Header Template

All configuration files start with a standardized header:

```zsh
# <filename>
# Time-stamp: <YYYY-MM-DD HH:MM:SS Day by Author>
# =============================================================================
# <One-line description>
#
# Loaded by: <shell types that load this file>
# Load order: <number> (after <prev_file>, before <next_file>)
#
# Prerequisites: (optional)
#   - <requirement 1>
#   - <requirement 2>
#
# Responsibilities:
#   1. <first responsibility>
#   2. <second responsibility>
#
# Do NOT add: <things that belong elsewhere>
#             → Put these in <correct file> (<reason>)
#
# Note: (optional)
#   <any additional context>
# =============================================================================
```

## Time-stamp Format

| Field | Format | Example |
|-------|--------|---------|
| Date | ISO 8601 (YYYY-MM-DD) | `2026-03-17` |
| Time | 24-hour format (HH:MM:SS) | `15:30:00` |
| Day | Full day name (Monday-Sunday) | `Monday` |
| Author | Git username or email prefix | `zhengyu.li` |

**Example:** `# Time-stamp: <2026-03-17 15:30:00 Monday by zhengyu.li>`

## Header Field Specifications

| Field | Required | Format | Description |
|-------|----------|--------|-------------|
| `<filename>` | Yes | Filename only (no path) | e.g., `00-env.zsh` |
| Time-stamp | Yes | `<YYYY-MM-DD HH:MM:SS Day by Author>` | Last modification timestamp |
| Description | Yes | Single line | Brief summary of file purpose |
| Loaded by | Yes | Shell type(s) | `.zshrc`, `.zprofile`, or "ALL shells" |
| Load order | Yes | `<num> (after X, before Y)` | Numeric prefix with context |
| Prerequisites | Optional | Dash list | Dependencies on other files/variables |
| Responsibilities | Yes | Numbered list | What this file manages |
| Do NOT add | Recommended | Arrow reference format | What belongs elsewhere |
| Note | Optional | Free text | Additional context |

## Section Comment Styles

### Major Sections (79 chars width)

Use full-width separator for top-level logical sections:

```zsh
# -----------------------------------------------------------------------------
# <Section Name>
# -----------------------------------------------------------------------------
# <Brief explanation of what this section does and why>
# <Any prerequisites or cross-references>
<code>
```

**Rules (CRITICAL):**
1. **No sandwiching**: description always goes AFTER the closing separator, never
   between opening and closing separators
2. **No double separators**: exactly ONE opening + ONE closing `---` separator per
   section; metadata (Prerequisites, Config, Usage) goes after the single closing
   separator, not wrapped in an extra `---` pair
3. **Description is optional**: omit if the section name is self-explanatory

### Subcategories (inline format)

Use short separator for subcategories within a major section:

```zsh
# --- <Category Name> ---
# <Brief explanation>
<code>
```

**Rules (CRITICAL):**
1. **No blank line** between the `# --- ---` header and the following
   description/code
2. **Always at column 0**: never indent `# --- ---` headers, even when inside
   `if/else` or other blocks

### Inline Comments

For single-line explanations, no separator needed:

```zsh
# <description>
<code>
```

## Comment Width Standards

| Element | Width | Example |
|---------|-------|---------|
| Header separator (`# ===...`) | 79 chars | `# =============================================================================` |
| Section separator (`# ---...`) | 79 chars | `# -----------------------------------------------------------------------------` |
| Subcategory separator | Variable | `# --- Recording ---` |

## Comment Formatting Rules

1. **Field names use colon suffix**: `Loaded by:`, `Load order:`, `Prerequisites:`
2. **Prerequisites always plural**: Use `Prerequisites:` (not `Prerequisite:`)
3. **Do NOT add uses arrow format**: `→ Put these in <file> (<reason>)`
4. **List items are indented**: 2 spaces for numbered, 2 spaces + dash for prerequisites

## Key Conventions

1. **Idempotency**: All conf.d files must be safe to source multiple times
2. **Numeric Prefixes**: Files load in lexicographic order (00-99)
3. **Single Concern**: Each file handles one logical area
4. **Prerequisites**: Document dependencies on other files
5. **Cross-References**: Mention related files in comments

## Idempotent Operations

```zsh
# Safe PATH addition - typeset -U ensures uniqueness
typeset -U path fpath manpath

# Safe array addition
path=(
  "$HOME/.local/bin"
  $path  # preserve existing
)

# Safe hook addition with -Q check
if (( ! ${+hooks[directory-history-chdir]} )); then
    autoload -Uz chpwd && add-zsh-hook chpwd omw/chpwd-handler
fi

# Safe alias definition (aliases are idempotent)
alias ll='ls -la'

# Safe option setting (setopt is idempotent)
setopt AUTO_CD
```
