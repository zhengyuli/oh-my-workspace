---
paths:
  - "shell/starship/**/*.toml"
  - "tool/yazi/**/*.toml"
  - "lang/**/*.toml"
---

# TOML Configuration Files

Standards for TOML configuration files (starship, uv, bun, yazi, etc.).

## File Header

```toml
# filename.toml -*- mode: toml; -*-
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

**Schema reference** (optional): `"$schema" = 'https://...'` — place on the
first line after the header, before any content.

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...` (79 chars)
**Level 1** (Primary Section): `# -----------...` (79 chars)
**Level 2** (Subsection): `# --- Title ---`

**Title Case required**: capitalize the first letter of every word in both
Section Title and Subsection Title (e.g., `Git Status`, `Doom Modeline`).
Abbreviations follow their established convention: ALL CAPS for standard
abbreviations (e.g., `FZF Preview`, `PDF Tools`, `JSON Mode`), lowercase
for established lowercase names (e.g., `cc Mode`, `sh Mode`, `xref`).

### TOML Table Headers vs. Delimiter Hierarchy

TOML `[table]` headers are **syntax-level structure** defined by the format —
they must not be removed or replaced by comment delimiters.

The delimiter hierarchy is an **additional organizational layer** in comments
above TOML tables. It groups multiple related tables under a shared heading.

Rules:

- One Level 1 block may contain multiple TOML tables that share a logical
  theme (e.g., all language module tables under "Language Modules").
- Use Level 2 (`# --- Title ---`) as a visual label directly above an
  individual `[table]` when it benefits readability.
- For single-key tables (e.g., starship's `[python]` with only `symbol`),
  Level 2 headers are optional — the table header itself is descriptive
  enough within a Level 1 group.

```toml
# -----------------------------------------------------------------------------
# Language Modules
# -----------------------------------------------------------------------------

# --- Python ---
[python]
symbol = " "

# --- Rust ---
[rust]
symbol = " "

# --- Go ---
[golang]
symbol = " "
format = "via [$symbol]($style)"
```

For files where each TOML table is a top-level concern (yazi's `[mgr]`,
`[preview]`, `[tasks]`), each table gets its own Level 1 block:

```toml
# -----------------------------------------------------------------------------
# Manager
# -----------------------------------------------------------------------------

[mgr]
ratio = [1, 4, 3]
sort_by = "natural"

# -----------------------------------------------------------------------------
# Preview
# -----------------------------------------------------------------------------

[preview]
wrap = "no"
tab_size = 2
```

## TOML Syntax Conventions

### Table Styles

- **Standard table** `[section]`: always preferred for any table with 3+ keys
  or when the table may grow.
- **Inline table** `{ key = "val" }`: acceptable for simple, fixed-structure
  entries with **2 keys or fewer**.
- **Array of tables** `[[section]]`: required for list entries (each `[[ ]]`
  adds a new element). Never rewrite as a single table.

```toml
# Standard table — the default
[preview]
wrap = "no"
tab_size = 2

# Inline table — compact, 2 keys or fewer, unlikely to grow
point = { x = 1, y = 2 }

# Array of tables — each [[ ]] is a separate list element
[[plugin.prepend_fetchers]]
id = "git"
url = "*"
run = "git"

[[plugin.prepend_fetchers]]
id = "git"
url = "*/"
run = "git"
```

### Dotted Keys

Prefer `[section]` + `key = "val"` over `section.key = "val"` for any table
with multiple keys — it scales better and is easier to reorder.

```toml
# PREFER — standard table for multi-key sections
[install.cache]
dir = "~/.cache/bun/install/cache"

# ACCEPTABLE — dotted key for a single top-level override
preview.tab_size = 4
```

### Multiline Strings

- **Basic multiline** `"""..."""`: supports escape sequences (`\n`, `\t`).
  Use for values that need escape processing.
- **Literal multiline** `'''...'''`: no escape processing. Use for regex
  patterns, format strings with backslashes, or any value where escapes
  would be misleading.

```toml
# Format string with $ syntax — literal to preserve $ literally
format = '''[$symbol$branch]($style)'''

# Regex pattern — literal to avoid double-escaping
pattern = '''^\d+\.\d+\.\d+$'''

# Human-readable text with intentional newlines — basic multiline
description = """
First line of description.
Second line continues here.
"""
```

## Blank Line Rules

Consistent spacing between TOML structural elements:

- **Between top-level tables**: exactly one blank line.
- **Between `[[array_of_tables]]` entries**: exactly one blank line.
- **Within a table** (between key-value pairs): no blank lines. Use a
  comment for logical grouping within a table instead.
- **Between logical groups** inside a table: one blank line preceded by
  a comment separator.

```toml
[mgr]
ratio = [1, 4, 3]
sort_by = "natural"
sort_reverse = false
# Directory-specific sorting
sort_dir_first = true
show_hidden = false
```

## Line Length

79 characters maximum.

Exceptions:

- Format strings and template values that cannot be meaningfully wrapped
  (e.g., starship `format = "..."` with long `$variable` chains)
- URLs and file paths that cannot be wrapped

## Code Patterns

### Comments

Explain WHY, not WHAT. Use separate comment lines — never inline.

```toml
# Use Nerd Font symbols for better visual identification
symbol = " "
```

### Value Types

- **Strings**: Always quoted `"value"` (single quotes `'value'` for literal
  strings where escapes must not be interpreted)
- **Numbers**: `timeout = 30` (int), `ratio = 0.8` (float)
- **Booleans**: `enabled = true` (not `"true"`)
- **Arrays**: single-line for short lists `items = ["a", "b"]`, multiline
  for longer ones
- **Tables**: see "Table Styles" section above

### Formatting

- Never align values with spaces
- One space around `=` in key-value pairs
- Trailing commas are NOT allowed in TOML — omit them

### Section Uniqueness

Each section title must be unique within the file at every delimiter level
(Level 1 and Level 2). Group related settings together — do not create
multiple sections of the same name.

```toml
# WRONG — duplicate section at Level 2
# --- Style ---
symbol = " "
# --- Other Config ---
disabled = false
# --- Style ---              ← same name reused
style = "bold"

# CORRECT
# --- Style ---
symbol = " "
style = "bold"
# --- Other Config ---
disabled = false
```

## Per-Tool Notes

### Starship

- `format` values are template strings using `$variable` syntax. These
  routinely exceed 79 characters and are exempt from line length limits.
- `[os.symbols]` is a single large table — it gets its own Level 1 block
  without per-entry Level 2 headers.
- `[custom.*]` tables define custom modules — group them under a shared
  Level 1 block.
- Schema: `"$schema" = 'https://starship.rs/config-schema.json'`

### Yazi

- Config is split across multiple files: `yazi.toml`, `keymap.toml`,
  `theme.toml`, `flavor.toml`. Each file is independent.
- `[[plugin.prepend_*]]` and `[[plugin.prepend_fetchers]]` use array-of-tables
  syntax — each entry is a separate `[[ ]]` block.
- Tables like `[mgr]`, `[preview]`, `[tasks]` map to yazi's internal modules
  — each gets its own Level 1 block.

### UV

- `[[index]]` uses array-of-tables syntax for package index entries.
- `uv.toml` is the global config; project-specific settings go in
  `pyproject.toml` under `[tool.uv]`.
- Flat key-value pairs at top level are common — no table header needed.

### Bun

- `.bunfig.toml` uses nested tables like `[install.cache]`.
- Relatively simple config — typically one or two sections.

## Anti-Patterns

### Don't: Unquoted Strings

```toml
# WRONG
name = value

# CORRECT
name = "value"
```

### Don't: End-of-Line Comments

Avoid end-of-line `# comment` annotations after a value.  Move the
explanation to a separate `#` comment line above.

```toml
# WRONG — end-of-line annotation
symbol = " "  # Python icon

# CORRECT — separate line explains reasoning
# Python language icon
symbol = " "
```

### Don't: Trailing Commas

TOML does not support trailing commas.

```toml
# WRONG
items = ["a", "b",]

# CORRECT
items = ["a", "b"]
```

## Security

### Secrets Management

Never hardcode sensitive data. TOML has no native include/merge mechanism
(unlike git config's `[include]`), so the strategy depends on tool support:

**Prefer environment variables** for secrets — most tools (uv, bun) support
env var interpolation or fallback. This is the universal approach.

```toml
# uv — environment variable (supported natively)
publish-url = "https://upload.pypi.org/legacy/"
# Token set via UV_PUBLISH_TOKEN environment variable
```

**Split-file strategy** only when the tool explicitly supports config merge:

```toml
# Only for tools that document config merging behavior
# Main config (committed)
[api]
endpoint = "https://api.example.com"

# config.local.toml (not committed, in .gitignore)
# [api]
# key = "sk-1234567890"
```

Add to `.gitignore`: `*.local.toml`, `*_secret.toml`

**Sensitive types**: API keys, tokens, passwords, private keys, certificates.

## References

1. [TOML Specification](https://toml.io/en/)
2. [Starship Config](https://starship.rs/config/)
3. [Yazi Configuration](https://yazi-rs.github.io/docs/configuration/yazi/)
4. [UV Configuration](https://docs.astral.sh/uv/concepts/configuration-files/)
5. [Bun Config](https://bun.sh/docs/runtime/bunfig)

## Validation

```bash
# Python 3.11+ (built-in tomllib)
python3 -c "import tomllib; tomllib.load(open('config.toml', 'rb'))"

# Python < 3.11 (requires: pip install tomli)
python3 -c "import tomli; tomli.load(open('config.toml', 'rb'))"

# Starship — validate config and print resolved values
starship config

# UV — check config is loaded
uv config list
```
