---
paths:
  - "tool/**/*.yml"
  - "tool/**/*.yaml"
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
#
# References:
#   1. ...
# =============================================================================
```

**Schema reference** (optional): `# yaml-language-server: $schema=https://...`

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...` (79 chars)
**Level 1** (Primary Section): `# -----------...` (79 chars)
**Level 2** (Subsection): `# --- Title ---`

**Title Case required**: capitalize the first letter of every word in both
Section Title and Subsection Title (e.g., `Git Status`, `Doom Modeline`).
Abbreviations follow their established convention: ALL CAPS for standard
abbreviations (e.g., `FZF Preview`, `PDF Tools`, `JSON Mode`), lowercase
for established lowercase names (e.g., `cc Mode`, `sh Mode`, `xref`).

### Blank Lines

Spacing rules around delimiters and YAML structural elements:

- **Around Level 1 delimiters**: one blank line before the opening
  delimiter and one blank line after the closing delimiter.
- **Around Level 2 delimiters**: no blank line after — code follows
  immediately on the next line.
- **Between top-level keys**: exactly one blank line.
- **Within a nested mapping** (between key-value pairs): no blank lines.
  Use a comment for logical grouping instead.
- **Between list items** that contain nested mappings: no blank lines
  unless separated by a comment.
- **After a block scalar** (`|` or `>`): one blank line before the next
  key.

**Prohibited**: two or more consecutive blank lines anywhere in the file.

```yaml
gui:
  nerdFontsVersion: "3"
  showFileTree: true
  # File tree sorting preferences
  showRandomMark: false

git:
  paging:
    colorArg: always
    pager: delta
```

### YAML Top-Level Keys vs. Delimiter Hierarchy

YAML top-level keys (`gui:`, `git:`, `keybinding:`) are **native structure**
defined by the tool — they must not be removed or replaced by comment
delimiters.

The delimiter hierarchy is an **additional organizational layer** in comments
above YAML keys. It groups related top-level keys under a shared heading.

Rules:

- One Level 1 block may contain multiple top-level keys that share a logical
  theme (e.g., all UI-related keys under "Interface").
- Use Level 2 (`# --- Title ---`) as a visual label directly above a
  top-level key when it benefits readability.
- For simple single-key sections, Level 2 headers are optional — the YAML
  key itself is descriptive enough within a Level 1 group.

```yaml
# -----------------------------------------------------------------------------
# Interface
# -----------------------------------------------------------------------------

# --- Appearance ---
gui:
  nerdFontsVersion: "3"
  showFileTree: true
  showRandomMark: false

# --- Navigation ---
keybinding:
  universal:
    quit: "q"
    return: "<esc>"

# -----------------------------------------------------------------------------
# Git
# -----------------------------------------------------------------------------

git:
  paging:
    colorArg: always
    pager: delta
```

## Indentation

Use **2-space indentation** consistently (YAML community standard). Never mix
2 and 4 spaces within a file.

- Indent nested mappings under their parent key
- Indent list items at the same level as the `-` marker, with content
  indented 2 spaces further

```yaml
# Nested mapping
git:
  paging:
    colorArg: always
    pager: delta

# List with nested content
customCommands:
  - key: "<c-p>"
    command: "git push"
    description: "Push to remote"
```

## Line Length

79 characters maximum.

Exceptions:

- URLs and file paths that cannot be wrapped
- Long command strings in `run:` fields (GitHub Actions, lazygit custom
  commands)
- Multiline block scalar content (the `|` / `>` block body is exempt)

## Comments

Comments explain *why*, not *what*. Use separate comment lines above
the setting — never end-of-line.

**Comment syntax**: `#` followed by a single space.

**When to comment**:
- Non-obvious values, theme color codes, tool-specific quirks
- Why a specific configuration was chosen
- Dependencies between settings

**When NOT to comment**:
- Self-documenting key names (`quit: "q"` needs no comment)
- Default values matching the tool's documentation

**No end-of-line comments** — place comments on a separate line above
the value (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```yaml
# WRONG — end-of-line annotation
theme: catppuccin  # color scheme

# CORRECT — separate line explains reasoning
# Catppuccin Mocha Blue theme for visual consistency
theme: catppuccin
```

## YAML Syntax Conventions

### String Quoting

- **Plain scalar** (default): no quotes when the value contains no special
  characters and is unambiguous.
- **Single quotes**: use when the value contains YAML special characters
  (`: # [ ] { } , & * ? | - < > = ! % @`).
- **Double quotes**: use only when escape sequences are needed (`\n`, `\t`,
  `\uXXXX`). Double quotes require escaping backslashes.

```yaml
# Plain scalar — safe values, no quotes needed
name: lazygit
version: 1.2.3

# Single quotes — value contains special characters
pattern: '*.txt'
message: 'error: file not found'
range: '[a-z]+'

# Double quotes — value needs escape sequences
output: "line1\nline2"
unicode: "\u2603 snowman"
```

### Multiline Strings

- **Literal block `|`**: preserves newlines exactly as written. Use for
  scripts, shell commands, and prose where line breaks matter.
- **Folded block `>`**: collapses newlines to spaces (produces a single
  line). Use for long single-paragraph text.
- **Chomp indicators**: default (clip — single final newline), `-` (strip —
  no trailing newline), `+` (keep — all trailing newlines).

```yaml
# Literal block — use for commands and scripts
# Default chomp (clip): keeps one trailing newline
command: |
  git log --oneline --graph --decorate --all

# Strip chomp: no trailing newline (most common for commands)
run: |-
  echo "Starting deployment"
  ./deploy.sh --production

# Folded block — use for long prose (newlines become spaces)
description: >
  This is a long description that will be folded into
  a single line when parsed. Useful for paragraphs.

# Keep chomp: preserve all trailing blank lines
template: |+
  First line

  Second line


```

### Null Values

Use explicit `null` keyword for null values. Never use `~` or empty value —
they are ambiguous and hard to grep.

```yaml
# WRONG — ambiguous null representations
timeout: ~
proxy:

# CORRECT — explicit and searchable
timeout: null
proxy: null
```

### Anchors and Aliases

Avoid YAML anchors (`&`) and aliases (`*`) in dotfiles configs for
readability. Anchors reduce duplication but harm grep-ability and confuse
editors.

Exception: acceptable in CI/CD templates with heavy repetition (e.g.,
repeated matrix configurations), but add a comment explaining the anchor.

```yaml
# AVOID in dotfiles — hard to search and understand
default: &defaults
  timeout: 30
  retries: 3

production:
  <<: *defaults
  timeout: 60

# PREFER — explicit and grep-friendly
production:
  timeout: 60
  retries: 3
```

## Code Patterns

### Value Types

- **Strings**: see "String Quoting" section above
- **Numbers**: `timeout: 30` (int), `ratio: 0.8` (float)
- **Booleans**: `enabled: true` (not `"true"`) — see anti-pattern below
- **Lists**: multiline preferred for readability; inline `["a", "b"]` for
  short, fixed lists of 3 items or fewer
- **Maps**: nested structure; inline `{x: 1, y: 2}` for compact 2-key maps
  only
- **Null**: `timeout: null` (see "Null Values" section above)

### Formatting

- One space after `:` in key-value pairs
- Never align values with spaces (see Anti-Patterns)
- Trailing spaces forbidden — use yamllint to enforce

### Section Uniqueness

Each section title must be unique within the file at every delimiter level
(Level 1 and Level 2). Group related settings together — do not create
multiple sections of the same name.

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

## Per-Tool Notes

### Lazygit

- Single config file: `~/.config/lazygit/config.yml`
- Top-level keys (`gui:`, `git:`, `keybinding:`) map to UI sections — each
  gets its own Level 1 block or is grouped logically.
- Custom commands use multiline `|` blocks for shell commands.
- `keybinding` sections are deeply nested (`keybinding.universal.quit`) —
  use the full nesting structure, not flat keys.
- **No config merge**: lazygit does not auto-merge `config.local.yml`. Use
  environment variables for machine-specific overrides.
- Schema: `# yaml-language-server: $schema=https://json.schemastore.org/lazygit`

### GitHub Actions

- Workflow files live at `.github/workflows/*.yml` (or `.yaml`)
- Top-level keys (`on:`, `jobs:`, `steps:`) map to workflow structure
- Use block scalars (`|-`) for multi-line shell commands in `run:` fields
- Always quote branch/tag names in `on:` triggers to avoid YAML boolean
  parsing (e.g., `on: [push]` is fine, but branch `NO` would be parsed
  as `false` without quoting)
- Schema: `# yaml-language-server: $schema=https://json.schemastore.org/github-workflow`

## Anti-Patterns

### Don't: Unquoted Special Characters

```yaml
# WRONG — `error:` parsed as mapping, `*.txt` parsed as alias/tag
message: error: file not found
pattern: *.txt

# CORRECT
message: 'error: file not found'
pattern: '*.txt'
```

### Don't: Boolean Implicit Conversion (YAML 1.1 Norway Problem)

YAML 1.1 parsers treat `yes`, `no`, `on`, `off` as boolean `true`/`false`.
This is the famous "Norway problem" — country code `NO` becomes `false`.

```yaml
# WRONG — parsed as boolean false
country: NO
enabled: yes
disabled: off

# CORRECT — always use explicit true/false for booleans
country: 'NO'
enabled: true
disabled: false
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

### Don't: End-of-Line Comments

Avoid end-of-line `# comment` annotations after a value.  Move the
explanation to a separate `#` comment line above.

```yaml
# WRONG — end-of-line annotation
theme: catppuccin  # color scheme

# CORRECT — separate line explains reasoning
# Catppuccin Mocha Blue theme for visual consistency
theme: catppuccin
```

## Security

### Secrets Management

Never hardcode sensitive data. Most YAML tools do **not** support automatic
config file merging, so the strategy depends on tool support.

**Prefer environment variables** for secrets — this is the universal approach
that works regardless of tool.

```yaml
# Lazygit — no native merge, use environment variables
# (lazygit does not auto-merge config.local.yml)
os:
  editPreset: "nvim-remote"
```

**Split-file strategy** only when the tool explicitly documents config merge
behavior:

```yaml
# Only for tools that document config merging
# Main config (committed)
api:
  endpoint: "https://api.example.com"

# config.local.yml (not committed, in .gitignore)
# api:
#   key: "sk-1234567890"
```

Add to `.gitignore`: `*.local.yml`, `*.local.yaml`, `*_secret.yml`

**Sensitive types**: API keys, tokens, passwords, private keys, certificates.

**Per-tool local override patterns**:
- lazygit: **not supported** — use environment variables instead

## References

1. [YAML Specification](https://yaml.org/spec/)
2. [YAMLlint Documentation](https://yamllint.readthedocs.io/)
3. [Lazygit Configuration](https://github.com/jesseduffield/lazygit/blob/master/docs/Config.md)

## Validation

```bash
# Syntax check — catches structural errors (requires: pip install pyyaml)
python3 -c "import yaml; yaml.safe_load(open('config.yml'))"

# Style lint — enforces indentation, line length, quoting rules
# (requires: pip install yamllint)
yamllint config.yml

# Tool-specific — verify lazygit can parse the config
lazygit --print-config
```
