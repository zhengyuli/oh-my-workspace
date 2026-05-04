---
paths:
  - "**/git/config"
  - "**/ripgrep/rc"
  - "**/ghostty/config"
---

# Configuration Files (No Extension)

Standards for `#`-commented, key-value configuration files without extensions.

**Scope**: git config, ripgrep rc, ghostty config, and similar tools that use
`#` for comments and `key = value` (or `--flag`) syntax. TOML, YAML, and INI
formats with their own comment syntax (`;`, `"`) are covered by their
respective rules.

**Out of scope**: `ssh_config` (has its own syntax quirks), `.editorconfig`
(key-value but different semantics), binary configs.

## XDG Path Mapping

This repository is XDG-compliant. Each tool's config location:

| Tool    | Path                            | XDG Native | Notes                     |
|---------|---------------------------------|------------|---------------------------|
| git     | `$XDG_CONFIG_HOME/git/config`   | Yes        | No setup needed           |
| ripgrep | `$XDG_CONFIG_HOME/ripgrep/rc`   | Via env    | Set `RIPGREP_CONFIG_PATH` |
| ghostty | `$XDG_CONFIG_HOME/ghostty/config` | Yes      | No setup needed           |

Tools that require environment variable redirection: set in `shell/zsh/00-env.zsh`.

## File Header

```
# filename -*- mode: <mode>; -*-
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

**Mode mappings**: `gitconfig` (git/config), `conf` (ripgrep/rc, ghostty/config).

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

Blank lines mark boundaries between delimiter levels and settings.

**Around delimiters** — one blank line before Level 1 opening, one after
Level 1 closing.  Level 2 has no blank line after the delimiter — settings
follow immediately.

```conf
# -----------------------------------------------------------------------------
# Core Settings
# -----------------------------------------------------------------------------

# --- Identity ---
[core]
    pager = delta
    autocrlf = input

# --- Diff ---
[diff]
    tool = vimdiff
```

**Between settings within the same subsection** — one blank line between
unrelated settings.  Related settings (e.g., key-value pairs within an INI
section) are not separated.

```conf
# --- Display ---
font-family = "SauceCodePro Nerd Font"
font-size = 15

shell-integration-features = cursor,sudo,title
```

**Prohibited**: two or more consecutive blank lines anywhere in the file.

### INI Section Headers vs. Delimiter Hierarchy

Git config uses INI-style `[section]` headers (e.g., `[core]`, `[alias]`).
These are **syntax-level structure** defined by the tool — they must not be
removed or replaced by comment delimiters.

The delimiter hierarchy is an **additional organizational layer** in comments
above the INI headers. Together they look like:

```conf
# -----------------------------------------------------------------------------
# Core Settings
# -----------------------------------------------------------------------------

[core]
    pager = delta
    autocrlf = input
```

Rules:
- One INI `[section]` per Level 1 block. Group related INI sections under
  a shared Level 1 header when they belong together logically.
- Never place a Level 1 delimiter *inside* an INI section — close the
  section first, then add the next delimiter block.
- For tools without INI syntax (ripgrep, ghostty), the delimiter hierarchy
  is the sole organizational structure.

## Indentation

- **Within INI sections** (git config): indent key-value pairs 4 spaces.
- **Flat key-value files** (ripgrep, ghostty): no indentation.

```conf
# Git config — indented within [section]
[core]
    pager = delta
    autocrlf = input

# Ghostty/ripgrep — flat, no indentation
font-family = "SauceCodePro Nerd Font"
--smart-case
```

## Line Length

79 characters maximum.

Exceptions:

- URLs and file paths that cannot be wrapped
- Tool-specific values that cannot be meaningfully split

## Comments

Comments explain *why*, not *what*. Use separate comment lines above
the setting — never end-of-line.

**Comment syntax**: `#` followed by a single space (all three tools use `#`).

**When to comment**:
- Non-obvious setting values, workarounds, tool quirks
- Why a specific value was chosen over the default
- Dependencies between settings

**When NOT to comment**:
- Self-documenting key names (`pager = delta` needs no "set pager")
- Default values that match the tool's documented defaults

**No end-of-line comments** — place comments on a separate line above
the setting (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```conf
# WRONG — restates the setting
pager = delta  # set pager to delta

# CORRECT — explains reasoning
# Use delta for syntax-highlighted diffs (falls back to less)
pager = delta
```

## Code Patterns

### Value Types

- **Booleans**: `option = true` (not `"true"`)
- **Strings**: Quoted when containing spaces — `font-family = "SauceCode Pro"`
- **Numbers**: `font-size = 15` (bare integer)
- **Paths**: Use `~/.config/app/file` (XDG-compliant) or `$XDG_CONFIG_HOME`
  expansion only when the tool supports it (see per-tool notes below).
  **Not all tools expand `~`** — ripgrep does not expand environment
  variables.
- **Lists**: Multi-valued keys use repeated entries (git), comma-separated
  values (ghostty), or repeated flags (ripgrep).

```conf
# Git — multi-value via repeated key
[diff]
    xfuncname = "^func pattern1"
    xfuncname = "^func pattern2"

# Ghostty — comma-separated
shell-integration-features = cursor,sudo,title

# Ripgrep — repeated flag
--glob=!target/*
--glob=!node_modules/*
```

- **Includes**: Use for machine-specific overrides (see Security section).

### Per-Tool Syntax

| Aspect            | git config          | ripgrep rc        | ghostty config     |
|-------------------|---------------------|-------------------|--------------------|
| Syntax            | INI `[section]`     | `--flag`          | `key = value`      |
| Values            | `key = value`       | `--flag=value`    | `key = value`      |
| Indent            | 4 spaces in section | None              | None               |
| Env var expansion | Yes (`$HOME`, etc.) | **No**            | Limited            |
| `~` expansion     | Yes                 | **No**            | Yes                |
| Multi-value       | Repeated key        | Repeated flag     | Comma-separated    |
| Local override    | `[include] path`    | N/A               | `config-file = ?`  |

**Ripgrep caveat**: `RIPGREP_CONFIG_PATH` must be set as an absolute path
(via `00-env.zsh`) because ripgrep does not expand `$HOME` or `~` in its
config file.

### Section Uniqueness

Each section title must be unique within the file at every delimiter level
(Level 1 and Level 2). Group related settings together — do not create
multiple sections of the same name.

```conf
# WRONG — duplicate section at Level 2
# --- Paths ---
pager = delta
# --- Other Config ---
editor = vim
# --- Paths ---              ← same name reused
default = main

# CORRECT
# --- Paths ---
pager = delta
default = main
# --- Other Config ---
editor = vim
```

## Anti-Patterns

### Don't: End-of-Line Comments

Avoid end-of-line `# comment` annotations after a setting.  Move the
explanation to a separate `#` comment line above.

```conf
# WRONG — end-of-line annotation
pager = delta  # syntax highlighting

# CORRECT — separate line explains reasoning
# Use delta for syntax-highlighted diffs
pager = delta
```

### Don't: Align Values

```conf
# WRONG
option1  = value1
option10 = value10

# CORRECT
option1 = value1
option10 = value10
```

## Security

### Secrets Management

Never commit sensitive data.

**Prefer environment variables** for secrets — this is the universal approach
that works regardless of tool.

For git specifically, use the split-file strategy with `[include]`:

```conf
# Main config (committed)
[core]
    editor = emacs
[include]
    path = ~/.config/git/config.local

# config.local (not committed, in .gitignore)
[user]
    email = your.email@company.com
```

**Sensitive types**: API keys, tokens, passwords, private keys, certificates.

**Per-tool local override patterns**:
- git: `[include] path = ~/.config/git/config.local`
- ghostty: `config-file = ?config.local` (the `?` prefix suppresses errors
  if the file is absent)

## References

1. [Git Configuration](https://git-scm.com/docs/git-config)
2. [Ripgrep Configuration Guide](https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md)
3. [Ghostty Configuration](https://ghostty.org/docs/config)
4. [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)

## Validation

```bash
# Git — list all config with source file
git config --list --show-origin

# Ripgrep — verify config is loaded (shows config path if set)
rg --debug 2>&1 | head -5

# Ghostty — validate config and list errors
ghostty +validate-config
```
