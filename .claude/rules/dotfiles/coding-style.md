---
paths:
  - "**/*.sh"
  - "**/config"
  - "**/*.toml"
  - "**/*.rc"
  - "**/Brewfile"
  - "**/.bunfig.toml"
---

# Configuration Comment Standards

> This file extends [~/.claude/rules/common/coding-style.md](file://...) with dotfiles-specific configuration file standards.

These standards apply to **all non-emacs/non-zsh configuration files** in the repository (bun, ghostty, git, homebrew, macos, ripgrep, uv, vim). For zsh and see [zsh.md](./zsh.md). For emacs, see [emacs.md](./emacs.md).

## Header Structure

All configuration files must start with a standardized header:

```
# <filename>[ -*- mode: <mode>; -*-][ Time-stamp: <...>]
# =============================================================================
# <One-line description>
#
# [Location: <path>]
# [XDG: <XDG compliance info>]
# [Loaded via: <loading mechanism>]
# [References:]
#   [1. <doc name>: <url>]
#   [2. <doc name>: <url>]
# [Note: <important caveats>]
# =============================================================================
```

## Header Fields

| Field | Required | Format | Purpose |
|-------|----------|--------|---------|
| `<filename>` | Yes | Filename only | e.g., `config`, `uv.toml`, `.bunfig.toml` |
| Description | Yes | Single line | Brief summary of file purpose |
| Location | Recommended | `~/.config/...` | Absolute path where file is symlinked |
| XDG | If applicable | Compliance notes | XDG support status and cache/data paths |
| Loaded via | If applicable | Loading method | How the tool discovers this config |
| References | Recommended | Numbered list | Official documentation links |
| Note | Optional | Free text | Important caveats or warnings |

## Time-stamp Convention

**Required for ALL configuration files.**

**Format:** `<YYYY-MM-DD HH:MM:SS Day by Author>`
- Date: ISO 8601 (YYYY-MM-DD)
- Time: 24-hour format (HH:MM:SS)
- Day: Full day name (Monday-Sunday)
- Author: Git username or email prefix

**Example:**
```
# config -*- mode: gitconfig; -*-
# Time-stamp: <2026-03-17 00:00:00 Monday by zhengyu.li>
# =============================================================================
```

## Section Separators

| Type | Format | Width | Usage |
|------|--------|-------|-------|
| Header | `# ===...===` | 79 chars | File header open/close ONLY |
| Major section | `# ---...---` | 79 chars | Top-level content sections |
| Subsection | `# --- Name ---` | Variable | Nested sections within major |

**Rules:**

1. **`===` is reserved for file headers.** All content sections use `---`.

2. **No sandwiching.** Description text always goes AFTER the closing separator,
   never between the opening and closing separator.

   **WRONG - description sandwiched between separators:**
   ```
   # -----------------------------------------------------------------------------
   # Section Name
   # Description of the section     <- sandwiched
   # -----------------------------------------------------------------------------
   <code>
   ```

   **CORRECT - description after closing separator:**
   ```
   # -----------------------------------------------------------------------------
   # Section Name
   # -----------------------------------------------------------------------------
   # Description of the section
   <code>
   ```

3. **No double separators.** Each section uses exactly one opening + one closing
   `---` separator. Never add an extra `---` line to wrap metadata.

   **WRONG - extra closing separator:**
   ```
   # -----------------------------------------------------------------------------
   # Tool Name
   # -----------------------------------------------------------------------------
   # Prerequisites: ...
   # -----------------------------------------------------------------------------   <- extra
   <code>
   ```

   **CORRECT - single open + single close:**
   ```
   # -----------------------------------------------------------------------------
   # Tool Name
   # -----------------------------------------------------------------------------
   # Prerequisites: ...
   <code>
   ```

4. **No blank line after closing separator.** The first line of content
   (description comment or code) follows the closing `---` immediately.

5. **Subsection headers always at column 0.** `# --- Name ---` is never indented,
   even when it appears inside an `if/else` block.

6. **No blank line after `# --- Name ---`.** Content follows immediately.

## Comment Characters by Format

| Format | Character | Example |
|--------|-----------|---------|
| Shell, Gitconfig, TOML, Brewfile, Ghostty, Ripgrep | `#` | `# comment` |
| Vim | `"` | `" comment` |

## Subsection Style

Use ASCII dashes for subsections (not Unicode em-dashes):

```bash
# --- Section Name ---
# Brief explanation
<configuration>
```

**Rationale:** ASCII ensures cross-editor portability and consistent rendering.

## Inline Comments (CRITICAL)

**Rule: Do NOT put comments on the same line as code.** All comments must be on the line above the code they describe.

**WRONG:**
```bash
setopt AUTO_CD # type a directory name to cd
bindkey -e    # emacs keymap
```

**CORRECT:**
```bash
# Type a directory name to cd
setopt AUTO_CD

# Emacs keymap
bindkey -e
```

**Rationale:**
1. Improves readability with clear separation
2. No alignment maintenance burden
3. Easier to scan code without comment noise
4. Prevents accidental removal of code-comment spacing

## Alignment Spaces (CRITICAL)

**Rule: Do NOT use extra spaces for visual alignment in code.**

Use exactly one space between elements. Never add padding spaces to align items vertically in code.

**AVOID - Alignment spaces in code:**
```bash
# Assignment operators with extra padding
name    = "value"
counter += 1

# Shell arrays with aligned comments
path=(
  "$HOME/.local/bin"      # User binaries
  /opt/homebrew/bin       # Homebrew
  $path                   # System
)

# Variable assignments with aligned equals
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
```

**CORRECT - Single space:**
```bash
# Assignment operators with single space
name = "value"
counter += 1

# Shell arrays without alignment
path=(
  "$HOME/.local/bin"
  /opt/homebrew/bin
  $path
)

# Variable assignments without aligned equals
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
```

**Rationale:**
1. Alignment spaces create unnecessary diff noise when items change
2. Single space is more consistent and predictable
3. Easier to maintain without auto-formatting tools
4. Cascading changes: one modification requires re-aligning all siblings

### Exceptions (alignment spaces MAY be used)

| Context | Scope | Example | Reason |
|---------|-------|---------|--------|
| Tables in comments | All files | `#   bg    #282c34    fg    #bbc2cf` | Readability for reference tables |
| Numbered lists | All files | `#   1. First item` | Standard indentation hierarchy |
| Continuation lines | All files | `#           This continues the above...` | Show logical grouping |
| Vim `hi` commands | `vimrc` color section, `*.vim` | Column alignment for color tables |
| Emacs let binding comments | `*.el` files | `(let* (;; Comment here` | Scoped to binding-level comments |

**Scope of Vim exception:** This applies ONLY to:
- `vim/.config/vim/vimrc` color scheme section
- Any `*.vim` files defining color schemes

**When uncertain:** Ask user to confirm whether alignment spaces are intentional.

## Line Length Limits

| Context | Limit |
|---------|-------|
| Code | 80 characters (soft limit) |
| Comments | 79 characters (hard limit) |
| URLs in comments | Exempt |

**Rationale:**
1. 79-char comments match section separator width
2. 80-char code limit is widely adopted standard
3. URLs are exempt to avoid broken links

## TODO/FIXME Format

Standardize comment markers for tracking work:

```bash
# TODO(author): Description of what needs to be done
# FIXME(author): Description of the bug or issue
# NOTE: Important information for maintainers
# HACK: Temporary workaround with explanation
```

**Examples:**
```bash
# TODO(zhengyu.li): Add support for Windows paths
# FIXME(zhengyu.li): Race condition in async handler
# NOTE: This breaks on non-ASCII characters
```

## Comment Language

**Rule: All comments must be in English.**

This includes:
- File headers
- Inline comments
- TODO/FIXME markers
- Documentation sections

**Rationale:**
1. Improves accessibility for international contributors
2. Consistent with industry standards
3. Easier to search and reference

## Header Examples

**Level 1 (ghostty/config, ripgrep/rc, bun/.bunfig.toml):**
```
# config
# =============================================================================
# Ghostty Terminal Configuration
#
# Location: ~/.config/ghostty/config
# Reference: https://ghostty.org/docs/config
# =============================================================================
```

**Level 1 with XDG (uv/uv.toml, bun/.bunfig.toml):**
```
# uv.toml
# =============================================================================
# UV Package Manager Global Configuration
#
# Location: ~/.config/uv/uv.toml
# XDG: Native support (no UV_* env overrides needed)
#      - Cache: $XDG_CACHE_HOME/uv (~/.cache/uv)
#      - Data:  $XDG_DATA_HOME/uv (~/.local/share/uv)
#
# References:
#   1. Official Docs: https://docs.astral.sh/uv/concepts/config/
# =============================================================================
```

**Level 2 with mode line (git/config):**
```
# config -*- mode: gitconfig; -*-
# Time-stamp: <2026-03-15 21:00:00 Saturday by zhengyu.li>
# =============================================================================
# Git Configuration
#
# Location: ~/.config/git/config
# =============================================================================
# Note: === is for file headers only, --- for content sections:
# -----------------------------------------------------------------------------
# User Identity
# -----------------------------------------------------------------------------
[user]
    name = Your Name
```
