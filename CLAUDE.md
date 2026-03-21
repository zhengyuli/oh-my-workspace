# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project Overview

This is a **dotfiles repository** providing comprehensive development environment setup for macOS. It uses **GNU Stow** for symlink management with XDG-compliant directory structure.

## Directory Structure

```
oh-my-dotfiles/
├── CLAUDE.md             # This file - project-wide guidance
├── setup.sh              # Interactive setup script
├── bun/                  # Bun configuration
│   ├── CLAUDE.md         # Bun-specific guidance
│   └── .config/bun/      # Symlinked to ~/.config/bun/
├── emacs/                # Emacs configuration (>= 30.2)
│   ├── CLAUDE.md         # Emacs-specific guidance (detailed)
│   └── .config/emacs/    # Symlinked to ~/.config/emacs/
├── ghostty/              # Ghostty terminal configuration
│   └── .config/ghostty/  # Symlinked to ~/.config/ghostty/
├── git/                  # Git configuration
│   └── .config/git/      # Symlinked to ~/.config/git/
├── homebrew/
│   └── Brewfile          # Homebrew bundle for all packages
├── macos/                # macOS-specific settings
├── ripgrep/              # Ripgrep configuration
│   └── .config/ripgrep/  # Symlinked to ~/.config/ripgrep/
├── starship/             # Starship prompt configuration
│   └── .config/starship.toml  # Symlinked to ~/.config/starship.toml
├── uv/                   # UV package manager configuration
│   └── .config/uv/       # Symlinked to ~/.config/uv/
├── vim/                  # Vim configuration
│   ├── CLAUDE.md         # Vim-specific guidance
│   └── .config/vim/      # Symlinked to ~/.config/vim/
└── zsh/                  # Zsh configuration
    ├── CLAUDE.md         # Zsh-specific guidance
    ├── .zshenv           # Symlinked to ~/.zshenv (bootstrap file)
    └── .config/zsh/      # Symlinked to ~/.config/zsh/
```

## Quick Start

### Setup

```bash
# 1. Clone the repository
git clone https://github.com/zhengyuli/oh-my-dotfiles.git ~/oh-my-dotfiles
cd ~/oh-my-dotfiles

# 2. Install Homebrew packages
brew bundle --file homebrew/Brewfile

# 3. Stow configuration packages
stow bun emacs ghostty git ripgrep starship uv vim zsh

# 4. Restart shell or source zshenv
source ~/.zshenv
```

### Common Commands

| Command | Description |
|---------|-------------|
| `stow zsh git` | Stow specific packages |
| `stow -D zsh git` | Unstow (remove symlinks) |
| `stow -R zsh git` | Restow (refresh symlinks) |
| `stow -n zsh` | Dry-run to preview changes |
| `brew bundle --file homebrew/Brewfile` | Install all Homebrew packages |

### Quick Validation

```bash
# Verify stow packages are linked
ls -la ~/.zshenv ~/.config/zsh/.zshrc ~/.config/git/config

# Test Zsh configuration
zsh -c 'echo $ZDOTDIR'

# Test Emacs configuration
emacs --debug-init
```

## Stow Package System

| Package | Contents |
|---------|----------|
| `bun/` | `.config/bun/` → `$HOME/.config/bun/` |
| `emacs/` | `.config/emacs/` → `$HOME/.config/emacs/` |
| `ghostty/` | `.config/ghostty/` → `$HOME/.config/ghostty/` |
| `git/` | `.config/git/` → `$HOME/.config/git/` |
| `ripgrep/` | `.config/ripgrep/` → `$HOME/.config/ripgrep/` |
| `starship/` | `.config/starship.toml` → `$HOME/.config/starship.toml` |
| `uv/` | `.config/uv/` → `$HOME/.config/uv/` |
| `vim/` | `.config/vim/` → `$HOME/.config/vim/` |
| `zsh/` | `.zshenv` → `$HOME/.zshenv`, `.config/zsh/` → `$HOME/.config/zsh/` |

**Note:** `homebrew/` and `macos/` are NOT stow packages - they provide scripts/utilities only.

## Architecture

### XDG Base Directory

Shell configuration exports XDG environment variables:

```
$HOME/.config/     ← XDG_CONFIG_HOME (configurations)
$HOME/.cache/      ← XDG_CACHE_HOME (cache files)
$HOME/.local/share/ ← XDG_DATA_HOME (data files)
$HOME/.local/state/ ← XDG_STATE_HOME (state files)
```

```bash
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
```

### Toolchain Overview

| Tool | Purpose | Replaces |
|------|---------|----------|
| **bun** | JS/TS runtime + package manager | node, npm, yarn, pnpm, fnm |
| **uv** | Python package manager | pip, pipenv, poetry, pyenv |
| **go** | Go programming language | (standard install) |

### Subdirectory CLAUDE.md Files

Each major component has its own `CLAUDE.md` with detailed guidance:

| File | Content |
|------|---------|
| **zsh/CLAUDE.md** | Zsh configuration structure, startup sequence, plugin system |
| **emacs/CLAUDE.md** | Emacs coding standards, module architecture, use-package patterns |
| **vim/CLAUDE.md** | Vim configuration, XDG paths, alignment exception |
| **bun/CLAUDE.md** | Bun configuration, tool installation |

These files are lazy-loaded when working in their respective directories.

## Toolchain

### LSP Server Installation

```bash
# JS/TS ecosystem (bun)
bun install -g typescript-language-server typescript
bun install -g vscode-langservers-extracted   # html/css/json/eslint
bun install -g bash-language-server
bun install -g @tailwindcss/language-server
bun install -g prettier

# Python ecosystem (uv)
uv tool install basedpyright
uv tool install ruff

# Go (standard install)
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest

# System-level (via Homebrew)
# clangd, lua-language-server, rust-analyzer
```

## Theme

Unified **Doom One** theme across:

| Tool | Configuration |
|------|---------------|
| **Emacs** | `doom-one` theme via `doom-themes` package |
| **Ghostty** | Custom color palette matching Doom One |
| **Starship** | Prompt configuration with Doom One colors |
| **fzf** | Doom One color configuration in `FZF_DEFAULT_OPTS` |

## Terminal

- **Ghostty**: Fast, native terminal emulator with XDG-compliant configuration

## Setup Scripts

### setup.sh

The main setup script provides interactive package management for stow operations.

**Key patterns:**

```bash
# Pure query functions (no side effects)
is_stowed()    # Check if package is stowed
has_stow()     # Check if stow is available
has_homebrew() # Check if homebrew is installed

# Operation functions (have side effects)
stow_package()   # Stow a single package
backup_file()    # Backup existing file
restore_file()   # Restore from backup
```

**Error handling:**
- Use explicit `if` statements (not `[[ ]] && cmd`)
- Use `set -euo pipefail` at script start
- All output via `printf` with helper functions: `log_ok`, `log_err`, `log_warn`

**Function structure:**
```bash
# Pure query - returns 0/1, no output
is_stowed() {
    local pkg="$1"
    [[ -d "$STOW_DIR/$pkg" ]] && stow -n -d "$STOW_DIR" -t "$HOME" "$pkg" &>/dev/null
}

# Operation - may output, may modify state
stow_package() {
    local pkg="$1"
    if is_stowed "$pkg"; then
        log_warn "Already stowed: $pkg"
        return 0
    fi
    stow -d "$STOW_DIR" -t "$HOME" "$pkg"
    log_ok "Stowed: $pkg"
}
```

### defaults.sh

macOS defaults configuration script for system preferences.

**Key patterns:**

```bash
# Domain grouping - organize by domain
# NSGlobalDomain, com.apple.finder, com.apple.dock, etc.

# Document customizations in header comments
# Note which settings require logout/restart
```

## Coding Standards

### Shell Script Standards

For shell script coding standards (conditionals, quoting, output, security), see [zsh/CLAUDE.md - Coding Standards](zsh/CLAUDE.md#coding-standards).

Key points:
- Use explicit `if` statements, avoid `[[ ]] && cmd || true`
- Use `[[ ]]` for tests, `(( ))` for arithmetic
- Quote all external data
- Use `printf` or `print`, never `echo`
- For environment variable assignment patterns, see [zsh/CLAUDE.md - Variable Assignment](zsh/CLAUDE.md#variable-assignment)

### Configuration Comment Standards

These standards apply to **all non-emacs/non-zsh configuration files** in the repository (bun, ghostty, git, homebrew, macos, ripgrep, uv, vim). For zsh and emacs, see their respective CLAUDE.md files.

#### Header Structure

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

#### Header Fields

| Field | Required | Format | Purpose |
|-------|----------|--------|---------|
| `<filename>` | Yes | Filename only | e.g., `config`, `uv.toml`, `.bunfig.toml` |
| Description | Yes | Single line | Brief summary of file purpose |
| Location | Recommended | `~/.config/...` | Absolute path where file is symlinked |
| XDG | If applicable | Compliance notes | XDG support status and cache/data paths |
| Loaded via | If applicable | Loading method | How the tool discovers this config |
| References | Recommended | Numbered list | Official documentation links |
| Note | Optional | Free text | Important caveats or warnings |

#### Time-stamp Convention

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

#### Section Separators

| Type | Format | Width | Usage |
|------|--------|-------|-------|
| Header | `# ===...===` | 79 chars | File header open/close ONLY |
| Major section | `# ---...---` | 79 chars | Top-level content sections |
| Subsection | `# --- Name ---` | Variable | Nested sections within major |

**Rules:**

1. **`===` is reserved for file headers.** All content sections use `---`.

2. **No sandwiching.** Description text always goes AFTER the closing separator,
   never between the opening and closing separator.

   **❌ WRONG — description sandwiched between separators:**
   ```
   # -----------------------------------------------------------------------------
   # Section Name
   # Description of the section     ← sandwiched
   # -----------------------------------------------------------------------------
   <code>
   ```

   **✅ CORRECT — description after closing separator:**
   ```
   # -----------------------------------------------------------------------------
   # Section Name
   # -----------------------------------------------------------------------------
   # Description of the section
   <code>
   ```

3. **No double separators.** Each section uses exactly one opening + one closing
   `---` separator. Never add an extra `---` line to wrap metadata.

   **❌ WRONG — extra closing separator:**
   ```
   # -----------------------------------------------------------------------------
   # Tool Name
   # -----------------------------------------------------------------------------
   # Prerequisites: ...
   # -----------------------------------------------------------------------------   ← extra
   <code>
   ```

   **✅ CORRECT — single open + single close:**
   ```
   # -----------------------------------------------------------------------------
   # Tool Name
   # -----------------------------------------------------------------------------
   # Prerequisites: ...
   <code>
   ```

4. **No blank line after closing separator.** The first line of content
   (description comment or code) follows the closing `---` immediately.

   **❌ WRONG — blank line after closing separator:**
   ```
   # -----------------------------------------------------------------------------
   # Section Name
   # -----------------------------------------------------------------------------
                                    ← blank line
   <code>
   ```

   **✅ CORRECT — content immediately after closing separator:**
   ```
   # -----------------------------------------------------------------------------
   # Section Name
   # -----------------------------------------------------------------------------
   <code>
   ```

5. **Subsection headers always at column 0.** `# --- Name ---` is never indented,
   even when it appears inside an `if/else` block.

6. **No blank line after `# --- Name ---`.** Content follows immediately.

   **❌ WRONG:**
   ```
   # --- Subsection ---
                        ← blank line
   <code>
   ```

   **✅ CORRECT:**
   ```
   # --- Subsection ---
   <code>
   ```

#### Comment Characters by Format

| Format | Character | Example |
|--------|-----------|---------|
| Shell, Gitconfig, TOML, Brewfile, Ghostty, Ripgrep | `#` | `# comment` |
| Vim | `"` | `" comment` |

#### Subsection Style

Use ASCII dashes for subsections (not Unicode em-dashes):

```bash
# --- Section Name ---
# Brief explanation
<configuration>
```

**Rationale:** ASCII ensures cross-editor portability and consistent rendering.

#### Inline Comments (CRITICAL)

**Rule: Do NOT put comments on the same line as code.** All comments must be on the line above the code they describe.

For detailed examples and rationale, see:
- [zsh/CLAUDE.md - Inline Comments](zsh/CLAUDE.md#inline-comments-critical)
- [emacs/CLAUDE.md - Inline Comments](emacs/CLAUDE.md#inline-comments)

#### Alignment Spaces (CRITICAL)

**Rule: Do NOT use extra spaces for visual alignment in code.**

Use exactly one space between elements. Never add padding spaces to align items vertically in code. This includes:

1. **Assignment operators (`=`, `+=`, etc.)** - One space on each side only
2. **Array elements** - No padding to align values
3. **Variable declarations** - No padding to align names or values

**❌ AVOID - Alignment spaces in code:**
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

**✅ CORRECT - Single space:**
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

**Exceptions (alignment spaces MAY be used):**

| Context | Scope | Example | Reason |
|---------|-------|---------|--------|
| Tables in comments | All files | `#   bg    #282c34    fg    #bbc2cf` | Readability for reference tables |
| Numbered lists | All files | `#   1. First item` | Standard indentation hierarchy |
| Continuation lines | All files | `#           This continues the above...` | Show logical grouping |
| Vim `hi` commands | `vimrc` color section, `*.vim` | Column alignment for color tables |
| Emacs let binding comments | `*.el` files | `(let* (;; Comment here` | Scoped to binding-level comments |

**Scope:** This exception applies ONLY to:
- `vim/.config/vim/vimrc` color scheme section (lines ~286-400)
- Any `*.vim` files defining color schemes

**When uncertain:** Ask user to confirm whether alignment spaces are intentional formatting or should be removed. Do NOT auto-modify without user approval.



**Exception: Vim Color Scheme Configuration**

Vim `hi` (highlight) commands in color scheme definitions MAY use alignment spaces for readability:

```vim
" ✅ ALLOWED - Color palette table in comments (readability)
"   bg       #282c34    bg-alt   #21242b    fg       #bbc2cf
"   red      #ff6c6b    orange   #da8548    green    #98be65

" ✅ ALLOWED - Highlight commands with column alignment (color scheme only)
hi Normal       guifg=#bbc2cf guibg=#282c34 gui=NONE
hi CursorLine   guifg=NONE    guibg=#23272e gui=NONE
hi Visual        guifg=NONE    guibg=#3e4454 gui=NONE
```

**Scope:** This exception applies ONLY to:
- `vim/.config/vim/vimrc` color scheme section (lines ~286-400)
- Any `*.vim` files defining color schemes

**When uncertain:** Ask user to confirm whether alignment spaces are intentional formatting or should be removed. Do NOT auto-modify without user approval.

**Note:** For Emacs Lisp alignment spaces in cons cells and hooks, see [emacs/CLAUDE.md](emacs/CLAUDE.md#alignment-spaces-critical) for Emacs-specific examples.

#### Line Length Limits

| Context | Limit |
|---------|-------|
| Code | 80 characters (soft limit) |
| Comments | 79 characters (hard limit) |
| URLs in comments | Exempt |

**Rationale:**
1. 79-char comments match section separator width
2. 80-char code limit is widely adopted standard
3. URLs are exempt to avoid broken links

#### TODO/FIXME Format

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

#### Comment Language

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

#### Header Examples

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

## Code Quality

### Validation Commands

**Verify stow symlinks:**
```bash
ls -la ~/.zshenv ~/.config/zsh/.zshrc ~/.config/git/config
```

**Test shell configuration:**
```bash
zsh -c 'echo $ZDOTDIR'
```

**Check for cache files before committing:**
```bash
git status --porcelain | grep -E '\.(zcompdump|swp|swo|bak|elc|pyc)$|cache/|undo/'
```

**Validate configuration files:**
```bash
# Check header format compliance
grep -L "^# ===" */.config/*/* 2>/dev/null

# Check Time-stamp in files with mode lines
grep -r "^# config.*mode:" --include="*" | while read -r line; do
    file=$(echo "$line" | cut -d: -f1)
    grep -q "Time-stamp:" "$file" || echo "Missing Time-stamp: $file"
done
```

### Compliance Checklist

- [ ] All stow packages properly symlinked
- [ ] No cache files committed (`.zcompdump`, `*.swp`, etc.)
- [ ] Configuration files have standardized headers
- [ ] Shell scripts follow coding standards (no `[[ ]] && cmd || true`)
- [ ] No `echo` in scripts (use `printf`)
- [ ] No inline comments in config files (comments on separate lines above code)
- [ ] Time-stamp present in ALL configuration files
- [ ] Section separators use `---` for content, `===` for headers only (79 chars)
- [ ] No alignment spaces (single space only)
- [ ] All comments in English
- [ ] Line length within limits (80 code, 79 comments)

### Troubleshooting

**Stow conflicts:**
```bash
# Remove existing files and restow
stow -D zsh
rm ~/.zshenv
stow zsh
```

**XDG paths not set:**
```bash
# Verify .zshenv is loaded first
zsh -c 'echo $XDG_CONFIG_HOME'
```

**Cache files in wrong location:**
```bash
# Check if XDG_* variables are set before tool initialization
echo $XDG_CACHE_HOME
```

## Best Practices

### 1. Stow Management

**Always use stow for symlink management:**
```bash
# ✅ CORRECT - use stow
stow zsh git

# ❌ AVOID - manual symlinks break stow tracking
ln -s ~/oh-my-dotfiles/zsh/.zshenv ~/.zshenv
```

### 2. Cache File Prevention

**Never commit runtime cache files:**
```bash
# Check before committing
git status --porcelain | grep -E '\.(zcompdump|swp|swo|bak|elc)$'
```

**Why:** If XDG variables aren't set, tools may create cache files in `.config/` instead of proper locations.

### 3. Configuration File Headers

**Use standardized headers for all configuration files:**
- Level 1: Simple files (ghostty, ripgrep)
- Level 2: Complex files with mode lines (gitconfig)

**Why:** Consistent headers improve discoverability and maintenance.

### 4. Shell Script Patterns

**Prefer explicit `if` over `&&` chains:**
```bash
# ✅ CORRECT
if [[ -n "$var" ]]; then
    cmd
fi

# ❌ AVOID
[[ -n "$var" ]] && cmd || true
```

**Why:** Explicit conditionals are more predictable and easier to debug.

### 5. Pre-Commit Verification

**Always validate before committing:**
```bash
# 1. Check for cache files
git status --porcelain | grep -E '\.(zcompdump|swp|elc)$'

# 2. Verify stow links
ls -la ~/.zshenv ~/.config/git/config

# 3. Test shell loads without errors
zsh -c 'source ~/.zshenv && echo OK'
```

**Why:** Catches common issues before they reach the repository.

## Key Conventions

- **macOS-focused**: All configurations assume macOS as primary OS
- **Stow-managed**: Config files are symlinked via GNU Stow
- **XDG-compliant**: Follows XDG Base Directory Specification
- **Emacs prefix**: Uses `omw/` prefix (oh-my-workspace) for custom functions/variables
- **Modern toolchain**: bun for JS/TS, uv for Python, no version managers needed

## Cache File Detection

**IMPORTANT**: Before committing, always check for runtime cache files that should not be tracked.

### Files to NEVER Commit

| Tool | Files | Proper Location |
|------|-------|-----------------|
| Zsh | `.zcompdump`, `.zcompdump.*`, `cache/` | `$XDG_CACHE_HOME/zsh/` |
| Vim | `*.swp`, `*.swo`, `*.bak`, `*~`, `undo/` | `$XDG_STATE_HOME/vim/`, `$XDG_DATA_HOME/vim/` |
| Emacs | `*.elc`, `.#*`, `*~`, `#*#` | Generated, should not be in repo |
| Git | `config.local`, `credentials` | Machine-specific |

### Detection Command

Before committing, run:

```bash
# Check for cache files that shouldn't be committed
git status --porcelain | grep -E '\.(zcompdump|swp|swo|bak|elc|pyc)$|cache/|undo/'

# If any files match, either:
# 1. Add them to .gitignore if not already covered
# 2. Remove with: git rm --cached <file>
```

### Why This Matters

If XDG environment variables (`XDG_CACHE_HOME`, `XDG_STATE_HOME`) are not properly set, tools may create cache files in `.config/` directories instead of their proper locations. The `.gitignore` includes fallback patterns to catch these cases.

---

## Quick Reference Card

### Essential Rules

1. **Stow Only:** Use `stow` for symlink management, never manual `ln -s`
2. **XDG Paths:** All tools must respect `$XDG_*` environment variables
3. **Shell Scripts:** Use `if` statements, avoid `[[ ]] && cmd || true`
4. **Output:** Use `printf`, never `echo`
5. **Config Headers:** Standardized format with `===` for headers, `---` for sections
6. **Time-stamp:** Required for ALL configuration files
7. **Inline Comments:** Prohibited (comments on line above code)
8. **Alignment Spaces:** Prohibited (single space only, including around `=`)
9. **Line Length:** 80 chars for code, 79 for comments
10. **Comment Language:** English only
11. **Cache Files:** Never commit `.zcompdump`, `*.swp`, `*.elc`, etc.
12. **Prefix Convention:** Emacs uses `omw/`, zsh uses no special prefix
13. **Modern Toolchain:** bun for JS/TS, uv for Python, no version managers
14. **Theme:** Doom One across all tools (Emacs, Ghostty, fzf)
15. **Subdirectory CLAUDE.md:** Check zsh/CLAUDE.md and emacs/CLAUDE.md for specific guidance

### Pre-Commit Checklist

- [ ] No cache files: `git status | grep -E '\.(zcompdump|swp|elc)$'`
- [ ] Stow symlinks valid: `ls -la ~/.zshenv ~/.config/git/config`
- [ ] Shell loads: `zsh -c 'source ~/.zshenv'`
- [ ] Config headers compliant: Standardized format with `===`/`---`
- [ ] Time-stamp present: ALL config files have Time-stamp
- [ ] No alignment spaces: Single space between elements (including around `=`)
- [ ] No `echo`: all output via `printf`
- [ ] All comments in English

### Common Patterns

```bash
# Stow package management
stow bun emacs ghostty git ripgrep starship uv vim zsh  # Install all
stow -D zsh                                              # Remove package
stow -R zsh                                              # Refresh package

# XDG environment
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# Shell conditional (correct pattern)
if [[ -n "$var" ]]; then
    cmd
fi

# Config file header
# filename
# =============================================================================
# Description
#
# Location: ~/.config/tool/filename
# =============================================================================
```
