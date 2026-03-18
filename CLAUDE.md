# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project Overview

This is a **dotfiles repository** providing comprehensive development environment setup for macOS. It uses **GNU Stow** for symlink management with XDG-compliant directory structure.

## Directory Structure

```
oh-my-dotfiles/
├── .stow-local-ignore    # Files to ignore when stowing
├── CLAUDE.md             # This file - project-wide guidance
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
├── uv/                   # UV package manager configuration
│   └── .config/uv/       # Symlinked to ~/.config/uv/
├── vim/                  # Vim configuration
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
stow zsh git vim emacs ghostty ripgrep

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
| `zsh/` | `.zshenv` → `$HOME/.zshenv`, `.config/zsh/` → `$HOME/.config/zsh/` |
| `git/` | `.config/git/` → `$HOME/.config/git/` |
| `vim/` | `.config/vim/` → `$HOME/.config/vim/` |
| `emacs/` | `.config/emacs/` → `$HOME/.config/emacs/` |
| `ghostty/` | `.config/ghostty/` → `$HOME/.config/ghostty/` |
| `ripgrep/` | `.config/ripgrep/` → `$HOME/.config/ripgrep/` |

**Note:** `homebrew/` and `macos/` are NOT stow packages — they provide scripts/utilities only.

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
| **fzf** | Doom One color configuration in `FZF_DEFAULT_OPTS` |

## Terminal

- **Ghostty**: Fast, native terminal emulator with XDG-compliant configuration

## Coding Standards

### Shell Script Standards

These conventions apply to **both bash and zsh scripts** throughout the repository.

#### 1. Avoid `[[ cond ]] && cmd || true` Pattern (CRITICAL)

**WRONG** - This pattern is error-prone and hides potential issues:

```bash
# Problem 1: Non-zero exit code from condition causes function to fail under set -e
[[ -n "$var" ]] && log_warn "message"

# Problem 2: Adding || true masks errors and is hard to read
[[ -n "$var" ]] && log_warn "message" || true
```

**CORRECT** - Use explicit `if` statements:

```bash
if [[ -n "$var" ]]; then
    log_warn "message"
fi
```

**Rationale:**
1. With `set -e`, a false condition (`[[ -n "$var" ]]` when var is empty) returns exit code 1, causing unexpected script termination
2. Adding `|| true` masks potential errors in the command itself
3. `if` statements are explicit, readable, and behave predictably

**Acceptable uses of `&&`:**
```bash
# Early return pattern (function continues if condition is false)
[[ -d "$dir" ]] && return

# Conditional execution where false condition is expected and harmless
[[ -n "$value" ]] && git config "$key" "$value"
```

#### 2. Use `[[ ]]` for Conditional Tests

Prefer `[[ ]]` over `[ ]` or `test` for condition checks:

```bash
# ✅ CORRECT
[[ -f "$file" ]]
[[ "$var" == "pattern" ]]
[[ -n "$value" ]]

# ❌ AVOID
[ -f "$file" ]
test -f "$file"
```

**Rationale:** `[[ ]]` prevents word-splitting, supports pattern matching, and is more readable.

#### 3. Use `(( ))` for Arithmetic

```bash
# ✅ CORRECT
(( count++ ))
(( total = a + b ))
local result=$(( a * b ))

# ❌ AVOID
let count+=1
result=$(expr $a + $b)
```

#### 4. Use `$()` for Command Substitution

```bash
# ✅ CORRECT
local dir=$(dirname "$file")

# ❌ AVOID - hard to nest, hard to read
local dir=`dirname "$file"`
```

#### 5. Quote All External Data

Any value from files, environment variables, or command output must be quoted:

```bash
# ✅ CORRECT
rm -- "${file}"
grep -- "${pattern}" "${file}"
source "${config_file}"

# ❌ WRONG - word-splitting and glob expansion risks
rm ${file}
grep ${pattern} ${file}
```

#### 6. Prefer `printf` Over `echo -e`

```bash
# ✅ CORRECT - portable and predictable
printf '%s\n' "$message"
printf 'Status: %s\n' "$status"

# ❌ AVOID - behavior varies between shells
echo -e "$message"
```

#### 7. Use `set -e` with Caution

```bash
# ✅ CORRECT - scoped to specific operations
(
    set -e
    cmd1
    cmd2
)

# ❌ AVOID globally in interactive shells
set -e  # Can cause unexpected exits
```

#### 8. Security: Validate Before Sourcing

```bash
# ✅ CORRECT - check before sourcing
if [[ -f "${file}" ]]; then
    source "${file}"
fi

# ❌ WRONG - error if file missing
source "${file}"
```

#### 9. Security: Never `eval` Untrusted Input

```bash
# ✅ CORRECT - use allowlist
local -a allowed=(rbenv pyenv nodenv)
if (( ${allowed[(I)${tool}]} )); then
    eval "$(${tool} init -)"
fi

# ❌ DANGEROUS - arbitrary code execution
eval "${user_input}"
```

#### 10. Check File Existence Before Operations

```bash
# ✅ CORRECT
[[ -f "$file" ]] && rm -- "$file"

# ❌ WRONG - error if file doesn't exist
rm "$file"
```

#### 11. Environment Variable Assignment Patterns

Choose the correct assignment pattern based on variable purpose (XDG variables, tool paths, config paths, variable concatenation).

**Rule 1: XDG Base Variables and Tool Paths → Use `${VAR:-default}`**

```bash
# ✅ CORRECT - With fallback, respects potential user customization
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export GOPATH="${GOPATH:-$XDG_DATA_HOME/go}"
```

**Rule 2: Config File Paths (XDG guaranteed) → Use `$VAR`**

```bash
# ✅ CORRECT - Concise, XDG variables already set in .zshenv
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/rc"
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship.toml"
```

**Rule 3: Variable Concatenation or Boundary Needed → Use `${VAR}`**

```bash
# ✅ CORRECT - Braces clarify variable boundaries
export PATH="${GOPATH}/bin:${PATH}"
export VIMINIT="source ${XDG_CONFIG_HOME}/vim/vimrc"
```

**❌ Anti-patterns**

```bash
# ❌ Avoid unnecessary braces (when no boundary ambiguity)
export STARSHIP_CONFIG="${XDG_CONFIG_HOME}/starship.toml"  # Redundant

# ❌ Avoid fallback for guaranteed variables
export RIPGREP_CONFIG_PATH="${XDG_CONFIG_HOME:-$HOME/.config}/ripgrep/rc"  # Redundant
```

**Decision Tree:**

```
Need to set variable?
├── User may have customized? → YES → ${VAR:-default}
│
└── NO → XDG variable reference?
    ├── YES → Non-separator char follows? → YES → ${VAR}
    │       └── NO → $VAR (concise)
    └── NO → Direct assignment
```

For zsh-specific conventions (startup files, setopt, arrays, etc.), see [zsh/CLAUDE.md](zsh/CLAUDE.md).

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

**Rule:** `===` is reserved for file headers. All content sections use `---`.

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

**Rule: Do NOT put comments on the same line as code.**

All comments must be on the line above the code they describe.

**❌ AVOID - Inline comments:**
```bash
setopt AUTO_CD # type a directory name to cd
bindkey -e    # emacs keymap
```

**✅ CORRECT - Comment above:**
```bash
# type a directory name to cd
setopt AUTO_CD
# emacs keymap
bindkey -e
```

**Rationale:**
1. Improves readability with clear separation
2. No alignment maintenance burden
3. Easier to scan code without comment noise
4. Prevents accidental removal of code-comment spacing

#### Alignment Spaces (CRITICAL)

**Rule: Do NOT use extra spaces for visual alignment in code.**

Use exactly one space between elements. Never add padding spaces to align items vertically in code.

**❌ AVOID - Alignment spaces in code:**
```bash
# Shell arrays
path=(
  "$HOME/.local/bin"      # User binaries
  /opt/homebrew/bin       # Homebrew
  $path                   # System
)

# Variable assignments
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
```

**✅ CORRECT - Single space:**
```bash
# Shell arrays
path=(
  "$HOME/.local/bin"
  /opt/homebrew/bin
  $path
)

# Variable assignments
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

| Context | Example | Reason |
|---------|---------|--------|
| Tables in comments | `#   bg    #282c34    fg    #bbc2cf` | Readability for reference tables |
| Numbered lists | `#   1. First item` | Standard indentation hierarchy |
| Continuation lines | `#           This continues the above...` | Show logical grouping |
| Vim color schemes | See detailed section below | Column alignment for color tables |

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
4. **Config Headers:** Standardized format with `===` for headers, `---` for sections
5. **Time-stamp:** Required for ALL configuration files
6. **Inline Comments:** Prohibited (comments on line above code)
7. **Alignment Spaces:** Prohibited (single space only)
8. **Line Length:** 80 chars for code, 79 for comments
9. **Comment Language:** English only
10. **Cache Files:** Never commit `.zcompdump`, `*.swp`, `*.elc`, etc.
11. **Prefix Convention:** Emacs uses `omw/`, zsh uses no special prefix
12. **Modern Toolchain:** bun for JS/TS, uv for Python, no version managers
13. **Theme:** Doom One across all tools (Emacs, Ghostty, fzf)
14. **Subdirectory CLAUDE.md:** Check zsh/CLAUDE.md and emacs/CLAUDE.md for specific guidance

### Pre-Commit Checklist

- [ ] No cache files: `git status | grep -E '\.(zcompdump|swp|elc)$'`
- [ ] Stow symlinks valid: `ls -la ~/.zshenv ~/.config/git/config`
- [ ] Shell loads: `zsh -c 'source ~/.zshenv'`
- [ ] Config headers compliant: Standardized format with `===`/`---`
- [ ] Time-stamp present: ALL config files have Time-stamp
- [ ] No alignment spaces: Single space between elements
- [ ] All comments in English

### Common Patterns

```bash
# Stow package management
stow zsh git vim emacs ghostty ripgrep  # Install all
stow -D zsh                             # Remove package
stow -R zsh                             # Refresh package

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
