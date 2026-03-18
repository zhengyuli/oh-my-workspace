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

#### 11. 环境变量赋值模式

根据变量用途选择正确的赋值模式：

**规则 1: XDG 基础变量和工具路径 → 使用 `${VAR:-default}`**

```bash
# ✅ 正确 - 带回退，尊重用户可能的自定义
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export GOPATH="${GOPATH:-$XDG_DATA_HOME/go}"
```

**规则 2: 配置文件路径（XDG 已保证存在） → 使用 `$VAR`**

```bash
# ✅ 正确 - 简洁，XDG 变量已在 .zshenv 中设置
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/rc"
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship.toml"
```

**规则 3: 变量拼接或边界需要 → 使用 `${VAR}`**

```bash
# ✅ 正确 - 花括号明确变量边界
export PATH="${GOPATH}/bin:${PATH}"
export VIMINIT="source ${XDG_CONFIG_HOME}/vim/vimrc"
```

**❌ 错误模式**

```bash
# ❌ 避免不必要的花括号（无边界歧义时）
export STARSHIP_CONFIG="${XDG_CONFIG_HOME}/starship.toml"  # 冗余

# ❌ 避免对已保证存在的变量使用回退
export RIPGREP_CONFIG_PATH="${XDG_CONFIG_HOME:-$HOME/.config}/ripgrep/rc"  # 冗余
```

**决策树**:

```
需要设置变量?
├── 用户可能已自定义? → YES → ${VAR:-default}
│
└── NO → XDG 变量引用?
    ├── YES → 变量后紧跟非分隔字符? → YES → ${VAR}
    │       └── NO → $VAR (简洁)
    └── NO → 直接赋值
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

| Level | Required | Format |
|-------|----------|--------|
| Level 1 (simple) | No | N/A |
| Level 2 (medium+) | Yes | `# Time-stamp: <YYYY-MM-DD HH:MM:SS Day by Author>` |

**When to add Time-stamp:**
- Files with mode lines (`-*- mode: ...; -*-`)
- Files with multiple sections
- Files that are manually edited

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
- [ ] Time-stamp present in files with mode lines
- [ ] Section separators use `---` for content, `===` for headers only

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
5. **Time-stamp:** Required for files with mode lines or multiple sections
6. **Cache Files:** Never commit `.zcompdump`, `*.swp`, `*.elc`, etc.
7. **Prefix Convention:** Emacs uses `omw/`, zsh uses no special prefix
8. **Modern Toolchain:** bun for JS/TS, uv for Python, no version managers
9. **Theme:** Doom One across all tools (Emacs, Ghostty, fzf)
10. **Subdirectory CLAUDE.md:** Check zsh/CLAUDE.md and emacs/CLAUDE.md for specific guidance

### Pre-Commit Checklist

- [ ] No cache files: `git status | grep -E '\.(zcompdump|swp|elc)$'`
- [ ] Stow symlinks valid: `ls -la ~/.zshenv ~/.config/git/config`
- [ ] Shell loads: `zsh -c 'source ~/.zshenv'`
- [ ] Config headers compliant: Standardized format with `===`/`---`
- [ ] Time-stamp present: Files with mode lines have Time-stamp

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
