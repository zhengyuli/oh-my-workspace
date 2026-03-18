# CLAUDE.md - ZSH Configuration

This file provides guidance for Claude Code when working with the Zsh configuration in this directory.

## Project Overview

This is an **XDG-compliant Zsh configuration** with modular organization, using **Zinit** plugin manager for performance and flexibility.

## Directory Structure

The Zsh configuration is located at `.config/zsh/` within the stow package:

```
zsh/
├── .zshenv                    # Bootstrap file (symlinked to ~/.zshenv)
└── .config/zsh/
    ├── .zprofile              # Login shell initialization
    ├── .zshrc                 # Interactive shell orchestrator
    ├── .zcompdump             # Completion cache (auto-generated)
    ├── cache/                 # Runtime cache (history, etc.)
    ├── completions/           # Custom completion scripts
    ├── conf.d/                # Modular configuration files
    │   ├── 00-env.zsh         # Core environment variables
    │   ├── 05-path.zsh        # PATH/FPATH/MANPATH management
    │   ├── 10-options.zsh     # Shell options (setopt/unsetopt)
    │   ├── 15-history.zsh     # History configuration
    │   ├── 20-aliases.zsh     # Command aliases
    │   ├── 30-completion.zsh  # Completion system initialization
    │   ├── 40-plugins.zsh     # Plugin loading (Zinit turbo mode)
    │   ├── 50-prompt.zsh      # Prompt configuration
    │   ├── 60-keybinds.zsh    # Key bindings
    │   ├── 70-tools.zsh       # Tool initialization (fzf, zoxide, etc.)
    │   └── 99-local.zsh.example # Local overrides template
    └── functions/             # Autoloaded shell functions
```

## Quick Start

### Setup

```bash
# Stow the zsh package (creates symlinks)
stow zsh

# Test configuration loads without errors
zsh -c 'echo "ZDOTDIR: $ZDOTDIR"'

# Start a new shell to verify
zsh
```

### Common Commands

| Command | Description |
|---------|-------------|
| `exec zsh` | Reload shell configuration |
| `source ~/.zshenv` | Re-source bootstrap file |
| `omz_reload` | Reload conf.d files (if function exists) |
| `zinit update` | Update all Zinit plugins |
| `zinit delete <plugin>` | Remove a plugin |

### Quick Validation

```bash
# Verify XDG paths are set
echo "XDG_CONFIG_HOME: $XDG_CONFIG_HOME"
echo "ZDOTDIR: $ZDOTDIR"

# Verify conf.d files are sourced
zsh -c 'typeset -p ZDOTDIR 2>/dev/null && echo "✅ Zsh config loaded"'

# Check for syntax errors
zsh -n .config/zsh/.zshrc
```

## Architecture

### Shell Startup Sequence

```
.zshenv (always) → .zprofile (login) → .zshrc (interactive)
```

| File | Loaded By | Purpose |
|------|-----------|---------|
| `.zshenv` | ALL shells | XDG paths, ZDOTDIR, tool redirects |
| `.zprofile` | Login shells | Environment, PATH, SSH agent |
| `.zshrc` | Interactive shells | Sources conf.d/* in order |

### Module Loading System

**Critical**: The `.zshrc` file loads conf.d modules in numeric order. Dependencies MUST be respected.

**Loading order** (see `.zshrc` for implementation):
1. **00-env.zsh** - Core XDG variables, tool redirects (no dependencies)
2. **05-path.zsh** - PATH/FPATH/MANPATH (depends on 00-env)
3. **10-options.zsh** - Shell options (no dependencies)
4. **15-history.zsh** - History config (depends on 00-env for XDG_CACHE_HOME)
5. **20-aliases.zsh** - Aliases (no dependencies)
6. **30-completion.zsh** - Completion init (depends on 05-path for fpath)
7. **40-plugins.zsh** - Zinit plugins (depends on 00-env, 30-completion)
8. **50-prompt.zsh** - Prompt theme (depends on 40-plugins for async)
9. **60-keybinds.zsh** - Key bindings (depends on 40-plugins for widgets)
10. **70-tools.zsh** - Tool init (depends on 00-env for paths)
11. **99-local.zsh** - Local overrides (loaded last, not tracked)

**When adding new modules:**
- Use appropriate numeric prefix based on dependencies
- Document prerequisites in header
- Ensure idempotency (safe to source multiple times)

### What Goes Where

| Content | File | Reason |
|---------|------|--------|
| XDG_* variables | `.zshenv` | Needed by all shells |
| ZDOTDIR | `.zshenv` | Must be set before other files load |
| Tool XDG redirects | `00-env.zsh` | Login shell context; sourced by .zprofile and .zshrc |
| PATH changes | `.zprofile` / `05-path.zsh` | Only needed at login |
| Editor/Pager | `.zprofile` / `00-env.zsh` | Only needed at login |
| Aliases | `.zshrc` / `20-aliases.zsh` | Interactive only |
| Functions | `functions/` | Autoloaded, interactive only |
| Prompt | `.zshrc` / `50-prompt.zsh` | Interactive only |
| Plugins | `.zshrc` / `40-plugins.zsh` | Interactive only |
| Local overrides | `99-local.zsh` | Machine-specific, not tracked |

## Comment Standards

### File Header Template

All configuration files must start with a standardized header:

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

### Time-stamp Format

| Field | Format | Example |
|-------|--------|---------|
| Date | ISO 8601 (YYYY-MM-DD) | `2026-03-17` |
| Time | 24-hour format (HH:MM:SS) | `15:30:00` |
| Day | Full day name (Monday-Sunday) | `Monday` |
| Author | Git username or email prefix | `zhengyu.li` |

**Example:** `# Time-stamp: <2026-03-17 15:30:00 Monday by zhengyu.li>`

### Header Field Specifications

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

### Section Comment Styles

#### Major Sections (79 chars width)

Use full-width separator for top-level logical sections:

```zsh
# -----------------------------------------------------------------------------
# <Section Name>
# -----------------------------------------------------------------------------
# <Brief explanation of what this section does and why>
# <Any prerequisites or cross-references>
```

#### Subcategories (inline format)

Use short separator for subcategories within a major section:

```zsh
# --- <Category Name> ---
# <Brief explanation>
<code>
```

#### Inline Comments

For single-line explanations, no separator needed:

```zsh
# <description>
<code>
```

### Comment Width Standards

| Element | Width | Example |
|---------|-------|---------|
| Header separator (`# ===...`) | 79 chars | `# =============================================================================` |
| Section separator (`# ---...`) | 79 chars | `# -----------------------------------------------------------------------------` |
| Subcategory separator | Variable | `# --- Recording ---` |

### Comment Formatting Rules

1. **Field names use colon suffix**: `Loaded by:`, `Load order:`, `Prerequisites:`
2. **Prerequisites always plural**: Use `Prerequisites:` (not `Prerequisite:`)
3. **Do NOT add uses arrow format**: `→ Put these in <file> (<reason>)`
4. **List items are indented**: 2 spaces for numbered, 2 spaces + dash for prerequisites

## Coding Standards

### Key Conventions

1. **Idempotency**: All conf.d files must be safe to source multiple times
2. **Numeric Prefixes**: Files load in lexicographic order (00-99)
3. **Single Concern**: Each file handles one logical area
4. **Prerequisites**: Document dependencies on other files
5. **Cross-References**: Mention related files in comments

### Alignment Spaces (CRITICAL)

**Rule: Do NOT use extra spaces for visual alignment.**

Use exactly one space before inline comments (`#`). Never add padding spaces
to align comments vertically.

**❌ AVOID - Alignment spaces before comments:**
```zsh
setopt AUTO_CD              # type a directory name to cd
setopt AUTO_PUSHD           # cd pushes old dir onto stack
```

**✅ CORRECT - Single space:**
```zsh
setopt AUTO_CD # type a directory name to cd
setopt AUTO_PUSHD # cd pushes old dir onto stack
```

**Rationale:**
1. Alignment spaces create unnecessary diff noise
2. Single space is more consistent and predictable
3. Easier to maintain without formatting tools

### Variable Assignment

根据变量用途选择正确的赋值模式：

**规则 1: XDG 基础变量和工具路径 → 使用 `${VAR:-default}`**

```zsh
# ✅ 正确 - 带回退，尊重用户可能的自定义
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export GOPATH="${GOPATH:-$XDG_DATA_HOME/go}"
```

**规则 2: 配置文件路径（XDG 已保证存在） → 使用 `$VAR`**

```zsh
# ✅ 正确 - 简洁，XDG 变量已在 .zshenv 中设置
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/rc"
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship.toml"
```

**规则 3: 变量拼接或边界需要 → 使用 `${VAR}`**

```zsh
# ✅ 正确 - 花括号明确变量边界
export PATH="${GOPATH}/bin:${PATH}"
export VIMINIT="source ${XDG_CONFIG_HOME}/vim/vimrc"
```

**❌ 错误模式**

```zsh
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

### Conditional Logic

```zsh
# ✅ CORRECT - explicit if statements
if [[ -d "$dir" ]]; then
    source "$dir/init.zsh"
fi

# ❌ AVOID - [[ ]] && cmd fails under set -e when condition is false
[[ -d "$dir" ]] && source "$dir/init.zsh"

# ❌ AVOID - [[ ]] && { compound; return } pattern
[[ -z "$1" ]] && { echo "Usage: cmd <arg>"; return 1 }

# ✅ CORRECT - explicit if for validation
if [[ -z "$1" ]]; then
    echo "Usage: cmd <arg>"
    return 1
fi

# ✅ ACCEPTABLE - && for success-dependent chaining (mkdir && cd)
mkdir -p "$dir" && cd "$dir"
```

### Conditional Tests

```zsh
# ✅ CORRECT - use [[ ]] for conditional tests
[[ -f "$file" ]]
[[ "$var" == "pattern" ]]
[[ -n "$value" ]]

# ❌ AVOID - [ ] or test command
[ -f "$file" ]
test -f "$file"
```

### Arithmetic Operations

```zsh
# ✅ CORRECT - use (( )) for arithmetic
(( count++ ))
(( total = a + b ))
local result=$(( a * b ))

# ❌ AVOID - let or expr
let count+=1
result=$(expr $a + $b)
```

### Command Substitution

```zsh
# ✅ CORRECT - use $() for command substitution
local dir=$(dirname "$file")

# ❌ AVOID - backticks (hard to nest, hard to read)
local dir=`dirname "$file"`
```

### Quoting

```zsh
# ✅ CORRECT - quote all variable expansions
rm -- "${file}"
grep -- "${pattern}" "${file}"
source "${config_file}"

# ❌ WRONG - unquoted variables (word-splitting and glob expansion risks)
rm ${file}
grep ${pattern} ${file}
```

### Output Commands

```zsh
# ✅ CORRECT - printf for portability and predictability
printf '%s\n' "$message"
printf 'Status: %s\n' "$status"

# ✅ CORRECT - zsh-idiomatic print for line-by-line output
print -l $path  # Print each path element on its own line
print -P '%F{green}Success%f'  # With prompt expansion for colors

# ❌ AVOID - echo -e has inconsistent behavior across shells
echo -e "$message"
```

### Array Management

```zsh
# PATH arrays - highest priority first, preserve existing
path=(
  "$HOME/.local/bin"
  /opt/homebrew/bin
  $path  # preserve system PATH
)

# Deduplication - ensures idempotency
typeset -U path fpath manpath

# Array iteration
for item in "${array[@]}"; do
  echo "$item"
done
```

### Safe Globbing

```zsh
# (N) qualifier: silently skip if no matches
for file in "$dir"/*.zsh(N); do
  source "$file"
done
```

### Security Practices

```zsh
# ✅ CORRECT - check file existence before sourcing
if [[ -f "${file}" ]]; then
    source "${file}"
fi

# ✅ CORRECT - use allowlist for eval (when absolutely necessary)
local -a allowed=(rbenv pyenv nodenv)
if (( ${allowed[(I)${tool}]} )); then
    eval "$(${tool} init -)"
fi

# ❌ DANGEROUS - never eval untrusted input
eval "${user_input}"

# ✅ CORRECT - check before operations
[[ -f "$file" ]] && rm -- "$file"
```

## Plugin System

This configuration uses **Zinit** plugin manager installed at `$XDG_DATA_HOME/zinit/`.

### Installed Plugins (via Zinit turbo mode)

| Plugin | Purpose |
|--------|---------|
| fast-syntax-highlighting | Real-time syntax highlighting |
| zsh-history-substring-search | Enhanced Ctrl-R history search |
| zsh-autosuggestions | Autosuggestions from history |
| fzf-tab | fzf-powered completion menu |
| zsh-completions | Additional completion definitions |
| autopair | Auto-close brackets and quotes |

See `40-plugins.zsh` for plugin configuration.

## Code Quality

### Validation Commands

**Test configuration loads without errors:**
```bash
# Quick syntax check
zsh -n .config/zsh/.zshrc

# Full load test
zsh -c 'source ~/.zshenv && source $ZDOTDIR/.zshrc && echo "✅ OK"'

# Verify XDG paths
zsh -c 'echo "CONFIG: $XDG_CONFIG_HOME" && echo "DATA: $XDG_DATA_HOME"'
```

**Check for common issues:**
```bash
# Find unquoted variable expansions (potential issues)
grep -rn '\$[A-Za-z_][A-Za-z0-9_]*[^}]' .config/zsh/conf.d --include="*.zsh" | grep -v '"\${' | head -20

# Find [[ ]] && patterns that should be if statements
grep -rn '\[\[.*\]\] &&' .config/zsh/conf.d --include="*.zsh"

# Check for echo -e (should use printf)
grep -rn 'echo -e' .config/zsh/conf.d --include="*.zsh"
```

**Check file header completeness:**
```bash
# Verify Time-stamp presence
grep -rn "^# Time-stamp:" .config/zsh/conf.d --include="*.zsh" | wc -l

# Verify Load order field
grep -rn "^# Load order:" .config/zsh/conf.d --include="*.zsh" | wc -l

# Verify Responsibilities field
grep -rn "^# Responsibilities:" .config/zsh/conf.d --include="*.zsh" | wc -l
```

### Compliance Checklist

- [ ] File header follows template with all required fields
- [ ] Time-stamp in format `<YYYY-MM-DD HH:MM:SS Day by Author>`
- [ ] Load order uses numbered format: `<num> (after X, before Y)`
- [ ] Prerequisites field uses plural with dash list
- [ ] Do NOT add uses arrow reference format
- [ ] Section separators are 79 characters
- [ ] All variables are quoted
- [ ] Conditional logic uses explicit `if` statements
- [ ] No `echo -e` (use `printf` or `print`)
- [ ] No unquoted variable expansions
- [ ] Idempotent (safe to source multiple times)

### Current Status

**Compliance Level:** 100% (as of 2026-03-17)

**Key metrics:**
- Naming convention violations: 0
- File header violations: 0
- Documentation gaps: 0
- Configuration load errors: 0

### Troubleshooting

**Shell fails to load:**
```bash
zsh -x  # Enable xtrace to see each command
```

**Zinit issues:**
```bash
# Reinstall Zinit
rm -rf $XDG_DATA_HOME/zinit
zsh  # Will auto-install on next start
```

**Completion not working:**
```bash
# Rebuild completion cache
rm -f $ZDOTDIR/.zcompdump*
exec zsh
```

**Slow startup:** Check which plugins are loading:
```bash
zsh -x 2>&1 | head -100
```

## Best Practices

### 1. Keep Modules Focused

Each conf.d file should handle one logical area. If a file grows beyond 100 lines, consider splitting.

**Examples from the codebase:**
- `00-env.zsh` - Only environment variables and tool redirects
- `20-aliases.zsh` - Only aliases, no functions
- `40-plugins.zsh` - Only plugin declarations and lightweight config

### 2. Use Explicit Conditionals

Avoid the `[[ ]] && cmd` pattern - it's error-prone under `set -e`:

```zsh
# ❌ AVOID
[[ -n "$var" ]] && log_warn "message"

# ✅ CORRECT
if [[ -n "$var" ]]; then
    log_warn "message"
fi
```

### 3. Test Before Committing

**Always run these commands before committing changes:**

```bash
# 1. Syntax check
zsh -n .config/zsh/.zshrc

# 2. Full load test
zsh -c 'source ~/.zshenv && source $ZDOTDIR/.zshrc && echo "✅ OK"'

# 3. Check for common issues
grep -rn 'echo -e' .config/zsh/conf.d --include="*.zsh"
grep -rn '\[\[.*\]\] &&' .config/zsh/conf.d --include="*.zsh"
```

**Why**: Catches common issues before they reach the repository.

### 4. Local Configuration

Use `99-local.zsh` for machine-specific settings:

```zsh
# 99-local.zsh - NOT tracked by git
# Local overrides for this machine only

# Example: Work-specific environment
export WORK_API_KEY="secret"

# Example: Additional aliases
alias workvpn='sudo openvpn --config /etc/openvpn/work.conf'
```

### 5. Idempotent Operations

Ensure all configuration is safe to source multiple times:

```zsh
# ✅ CORRECT - typeset -U prevents duplicates
typeset -U path fpath manpath

# ✅ CORRECT - check before adding
if (( ! ${path[(I)$HOME/.local/bin]} )); then
    path=("$HOME/.local/bin" $path)
fi

# ❌ WRONG - adds duplicate on each source
path=("$HOME/.local/bin" $path)
```

## Quick Reference Card

### Essential Rules

1. **File Header:** Must include Time-stamp, Description, Loaded by, Load order, Responsibilities
2. **Conditionals:** Use explicit `if` statements, avoid `[[ ]] && cmd`
3. **Quoting:** Quote ALL variable expansions
4. **Output:** Use `printf` or `print`, never `echo -e`
5. **Tests:** Use `[[ ]]` not `[ ]`
6. **Arithmetic:** Use `(( ))` not `let` or `expr`
7. **Substitution:** Use `$()` not backticks
8. **Separators:** `# ===` for headers, `# ---` for sections (79 chars)
9. **Arrays:** Use `typeset -U` for deduplication
10. **Globbing:** Use `(N)` qualifier to skip no-match errors

### Pre-Commit Checklist

- [ ] Configuration loads: `zsh -c 'source ~/.zshenv && source $ZDOTDIR/.zshrc'`
- [ ] No syntax errors: `zsh -n .config/zsh/.zshrc`
- [ ] Headers complete: Time-stamp, Load order, Responsibilities
- [ ] All variables quoted
- [ ] No `echo -e` (use `printf`)
- [ ] No `[[ ]] && cmd` patterns (use `if`)

### Common Patterns

```zsh
# Safe file sourcing
if [[ -f "${file}" ]]; then
    source "${file}"
fi

# Safe glob iteration
for f in "$dir"/*.zsh(N); do
    source "$f"
done

# Idempotent PATH addition
typeset -U path
path=("$HOME/.local/bin" $path)

# Environment variable with default
export MY_VAR="${MY_VAR:-$XDG_DATA_HOME/myapp}"

# Conditional with explicit if
if [[ -n "$value" ]]; then
    printf '%s\n' "$value"
fi

# Array deduplication
typeset -U path fpath manpath

# Zsh-idiomatic output
print -l $path              # Each element on new line
print -P '%F{green}OK%f'    # Colored output
printf '%s\n' "$message"    # Portable output
```

### Startup File Quick Reference

| File | When | What to put here |
|------|------|------------------|
| `.zshenv` | Always | ZDOTDIR, minimal XDG setup |
| `.zprofile` | Login | PATH, env vars, SSH agent |
| `.zshrc` | Interactive | Aliases, functions, plugins, prompt |
