---
paths:
  - "**/*.sh"
  - "**/*.zsh"
  - "**/conf.d/*"
---

# Shell Coding Style

> This file extends [../common/coding-style.md](../common/coding-style.md) with shell-specific standards.

## Variable Assignment

Choose the correct assignment pattern based on variable purpose.

### Rule 1: XDG Base Variables and Tool Paths → Use `${VAR:-default}`

```zsh
# With fallback, respects potential user customization
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export GOPATH="${GOPATH:-$XDG_DATA_HOME/go}"
```

### Rule 2: Config File Paths (XDG guaranteed) → Use `$VAR`

```zsh
# Concise, XDG variables already set in .zshenv
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/rc"
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship.toml"
```

### Rule 3: Variable Concatenation or Boundary Needed → Use `${VAR}`

```zsh
# Braces clarify variable boundaries
export PATH="${GOPATH}/bin:${PATH}"
export VIMINIT="source ${XDG_CONFIG_HOME}/vim/vimrc"
```

### Anti-patterns

```zsh
# Avoid unnecessary braces (when no boundary ambiguity)
export STARSHIP_CONFIG="${XDG_CONFIG_HOME}/starship.toml"  # Redundant

# Avoid fallback for guaranteed variables
export RIPGREP_CONFIG_PATH="${XDG_CONFIG_HOME:-$HOME/.config}/ripgrep/rc"  # Redundant
```

### Decision Tree

```
Need to set variable?
├── User may have customized? → YES → ${VAR:-default}
│
└── NO → XDG variable reference?
    ├── YES → Non-separator char follows? → YES → ${VAR}
    │       └── NO → $VAR (concise)
    └── NO → Direct assignment
```

## Conditional Logic

> **See [../common/coding-style.md](../common/coding-style.md) for base patterns (inline comments, alignment, line length).**

```zsh
# CORRECT - explicit if statements
if [[ -d "$dir" ]]; then
    source "$dir/init.zsh"
fi

# AVOID - [[ ]] && cmd fails under set -e when condition is false
[[ -d "$dir" ]] && source "$dir/init.zsh"

# AVOID - [[ ]] && { compound; return } pattern
[[ -z "$1" ]] && { echo "Usage: cmd <arg>"; return 1 }

# CORRECT - explicit if for validation
if [[ -z "$1" ]]; then
    printf '%s\n' "Usage: cmd <arg>"
    return 1
fi

# ACCEPTABLE - && for success-dependent chaining (mkdir && cd)
mkdir -p "$dir" && cd "$dir"
```

## Conditional Tests

```zsh
# CORRECT - use [[ ]] for conditional tests
[[ -f "$file" ]]
[[ "$var" == "pattern" ]]
[[ -n "$value" ]]

# AVOID - [ ] or test command
[ -f "$file" ]
test -f "$file"
```

## Arithmetic Operations

```zsh
# CORRECT - use (( )) for arithmetic
(( count++ ))
(( total = a + b ))
local result=$(( a * b ))

# AVOID - let or expr
let count+=1
result=$(expr $a + $b)
```

## Command Substitution

```zsh
# CORRECT - use $() for command substitution
local dir=$(dirname "$file")

# AVOID - backticks (hard to nest, hard to read)
local dir=`dirname "$file"`
```

## Quoting

```zsh
# CORRECT - quote all variable expansions
rm -- "${file}"
grep -- "${pattern}" "${file}"
source "${config_file}"

# WRONG - unquoted variables (word-splitting and glob expansion risks)
rm ${file}
grep ${pattern} ${file}
```

## Output Commands

```zsh
# CORRECT - printf for portability and predictability
printf '%s\n' "$message"
printf 'Status: %s\n' "$status"

# CORRECT - zsh-idiomatic print for line-by-line output
print -l $path  # Print each path element on its own line
print -P '%F{green}Success%f'  # With prompt expansion for colors

# AVOID - echo has inconsistent behavior across shells
echo "$message"
echo -e "$message"
```

## Array Management

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
  printf '%s\n' "$item"
done
```

## Safe Globbing

```zsh
# (N) qualifier: silently skip if no matches
for file in "$dir"/*.zsh(N); do
  source "$file"
done
```

## Error Handling

```zsh
# CORRECT - scoped to specific operations
(
    set -e
    cmd1
    cmd2
)

# AVOID globally in interactive shells
set -e  # Can cause unexpected exits
```

**Rationale:** `set -e` causes scripts to exit on any command failure, which can be unpredictable in interactive shells or complex scripts.

## Idempotent Operations

All shell configurations must be safe to source multiple times:

```zsh
# PATH deduplication
typeset -U path fpath manpath

# Array assignment preserves existing
path=(
  "$HOME/.local/bin"
  $path  # preserve existing
)

# Conditional hook registration
if (( ! ${+hooks[directory-history-chdir]} )); then
    autoload -Uz chpwd && add-zsh-hook chpwd omw/chpwd-handler
fi

# Safe alias definition (aliases are idempotent)
alias ll='ls -la'

# Safe option setting (setopt is idempotent)
setopt AUTO_CD
```

## Zsh Configuration File Structure

### File Header Template

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

### Numeric Prefix Loading

Zsh conf.d files use numeric prefixes for load order:

```
00-env.zsh       # Environment variables first
10-path.zsh      # PATH setup
20-tools.zsh     # Tool configuration
30-aliases.zsh   # Aliases
40-completion.zsh # Completion setup
```

### Section Comment Styles

#### Major Sections (79 chars width)

```zsh
# -----------------------------------------------------------------------------
# <Section Name>
# -----------------------------------------------------------------------------
# <Brief explanation of what this section does and why>
# <Any prerequisites or cross-references>
<code>
```

**Rules (CRITICAL):**
1. **No sandwiching**: description always goes AFTER the closing separator
2. **No double separators**: exactly ONE opening + ONE closing `---` separator per section
3. **Description is optional**: omit if the section name is self-explanatory

#### Subcategories (inline format)

```zsh
# --- <Category Name> ---
# <Brief explanation>
<code>
```

**Rules (CRITICAL):**
1. **No blank line** between the `# --- ---` header and the following description/code
2. **Always at column 0**: never indent `# --- ---` headers, even when inside blocks

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
