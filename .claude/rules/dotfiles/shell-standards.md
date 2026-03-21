---
paths:
  - "**/*.sh"
  - "**/*.zsh"
  - "**/conf.d/*"
---

# Shell Script Standards

> This file extends [~/.claude/rules/common/coding-style.md](file://...) with dotfiles-specific shell script patterns.

## Variable Assignment

Choose the correct assignment pattern based on variable purpose.

**Rule 1: XDG Base Variables and Tool Paths -> Use `${VAR:-default}`**

```zsh
# With fallback, respects potential user customization
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export GOPATH="${GOPATH:-$XDG_DATA_HOME/go}"
```

**Rule 2: Config File Paths (XDG guaranteed) -> Use `$VAR`**

```zsh
# Concise, XDG variables already set in .zshenv
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/rc"
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship.toml"
```

**Rule 3: Variable Concatenation or Boundary Needed -> Use `${VAR}`**

```zsh
# Braces clarify variable boundaries
export PATH="${GOPATH}/bin:${PATH}"
export VIMINIT="source ${XDG_CONFIG_HOME}/vim/vimrc"
```

**Anti-patterns:**
```zsh
# Avoid unnecessary braces (when no boundary ambiguity)
export STARSHIP_CONFIG="${XDG_CONFIG_HOME}/starship.toml"  # Redundant

# Avoid fallback for guaranteed variables
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

## Conditional Logic

> **See [~/.claude/rules/common/coding-style.md](file://...) for base shell patterns (explicit if statements).**

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

## Security Practices

```zsh
# CORRECT - check file existence before sourcing
if [[ -f "${file}" ]]; then
    source "${file}"
fi

# CORRECT - use allowlist for eval (when absolutely necessary)
local -a allowed=(rbenv pyenv nodenv)
if (( ${allowed[(I)${tool}]} )); then
    eval "$(${tool} init -)"
fi

# DANGEROUS - never eval untrusted input
eval "${user_input}"

# CORRECT - check before operations
if [[ -f "$file" ]]; then
    rm -- "$file"
fi
```
