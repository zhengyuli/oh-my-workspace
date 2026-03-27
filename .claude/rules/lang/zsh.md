---
globs:
  - "**/*.zsh"
  - "**/.zshrc"
  - "**/.zprofile"
  - "**/.zshenv"
  - "**/zshrc"
  - "**/zprofile"
  - "**/zshenv"
  - "**/zlogin"
---

# Zsh-Specific Conventions

Zsh-specific features extending the common shell practices.

## File Header (MANDATORY)

```zsh
#!/usr/bin/env zsh
# script.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# Script Title - Brief Description
#
# Location: $WORKSPACE_DIR/path/to/script.zsh
# Usage: ./script.zsh [options]
# Dependencies: zsh 5.0+
# References:
#   1. Official documentation URL
# =============================================================================
```

**Shebang**: `#!/usr/bin/env zsh` (use `env` for portability)

## Delimiter Hierarchy (MANDATORY)

**Level 0** (File Header): `# ===` * 77 (79 chars)
**Level 1** (Primary Section): `# ---` * 77 (79 chars)
**Level 2** (Subsection): `# --- Title ---` (inline style)

**Example:**
```zsh
#!/usr/bin/env zsh
# =============================================================================
# Setup Script
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------
readonly SCRIPT_NAME="$(basename "$0")"

# -----------------------------------------------------------------------------
# Functions
# -----------------------------------------------------------------------------

# --- Logging ---
log_info() {
  printf '[info] %s\n' "$1"
}

# --- Validation ---
validate_package() {
  local -r pkg="$1"
  [[ -d "$SCRIPT_DIR/$pkg" ]] || return 1
}
```

## Error Handling

**Strict Mode** (MANDATORY for scripts):
```zsh
set -euo pipefail
```

- `-e` - Exit on non-zero status
- `-u` - Treat unset variables as error
- `-o pipefail` - Pipeline fails on first error

**ERR Trap**:
```zsh
_err_handler() {
  printf '[error] %s: line %d: exit %d\n' \
    "${funcstack[2]:-main}" "${funcfiletrace[1]##*:}" "$?" >&2
}
trap '_err_handler' ERR
```

**Exit Codes**: `0` success, `1` error, `2` misuse, `126` not executable, `127` not found

**Note**: For interactive `.zshrc` files, **do not** use `set -e` — it causes unexpected exits on failed commands (e.g. `grep` finding no match).

## Comments & Patterns

**Comments**: Explain WHY, not WHAT. Use separate lines.

```zsh
# Validate package exists before stow operations
_validate_package() {
  local -r pkg="$1"
  [[ -d "$SCRIPT_DIR/$pkg" ]] || return 1
}
```

**Variables**: Always quote, always local in functions
```zsh
# CORRECT
_process_file() {
  local -r input="$1"
  rm -rf "$dir"
}

# WRONG
_process_file() {
  input=$1
  rm -rf $dir
}
```

**Conditionals**: `[[ ]]` for strings. `(( ))` for arithmetic
```zsh
if [[ "$var" == "value" ]]; then  # String
if (( count > 0 )); then          # Arithmetic
```

**Default Values**: `${VAR:-default}`
```zsh
WORKSPACE_DIR="${WORKSPACE_DIR:-$(pwd)}"
```

**Output**:
- Use `printf` over `echo` — `echo` behavior varies across shells and BSD/GNU
  implementations; `printf` is predictable and POSIX-compliant
- All error and warning messages must go to stderr; stdout is for program output

```zsh
# Correct — errors to stderr, output to stdout
printf 'error: %s not found\n' "$pkg" >&2
printf '%s\n' "$result"
```

**Naming**:
- Constants and exported variables: `UPPER_SNAKE_CASE`
- Local and temporary variables: `lower_snake_case`

```zsh
readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"  # constant
local temp_file                                         # local variable
```

**Formatting Rules:**
- 2-space indentation (never tabs)
- Never align values with spaces
- Never use inline comments for explanations
- Avoid `A || B` and `A && B` patterns, prefer `if-else` for clarity
- Long pipelines: split at `|` with each stage on its own line

```zsh
# WRONG - aligned
SCRIPT_NAME="$(basename "$0")"
SCRIPT_DIR ="$(cd "$(dirname "$0")" && pwd)"

# WRONG - short-circuit logic (hard to read)
[[ -f "$file" ]] && cat "$file"
[[ -d "$dir" ]] || mkdir -p "$dir"

# WRONG - long pipeline on one line
find . -name '*.zsh' | xargs grep 'TODO' | sort | uniq

# CORRECT
SCRIPT_NAME="$(basename "$0")"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# CORRECT - explicit conditionals (clear intent)
if [[ -f "$file" ]]; then
  cat "$file"
fi

if [[ ! -d "$dir" ]]; then
  mkdir -p "$dir"
fi

# CORRECT - pipeline split across lines
find . -name '*.zsh' \
  | xargs grep 'TODO' \
  | sort \
  | uniq
```

## Functions

**Single Responsibility**: One thing per function
```zsh
_validate_package() { ... }  # Validation only
_install_package() { ... }   # Installation only
```

**Parameter Validation**: Validate at start
```zsh
_validate_package() {
  local -r pkg="$1"
  if [[ -z "$pkg" ]]; then
    printf 'error: package required\n' >&2
    return 1
  fi
}
```

**`main()` function**: For scripts longer than ~20 lines, wrap all logic in
`main()` to allow function hoisting and make the entry point explicit:

```zsh
main() {
  local -r pkg="$1"
  _validate_package "$pkg"
  _install_package "$pkg"
}

main "$@"
```

**`local` + command substitution**: Always declare and assign on separate
lines when the right-hand side is a command substitution. `local` is itself a
command that always exits 0, so `local var="$(cmd)"` masks `cmd`'s failure —
`set -e` will NOT catch it:

```zsh
# WRONG — local masks the exit code of dirname
local dir="$(dirname "$file")"

# CORRECT — exit code of dirname is preserved
local dir
dir="$(dirname "$file")"
```

**Command existence check**: Use `command -v` (POSIX) not `which` (non-POSIX,
behavior varies across systems):

```zsh
if command -v emacs >/dev/null 2>&1; then
  emacs "$@"
fi
```

## Zsh-Specific Features

### Array Indexing

**Arrays start at 1, not 0:**
```zsh
arr=(one two three)
echo $arr[1]     # "one" (not "two" as in bash)
echo $arr[2]     # "two"
echo $arr[-1]    # "three" (last element)
```

### typeset vs declare

Zsh prefers `typeset`:
```zsh
typeset -a my_array
typeset -A my_hash

# Also valid
declare -a my_array
declare -A my_hash
```

### Globbing Extensions

```zsh
# Recursive glob
files=(**/*.sh)

# Exclude pattern
files=(^*.test.sh)

# Numeric sort
files=(*(n))

# By modification time
files=*(.om[1,5])   # 5 most recently modified files
```

### Parameter Expansion

```zsh
# Array length
arr=(a b c)
echo ${#arr}        # 3 (number of elements)
echo ${#arr[1]}     # 1 (length of first element)

# Modify on expansion
echo ${arr:u}       # Uppercase
echo ${arr:l}       # Lowercase

# Split/join
str="a:b:c"
arr=(${(s/:/)str})  # Split by :
echo ${(j/-/)arr}   # Join with -
```

### Associative Arrays

```zsh
typeset -A config
config=(
  editor emacs
  shell zsh
  term ghostty
)

# Access
echo $config[editor]

# Iterate
for key value in ${(kv)config}; do
  echo "$key = $value"
done
```

## Security

### Avoid eval

Never use `eval` in scripts — it executes arbitrary strings and bypasses all
input validation. Use `case` for dispatch instead:

```zsh
# Dangerous — arbitrary code execution
eval "$user_input"

# Safe — explicit dispatch
case "$user_input" in
  install)   _install ;;
  uninstall) _uninstall ;;
  *)         printf 'error: unknown command: %s\n' "$user_input" >&2; exit 1 ;;
esac
```

### File Permissions

When scripts create or manage files, set permissions explicitly:

```zsh
# Set script executable
chmod 755 "$script_file"

# Verify secret file permissions (cross-platform)
# find returns output only if permissions match; empty = wrong perms
if [[ -z "$(find "$secret_file" -maxdepth 0 -perm 0600 2>/dev/null)" ]]; then
  printf 'error: %s must be 600\n' "$secret_file" >&2
  chmod 600 "$secret_file"
fi
```

> **Note:** Use `find -perm` for portable permission checks — avoid `stat -f`
> (macOS-only) and `stat -c` (Linux-only).

Recommended permissions:

| File Type    | Octal |
|--------------|-------|
| Scripts      | 755   |
| Config files | 644   |
| SSH keys     | 600   |
| Secret files | 600   |

### Secrets in Scripts

Never hardcode secrets. Read from environment variables with safe defaults:

```zsh
# Bad — hardcoded
API_KEY="sk-1234567890"

# Good — from environment, empty default if unset
API_KEY="${API_KEY:-}"
```

## Validation

```zsh
zsh -n script.zsh      # Syntax check
shellcheck script.sh   # If installed
```
