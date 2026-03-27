---
globs:
  - "**/*.sh"
  - "**/*.bash"
---

# Bash Conventions

Bash-specific features and universal shell practices.

## File Header (MANDATORY)

```bash
#!/usr/bin/env bash
# script.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>
# =============================================================================
# Script Title - Brief Description
#
# Location: $WORKSPACE_DIR/path/to/script.sh
# Usage: ./script.sh [options]
# Dependencies: bash 4.3+
# References:
#   1. Official documentation URL
# =============================================================================
```

**Shebang**: `#!/usr/bin/env bash` (use `env` for portability)

## Delimiter Hierarchy (MANDATORY)

**Level 0** (File Header): `# ===` * 77 (79 chars)
**Level 1** (Primary Section): `# ---` * 77 (79 chars)
**Level 2** (Subsection): `# --- Title ---` (inline style)

**Example:**
```bash
#!/usr/bin/env bash
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
```bash
set -euo pipefail
```

- `-e` - Exit on non-zero status
- `-u` - Treat unset variables as error
- `-o pipefail` - Pipeline fails on first error

**ERR Trap** (Bash-specific):
```bash
_err_handler() {
  local -r code=$?
  printf '[error] %s() line %d: exit %d\n' \
    "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
}
trap '_err_handler' ERR
```

**Exit Codes**: `0` success, `1` error, `2` misuse, `126` not executable, `127` not found

## Comments & Patterns

**Comments**: Explain WHY, not WHAT. Use separate lines.

```bash
# Validate package exists before stow operations
_validate_package() {
  local -r pkg="$1"
  [[ -d "$SCRIPT_DIR/$pkg" ]] || return 1
}
```

**Variables**: Always quote, always local in functions
```bash
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

**Conditionals**: `[[ ]]` for strings, `(( ))` for arithmetic
```bash
if [[ "$var" == "value" ]]; then  # String
if (( count > 0 )); then          # Arithmetic
```

**Default Values**: `${VAR:-default}`
```bash
WORKSPACE_DIR="${WORKSPACE_DIR:-$(pwd)}"
```

**Formatting Rules:**
- Never align values with spaces
- Never use inline comments for explanations
- Avoid `A || B` and `A && B` patterns, prefer `if-else` for clarity

```bash
# WRONG - aligned
SCRIPT_NAME="$(basename "$0")"
SCRIPT_DIR ="$(cd "$(dirname "$0")" && pwd)"

# WRONG - short-circuit operators (unclear intent)
[[ -f "$file" ]] && cat "$file"
[[ ! -d "$dir" ]] || mkdir -p "$dir"

# CORRECT - explicit conditionals (clear intent)
if [[ -f "$file" ]]; then
  cat "$file"
fi

if [[ ! -d "$dir" ]]; then
  mkdir -p "$dir"
fi
```

## Functions

**Single Responsibility**: One thing per function
```bash
_validate_package() { ... }  # Validation only
_install_package() { ... }   # Installation only
```

**Parameter Validation**: Validate at start
```bash
_validate_package() {
  local -r pkg="$1"
  if [[ -z "$pkg" ]]; then
    printf 'error: package required\n' >&2
    return 1
  fi
}
```

## Parameter Handling

**getopts**: Use for option parsing
```bash
while getopts ":hd" opt; do
  case "$opt" in
    h) _show_help; exit 0 ;;
    d) DRY_RUN=true ;;
    \?) printf 'error: invalid option -%s\n' "$OPTARG" >&2; exit 1 ;;
  esac
done
shift $((OPTIND - 1))
```

## Bash-Specific Features

### Namerefs (Bash 4.3+)

Use `local -n` for pass-by-reference:

```bash
_process_array() {
  local -n arr="$1"  # nameref
  local item

  for item in "${arr[@]}"; do
    printf '%s\n' "$item"
  done
}

declare -a my_array=(one two three)
_process_array my_array
```

### Associative Arrays

Use `declare -A` for key-value lookups

```bash
declare -A config=(
  [editor]="emacs"
  [shell]="zsh"
)

printf 'Editor: %s\n' "${config[editor]}"

for key in "${!config[@]}"; do
  printf '%s = %s\n' "$key" "${config[$key]}"
done
```

### Readonly Arrays

Use `readonly -a` for constant arrays

```bash
readonly -a PKG_ALL=(
  shell/zsh
  editor/emacs
  term/ghostty
)
```

### mapfile

Use `mapfile` instead of `while read` loop

```bash
# Good — safe with filenames containing spaces/newlines
mapfile -t files < <(find . -name '*.sh' -type f)

for f in "${files[@]}"; do
  bash -n "$f"
done
```

Key flags: `-t` (strip trailing newline), `-n N` (read at most N lines)

### Process Substitution

```bash
# Compare outputs
diff <(cmd1) <(cmd2)

# Read into array
mapfile -t lines < <(find . -name '*.sh')
```

### Coproc

For bidirectional communication

```bash
coproc backend {
  ./server.sh
}

printf 'request\n' >&${backend[1]}
read -r response <&${backend[0]}
```

## Debugging

### Bash-Specific Options

```bash
set -v       # Verbose - show commands as read
set -x       # xtrace - show commands after expansion
set -euxo pipefail  # Combined with strict mode
```

### Debugging Functions

```bash
_debug_caller() {
  printf '[debug] %s called from %s at line %d\n' \
    "${FUNCNAME[1]}" "${BASH_SOURCE[2]}" "${BASH_LINENO[1]}"
}
```

## Security

### Shell History

Prefix commands with a space to exclude them from history

```bash
export HISTCONTROL=ignoreboth

# Then sensitive commands starting with a space are not recorded
 API_KEY=secret my-command   # not saved to history
```

## Compatibility

### Avoid Bash-Only Features for Portable Scripts

If portability is required:
- Avoid `[[ ]]` (use `[ ]`)
- Avoid `(( ))` (use `$(( ))`)
- Avoid `local -n`
- Avoid associative arrays
- Use POSIX shell instead

### Check Bash Version

```bash
if (( BASH_VERSINFO[0] < 4 ||
      ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] < 3 ) )); then
  printf 'error: bash 4.3+ required\n' >&2
  exit 1
fi
```

## Validation

```bash
bash -n script.sh      # Syntax check
shellcheck script.sh   # If installed
```
