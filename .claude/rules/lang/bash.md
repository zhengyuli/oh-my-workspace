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

## Documentation & Code Patterns

**Comment Philosophy**:
- Explain rationale (WHY), not mechanics (WHAT)
- Document non-obvious design decisions and constraints
- Use separate comment lines for clarity

```bash
# Validate package exists before stow operations
_validate_package() {
  local -r pkg="$1"
  [[ -d "$SCRIPT_DIR/$pkg" ]] || return 1
}
```

**Variable Handling**: Quote all variables, use `local` scope in functions
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

**Output Standards**:
- Prefer `printf` over `echo` for predictable output formatting
  - Rationale: `echo` behavior is inconsistent across shell implementations (BSD/GNU variants)
  - `printf` is POSIX-compliant with consistent behavior
- Direct all error and warning messages to stderr; reserve stdout for program output

```bash
# Correct — errors to stderr, output to stdout
printf 'error: %s not found\n' "$pkg" >&2
printf '%s\n' "$result"
```

**Naming Conventions**:
- Constants and exported variables: Require `UPPER_SNAKE_CASE`
- Local and temporary variables: Require `lower_snake_case`

```bash
readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"  # constant
local temp_file                                         # local variable
```

**Code Formatting:**
- 2-space indentation (never tabs)
- Never align values with spaces
- Never use inline comments for explanations
- Avoid `A || B` and `A && B` patterns, prefer `if-else` for clarity
- Long pipelines: split at `|` with each stage on its own line

```bash
# WRONG - aligned
SCRIPT_NAME="$(basename "$0")"
SCRIPT_DIR ="$(cd "$(dirname "$0")" && pwd)"

# WRONG - short-circuit operators (unclear intent)
[[ -f "$file" ]] && cat "$file"
[[ ! -d "$dir" ]] || mkdir -p "$dir"

# WRONG - long pipeline on one line
find . -name '*.sh' | xargs grep 'TODO' | sort | uniq

# CORRECT - explicit conditionals (clear intent)
if [[ -f "$file" ]]; then
  cat "$file"
fi

if [[ ! -d "$dir" ]]; then
  mkdir -p "$dir"
fi

# CORRECT - pipeline split across lines
find . -name '*.sh' \
  | xargs grep 'TODO' \
  | sort \
  | uniq
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

**`main()` function**: For scripts longer than ~20 lines, wrap all logic in
`main()` to allow function hoisting and make the entry point explicit:

```bash
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

```bash
# WRONG — local masks the exit code of dirname
local dir="$(dirname "$file")"

# CORRECT — exit code of dirname is preserved
local dir
dir="$(dirname "$file")"
```

**Command existence check**: Use `command -v` (POSIX) not `which` (non-POSIX,
behavior varies across systems):

```bash
if command -v emacs >/dev/null 2>&1; then
  emacs "$@"
fi
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

## Security

### Code Injection Prevention

**Prohibition**: Never use `eval` to execute user input or variable content.

**Rationale**: `eval` bypasses input validation and enables arbitrary code execution.

**Alternative Pattern**: Use explicit `case` dispatch for command routing.

```bash
# Pattern: Explicit dispatch (not eval)
case "$user_input" in
  install)   _install ;;
  uninstall) _uninstall ;;
  *)         printf 'error: invalid command: %s\n' "$user_input" >&2; exit 1 ;;
esac
```

### Permission Management

**Principles**:
- Set file permissions explicitly during creation
- Use portable permission checking (avoid platform-specific `stat` variants)
- Follow principle of least privilege for sensitive files

**Recommended Permissions**:

| File Type    | Mode | Rationale                        |
|--------------|------|----------------------------------|
| Scripts      | 755  | Executable by owner/group/other  |
| Config files | 644  | Readable by all, writable by owner |
| SSH keys     | 600  | Accessible only by owner         |
| Secret files | 600  | Accessible only by owner         |

### Secrets Management

**Principles**:
- Prohibit hardcoding credentials, API keys, or tokens in scripts
- Read secrets from environment variables with safe defaults
- Use `${VAR:-}` pattern to prevent errors from unset variables

```bash
# Pattern: Environment variable with safe default
API_KEY="${API_KEY:-}"
```

## Validation

```bash
bash -n script.sh      # Syntax check
shellcheck script.sh   # If installed
```
