---
paths:
  - "**/*.sh"
  - "**/*.bash"
---

# Bash Conventions

Bash-specific features and universal shell practices.

## File Header

```bash
#!/usr/bin/env bash
# script.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
#
# =============================================================================
# Script Title - Brief Description
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: keyword1, keyword2
# Dependencies: bash 4.3+
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   2026-MM-DD HH:MM zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   Detailed description of what this script does.
# =============================================================================
```

**Shebang**: `#!/usr/bin/env bash` is required only for directly executable
scripts.  Sourced files (function libraries, completion scripts, env setup)
must omit the shebang.

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...` (79 chars)
**Level 1** (Primary Section): `# -----------...` (79 chars)
**Level 2** (Subsection): `# --- Title ---`

Blank line is required after every Level 1 closing line before code.

```bash
# Level 0 (file header — shown in File Header section above)

# Level 1 (primary section)
# -----------------------------------------------------------------------------
# Section Title
# -----------------------------------------------------------------------------
# ← blank line required here

# Level 2 (subsection)
# --- Subsection Title ---
```

## Line Length

79 characters maximum.

## Error Handling

### Strict Mode (MANDATORY for scripts)

```bash
set -euo pipefail
```

- `-e` - Exit on non-zero status
- `-u` - Treat unset variables as error
- `-o pipefail` - Pipeline fails on first error

**Note**: For interactive `.bashrc` files, **do not** use `set -e`.

### ERR Trap

```bash
_err_handler() {
  local -r code=$?
  printf '[error] %s() line %d: exit %d\n' \
    "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
}
trap '_err_handler' ERR
```

### Exit Codes

`0` success, `1` error, `2` misuse, `126` not executable, `127` not found

## Code Patterns

### Variable Handling

Quote all variables, use `local` scope in functions.

```bash
_process_file() {
  local -r input="$1"
  rm -rf "$dir"
}
```

### Conditionals

Use `[[ ]]` for strings and `(( ))` for arithmetic.

```bash
if [[ "$var" == "value" ]]; then  # String
if (( count > 0 )); then          # Arithmetic
```

### Default Values

Use `${VAR:-default}` pattern.

```bash
WORKSPACE_DIR="${WORKSPACE_DIR:-$(pwd)}"
```

### Output Standards

Use `printf` over `echo` (POSIX-compliant). Direct errors to stderr.

```bash
printf 'error: %s not found\n' "$pkg" >&2
```

### Naming Conventions

Constants: `UPPER_SNAKE_CASE`, Local: `lower_snake_case`

```bash
readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
local temp_file
```

### Formatting

Split long pipelines at `|` with each stage on its own line.

### Section Uniqueness

Each section title must be unique within the file at every delimiter level (Level 1 and Level 2). Group related settings together — do not create multiple sections of the same name.

```bash
# WRONG — duplicate section at Level 2
# --- Paths ---
export PATH_A="/a"
# --- Other Config ---
export FOO="bar"
# --- Paths ---              ← same name reused
export PATH_B="/b"

# CORRECT
# --- Paths ---
export PATH_A="/a"
export PATH_B="/b"
# --- Other Config ---
export FOO="bar"
```

### Validation at Boundaries

Validate inputs at system boundaries (user input, file reads, API responses).

```bash
_process_user_input() {
  local -r input="$1"
  if [[ -z "${input// /}" ]]; then
    printf 'error: input cannot be empty\n' >&2
    return 1
  fi
}
```

### Nesting Limit

Maximum 3 nesting levels. Use early-return guards to flatten structure.

```bash
# WRONG — 4 levels deep
if [[ -f "$file" ]]; then
  if [[ -r "$file" ]]; then
    if [[ -s "$file" ]]; then
      if grep -q "pattern" "$file"; then
        process "$file"
      fi
    fi
  fi
fi

# CORRECT — early returns, 2 levels max
if [[ ! -f "$file" ]]; then
  return 0
fi
if [[ ! -r "$file" ]] || [[ ! -s "$file" ]]; then
  return 0
fi
if grep -q "pattern" "$file"; then
  process "$file"
fi
```

### No Magic Numbers

Never use bare numeric literals. Use `readonly` named constants.

```bash
# WRONG
sleep 30
if [[ "$count" -gt 100 ]]; then
  ...
fi

# CORRECT
readonly DEFAULT_TIMEOUT=30
readonly MAX_THRESHOLD=100
sleep "$DEFAULT_TIMEOUT"
if [[ "$count" -gt "$MAX_THRESHOLD" ]]; then
  ...
fi
```

## Functions

### Single Responsibility

Each function does one thing only.

### Parameter Validation

Validate required parameters at the start.

```bash
_validate_package() {
  local -r pkg="$1"
  if [[ -z "$pkg" ]]; then
    printf 'error: package required\n' >&2
    return 1
  fi
}
```

### Main Function

For scripts longer than ~20 lines, wrap all logic in `main()`.

```bash
main() {
  local -r pkg="$1"
  _validate_package "$pkg"
  _install_package "$pkg"
}
main "$@"
```

### Local Command Substitution

Declare and assign on separate lines when RHS is command substitution.

```bash
local dir
dir="$(dirname "$file")"
```

## Anti-Patterns

### Don't: eval for User Input

```bash
# WRONG
eval "$user_input"

# CORRECT — explicit dispatch
case "$user_input" in
  install)   _install ;;
  uninstall) _uninstall ;;
  *)         printf 'error: invalid command: %s\n' "$user_input" >&2; exit 1 ;;
esac
```

### Don't: local Masks Exit Codes

```bash
# WRONG — local always exits 0
local dir="$(dirname "$file")"

# CORRECT — exit code preserved
local dir
dir="$(dirname "$file")"
```

### Don't: Short-Circuit Control Flow

Do not use `&&` or `||` as shorthand for conditionals.
They obscure control flow intent: `&&` means "proceed only on success"
and `||` means "proceed only on failure", but neither makes the
branching logic readable at a glance.

```bash
# WRONG — control flow hidden behind operators
[[ -f "$file" ]] && cat "$file"
[[ ! -d "$dir" ]] || mkdir -p "$dir"

# CORRECT — explicit conditionals make intent obvious
if [[ -f "$file" ]]; then
  cat "$file"
fi
if [[ ! -d "$dir" ]]; then
  mkdir -p "$dir"
fi
```

## Security

### Code Injection Prevention

Never use `eval` to execute user input. Use explicit `case` dispatch.

### Permission Management

| File Type    | Mode | Rationale                          |
|--------------|------|------------------------------------|
| Scripts      | 755  | Executable by owner/group/other    |
| Config files | 644  | Readable by all, writable by owner |
| SSH keys     | 600  | Accessible only by owner           |
| Secret files | 600  | Accessible only by owner           |

### Secrets Management

Read secrets from environment variables with safe defaults.

```bash
API_KEY="${API_KEY:-}"
```

## References

1. [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html)
2. [Bash Manual](https://www.gnu.org/software/bash/manual/)

## Validation

```bash
bash -n script.sh      # Syntax check
shellcheck script.sh   # Lint (if installed)
```
