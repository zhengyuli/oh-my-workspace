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

## File Header

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

## Delimiter Hierarchy

**Level 0** (File Header):
```
# =============================================================================
```

**Level 1** (Primary Section):
```
# ----------------------------------------------------------------------------
```

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
  print "[info] $1"
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
  print -u2 "[error] ${funcstack[2]:-main}: line ${funcfiletrace[1]##*:}: exit $?"
}
trap '_err_handler' ERR
```

**Exit Codes**: `0` success, `1` error, `2` misuse, `126` not executable, `127` not found

**Note**: For interactive `.zshrc` files, **do not** use `set -e` — it causes unexpected exits on failed commands (e.g. `grep` finding no match).

## Documentation & Code Patterns

### Comments

Explain rationale (WHY), not mechanics (WHAT). Document non-obvious
design decisions and constraints. Use separate comment lines for clarity.

```zsh
# Validate package exists before stow operations
_validate_package() {
  local -r pkg="$1"
  [[ -d "$SCRIPT_DIR/$pkg" ]] || return 1
}
```

### Variable Handling

Quote all variables, use `local` scope in functions:

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

### Conditionals

`[[ ]]` for strings, `(( ))` for arithmetic:

```zsh
if [[ "$var" == "value" ]]; then  # String
if (( count > 0 )); then          # Arithmetic
```

### Default Values

`${VAR:-default}`:

```zsh
WORKSPACE_DIR="${WORKSPACE_DIR:-$(pwd)}"
```

### Output Standards

Use zsh `print` built-in for output formatting.

**Rationale:**
- `print` is zsh-native with rich formatting options
- `print` provides consistent behavior and better integration with zsh features

Direct all error and warning messages to stderr; reserve stdout for program output:

```zsh
# CORRECT — errors to stderr, output to stdout
print -u2 "error: $pkg not found"
print "$result"

# WRONG — errors to stdout (breaks piping)
print "error: $pkg not found"
print "$result" >&2
```

### Naming Conventions

Constants and exported variables: `UPPER_SNAKE_CASE`
Local and temporary variables: `lower_snake_case`

```zsh
readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"  # constant
local temp_file                                         # local variable
```

### Formatting

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

### Single Responsibility

One thing per function:

```zsh
_validate_package() { ... }  # Validation only
_install_package() { ... }   # Installation only
```

### Parameter Validation

Validate at start:

```zsh
_validate_package() {
  local -r pkg="$1"
  if [[ -z "$pkg" ]]; then
    print -u2 "error: package required"
    return 1
  fi
}
```

### Main Function

For scripts longer than ~20 lines, wrap all logic in `main()` to allow
function hoisting and make the entry point explicit:

```zsh
main() {
  local -r pkg="$1"
  _validate_package "$pkg"
  _install_package "$pkg"
}

main "$@"
```

### Local Command Substitution

Always declare and assign on separate lines when the right-hand side is a
command substitution. `local` is itself a command that always exits 0, so
`local var="$(cmd)"` masks `cmd`'s failure — `set -e` will NOT catch it:

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

## Security

### Code Injection Prevention

**Prohibition**: Never use `eval` to execute user input or variable content.

**Rationale**: `eval` bypasses input validation and enables arbitrary code execution.

**Alternative Pattern**: Use explicit `case` dispatch for command routing.

```zsh
# Pattern: Explicit dispatch (not eval)
case "$user_input" in
  install)   _install ;;
  uninstall) _uninstall ;;
  *)         print -u2 "error: invalid command: $user_input"; exit 1 ;;
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

```zsh
# Pattern: Environment variable with safe default
API_KEY="${API_KEY:-}"
```

## Validation

```zsh
zsh -n script.zsh      # Syntax check
shellcheck script.sh   # If installed
```
