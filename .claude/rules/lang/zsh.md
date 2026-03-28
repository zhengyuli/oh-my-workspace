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

# Zsh Conventions

Zsh-specific features and universal shell practices.

# =============================================================================
## File Header
# =============================================================================

```zsh
#!/usr/bin/env zsh
# script.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Script Title - Brief Description
#
# Location: $WORKSPACE_DIR/path/to/script.zsh
# Usage: ./script.zsh [options]
# Dependencies: zsh 5.0+
# =============================================================================
```

**Shebang**: `#!/usr/bin/env zsh` (use `env` for portability)

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...` (79 chars)
**Level 1** (Primary Section): `# -----------...` (79 chars)
**Level 2** (Subsection): `# --- Title ---`

# -----------------------------------------------------------------------------
## Error Handling
# -----------------------------------------------------------------------------

### Strict Mode (MANDATORY for scripts)

```zsh
set -euo pipefail
```

- `-e` - Exit on non-zero status
- `-u` - Treat unset variables as error
- `-o pipefail` - Pipeline fails on first error

**Note**: For interactive `.zshrc` files, **do not** use `set -e` — it causes unexpected exits on failed commands (e.g., `grep` finding no match).

### ERR Trap (Zsh-specific)

```zsh
_err_handler() {
  print -u2 "[error] ${funcstack[2]:-main}: line ${funcfiletrace[1]##*:}: exit $?"
}
trap '_err_handler' ERR
```

### Exit Codes

`0` success, `1` error, `2` misuse, `126` not executable, `127` not found

# -----------------------------------------------------------------------------
## Code Patterns
# -----------------------------------------------------------------------------

### Comments

Explain WHY, not WHAT. Use separate comment lines.

```zsh
# Validate package exists before stow operations
_validate_package() { ... }
```

### Variable Handling

Quote all variables, use `local` scope in functions.

```zsh
# CORRECT
_process_file() {
  local -r input="$1"
  rm -rf "$dir"
}
```

### Conditionals

Use `[[ ]]` for strings and `(( ))` for arithmetic.

```zsh
if [[ "$var" == "value" ]]; then  # String
if (( count > 0 )); then          # Arithmetic
```

### Default Values

Use `${VAR:-default}` pattern.

```zsh
WORKSPACE_DIR="${WORKSPACE_DIR:-$(pwd)}"
```

### Output Standards

Use zsh `print` built-in for output formatting (zsh-native with rich options).

Direct errors to stderr, output to stdout.

```zsh
# CORRECT — errors to stderr
print -u2 "error: $pkg not found"
print "$result"
```

### Naming Conventions

Constants: `UPPER_SNAKE_CASE`, Local: `lower_snake_case`

```zsh
readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
local temp_file
```

### Formatting

- 2-space indentation (never tabs)
- Never align values with spaces
- Split long pipelines at `|` with each stage on its own line

# -----------------------------------------------------------------------------
## Functions
# -----------------------------------------------------------------------------

### Single Responsibility

Each function does one thing only.

### Parameter Validation

Validate required parameters at the start.

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

For scripts longer than ~20 lines, wrap all logic in `main()`.

```zsh
main() {
  local -r pkg="$1"
  _validate_package "$pkg"
  _install_package "$pkg"
}

main "$@"
```

### Local Command Substitution

Declare and assign on separate lines when RHS is command substitution.

```zsh
# CORRECT — exit code preserved
local dir
dir="$(dirname "$file")"
```

# -----------------------------------------------------------------------------
## Anti-Patterns
# -----------------------------------------------------------------------------

### Don't: eval for User Input

```zsh
# WRONG
eval "$user_input"

# CORRECT — explicit dispatch
case "$user_input" in
  install)   _install ;;
  uninstall) _uninstall ;;
  *)         print -u2 "error: invalid command: $user_input"; exit 1 ;;
esac
```

### Don't: local Masks Exit Codes

```zsh
# WRONG — local always exits 0
local dir="$(dirname "$file")"

# CORRECT — exit code preserved
local dir
dir="$(dirname "$file")"
```

### Don't: Short-Circuit Operators

```zsh
# WRONG — unclear intent
[[ -f "$file" ]] && cat "$file"
[[ ! -d "$dir" ]] || mkdir -p "$dir"

# CORRECT — explicit conditionals
if [[ -f "$file" ]]; then
  cat "$file"
fi

if [[ ! -d "$dir" ]]; then
  mkdir -p "$dir"
fi
```

# -----------------------------------------------------------------------------
## Security
# -----------------------------------------------------------------------------

### Code Injection Prevention

Never use `eval` to execute user input. Use explicit `case` dispatch.

### Permission Management

| File Type    | Mode | Rationale                        |
|--------------|------|----------------------------------|
| Scripts      | 755  | Executable by owner/group/other  |
| Config files | 644  | Readable by all, writable by owner |
| SSH keys     | 600  | Accessible only by owner         |
| Secret files | 600  | Accessible only by owner         |

### Secrets Management

Read secrets from environment variables with safe defaults.

```zsh
API_KEY="${API_KEY:-}"
```

# -----------------------------------------------------------------------------
## References
# -----------------------------------------------------------------------------

1. [Zsh Manual](http://zsh.sourceforge.net/Doc/)
2. [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html)

# -----------------------------------------------------------------------------
## Validation
# -----------------------------------------------------------------------------

```zsh
zsh -n script.zsh      # Syntax check
shellcheck script.sh   # Lint (if installed)
```
