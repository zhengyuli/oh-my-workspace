---
paths:
  - "setup.sh"
  - "claude/**/*.sh"
  - "platform/**/*.sh"
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

**Title Case required**: capitalize the first letter of every word in both
Section Title and Subsection Title (e.g., `Git Status`, `Doom Modeline`).
Abbreviations follow their established convention: ALL CAPS for standard
abbreviations (e.g., `FZF Preview`, `PDF Tools`, `JSON Mode`), lowercase
for established lowercase names (e.g., `cc Mode`, `sh Mode`, `xref`).

### Blank Lines

Blank lines mark boundaries between delimiter levels and top-level statements.

**Around delimiters** — one blank line before Level 1 opening, one after
Level 1 closing.  Level 2 has no blank line after the delimiter — code follows
immediately.

```bash
# -----------------------------------------------------------------------------
# Package Management
# -----------------------------------------------------------------------------

# --- Core Packages ---
readonly CORE_PKGS=("git" "vim" "zsh")
readonly EXTRA_PKGS=("lazygit" "ripgrep")

# --- Optional Packages ---
readonly OPT_PKGS=("fzf" "bat")
```

**Between top-level statements within the same subsection** — one blank line.
Related statements (e.g., consecutive `export` or `readonly`) are not separated.

```bash
# --- Paths ---
export PATH="$HOME/.local/bin:$PATH"
export MANPATH="$HOME/.local/share/man:$MANPATH"

readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

_cleanup() { rm -f "$_tmp_file"; }
```

**Inside function bodies** — one blank line between logical steps.
Single-statement functions have no extra blank lines.

```bash
# Multi-step body
_install_package() {
  local -r pkg="$1"
  _validate_package "$pkg"

  printf 'installing %s...\n' "$pkg"
  sudo apt-get install -y "$pkg"
}

# Single-statement body — no extra blank lines
_cleanup() { rm -f "$_tmp_file"; }
```

**Prohibited**: two or more consecutive blank lines anywhere in the file.

## Indentation

2-space indent for block bodies (`if`, `for`, `while`, `case`, functions).
4-space indent for continuation lines that do not align to an opening
delimiter.

```bash
# Block indent — 2 spaces
if [[ -f "$file" ]]; then
  process "$file"
fi

# Continuation — align under opening [[ when possible
if [[ "$mode" == "install" ]] \
   && [[ -n "$pkg" ]]; then
  _install_package "$pkg"
fi

# Continuation — 2-space indent when no alignment anchor
printf 'error: cannot install %s — missing dependency %s\n' \
  "$pkg" \
  "$dep" \
  >&2
```

## Line Length

79 characters maximum.

Exceptions:

- URLs and file paths that cannot be wrapped
- Help text strings in `printf` / `echo` (user-facing output)

## Comments

Comments explain *why*, not *what*. The code itself should be readable
enough to show what it does.

**Comment syntax**: `#` followed by a single space.

**When to comment**:
- Non-obvious logic, workarounds, performance trade-offs
- Why a particular approach was chosen over alternatives
- Constraints or assumptions that are not self-evident

**When NOT to comment**:
- Self-documenting variable/function names
- Obvious operations (`# Increment count` before `(( count += 1 ))`)

**No end-of-line comments** — place comments on a separate line above
the code (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```bash
# WRONG — restates the code
# Increment count by 1
(( count += 1 ))

# CORRECT — explains the reasoning
# Retries are capped at 3 to avoid hammering a down service
(( count += 1 ))
```

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

### EXIT Trap (Cleanup)

Use `trap ... EXIT` for resource cleanup (temp files, lock files).

```bash
_cleanup() {
  rm -f "$_tmp_file"
}
trap '_cleanup' EXIT
```

### Exit Codes

`0` success, `1` error, `2` misuse, `126` not executable, `127` not found

## Code Patterns

### Variable Handling

Quote all variables, use `local` scope in functions.
Use `readonly` for top-level constants, `local -r` for function-scoped
immutables.

```bash
readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

_process_file() {
  local -r input="$1"
  _validate_input "$input"
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
Format strings must use **single quotes** (no variable expansion needed
in the format).

```bash
printf 'error: %s not found\n' "$pkg" >&2
```

### ANSI Escape Sequences

Use `$'...'` (ANSI-C quoting) for strings containing escape sequences
(`\033`, `\n`, `\t`, etc.).  Single-quoted `'\033'` stores literal text,
not the actual ESC byte — colors will print as raw text.

```bash
# CORRECT — $'...' converts \033 to the actual ESC character
readonly _RED=$'\033[0;31m'
readonly _RESET=$'\033[0m'
printf '  %s[error]%s %s\n' "$_RED" "$_RESET" "$msg"

# WRONG — single quotes store literal "\033[0;31m" text
readonly _RED='\033[0;31m'
```

### Naming Conventions

Constants: `UPPER_SNAKE_CASE`, Local: `lower_snake_case`.
Private/internal functions use `_` prefix; public entry functions (e.g.,
`main`) have no prefix.

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
if (( count > MAX_THRESHOLD )); then
  ...
fi
```

## Bash-Specific Patterns

### Arrays

Declare arrays explicitly. Always quote `"${arr[@]}"` to preserve
word-splitting safety.

```bash
# Indexed array
local -a pkgs=("git" "vim" "zsh")
for pkg in "${pkgs[@]}"; do
  _install_package "$pkg"
done

# Array length
local -r count="${#pkgs[@]}"

# Appending
pkgs+=("lazygit")

# Associative array (bash 4.3+)
declare -A defaults=(
  ["editor"]="vim"
  ["pager"]="less"
)
printf '%s\n' "${defaults["editor"]}"
```

### Line Continuation

Break long lines with `\`. For continued conditions, align operators
(`&&`, `||`) with the opening `[[`. For multi-value arguments (printf,
arrays), use 2-space indent.

```bash
# Long condition
if [[ "$mode" == "install" ]] \
   && [[ -n "$pkg" ]] \
   && [[ -r "$config_file" ]]; then
  _install_package "$pkg"
fi

# Long printf
printf 'error: cannot install %s — missing dependency %s\n' \
  "$pkg" \
  "$dep" \
  >&2
```

### Dry-Run Pattern

When a script supports dry-run, check the flag at the point of action,
not at the call site.

```bash
readonly DRY_RUN="${DRY_RUN:-0}"

_run() {
  if [[ "$DRY_RUN" -eq 1 ]]; then
    printf '[dry-run] %s\n' "$*"
    return 0
  fi
  "$@"
}
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
  install) _install ;;
  uninstall) _uninstall ;;
  *) printf 'error: invalid command: %s\n' "$user_input" >&2; exit 1 ;;
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

Do not use `&&` or `||` as shorthand for conditionals on **standalone
statements**.  They obscure control flow intent: `&&` means "proceed only
on success" and `||` means "proceed only on failure", but neither makes
the branching logic readable at a glance.

**This rule applies only to standalone statements** — `&&` / `||` inside
`if`, `while`, or `until` conditions are boolean expressions and must
NOT be split.

```bash
# WRONG — standalone control flow hidden behind operators
[[ -f "$file" ]] && cat "$file"
[[ ! -d "$dir" ]] || mkdir -p "$dir"

# CORRECT — explicit conditionals for standalone statements
if [[ -f "$file" ]]; then
  cat "$file"
fi
if [[ ! -d "$dir" ]]; then
  mkdir -p "$dir"
fi

# CORRECT — boolean expressions inside if/while are fine, do NOT split
if [[ ! -r "$file" ]] || [[ ! -s "$file" ]]; then
  return 0
fi
while [[ -n "$line" ]] && [[ "$line" != "EOF" ]]; do
  _process_line "$line"
done
```

### Don't: End-of-Line Comments

Avoid end-of-line `# comment` annotations after a statement.  Move the
explanation to a separate `#` comment line above the code — end-of-line
comments are easily missed during review and typically restate the obvious.

```bash
# WRONG — end-of-line annotation restates the obvious
export PATH="$HOME/bin:$PATH"  # Add bin to PATH

# CORRECT — separate line explains reasoning
# Personal builds take precedence over system packages
export PATH="$HOME/bin:$PATH"
```

### Don't: Align Values

Do not pad `=` in assignments or align continuation markers with extra
spaces — it creates noisy diffs when names or values change.

```bash
# WRONG — alignment breaks on first rename
case "$mode" in
  install)    _install  ;;
  uninstall)  _remove   ;;
esac

# CORRECT
case "$mode" in
  install) _install ;;
  uninstall) _remove ;;
esac
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

**Sensitive types**: API keys, tokens, passwords, private keys, certificates.

### Umask

For scripts handling sensitive data, set restrictive umask at the top.

```bash
# Prevent sensitive temp files from being read by other users
umask 077
```

## References

1. [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html)
2. [Bash Manual](https://www.gnu.org/software/bash/manual/)

## Validation

```bash
bash -n script.sh      # Syntax check
shellcheck script.sh   # Lint (if installed)
```
