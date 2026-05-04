---
paths:
  - "shell/zsh/**"
---

# Zsh Conventions

Zsh-specific features and conventions.

## File Header

```zsh
#!/usr/bin/env zsh
# script.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
#
# =============================================================================
# Script Title - Brief Description
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: keyword1, keyword2
# Dependencies: zsh 5.0+
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

**Shebang**: `#!/usr/bin/env zsh` is required only for directly executable
scripts.  Sourced files (`.zshrc`, `conf.d/*.zsh`, completion scripts)
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

```zsh
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

```zsh
# --- Paths ---
export PATH="$HOME/.local/bin:$PATH"
export MANPATH="$HOME/.local/share/man:$MANPATH"

readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

_cleanup() { rm -f "$_tmp_file"; }
```

**Inside function bodies** — one blank line between logical steps.
Single-statement functions have no extra blank lines.

```zsh
# Multi-step body
_install_package() {
  local -r pkg="$1"
  _validate_package "$pkg"

  print "installing $pkg..."
  sudo apt-get install -y "$pkg"
}

# Single-statement body — no extra blank lines
_cleanup() { rm -f "$_tmp_file"; }
```

**Prohibited**: two or more consecutive blank lines anywhere in the file.

## Indentation

2-space indent for block bodies (`if`, `for`, `while`, `case`, functions).

```zsh
if [[ -f "$file" ]]; then
  process "$file"
fi

for pkg in "${pkgs[@]}"; do
  _install_package "$pkg"
done
```

## Line Length

79 characters maximum.

Exceptions:

- URLs and file paths that cannot be wrapped
- Help text strings in `print` / `printf` (user-facing output)

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
- Obvious operations (`# Export PATH` before `export PATH=...`)

**No end-of-line comments** — place comments on a separate line above
the code (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```zsh
# WRONG — restates the code
# Export PATH
export PATH="$HOME/.local/bin:$PATH"

# CORRECT — explains the reasoning
# Local bin takes precedence over system packages (personal builds)
export PATH="$HOME/.local/bin:$PATH"
```

## Error Handling

### Strict Mode (MANDATORY for scripts)

```zsh
set -euo pipefail
```

- `-e` - Exit on non-zero status
- `-u` - Treat unset variables as error
- `-o pipefail` - Pipeline fails on first error

**Note**: For interactive `.zshrc` files, **do not** use `set -e` — it causes
unexpected exits on failed commands (e.g., `grep` finding no match).

### ERR Trap

```zsh
_err_handler() {
  print -u2 "[error] ${funcstack[2]:-main}: line ${funcfiletrace[1]##*:}: exit $?"
}
trap '_err_handler' ERR
```

### EXIT Trap (Cleanup)

Use `trap ... EXIT` for resource cleanup (temp files, lock files).

```zsh
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

```zsh
_process_file() {
  # Immutable: function arg never reassigned
  local -r src="$1"
  # Mutable: assigned after declaration
  local dest
  dest="${src%.txt}.out"
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

Use `print` for all output. It is the zsh-native built-in with rich options
(`-u` for fd, `-r` for raw, `-P` for prompt expansion).

Use `printf` only when you need format specifiers (`%s`, `%d`, `%04x`).

```zsh
# Simple output — always use print
print "processing $file"
print -u2 "error: $pkg not found"     # stderr via -u2

# Formatted output — use printf
printf '[error] %s() line %d: exit %d\n' "$func" "$line" "$code" >&2

# Raw output (no escape interpretation) — use print -r
print -r -- "$untrusted_content"
```

### ANSI Escape Sequences

Use `$'...'` (ANSI-C quoting) for strings containing escape sequences
(`\033`, `\n`, `\t`, etc.).  Single-quoted `'\033'` stores literal text,
not the actual ESC byte — colors will print as raw text.

```zsh
# CORRECT — $'...' converts \033 to the actual ESC character
readonly _RED=$'\033[0;31m'
readonly _RESET=$'\033[0m'
print "  ${_RED}error${_RESET} $msg"

# WRONG — single quotes store literal "\033[0;31m" text
readonly _RED='\033[0;31m'
```

### Naming Conventions

Constants: `UPPER_SNAKE_CASE`, Local: `lower_snake_case`.
Private/internal functions use `_` prefix; public entry functions (e.g.,
`main`) have no prefix.

```zsh
readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
local temp_file
```

### Formatting

Split long pipelines at `|` with each stage on its own line. This
improves readability, simplifies debugging (test each stage
independently), and produces cleaner diffs when stages are added or
removed.

```zsh
# WRONG — single line, hard to read and debug
cat /var/log/syslog | grep "error" | cut -d: -f2 | sort | uniq -c | sort -rn | head -10

# CORRECT — one stage per line, 2-space indent for continuation
cat /var/log/syslog \
  | grep "error" \
  | cut -d: -f2 \
  | sort \
  | uniq -c \
  | sort -rn \
  | head -10
```

### Line Continuation

Break long lines with `\`. For continued conditions, align operators
(`&&`, `||`) with the opening `[[`. For multi-value arguments (zstyle,
arrays), use 2-space indent.

```zsh
# Long condition
if [[ "$mode" == "install" ]] \
   && [[ -n "$pkg" ]] \
   && [[ -r "$config_file" ]]; then
  _install_package "$pkg"
fi

# Long zstyle
zstyle ':completion:*' matcher-list \
  'm:{a-z}={A-Za-z}' \
  'r:|[._-]=* r:|=*' \
  'l:|=* r:|=*'
```

### Section Uniqueness

Each section title must be unique within the file at every delimiter level (Level 1 and Level 2).
Group related settings together — do not create multiple sections of the same name.

```zsh
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

```zsh
_process_user_input() {
  local -r input="$1"
  if [[ -z "${input// /}" ]]; then
    print -u2 "error: input cannot be empty"
    return 1
  fi
}
```

### Nesting Limit

Maximum 3 nesting levels. Use early-return guards to flatten structure.

```zsh
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

```zsh
# WRONG
sleep 30
if (( count > 100 )); then
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

## Zsh-Specific Features

### Glob Qualifiers

Glob qualifiers are zsh's most powerful file selection mechanism — use them
instead of `find` + `grep` pipelines. Common qualifiers:

| Qualifier | Meaning | Example |
|-----------|---------|---------|
| `.` | Regular file | `*(.)` — all files |
| `/` | Directory | `*(/)` — all dirs |
| `N` | Null glob (no error on empty) | `*(N)` — safe empty check |
| `om` | Order by mtime (newest first) | `*(om)` |
| `[1]` | First match only | `*(om[1])` — newest file |
| `L+0` | Size > 0 bytes | `*(.L+0)` — non-empty files |

Always combine `N` with other qualifiers in config scripts to prevent
`zsh: no matches found` errors.

```zsh
# WRONG — forks find, fragile parsing
newest="$(find . -maxdepth 1 -type f -printf '%T@ %p\n' | sort -rn | head -1)"

# CORRECT — zsh-native, no subshells
newest=( *(.Nom[1]) )
```

### Parameter Expansion Flags

Zsh parameter flags replace many external tools (`cut`, `tr`, `awk`):

```zsh
# Split string by delimiter
path_parts=( "${(@s.:.)LS_COLORS}" )    # split LS_COLORS on :

# Split by lines
lines=( "${(f)content}" )                # split on \n (no IFS pollution)

# Uppercase / lowercase
print "${(U)var}"                         # uppercase
print "${(L)var}"                         # lowercase

# Join array
print "${(j., .)array}"                   # join with ", "

# Expand to get variable value (indirection)
print "${(P)var_name}"                    # like bash ${!var_name}
```

Prefer parameter expansion flags over forking external commands.

### Anonymous Functions

Use `() { ... }` for temporary scope isolation without naming a function:

```zsh
# Isolate local variables to avoid polluting outer scope
() {
  local -a temp=( *.tmp(N) )
  if (( ${#temp} )); then
    rm -v "${temp[@]}"
  fi
}
```

### Array and Hash Types

```zsh
# Associative array (hash map)
typeset -A color_map=(
  error   red
  warning yellow
  info    green
)

# Global associative array (for plugin state, cross-function sharing)
typeset -gA ZINIT=( )

# Typed arrays
local -a files=( )             # array
local -A opts=( )              # associative array
```

### Autoload

```zsh
# -U: suppress alias expansion (prevents alias shadowing)
# -z: use zsh-style function loading
autoload -Uz compinit
```

## Shell Options

### Conventions

One option per line, with a comment explaining what it does. Group related
options by category using Level 1 delimiters.

```zsh
# -----------------------------------------------------------------------------
# Directory Navigation
# -----------------------------------------------------------------------------

# type a directory name to cd into it
setopt AUTO_CD
# suppress output of pushd / popd
setopt PUSHD_SILENT
```

### Where to Set Options

Place all `setopt`/`unsetopt` in a dedicated conf.d file (e.g., `10-options.zsh`).
Do not scatter options across multiple files.

**Exceptions**: completion-related options (e.g., `setopt AUTO_MENU`) may live
in the completion config file for co-location with their zstyle settings.

## conf.d/ Organization

### Numbering Ranges

| Range  | Purpose                    | Examples                        |
|--------|----------------------------|---------------------------------|
| 00-09  | Environment & paths        | 00-env.zsh, 05-path.zsh        |
| 10-19  | Shell behavior & options   | 10-options.zsh, 15-history.zsh  |
| 20-29  | Aliases & abbreviations    | 20-aliases.zsh                  |
| 30-39  | Completion system          | 30-completion.zsh               |
| 40-49  | Plugin management          | 40-plugins.zsh                  |
| 50-59  | Prompt theme               | 50-prompt.zsh                   |
| 60-69  | Key bindings               | 60-keybinds.zsh                 |
| 70-89  | Tool integrations          | 70-tools.zsh                    |
| 90-99  | Local overrides            | 99-local.zsh                    |

### File Naming

```
NN-name.zsh
```

- `NN` — two-digit number with leading zero
- `name` — lowercase, hyphen-separated, descriptive
- Extension — `.zsh` (never `.sh`)

## Plugin Management

### Zinit Plugin Declarations

Use `zinit ice` + `zinit light` pairs. Place ice modifiers on a single line
with space-separated key-value pairs:

```zsh
# Sync plugin — no ice needed
zinit light owner/plugin

# Turbo plugin with ice modifiers
zinit ice wait"0b" lucid atload'bindkey ...'
zinit light owner/plugin
```

### Grouping

Group plugins by load timing with Level 2 subsection delimiters:

```zsh
# --- Sync: Plugin Name ---
zinit light owner/plugin

# --- Turbo 0a: Category ---
zinit ice wait"0a" lucid blockf
zinit light owner/plugin

# --- Turbo 0b: Category ---
zinit ice wait"0b" lucid
zinit light owner/plugin
```

### Comment Requirements

Every plugin declaration must include:

1. **Why it loads at this timing** — sync vs turbo, and the suffix letter
   reason (a/b/c ordering)
2. **Config variables** — set BEFORE the `ice`/`light` pair with a comment
   explaining when they are read
3. **Ice modifier explanations** — only for non-obvious modifiers
   (e.g., `blockf`, `atload"!"`)

## Completion System

### compinit

Cache the completion dump in `$XDG_CACHE_HOME/zsh/zcompdump` with a freshness
check using zsh glob qualifiers (avoid forking `find` or `stat`):

```zsh
autoload -Uz compinit
_zcompdump="$XDG_CACHE_HOME/zsh/zcompdump"

# Rebuild if stale/missing; skip audit if fresh
_zcompdump_fresh=( ${_zcompdump}(N.mh-${COMPDUMP_MAX_AGE_HOURS:-20}) )
if (( ${#_zcompdump_fresh} )); then
  compinit -C -d "$_zcompdump"
else
  compinit -d "$_zcompdump"
fi
```

### zstyle Formatting

- Use continuation lines (`\`) when zstyle value exceeds 79 chars
- Comment complex matcher-list or list-colors patterns
- Group zstyle by context (`:completion:*`, `:fzf-tab:*`)

```zsh
# Matcher: smart-case → partial-word → substring
zstyle ':completion:*' matcher-list \
  'm:{a-z}={A-Za-z}' \
  'r:|[._-]=* r:|=*' \
  'l:|=* r:|=*'
```

### fzf-tab Coexistence

When using fzf-tab, set `zstyle ':completion:*' menu no` and document that
fzf-tab owns all Tab completion UI. Do NOT set `menu select` in completion
config — it creates dead config that fzf-tab overrides.

## Functions

### emulate for Portable Functions

Use `emulate -L zsh` at the top of autoloaded functions and shared library
functions that may be sourced from different emulation contexts (bash
compatibility mode, ksh emulation, etc.).

```zsh
# Autoloaded function — emulation context is unknown at load time
_my_utility() {
  emulate -L zsh
  # Now guaranteed: extended_glob, zsh parameter expansion, zsh globs
  local -a files=( **/*.md(N) )
}
```

**When to use**: autoloaded functions (`autoload -Uz`), functions in shared
libraries, completion functions.

**When NOT needed**: functions defined within your own conf.d/ files (already
running in zsh emulation), scripts with `#!/usr/bin/env zsh` shebang.

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
Unlike bash, zsh preserves the exit code when combining declaration and
assignment — separate lines are still preferred for readability and
consistency with bash conventions.

```zsh
local dir
dir="$(dirname "$file")"
```

## Anti-Patterns

### Don't: eval for User Input

```zsh
# WRONG
eval "$user_input"

# CORRECT — explicit dispatch
case "$user_input" in
  install) _install ;;
  uninstall) _uninstall ;;
  *) print -u2 "error: invalid command: $user_input"; exit 1 ;;
esac
```

### Don't: Short-Circuit Control Flow

Do not use `&&` or `||` as shorthand for conditionals on **standalone
statements**.  They obscure control flow intent: `&&` means "proceed only
on success" and `||` means "proceed only on failure", but neither makes
the branching logic readable at a glance.

**This rule applies only to standalone statements** — `&&` / `||` inside
`if`, `while`, or `until` conditions are boolean expressions and must
NOT be split.

```zsh
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

```zsh
# WRONG — end-of-line annotation restates the obvious
export PATH="$HOME/bin:$PATH"  # Add bin to PATH

# CORRECT — separate line explains reasoning
# Personal builds take precedence over system packages
export PATH="$HOME/bin:$PATH"
```

### Don't: Align Values

Do not pad `=` in assignments or align continuation markers with extra
spaces — it creates noisy diffs when names or values change.

```zsh
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

See Anti-Patterns: [Don't: eval for User Input](#dont-eval-for-user-input).

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

**Sensitive types**: API keys, tokens, passwords, private keys, certificates.

### Umask

For scripts handling sensitive data, set restrictive umask at the top.

```zsh
# Prevent sensitive temp files from being read by other users
umask 077
```

## References

1. [Zsh Manual](http://zsh.sourceforge.net/Doc/)
2. [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html)

## Validation

```zsh
# Syntax check — the only reliable checker for zsh-specific syntax
# (built-in, no install needed)
zsh -n script.zsh
```

**Note**: `shellcheck` has limited zsh support — it does not recognize
zsh-only syntax (glob qualifiers, parameter expansion flags, `typeset -A`,
`zstyle`, `zmodload`, etc.). Only use `shellcheck` on scripts that stick to
POSIX-compatible constructs. For zsh-heavy files, `zsh -n` is the only
reliable syntax checker.
