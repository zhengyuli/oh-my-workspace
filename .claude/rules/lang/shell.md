---
globs: ["**/*.sh", "**/*.bash"]
version: "1.0.0"
last-updated: "2026-03-26"
maintainer: "zhengyu.li"
---
# Shell Best Practices

Universal shell conventions shared by Bash and Zsh.

## References

- [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html) - Primary reference

## Error Handling

### Strict Mode

Always use strict mode at the start of scripts:

```bash
set -euo pipefail
```

- `-e` - Exit immediately if a command exits with non-zero status
- `-u` - Treat unset variables as an error
- `-o pipefail` - Return value of a pipeline is the status of the last command to exit with non-zero status

### ERR Trap

Implement error handler for cleanup. Use a POSIX-compatible form here;
for Bash-specific enhancements (function name, line number) see `bash.md`:

```sh
# POSIX-compatible error handler
_err_handler() {
  printf '[error] command failed with exit %d\n' "$?" >&2
}
trap '_err_handler' ERR
```

```bash
# Bash-specific — richer context (see lang/bash.md)
_err_handler() {
  local -r code=$?
  printf '[error] %s() line %d: exit %d\n' \
    "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
}
trap '_err_handler' ERR
```

### Exit Codes

Use meaningful exit codes:
- `0` - Success
- `1` - General error
- `2` - Misuse of shell command
- `126` - Command not executable
- `127` - Command not found

## Function Design

### Single Responsibility

Each function should do one thing:

```bash
# Good - single purpose
_validate_package() { ... }
_install_package() { ... }
_cleanup_backup() { ... }

# Bad - multiple responsibilities
_install_and_validate_and_cleanup() { ... }
```

### Local Variables

Always use `local` for function variables:

```bash
_process_file() {
  local -r input="$1"
  local output
  output="${input%.txt}.processed"
  ...
}
```

### Parameter Validation

Validate at function start:

```bash
_validate_package() {
  local -r pkg="$1"

  if [[ -z "$pkg" ]]; then
    printf 'error: package name required\n' >&2
    return 1
  fi

  if [[ ! -v PKG_ALL ]]; then
    printf 'error: PKG_ALL array not defined\n' >&2
    return 1
  fi
}
```

## Parameter Handling

### getopts for Flags

Use `getopts` for option parsing:

```bash
while getopts ":hd" opt; do
  case "$opt" in
    h) _show_help; exit 0 ;;
    d) DRY_RUN=true ;;
    \?) printf 'error: invalid option -%s\n' "$OPTARG" >&2; exit 1 ;;
    :) printf 'error: option -%s requires argument\n' "$OPTARG" >&2; exit 1 ;;
  esac
done
shift $((OPTIND - 1))
```

### Help and Version

Always provide:

```bash
_show_help() {
  cat <<'EOF'
Usage: setup.sh [OPTIONS] [COMMAND]

Commands:
  status    Show current stow status
  install   Install packages
  help      Show this help

Options:
  -h, --help     Show help
  -d, --dry-run  Preview changes
  -v, --version  Show version
EOF
}
```

### Default Values

Use sensible defaults:

```bash
# Use environment variable or default
WORKSPACE_DIR="${WORKSPACE_DIR:-$(pwd)}"
NETWORK_TIMEOUT="${NETWORK_TIMEOUT:-60}"
```

## Testing and Quoting

### Prefer [[ ]] over [ ]

```bash
# Good - more robust
if [[ "$var" == "value" ]]; then
  ...
fi

# Less preferred - traditional
if [ "$var" = "value" ]; then
  ...
fi
```

### Arithmetic with (( ))

```bash
# Good - arithmetic context
if (( count > 0 )); then
  ...
fi

# Less preferred
if [[ $count -gt 0 ]]; then
  ...
fi
```

### Always Quote Variables

```bash
# Good - prevents word splitting
rm -rf "$dir"
printf '%s\n' "$message"

# Bad - unsafe
rm -rf $dir
printf '%s\n' $message
```

## File Headers

Every shell script MUST include a standard header:

```bash
# setup.sh -*- mode: sh; -*-
# =============================================================================
# oh-my-workspace Setup Script
#
# Location: $WORKSPACE_DIR/setup.sh
# Usage:    ./setup.sh help
# Dependencies: GNU Stow, Bash 4.3+
# =============================================================================
```

## Function Documentation

Use `@param` and `@return` annotations in function comments:

```bash
# Validate package name exists in PKG_ALL array.
#
# @param $1 - Package name to validate
# @return 0 if valid, 1 if not found
_validate_package() {
  local -r pkg="$1"
  ...
}
```

## Comments

Comments must explain **why**, not **what**:

```bash
# Good — explains WHY
# Use printf instead of echo for portability (POSIX compliance)
printf '%s\n' "$message"

# Bad — explains WHAT (never write comments like this)
# Print the message
printf '%s\n' "$message"
```

## Naming

Use meaningful, descriptive names:

```bash
# Bad - cryptic
x=$(pwd)

# Good - descriptive
workspace_dir=$(pwd)
```

Private helper functions should be prefixed with `_`.

## Immutability

Prefer read-only variables and arrays:

```bash
# Good - readonly prevents accidental modification
local -r config_path="$HOME/.config"
readonly -a REQUIRED_PACKAGES=(zsh git emacs)

# Avoid - mutable without reason
local config_path="$HOME/.config"
```

Never modify files in-place when a backup approach is possible:

```bash
# WRONG: Modifies file in-place — no rollback
sed -i 's/old/new/' ~/.zshrc

# CORRECT: Creates new version with backup
cp ~/.zshrc ~/.zshrc.bak
sed 's/old/new/' ~/.zshrc.bak > ~/.zshrc
```

## File Structure

Organize shell scripts in this order:

```
1. Header (purpose, usage, dependencies)
2. Constants and configuration (readonly)
3. Utility functions (private, _prefixed)
4. Core functions (public)
5. Main entry point / dispatch (if executable)
```

## Safe Defaults

Always provide fallback values for environment variables:

```bash
# Good - uses default if not set
EDITOR="${EDITOR:-emacs}"
PAGER="${PAGER:-less}"

# Provide sensible XDG defaults
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
```

## Design Patterns

### Validation Pattern

Validate configs before sourcing or using them:

```bash
_validate_config() {
  local -r config_file="$1"

  [[ -f "$config_file" ]] || return 1
  [[ -r "$config_file" ]] || return 1
  bash -n "$config_file" 2>/dev/null || return 1
  return 0
}

if _validate_config "$config"; then
  source "$config"
else
  printf 'error: invalid config %s\n' "$config" >&2
  return 1
fi
```

## Anti-Patterns

### Deep Nesting

Limit nesting to 3 levels maximum. Use early-return guards:

```bash
# WRONG - 5 levels deep
if condition1; then
  if condition2; then
    if condition3; then
      if condition4; then
        if condition5; then
          # Too deep!
        fi
      fi
    fi
  fi
fi

# CORRECT - early returns flatten the structure
_do_thing() {
  condition1 || return 1
  condition2 || return 1
  condition3 || return 1
  condition4 || return 1
  # Execute logic here
}
```

### Magic Numbers

Use named constants instead of bare values:

```bash
# WRONG
sleep 30
timeout 300

# CORRECT
readonly CONNECT_TIMEOUT=30
readonly RETRY_DELAY=5

sleep "$RETRY_DELAY"
timeout "$CONNECT_TIMEOUT"
```

### Hardcoded Paths

Never hardcode absolute paths to home directories:

```bash
# WRONG
source /home/user/.config/zsh/aliases.zsh

# CORRECT
source "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/aliases.zsh"
```

### Large Monolithic Configs

Split large configs into focused files:

```bash
# WRONG - 2000 line .zshrc
# Everything in one file

# CORRECT - Modular structure
source "$ZDOTDIR/aliases.zsh"
source "$ZDOTDIR/functions.zsh"
source "$ZDOTDIR/options.zsh"
source "$ZDOTDIR/completion.zsh"
```

## Language-Specific Extensions

For Bash-specific features, see `bash.md`.
For Zsh-specific features, see `zsh.md`.