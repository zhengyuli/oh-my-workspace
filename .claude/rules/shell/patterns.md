---
paths:
  - "**/*.sh"
  - "**/*.zsh"
  - "**/conf.d/*"
---

# Shell Patterns

> This file extends [../common/patterns.md](../common/patterns.md) with shell-specific patterns.

## Script Structure

### Standard Script Template

```zsh
#!/usr/bin/env zsh
# script-name.zsh
# Time-stamp: <2026-03-22 10:00:00 Sunday by zhengyu.li>
# =============================================================================
# Brief description of script purpose
#
# Usage: script-name.zsh [options] <arguments>
#
# Exit codes:
#   0 - Success
#   1 - General error
#   2 - Invalid arguments
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------
readonly SCRIPT_NAME="${0:t}"
readonly SCRIPT_DIR="${0:A:h}"

# -----------------------------------------------------------------------------
# Functions
# -----------------------------------------------------------------------------
usage() {
    cat <<EOF
Usage: ${SCRIPT_NAME} [options] <arguments>

Options:
    -h, --help     Show this help message
    -v, --version  Show version

Arguments:
    <argument>     Description of argument
EOF
}

main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help)
                usage
                exit 0
                ;;
            *)
                printf 'Unknown option: %s\n' "$1" >&2
                exit 2
                ;;
        esac
        shift
    done

    # Main logic here
}

main "$@"
```

## Function Patterns

### Function Definition

```zsh
# CORRECT - documented function
# Brief description of function purpose
# Arguments:
#   $1 - Description of first argument
#   $2 - Description of second argument
# Returns:
#   0 on success, non-zero on failure
my_function() {
    local arg1="$1"
    local arg2="${2:-default}"

    # Function logic
    if [[ -z "$arg1" ]]; then
        printf 'Error: arg1 required\n' >&2
        return 1
    fi

    printf '%s\n' "$arg1"
}
```

### Return vs Exit

```zsh
# Use return in functions
my_function() {
    if [[ -z "$1" ]]; then
        return 1  # CORRECT for functions
    fi
}

# Use exit in scripts
if [[ -z "$1" ]]; then
    printf 'Error: argument required\n' >&2
    exit 1  # CORRECT for scripts
fi
```

### Local Variables

```zsh
# Always use local for function variables
my_function() {
    local var1="value"
    local -a array=("item1" "item2")

    # Use typeset for typed locals
    typeset -i count=0
}
```

## Path Management

### XDG Path Setup

```zsh
# XDG Base Directory setup
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
```

### PATH Management

```zsh
# Idempotent PATH addition with deduplication
typeset -U path

path=(
    "$HOME/.local/bin"
    /opt/homebrew/bin
    $path  # Preserve existing
)

# Or add to front/back conditionally
if [[ -d "$HOME/.local/bin" ]]; then
    path=("$HOME/.local/bin" $path)
fi
```

### Tool-Specific Paths

```zsh
# Go
export GOPATH="${GOPATH:-$XDG_DATA_HOME/go}"
path=("$GOPATH/bin" $path)

# Rust
path=("$HOME/.cargo/bin" $path)

# Python (uv)
path=("$HOME/.local/bin" $path)
```

## Configuration Loading

### Conf.d Pattern

```zsh
# Load all .zsh files in conf.d directory
for config_file in "$XDG_CONFIG_HOME/zsh/conf.d"/*.zsh(N); do
    source "$config_file"
done
```

### Conditional Loading

```zsh
# Load only if file exists
if [[ -f "$XDG_CONFIG_HOME/zsh/.zshrc.local" ]]; then
    source "$XDG_CONFIG_HOME/zsh/.zshrc.local"
fi

# Load only if command exists
if command -v starship >/dev/null 2>&1; then
    eval "$(starship init zsh)"
fi
```

### Tool Initialization

```zsh
# Safe tool initialization with allowlist
local -a allowed_tools=(rbenv pyenv nodenv)
local tool

for tool in "${allowed_tools[@]}"; do
    if command -v "$tool" >/dev/null 2>&1; then
        eval "$("$tool" init -)"
    fi
done
```

## Output Formatting

### Printf Patterns

```zsh
# Simple message
printf '%s\n' "Message"

# Status message
printf 'Status: %s\n' "$status"

# Error message
printf 'Error: %s\n' "$error" >&2

# Table output
printf '%-20s %s\n' "Key" "Value"
printf '%-20s %s\n' "----" "-----"
printf '%-20s %s\n' "$key" "$value"
```

### Color Output

```zsh
# ANSI color codes
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly RESET='\033[0m'

# Color output function
print_color() {
    local color="$1"
    local message="$2"
    printf '%b%s%b\n' "$color" "$message" "$RESET"
}

# Usage
print_color "$GREEN" "Success"
print_color "$RED" "Error"
```

### Progress Indication

```zsh
# Simple progress
printf 'Processing...'
# ... work ...
printf 'Done\n'

# Spinner
spinner() {
    local pid=$1
    local spin='-\|/'
    local i=0
    while kill -0 "$pid" 2>/dev/null; do
        i=$(( (i+1) % 4 ))
        printf '\r%s' "${spin:$i:1}"
        sleep 0.1
    done
    printf '\r'
}
```

## Argument Parsing

### Simple Arguments

```zsh
while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            usage
            exit 0
            ;;
        -v|--verbose)
            verbose=1
            shift
            ;;
        --)
            shift
            break
            ;;
        -*)
            printf 'Unknown option: %s\n' "$1" >&2
            exit 2
            ;;
        *)
            positional+=("$1")
            shift
            ;;
    esac
done
```

### With Default Values

```zsh
# Set defaults
local output="default.txt"
local verbose=0

# Override from arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -o|--output)
            output="$2"
            shift 2
            ;;
        -v|--verbose)
            verbose=1
            shift
            ;;
    esac
done
```

## Cleanup Patterns

### Trap-Based Cleanup

```zsh
# Setup cleanup on exit
cleanup() {
    rm -f "$temp_file"
    printf 'Cleanup complete\n'
}
trap cleanup EXIT

# Create temp file
temp_file=$(mktemp)
```

### Resource Management

```zsh
# Safe directory operations
pushd "$dir" || exit 1
# ... work ...
popd || exit 1

# Or use subshell
(
    cd "$dir" || exit 1
    # ... work ...
)
```
