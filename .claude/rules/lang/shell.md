---
globs: ["**/*.sh", "**/*.bash", "shell/**"]
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

## Language-Specific Extensions

For Bash-specific features, see `bash.md`.
For Zsh-specific features, see `zsh.md`.