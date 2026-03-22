---
paths:
  - "**/*.sh"
  - "**/*.zsh"
  - "**/conf.d/*"
---

# Shell Hooks

> This file extends [../common/hooks.md](../common/hooks.md) with shell-specific hooks.

## Pre-Commit Hooks

### Shellcheck Integration

Use shellcheck for static analysis:

```bash
# Check single file
shellcheck script.sh

# Check with zsh support (if available)
shellcheck --shell=bash script.sh

# Check all shell scripts
shellcheck **/*.sh **/*.zsh
```

### Common Shellcheck Warnings

| Code | Issue | Fix |
|------|-------|-----|
| SC2086 | Double quote to prevent globbing | `"$var"` instead of `$var` |
| SC2181 | Check exit code directly | `if cmd; then` instead of `cmd; if [ $? -eq 0 ]` |
| SC2164 | Use `cd -P` or check return | `cd "$dir" || exit` |
| SC2039 | In POSIX sh, certain features undefined | Use bash/zsh features intentionally |
| SC2016 | Expressions don't expand in single quotes | Use double quotes for expansion |

### Zsh Syntax Check

```bash
# Check zsh syntax without execution
zsh -n script.zsh
zsh -n .config/zsh/.zshrc

# Check all zsh files
for f in **/*.zsh; do
    zsh -n "$f" || printf 'Syntax error in %s\n' "$f"
done
```

### Shfmt Integration

Use shfmt for shell script formatting:

```bash
# Format file in-place
shfmt -w script.sh

# Check formatting without modifying
shfmt -d script.sh

# Format with specific options
shfmt -i 4 -ci -w script.sh  # 4-space indent, switch case indent
```

## Pre-Commit Verification

### Before Committing Shell Scripts

```bash
# 1. Syntax check
zsh -n script.zsh

# 2. Shellcheck
shellcheck script.sh

# 3. Test sourcing
zsh -c 'source script.zsh && echo OK'

# 4. Check for common issues
grep -E '\$\{?[A-Za-z_][A-Za-z0-9_]*\}?' script.zsh | grep -v '"'
```

### Automated Pre-Commit

Add to `.git/hooks/pre-commit`:

```bash
#!/bin/bash
# Pre-commit hook for shell script validation

set -e

# Get staged shell files
staged_files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(sh|zsh)$')

if [[ -n "$staged_files" ]]; then
    printf 'Checking shell scripts...\n'
    for file in $staged_files; do
        printf '  %s\n' "$file"
        # Syntax check
        zsh -n "$file"
        # Shellcheck (if available)
        if command -v shellcheck >/dev/null 2>&1; then
            shellcheck "$file"
        fi
    done
fi

exit 0
```

## Zsh Hook Functions

### Chpwd Hook

Run on directory change:

```zsh
# Register chpwd hook
autoload -Uz add-zsh-hook

omw_chpwd_hook() {
    # Actions on directory change
    ls -a
}
add-zsh-hook chpwd omw_chpwd_hook
```

### Preexec Hook

Run before command execution:

```zsh
omw_preexec_hook() {
    # Store command start time
    omw_cmd_start_time=$EPOCHSECONDS
}
add-zsh-hook preexec omw_preexec_hook
```

### Precmd Hook

Run before prompt display:

```zsh
omw_precmd_hook() {
    # Calculate command duration
    if [[ -n "$omw_cmd_start_time" ]]; then
        omw_cmd_duration=$(( EPOCHSECONDS - omw_cmd_start_time ))
        unset omw_cmd_start_time
    fi
}
add-zsh-hook precmd omw_precmd_hook
```

### Idempotent Hook Registration

```zsh
# Check if hook already registered
if (( ! ${+functions[omw_chpwd_hook]} )); then
    omw_chpwd_hook() { ... }
    add-zsh-hook chpwd omw_chpwd_hook
fi
```

## Trap Hooks

### EXIT Trap

```zsh
# Cleanup on script exit
cleanup() {
    rm -f "$temp_file"
}
trap cleanup EXIT
```

### INT/TERM Trap

```zsh
# Handle interrupts gracefully
interrupt_handler() {
    printf '\nInterrupted\n'
    exit 130
}
trap interrupt_handler INT TERM
```

### ERR Trap

```zsh
# Error handling (bash)
error_handler() {
    local line_no=$1
    printf 'Error on line %d\n' "$line_no" >&2
}
trap 'error_handler $LINENO' ERR
```

## Hook Best Practices

### Use Named Functions

```zsh
# CORRECT - named function for debugging
omw_precmd_hook() { ... }
add-zsh-hook precmd omw_precmd_hook

# AVOID - anonymous functions harder to debug
add-zsh-hook precmd 'echo "prompt"'
```

### Conditional Registration

```zsh
# Only register if not already present
if ! (( ${chpwd_functions[(I)omw_chpwd_hook]} )); then
    add-zsh-hook chpwd omw_chpwd_hook
fi
```

### Cleanup on Unload

```zsh
# Remove hook when module is unloaded
omw_module_cleanup() {
    add-zsh-hook -D chpwd omw_chpwd_hook
}
```

## Hook Verification

### List Registered Hooks

```zsh
# List chpwd hooks
printf '%s\n' "${chpwd_functions[@]}"

# List all precmd hooks
printf '%s\n' "${precmd_functions[@]}"
```

### Debug Hook Execution

```zsh
# Enable debug mode
setopt xtrace

# Or use DEBUG trap
trap 'printf "Executing: %s\n" "$BASH_COMMAND"' DEBUG
```
