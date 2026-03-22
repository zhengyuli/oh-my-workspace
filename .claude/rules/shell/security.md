---
paths:
  - "**/*.sh"
  - "**/*.zsh"
  - "**/conf.d/*"
---

# Shell Security

> This file extends [../common/security.md](../common/security.md) with shell-specific security practices.

## Input Validation

### Validate Arguments

```zsh
# CORRECT - validate required arguments
if [[ -z "$1" ]]; then
    printf 'Error: argument required\n' >&2
    exit 1
fi

# CORRECT - validate argument format
if [[ ! "$1" =~ ^[a-zA-Z0-9_-]+$ ]]; then
    printf 'Error: invalid characters in argument\n' >&2
    exit 1
fi

# CORRECT - validate numeric input
if [[ ! "$1" =~ ^[0-9]+$ ]]; then
    printf 'Error: numeric argument required\n' >&2
    exit 1
fi
```

### Validate File Paths

```zsh
# Check file exists
if [[ ! -f "$file" ]]; then
    printf 'Error: file not found: %s\n' "$file" >&2
    exit 1
fi

# Check directory exists
if [[ ! -d "$dir" ]]; then
    printf 'Error: directory not found: %s\n' "$dir" >&2
    exit 1
fi

# Check file is readable
if [[ ! -r "$file" ]]; then
    printf 'Error: cannot read file: %s\n' "$file" >&2
    exit 1
fi
```

### Path Traversal Prevention

```zsh
# CORRECT - validate path doesn't escape intended directory
validate_path() {
    local base_dir="$1"
    local user_path="$2"
    local resolved

    # Resolve to absolute path
    resolved=$(cd "$(dirname "$user_path")" 2>/dev/null && pwd)/$(basename "$user_path")

    if [[ ! "$resolved" =~ ^"$base_dir" ]]; then
        printf 'Error: path traversal attempt detected\n' >&2
        return 1
    fi
    return 0
}
```

## Command Injection Prevention

### Avoid Eval with Untrusted Input

```zsh
# DANGEROUS - never eval untrusted input
eval "$user_input"  # NEVER do this

# DANGEROUS - eval with user-provided command
eval "echo $user_var"  # NEVER do this
```

### Use Arrays Instead of String Concatenation

```zsh
# CORRECT - use arrays for command arguments
local -a args=("--option" "$value" "$file")
command "${args[@]}"

# WRONG - vulnerable to injection
command "--option $value $file"
```

### Safe Tool Initialization

```zsh
# CORRECT - allowlist validated tools
local -a allowed_tools=(rbenv pyenv nodenv direnv)
local tool="$1"

if (( ${allowed_tools[(I)$tool]} )); then
    eval "$($tool init -)"
else
    printf 'Error: unknown tool: %s\n' "$tool" >&2
    return 1
fi
```

### Indirect Expansion Instead of Eval

```zsh
# CORRECT - use indirect expansion (zsh)
local var_name="MY_VAR"
print "${(P)var_name}"

# CORRECT - use associative arrays
typeset -A config
config[key]="value"
print "${config[key]}"

# DANGEROUS - eval
eval "echo \$$var_name"  # NEVER do this
```

## Quoting Best Practices

### Always Quote Variables

```zsh
# CORRECT - quote all variable expansions
rm -- "${file}"
grep -- "${pattern}" "${file}"
source "${config_file}"

# WRONG - unquoted variables
rm ${file}  # Word splitting and glob expansion risks
grep ${pattern} ${file}
```

### Quote Command Substitutions

```zsh
# CORRECT - quote command substitution
local dir="$(dirname "$file")"
local content="$(cat "$file")"

# WRONG - unquoted command substitution
local dir=$(dirname "$file")
```

### Use `--` to End Options

```zsh
# CORRECT - prevents filenames starting with - from being interpreted as options
rm -- "$file"
cat -- "$file"

# WRONG - filename like "-f" could be interpreted as option
rm "$file"
```

## File Operations

### Safe File Deletion

```zsh
# CORRECT - check before deletion
if [[ -f "$file" ]]; then
    rm -- "$file"
fi

# CORRECT - use trash instead of rm for safety
if command -v trash >/dev/null 2>&1; then
    trash "$file"
else
    rm -- "$file"
fi
```

### Safe Temp Files

```zsh
# CORRECT - use mktemp
temp_file=$(mktemp)
trap 'rm -f "$temp_file"' EXIT

# CORRECT - with specific prefix
temp_file=$(mktemp -t myscript-XXXXXX)

# WRONG - predictable temp file
temp_file="/tmp/myscript.$$"  # Race condition
```

### Safe Directory Creation

```zsh
# CORRECT - check before mkdir
if [[ ! -d "$dir" ]]; then
    mkdir -p "$dir"
fi

# CORRECT - with error handling
mkdir -p "$dir" || {
    printf 'Error: failed to create directory: %s\n' "$dir" >&2
    exit 1
}
```

## Secrets Management

### Never Hardcode Secrets

```zsh
# WRONG - hardcoded secrets
API_KEY="sk-1234567890abcdef"
PASSWORD="my-secret-password"

# CORRECT - use environment variables
API_KEY="${API_KEY:?Error: API_KEY not set}"
PASSWORD="${PASSWORD:-}"  # Optional with empty default
```

### Load Secrets from Files

```zsh
# CORRECT - load from gitignored file
if [[ -f "$XDG_CONFIG_HOME/secrets.env" ]]; then
    source "$XDG_CONFIG_HOME/secrets.env"
fi

# CORRECT - read specific secret
if [[ -f "$XDG_CONFIG_HOME/api_key" ]]; then
    read -r API_KEY < "$XDG_CONFIG_HOME/api_key"
fi
```

### Mask Sensitive Output

```zsh
# CORRECT - mask sensitive values in logs
log_api_call() {
    local key="$1"
    local masked_key="${key:0:4}...${key: -4}"
    printf 'API call with key: %s\n' "$masked_key"
}
```

## Permissions

### Check Before Sensitive Operations

```zsh
# Check file permissions
if [[ -r "$file" && -w "$file" ]]; then
    # Safe to read and write
    ...
fi

# Check directory permissions
if [[ -x "$dir" && -w "$dir" ]]; then
    # Safe to enter and modify
    ...
fi
```

### Set Restrictive Umask

```zsh
# Set restrictive umask for sensitive operations
(
    umask 077
    # Create files with 600 permissions
    echo "secret" > "$secrets_file"
)
```

## Security Checklist

Before committing shell scripts:

- [ ] No hardcoded secrets or API keys
- [ ] All variables properly quoted
- [ ] No `eval` with untrusted input
- [ ] File paths validated before operations
- [ ] Temp files created with `mktemp`
- [ ] Cleanup registered with `trap`
- [ ] Input validation for all arguments
- [ ] Use `--` to end option parsing
