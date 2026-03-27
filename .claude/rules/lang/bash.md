---
globs: ["**/*.sh", "**/*.bash"]
version: "1.0.0"
last-updated: "2026-03-26"
maintainer: "zhengyu.li"
---
# Bash-Specific Conventions

Bash-specific features extending the common shell practices.

## Base Requirements

This file extends `shell.md`. Read that first.

## Version Requirements

- **Minimum:** Bash 4.3+ (for namerefs and associative arrays)
- **Check version:** `echo "${BASH_VERSION}"`

## Shebang

Always use `#!/usr/bin/env bash`:

```bash
#!/usr/bin/env bash
```

**Rationale:**
- Works across different systems
- Respects user's PATH
- Not hardcoded to `/bin/bash`

## Advanced Features

### Namerefs (Bash 4.3+)

Use `local -n` for pass-by-reference:

```bash
# Pass array by reference
_process_array() {
  local -n arr="$1"  # nameref
  local item

  for item in "${arr[@]}"; do
    printf '%s\n' "$item"
  done
}

declare -a my_array=(one two three)
_process_array my_array
```

### Associative Arrays

Use `declare -A` for key-value lookups:

```bash
declare -A config=(
  [editor]="emacs"
  [shell]="zsh"
  [term]="ghostty"
)

# Access
printf 'Editor: %s\n' "${config[editor]}"

# Iterate
for key in "${!config[@]}"; do
  printf '%s = %s\n' "$key" "${config[$key]}"
done
```

### Readonly Arrays

Use `readonly -a` for constant arrays:

```bash
readonly -a PKG_ALL=(
  shell/zsh
  editor/emacs
  term/ghostty
)
```

### Reading Lines into Arrays with mapfile

Use `mapfile` (alias `readarray`) instead of a `while read` loop when
capturing multi-line command output into an array:

```bash
# Good — mapfile is safe with filenames containing spaces/newlines
mapfile -t files < <(find . -name '*.sh' -type f)

for f in "${files[@]}"; do
  bash -n "$f"
done

# Also good — read from a file directly
mapfile -t lines < /etc/hosts

# Avoid — word-splitting breaks filenames with spaces
files=($(find . -name '*.sh'))  # WRONG
```

Key flags:

| Flag | Meaning |
|------|---------|
| `-t` | Strip the trailing newline from each element (almost always wanted) |
| `-n N` | Read at most N lines |
| `-s N` | Skip the first N lines |
| `-d DELIM` | Use DELIM as the line terminator instead of newline |

`mapfile` requires Bash 4.0+.

## Bash-Specific Syntax

### Parameter Expansion

```bash
# Default value
dir="${path:-/default/path}"

# Error if unset
file="${config:?config is required}"

# Substring
version="${BASH_VERSION:0:3}"

# String replacement
path="${filepath//\//\\}"  # Replace all / with \
```

### Process Substitution

```bash
# Compare outputs
diff <(cmd1) <(cmd2)

# Read into array
mapfile -t lines < <(find . -name '*.sh')
```

### Coproc

For bidirectional communication:

```bash
coproc backend {
  ./server.sh
}

printf 'request\n' >&${backend[1]}
read -r response <&${backend[0]}
```

## Debugging

### Bash-Specific Options

```bash
# Verbose - show commands as read
set -v

# xtrace - show commands after expansion
set -x

# Combined with strict mode
set -euxo pipefail
```

### Debugging Functions

```bash
_debug_caller() {
  printf '[debug] %s called from %s at line %d\n' \
    "${FUNCNAME[1]}" "${BASH_SOURCE[2]}" "${BASH_LINENO[1]}"
}
```

## Security

### Shell History

Prefix commands with a space to exclude them from history.
Set `HISTCONTROL` in `.bashrc` / `.bash_profile`:

```bash
# Ignore commands prefixed with a space (and deduplicate)
export HISTCONTROL=ignoreboth

# Then sensitive commands starting with a space are not recorded
 API_KEY=secret my-command   # not saved to history
```

## Compatibility Notes

### Avoid Bash-Only Features for Portable Scripts

If portability is required:
- Avoid `[[ ]]` (use `[ ]`)
- Avoid `(( ))` (use `$(( ))`)
- Avoid `local -n`
- Avoid associative arrays
- Use POSIX shell instead

### Check Bash Version

```bash
if (( BASH_VERSINFO[0] < 4 ||
      ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] < 3 ) )); then
  printf 'error: bash 4.3+ required\n' >&2
  exit 1
fi
```