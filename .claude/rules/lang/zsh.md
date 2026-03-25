---
globs: ["**/*.zsh", "zsh/**"]
---
# Zsh-Specific Conventions

Zsh-specific features extending the common shell practices.

## Base Requirements

This file extends `shell.md`. Read that first.

## Shebang

Always use `#!/usr/bin/env zsh`:

```zsh
#!/usr/bin/env zsh
```

## Key Differences from Bash

### Array Indexing

**Arrays start at 1, not 0:**

```zsh
arr=(one two three)
echo $arr[1]     # "one" (not "two" as in bash)
echo $arr[2]     # "two"
echo $arr[-1]    # "three" (last element)
```

### typeset vs declare

Zsh prefers `typeset`:

```zsh
# Both work, but typeset is more idiomatic in zsh
typeset -a my_array
typeset -A my_hash

# Also valid
declare -a my_array
declare -A my_hash
```

### Globbing Extensions

Zsh has powerful globbing:

```zsh
# Recursive glob
files=(**/*.sh)

# Exclude pattern
files=(^*.test.sh)

# Numeric sort
files=(*(n))

# By modification time
files=*(.om[1,5])   # 5 most recently modified files
```

### Parameter Expansion

Additional features:

```zsh
# Array length
arr=(a b c)
echo ${#arr}        # 3 (number of elements)
echo ${#arr[1]}     # 1 (length of first element)

# Modify on expansion
echo ${arr:u}       # Uppercase
echo ${arr:l}       # Lowercase

# Split/join
str="a:b:c"
arr=(${(s/:/)str})  # Split by :
echo ${(j/-/)arr}   # Join with -
```

## Zsh-Specific Features

### Options

```zsh
# Better globbing
setopt EXTENDED_GLOB

# No match is error (not pass-through)
setopt NOMATCH

# Auto cd (type directory name)
setopt AUTO_CD

# Correct typos
setopt CORRECT

# Share history
setopt SHARE_HISTORY

# Ignore dupes
setopt HIST_IGNORE_ALL_DUPS
```

### Associative Arrays

```zsh
typeset -A config
config=(
  editor emacs
  shell zsh
  term ghostty
)

# Access
echo $config[editor]

# Iterate
for key value in ${(kv)config}; do
  echo "$key = $value"
done
```

### Functions with Explicit Arguments

```zsh
# Named arguments (zsh 5.4+)
func() {
  local -A opts
  zparseopts -A opts -- -help h -verbose v

  (( ${+opts[--help]} )) && { show_help; return }
  (( ${+opts[-v]} )) && verbose=1
}
```

### Hooks

```zsh
# Run before each command
precmd() {
  # Update prompt, etc.
}

# Run after each command
preexec() {
  # Log command, etc.
}

# Chpwd - on directory change
chpwd() {
  ls -la
}
```

## Prompt Customization

Zsh has rich prompt support:

```zsh
# Simple
PROMPT='%n@%m %# '

# With git info (requires vcs_info)
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
PROMPT='%~ %F{green}${vcs_info_msg_0_}%f %# '
```

## Compatibility

### Writing Bash-Compatible Zsh

For scripts that should work in both:

```zsh
# Avoid zsh-specific features
# Use POSIX-compatible syntax
# Test in both shells
```

### Zsh Emulation

```zsh
# Emulate sh/ksh
emulate -L sh
```

## Error Handling

Zsh supports the same strict mode as Bash (see `shell.md`):

```zsh
set -euo pipefail
```

For interactive `.zshrc` files **do not** use `set -e` — it causes
unexpected exits on failed commands (e.g. `grep` finding no match).
Reserve strict mode for non-interactive scripts with a `#!/usr/bin/env zsh`
shebang.

ERR trap in Zsh:

```zsh
_err_handler() {
  printf '[error] %s: line %d: exit %d\n' \
    "${funcstack[2]:-main}" "${funcfiletrace[1]##*:}" "$?" >&2
}
trap '_err_handler' ERR
```

## When to Use Zsh vs Bash vs sh

| Situation | Recommended shell |
|-----------|------------------|
| Interactive config (`.zshrc`) | Zsh — full feature set |
| Dotfiles installer / setup script | Bash — wider availability |
| Portable one-off script | sh (POSIX) — broadest compat |
| Plugin / function autoloaded by Zsh | Zsh |
| Script run by CI (unknown env) | Bash or sh |

Use Zsh when you need its extended globbing, associative arrays, or
`zparseopts`. Use Bash when the script may run on systems where Zsh
is not guaranteed to be installed.