# ZSH Configuration

This directory contains XDG-compliant Zsh configuration with modular organization.

## File Structure

```
zsh/
├── .zshenv                    # Bootstrap (in $HOME, others in $ZDOTDIR)
└── .config/zsh/
    ├── .zprofile              # Login shell initialization
    ├── .zshrc                 # Interactive shell orchestrator
    ├── .zcompdump             # Completion cache (auto-generated)
    ├── cache/                 # Runtime cache (history, etc.)
    ├── completions/           # Custom completion scripts
    ├── conf.d/                # Modular configuration files
    │   ├── 00-env.zsh         # Core environment variables
    │   ├── 05-path.zsh        # PATH/FPATH/MANPATH management
    │   ├── 10-options.zsh     # Shell options (setopt/unsetopt)
    │   ├── 15-history.zsh     # History configuration
    │   ├── 20-aliases.zsh     # Command aliases
    │   ├── 30-completion.zsh  # Completion system initialization
    │   ├── 40-plugins.zsh     # Plugin loading (syntax-highlighting, etc.)
    │   ├── 50-prompt.zsh      # Prompt configuration
    │   ├── 60-keybinds.zsh    # Key bindings
    │   ├── 70-tools.zsh       # Tool initialization (pyenv, fnm, etc.)
    │   └── 99-local.zsh.example # Local overrides template (copy to 99-local.zsh)
    └── functions/             # Autoloaded shell functions
```

## Shell Startup Sequence

```
.zshenv (always) → .zprofile (login) → .zshrc (interactive)
```

| File | Loaded By | Purpose |
|------|-----------|---------|
| `.zshenv` | ALL shells | XDG paths, ZDOTDIR, tool redirects |
| `.zprofile` | Login shells | Environment, PATH, SSH agent |
| `.zshrc` | Interactive shells | Sources conf.d/* in order |

---

## Comment Standards

### File Header Template

All configuration files must start with a standardized header:

```zsh
# <filename>
# =============================================================================
# <One-line description>
#
# Loaded by: <shell types that load this file>
# Load order: <number> (after <prev_file>, before <next_file>)
#
# Prerequisites: (optional)
#   - <requirement 1>
#   - <requirement 2>
#
# Responsibilities:
#   1. <first responsibility>
#   2. <second responsibility>
#
# Do NOT add: <things that belong elsewhere>
#             → Put these in <correct file> (<reason>)
#
# Note: (optional)
#   <any additional context>
# =============================================================================
```

### Header Field Specifications

| Field | Required | Format | Description |
|-------|----------|--------|-------------|
| `<filename>` | Yes | Filename only (no path) | e.g., `00-env.zsh` |
| Description | Yes | Single line | Brief summary of file purpose |
| Loaded by | Yes | Shell type(s) | `.zshrc`, `.zprofile`, or "ALL shells" |
| Load order | Yes | `<num> (after X, before Y)` | Numeric prefix with context |
| Prerequisites | Optional | Dash list | Dependencies on other files/variables |
| Responsibilities | Yes | Numbered list | What this file manages |
| Do NOT add | Recommended | Arrow reference format | What belongs elsewhere |
| Note | Optional | Free text | Additional context |

### Section Comment Styles

#### 1. Major Sections (79 chars width)

Use full-width separator for top-level logical sections:

```zsh
# -----------------------------------------------------------------------------
# <Section Name>
# -----------------------------------------------------------------------------
# <Brief explanation of what this section does and why>
# <Any prerequisites or cross-references>
```

#### 2. Subcategories (inline format)

Use short separator for subcategories within a major section:

```zsh
# --- <Category Name> ---
# <Brief explanation>
<code>
```

#### 3. Inline Comments

For single-line explanations, no separator needed:

```zsh
# <description>
<code>
```

### Comment Width Standards

| Element | Width | Example |
|---------|-------|---------|
| Header separator (`# ===...`) | 79 chars | `# =============================================================================` |
| Section separator (`# ---...`) | 79 chars | `# -----------------------------------------------------------------------------` |
| Subcategory separator | Variable | `# --- Recording ---` |

### Comment Formatting Rules

1. **Field names use colon suffix**: `Loaded by:`, `Load order:`, `Prerequisites:`
2. **Prerequisites always plural**: Use `Prerequisites:` (not `Prerequisite:`)
3. **Do NOT add uses arrow format**: `→ Put these in <file> (<reason>)`
4. **List items are indented**: 2 spaces for numbered, 2 spaces + dash for prerequisites

---

## Code Standards

### Key Conventions

1. **Idempotency**: All conf.d files must be safe to source multiple times
2. **Numeric Prefixes**: Files load in lexicographic order (00-99)
3. **Single Concern**: Each file handles one logical area
4. **Prerequisites**: Document dependencies on other files
5. **Cross-References**: Mention related files in comments

### Variable Assignment

```zsh
# ✅ CORRECT - use ${VAR:-default} for overrides
export MY_VAR="${MY_VAR:-$XDG_DATA_HOME/myapp}"

# ✅ CORRECT - direct assignment when no override needed
export MY_VAR="$XDG_DATA_HOME/myapp"

# ✅ CORRECT - quote all variable expansions
local dir="$HOME/.config"
```

### Conditional Logic

```zsh
# ✅ CORRECT - explicit if statements
if [[ -d "$dir" ]]; then
    source "$dir/init.zsh"
fi

# ❌ AVOID - [[ ]] && cmd fails under set -e when condition is false
[[ -d "$dir" ]] && source "$dir/init.zsh"

# ❌ AVOID - [[ ]] && { compound; return } pattern
[[ -z "$1" ]] && { echo "Usage: cmd <arg>"; return 1 }

# ✅ CORRECT - explicit if for validation
if [[ -z "$1" ]]; then
    echo "Usage: cmd <arg>"
    return 1
fi

# ✅ ACCEPTABLE - && for success-dependent chaining (mkdir && cd)
mkdir -p "$dir" && cd "$dir"
```

### Conditional Tests

```zsh
# ✅ CORRECT - use [[ ]] for conditional tests
[[ -f "$file" ]]
[[ "$var" == "pattern" ]]
[[ -n "$value" ]]

# ❌ AVOID - [ ] or test command
[ -f "$file" ]
test -f "$file"
```

### Arithmetic Operations

```zsh
# ✅ CORRECT - use (( )) for arithmetic
(( count++ ))
(( total = a + b ))
local result=$(( a * b ))

# ❌ AVOID - let or expr
let count+=1
result=$(expr $a + $b)
```

### Command Substitution

```zsh
# ✅ CORRECT - use $() for command substitution
local dir=$(dirname "$file")

# ❌ AVOID - backticks (hard to nest, hard to read)
local dir=`dirname "$file"`
```

### Quoting

```zsh
# ✅ CORRECT - quote all variable expansions
rm -- "${file}"
grep -- "${pattern}" "${file}"
source "${config_file}"

# ❌ WRONG - unquoted variables (word-splitting and glob expansion risks)
rm ${file}
grep ${pattern} ${file}
```

### Output Commands

```zsh
# ✅ CORRECT - printf for portability and predictability
printf '%s\n' "$message"
printf 'Status: %s\n' "$status"

# ✅ CORRECT - zsh-idiomatic print for line-by-line output
print -l $path  # Print each path element on its own line
print -P '%F{green}Success%f'  # With prompt expansion for colors

# ❌ AVOID - echo -e has inconsistent behavior across shells
echo -e "$message"
```

### Array Management

```zsh
# PATH arrays - highest priority first, preserve existing
path=(
  "$HOME/.local/bin"
  /opt/homebrew/bin
  $path  # preserve system PATH
)

# Deduplication - ensures idempotency
typeset -U path fpath manpath

# Array iteration
for item in "${array[@]}"; do
  echo "$item"
done
```

### Safe Globbing

```zsh
# (N) qualifier: silently skip if no matches
for file in "$dir"/*.zsh(N); do
  source "$file"
done
```

### Security Practices

```zsh
# ✅ CORRECT - check file existence before sourcing
if [[ -f "${file}" ]]; then
    source "${file}"
fi

# ✅ CORRECT - use allowlist for eval (when absolutely necessary)
local -a allowed=(rbenv pyenv nodenv)
if (( ${allowed[(I)${tool}]} )); then
    eval "$(${tool} init -)"
fi

# ❌ DANGEROUS - never eval untrusted input
eval "${user_input}"

# ✅ CORRECT - check before operations
[[ -f "$file" ]] && rm -- "$file"
```

---

## What Goes Where

| Content | File | Reason |
|---------|------|--------|
| XDG_* variables | `.zshenv` | Needed by all shells |
| ZDOTDIR | `.zshenv` | Must be set before other files load |
| Tool XDG redirects | `.zshenv` | Needed even in non-interactive scripts |
| PATH changes | `.zprofile` / `05-path.zsh` | Only needed at login |
| Editor/Pager | `.zprofile` / `00-env.zsh` | Only needed at login |
| Aliases | `.zshrc` / `20-aliases.zsh` | Interactive only |
| Functions | `functions/` | Autoloaded, interactive only |
| Prompt | `.zshrc` / `50-prompt.zsh` | Interactive only |
| Plugins | `.zshrc` / `40-plugins.zsh` | Interactive only |
| Local overrides | `99-local.zsh` | Machine-specific, not tracked |

---

## Plugin System

This configuration uses **Zinit** plugin manager installed at `$XDG_DATA_HOME/zinit/`.

### Installed Plugins (via Zinit turbo mode)

- **fast-syntax-highlighting** - syntax highlighting
- **zsh-history-substring-search** - enhanced Ctrl-R history search
- **zsh-autosuggestions** - autosuggestions from history
- **fzf-tab** - fzf-powered completion menu
- **zsh-completions** - additional completion definitions
- **autopair** - auto-close brackets and quotes

See `40-plugins.zsh` for plugin configuration.

---

## Adding New Configuration

1. Determine the correct file based on the table above
2. Follow the comment templates exactly
3. Ensure idempotency (safe to source multiple times)
4. Document prerequisites and cross-references
5. Use appropriate numeric prefix in conf.d/
6. Maintain 79-character separator width
7. Quote all variable expansions

---

## Quick Reference Checklist

Before committing changes to zsh configuration:

- [ ] File header follows template with all required fields
- [ ] Load order uses numbered format: `<num> (after X, before Y)`
- [ ] Prerequisites field uses plural with dash list
- [ ] Do NOT add uses arrow reference format
- [ ] Section separators are 79 characters
- [ ] All variables are quoted
- [ ] Conditional logic uses explicit `if` statements
- [ ] No `echo -e` (use `printf` or `print`)
- [ ] No unquoted variable expansions
- [ ] Idempotent (safe to source multiple times)
