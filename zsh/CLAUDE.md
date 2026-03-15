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

## Comment and Code Standards

### File Header Template

```zsh
# <filename>
# =============================================================================
# <One-line description>
#
# Loaded by: <shell types that load this file>
# Load order: <when this loads relative to other files>
#
# Responsibilities:
#   1. <first responsibility>
#   2. <second responsibility>
#
# Do NOT add: <things that belong elsewhere>
#             → Put these in <correct file> (<reason>)
# =============================================================================
```

### Module Section Template

```zsh
# -----------------------------------------------------------------------------
# <Section Name>
# -----------------------------------------------------------------------------
# <Brief explanation of what this section does and why>
# <Any prerequisites or cross-references>
```

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

# Acceptable: && for success-dependent chaining (mkdir && cd)
mkdir -p "$dir" && cd "$dir"
```

### Output Commands

```zsh
# ✅ CORRECT - printf for portability and predictability
printf '%s\n' "$message"
printf 'Status: %s\n' "$status"

# ✅ CORRECT - zsh-idiomatic print -l for line-by-line output
print -l $path  # Print each path element on its own line

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
```

### Safe Globbing

```zsh
# (N) qualifier: silently skip if no matches
for file in "$dir"/*.zsh(N); do
  source "$file"
done
```

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

## Plugin System

This configuration uses **Zinit** plugin manager for installed at `$XDG_DATA_HOME/zinit/`.

### Installed Plugins (via Zinit turbo mode)

- **fast-syntax-highlighting** - syntax highlighting
- **zsh-history-substring-search** - enhanced Ctrl-R history search
- **zsh-autosuggestions** - autosuggestions from history
- **fzf-tab** - fzf-powered completion menu
- **zsh-completions** - additional completion definitions
- **autopair** - auto-close brackets and quotes

See `40-plugins.zsh` for plugin configuration and source them in `40-plugins.zsh` after completions are initialized.

## Adding New Configuration

1. Determine the correct file based on the table above
2. Follow the comment templates
3. Ensure idempotency (safe to source multiple times)
4. Document prerequisites and cross-references
5. Use appropriate numeric prefix in conf.d/
