# CLAUDE.md - ZSH Configuration

This file provides guidance to Claude Code when working with the zsh configuration in this directory.

## Project Overview

This is an **XDG-compliant zsh configuration** with modular organization using **Zinit** plugin manager for performance and flexibility.

## Directory Structure

```
zsh/
├── .zshenv                # Bootstrap file (symlinked to ~/.zshenv)
└── .config/zsh/
    ├── .zprofile           # Login shell initialization
    ├── .zshrc              # Interactive shell orchestrator
    ├── completions/        # Custom completion scripts
    ├── conf.d/
    │   ├── 00-env.zsh      # Core environment variables
    │   ├── 05-path.zsh      # PATH/FPATH/MANPATH management
    │   ├── 10-options.zsh   # Shell options (setopt/unsetopt)
    │   ├── 15-history.zsh   # History configuration
    │   ├── 20-aliases.zsh   # Command aliases
    │   ├── 30-completion.zsh # Completion system initialization
    │   ├── 40-plugins.zsh   # Plugin loading (Zinit turbo mode)
    │   ├── 50-prompt.zsh    # Prompt configuration
    │   ├── 60-keybinds.zsh  # Key bindings
    │   ├── 70-tools.zsh     # Tool initialization (fzf, zoxide, etc.)
    │   └── 99-local.zsh     # Local overrides (gitignored)
    └── functions/           # Autoloaded shell functions
```

## Quick Start

### Setup

```bash
# Stow the zsh package (creates symlinks)
stow zsh

# Test configuration loads without errors
zsh -c 'echo "ZDOTDIR: $ZDOTDIR"'
```

### Common Commands

| Command | Description |
|---------|-------------|
| `exec zsh` | Reload shell configuration |
| `zinit update` | Update all plugins |
| `zinit delete <plugin>` | Remove a plugin |
| `zinit report` | Show plugin load times |

### Quick Validation

```bash
# Verify XDG paths are set
zsh -c 'echo "CONFIG: $XDG_CONFIG_HOME" && echo "DATA: $XDG_DATA_HOME"'

# Verify conf.d files are sourced
zsh -c 'typeset -m "OMW_*"'

# Check for syntax errors
zsh -n .config/zsh/.zshrc
```

## Architecture

### Shell Startup Sequence

```
.zshenv (always) → .zprofile (login) → .zshrc (interactive)
```

| File | Loaded by | Purpose |
|------|-----------|---------|
| `.zshenv` | ALL shells | XDG paths, ZDOTDIR, tool redirects |
| `.zprofile` | Login shells | Environment, PATH, SSH agent |
| `.zshrc` | Interactive shells | Sources conf.d/* in order |

### Module Loading System

**Critical**: The `.zshrc` file loads conf.d modules in numeric order. Dependencies must be respected.

**Loading order:**
1. **00-env.zsh** - Core XDG variables, tool redirects (no dependencies)
2. **05-path.zsh** - PATH/FPATH/MANPATH (depends on 00-env)
3. **10-options.zsh** - Shell options (no dependencies)
4. **15-history.zsh** - History config (depends on 00-env for XDG_CACHE_HOME)
5. **20-aliases.zsh** - Aliases (no dependencies)
6. **30-completion.zsh** - Completion init (depends on 05-path for fpath)
7. **40-plugins.zsh** - Zinit plugins (depends on 00-env, 30-completion)
8. **50-prompt.zsh** - Prompt theme (depends on 40-plugins for async)
9. **60-keybinds.zsh** - Key bindings (depends on 40-plugins for widgets)
10. **70-tools.zsh** - Tool init (depends on 00-env for paths)
11. **99-local.zsh** - Local overrides (loaded last, not tracked)

**When adding new modules:**
- Use appropriate numeric prefix based on dependencies
- Document prerequisites in header
- Ensure idempotency (safe to source multiple times)

### What Goes Where

| Content | File | Reason |
|---------|------|--------|
| XDG_* variables | `.zshenv` | Needed by all shells |
| ZDOTDIR | `.zshenv` | Must be set before other files load |
| Tool XDG redirects | `00-env.zsh` | Login shell context |
| PATH changes | `.zprofile` / `05-path.zsh` | Only needed at login |
| Editor/Pager | `.zprofile` / `00-env.zsh` | Only needed at login |
| Aliases | `.zshrc` / `20-aliases.zsh` | Interactive only |
| Functions | `functions/` | Autoloaded, interactive only |
| Prompt | `.zshrc` / `50-prompt.zsh` | Interactive only |
| Plugins | `.zshrc` / `40-plugins.zsh` | Interactive only |
| Local overrides | `99-local.zsh` | Machine-specific, not tracked |

## Coding Standards

See `.claude/rules/dotfiles/`:
- **zsh.md** - Zsh-specific rules (conf.d, idempotency)
- **shell-standards.md** - Shell script patterns
- **coding-style.md** - Universal config standards

## Plugin System

This configuration uses **Zinit** plugin manager installed at `$XDG_DATA_HOME/zinit/`.

### Installed Plugins (via Zinit turbo mode)

| Plugin | Purpose |
|--------|---------|
| fast-syntax-highlighting | Real-time syntax highlighting |
| zsh-history-substring-search | Enhanced Ctrl-R history search |
| zsh-autosuggestions | Autosuggestions from history |
| fzf-tab | fzf-powered completion menu |
| zsh-completions | Additional completion definitions |
| autopair | Auto-close brackets and quotes |

See `40-plugins.zsh` for plugin configuration.

## Code Quality

### Validation Commands

```bash
# Quick syntax check
zsh -n .config/zsh/.zshrc

# Full load test
zsh -c 'source ~/.zshenv && source $ZDOTDIR/.zshrc && echo "OK"'

# Verify XDG paths
zsh -c 'echo "CONFIG: $XDG_CONFIG_HOME" && echo "DATA: $XDG_DATA_HOME"'
```

### Compliance Checklist

- [ ] File header follows template with all required fields
- [ ] Time-stamp in format `<YYYY-MM-DD HH:MM:SS Day by Author>`
- [ ] Load order uses numbered format
- [ ] Section separators are 79 characters
- [ ] No inline comments (comments on line above code)
- [ ] No alignment spaces (single space only)
- [ ] All variables are quoted
- [ ] Conditional logic uses explicit `if` statements
- [ ] No `echo` (use `printf` or `print`)
- [ ] All comments in English
- [ ] Idempotent (safe to source multiple times)

## Best Practices

### 1. Keep Modules Focused

Each conf.d file should handle one logical area.

### 2. Use Explicit Conditionals

Avoid the `[[ ]] && cmd` pattern - it's error-prone under `set -e`.

### 3. Test Before Committing

Always run syntax checks before committing changes.

### 4. Local Configuration

Use `99-local.zsh` for machine-specific settings, not tracked by git.

### 5. Idempotent Operations

Ensure all configuration is safe to source multiple times.

## Quick Reference Card

### Essential Rules

1. **File Header:** Must include time-stamp, description, loaded by, load order, responsibilities
2. **Inline Comments:** Prohibited (comments on line above code)
3. **Alignment Spaces:** Prohibited (single space only)
4. **Conditionals:** Use explicit `if` statements, avoid `[[ ]] && cmd`
5. **Quoting:** Quote ALL variable expansions
6. **Output:** Use `printf` or `print`, never `echo`
7. **Tests:** Use `[[ ]]` not `[ ]`
8. **Arithmetic:** Use `(( ))` not `let` or `expr`
9. **Substitution:** Use `$()` not backticks
10. **Separators:** `# ===` for headers, `# ---` for sections (79 chars)
11. **Arrays:** Use `typeset -U` for deduplication
12. **Globbing:** Use `(N)` qualifier to skip no-match errors
13. **Comment Language:** English only

### Pre-Commit Checklist

- [ ] Configuration loads: `zsh -c 'source ~/.zshenv && source $ZDOTDIR/.zshrc'`
- [ ] No syntax errors: `zsh -n .config/zsh/.zshrc`
- [ ] Headers complete: time-stamp, load order, responsibilities
- [ ] No inline comments
- [ ] No alignment spaces
- [ ] All variables quoted
- [ ] No `[[ ]] && cmd` patterns (use `if`)
- [ ] No `echo` (use `printf` or `print`)
- [ ] All comments in English

### Common Patterns

```zsh
# Safe file sourcing
if [[ -f "${file}" ]]; then
    source "${file}"
fi

# Safe glob iteration
for f in "$dir"/*.zsh(N); do
    source "$f"
done

# Idempotent PATH addition
typeset -U path
path=("$HOME/.local/bin" $path)

# Environment variable with default
export MY_VAR="${MY_VAR:-$XDG_DATA_HOME/myapp}"

# Conditional with explicit if
if [[ -n "$value" ]]; then
    printf '%s\n' "$value"
fi

# Array deduplication
typeset -U path fpath manpath
```
