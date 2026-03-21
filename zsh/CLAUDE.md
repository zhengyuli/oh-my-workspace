# CLAUDE.md - Zsh Configuration

This file provides guidance to Claude Code when working with the Zsh configuration.

## Project Overview

**XDG-compliant Zsh configuration** with modular organization using **Zinit** plugin manager.

## Directory Structure

```
zsh/
├── .zshenv                # Bootstrap (symlinked to ~/.zshenv)
└── .config/zsh/
    ├── .zprofile           # Login shell initialization
    ├── .zshrc              # Interactive shell orchestrator
    ├── completions/        # Custom completion scripts
    ├── conf.d/
    │   ├── 00-env.zsh      # Core environment variables
    │   ├── 05-path.zsh     # PATH/FPATH/MANPATH management
    │   ├── 10-options.zsh  # Shell options
    │   ├── 15-history.zsh  # History configuration
    │   ├── 20-aliases.zsh  # Command aliases
    │   ├── 30-completion.zsh # Completion system
    │   ├── 40-plugins.zsh  # Zinit plugin loading (turbo mode)
    │   ├── 50-prompt.zsh   # Prompt configuration
    │   ├── 60-keybinds.zsh # Key bindings
    │   ├── 70-tools.zsh    # Tool initialization (fzf, zoxide, etc.)
    │   └── 99-local.zsh    # Local overrides (gitignored)
    └── functions/          # Autoloaded shell functions
```

## Setup

```bash
stow zsh
zsh -c 'echo "ZDOTDIR: $ZDOTDIR"'
```

## Architecture

### Shell Startup Sequence

```
.zshenv (ALL shells) → .zprofile (login) → .zshrc (interactive)
```

| File | Loaded by | Purpose |
|------|-----------|---------|
| `.zshenv` | ALL | XDG paths, ZDOTDIR, tool redirects |
| `.zprofile` | Login | Environment, PATH, SSH agent |
| `.zshrc` | Interactive | Sources conf.d/* in order |

### Module Loading Order

**Critical**: `.zshrc` sources `conf.d/` in numeric order. Dependencies must be respected.

| Module | Depends on | Purpose |
|--------|------------|---------|
| `00-env.zsh` | — | XDG variables, tool redirects |
| `05-path.zsh` | 00 | PATH/FPATH/MANPATH |
| `10-options.zsh` | — | Shell options |
| `15-history.zsh` | 00 (XDG_CACHE_HOME) | History config |
| `20-aliases.zsh` | — | Aliases |
| `30-completion.zsh` | 05 (fpath) | Completion init |
| `40-plugins.zsh` | 00, 30 | Zinit plugins |
| `50-prompt.zsh` | 40 (async) | Prompt theme |
| `60-keybinds.zsh` | 40 (widgets) | Key bindings |
| `70-tools.zsh` | 00 (paths) | Tool initialization |
| `99-local.zsh` | — | Machine-specific, not tracked |

When adding modules: use appropriate numeric prefix, document prerequisites in header, ensure idempotency.

### What Goes Where

| Content | File | Reason |
|---------|------|--------|
| XDG_* variables | `.zshenv` | Needed by all shells |
| ZDOTDIR | `.zshenv` | Must be set before other files |
| Tool XDG redirects | `00-env.zsh` | Login shell context |
| PATH changes | `05-path.zsh` | Login only |
| Aliases | `20-aliases.zsh` | Interactive only |
| Functions | `functions/` | Autoloaded, interactive only |
| Plugins | `40-plugins.zsh` | Interactive only |
| Local overrides | `99-local.zsh` | Machine-specific |

## Plugin System (Zinit)

Installed at `$XDG_DATA_HOME/zinit/` via turbo mode in `40-plugins.zsh`:

| Plugin | Purpose |
|--------|---------|
| fast-syntax-highlighting | Real-time syntax highlighting |
| zsh-history-substring-search | Enhanced history search |
| zsh-autosuggestions | Autosuggestions from history |
| fzf-tab | fzf-powered completion menu |
| zsh-completions | Additional completions |
| autopair | Auto-close brackets/quotes |

## Coding Standards

See `.claude/rules/shell/` for: `shell.md` (scripting patterns), `zsh.md` (file headers, section styles, idempotency).

## Validation

```bash
zsh -n .config/zsh/.zshrc                                    # Syntax check
zsh -c 'source ~/.zshenv && source $ZDOTDIR/.zshrc && echo OK'  # Full load
exec zsh                                                      # Reload shell
```
