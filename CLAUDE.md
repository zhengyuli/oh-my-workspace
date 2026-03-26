# CLAUDE.md

macOS dotfiles repository using GNU Stow for XDG-compliant configuration management.

## Tech Stack

- **Shell**: Zsh + Starship prompt, zoxide, direnv, carapace
- **Editors**: Neovim, Emacs (modular configurations)
- **Languages**: Python (uv), TypeScript (bun), Go, Rust
- **Tools**: git, lazygit, ripgrep, fd, fzf, eza, bat, yazi
- **Terminal**: Ghostty

## Architecture

### Package Structure

Organized by category in repository root:
- `shell/` — Shell configs (zsh, starship)
- `editor/` — Text editors (vim, emacs)
- `term/` — Terminal emulators (ghostty)
- `tool/` — CLI utilities (git, lazygit, ripgrep, yazi)
- `lang/` — Language runtimes (python/uv, typescript/bun)
- `platform/` — Platform-specific configs (darwin)

Files follow GNU Stow convention: placed as they appear in `$HOME`.

### Key Components

**Zsh**: XDG-compliant two-stage loading (`~/.zshenv` → `$ZDOTDIR/conf.d/*.zsh`)

**Emacs**: Modular structure (`init.el` → `lisp/{editor,lang,tool,lib}/`)

**Stow**: Automatic conflict resolution via `_remove_stow_conflicts()` in setup.sh

## Development Workflow

```bash
# Full setup (prerequisites + packages + symlinks)
./setup.sh install --all

# Stow specific packages
./setup.sh install zsh git vim

# Restow after changes
./setup.sh install --force zsh

# Preview changes
./setup.sh install --dry-run --all

# Check status
./setup.sh status
```

**Package Registry**: See `PKG_ALL` array in `setup.sh`

**Dependencies**: Managed via `pkg/homebrew/Brewfile`

## Coding Conventions

Detailed conventions in `.claude/rules/`:

### Universal Standards
- `coding-style.md` — Line length (80 chars), file headers, documentation
- `ai-generation.md` — AI-specific constraints and quality checklists
- `patterns.md` — Design patterns, immutability principle
- `security.md` — Secrets management, input validation

### Workflow & Process
- `development-workflow.md` — Research-first approach, verification phases
- `git-workflow.md` — Conventional Commits, branch naming
- `hooks.md` — Claude Code automation hooks

### Language-Specific (conditionally loaded)
- `lang/shell.md` — Universal shell practices (base)
- `lang/bash.md` — Bash-specific features (namerefs, associative arrays)
- `lang/zsh.md` — Zsh-specific features (globbing, hooks)
- `lang/elisp.md` — Emacs Lisp conventions (lexical binding, ERT testing)
- `lang/python.md` — Python standards (type hints, pytest)

**See `.claude/rules/README.md` for complete rule documentation.**

## Quick Reference

| Task | Command |
|------|---------|
| Full setup | `./setup.sh install --all` |
| Stow package | `./setup.sh install <package>` |
| Restow | `./setup.sh install --force <package>` |
| Preview | `./setup.sh install --dry-run <package>` |
| Status | `./setup.sh status` |
| Unstow | `./setup.sh uninstall <package>` |

---

For detailed coding standards and workflows, see `.claude/rules/` directory.
