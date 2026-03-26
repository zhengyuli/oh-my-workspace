---
version: "1.0.0"
last-updated: "2026-03-27"
maintainer: "zhengyu.li"
---

# CLAUDE.md

macOS dotfiles repository using GNU Stow for XDG-compliant configuration management.

## Tech Stack

@.claude/shared/tech-stack.md

## Prerequisites

Before using this repository, ensure you have:

- **GNU Stow** - For symlink management
  ```bash
  brew install stow
  ```
- **Git** - Version control (usually pre-installed on macOS)
- **Zsh** - Default shell (macOS default since Catalina)
- **Emacs** - For Emacs configuration (optional, `brew install emacs`)

Additional tools are documented in the Tech Stack section above.

## Getting Started

For new users setting up the dotfiles repository:

1. **Clone the repository:**
   ```bash
   git clone https://github.com/zhengyu-li/oh-my-workspace.git
   cd oh-my-workspace
   ```

2. **Install prerequisites:**
   ```bash
   brew install stow
   ```

3. **Preview changes (recommended):**
   ```bash
   ./setup.sh install --dry-run --all
   ```

4. **Run full setup:**
   ```bash
   ./setup.sh install --all
   ```

5. **Verify installation:**
   ```bash
   ./setup.sh status
   ```

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

### Directory Structure

```
oh-my-workspace/
├── claude/             # Claude Code environment
│   └── setup.md        # Setup guide for Claude Code
├── shell/              # Shell configurations
│   ├── starship/      # Starship prompt config
│   └── zsh/           # Zsh config (.zshrc, .zshenv)
├── editor/            # Text editors
│   ├── emacs/         # Emacs modular config
│   └── vim/           # Neovim config
├── lang/              # Language runtimes
│   ├── python/        # Python (uv) config
│   │   └── uv/       # uv package manager
│   └── typescript/    # TypeScript (bun) config
│       └── bun/      # bun runtime
├── tool/              # CLI utilities
│   ├── git/           # Git config
│   ├── lazygit/       # Lazygit config
│   ├── ripgrep/       # Ripgrep config
│   └── yazi/          # Yazi file manager
├── term/              # Terminal emulators
│   └── ghostty/       # Ghostty config
└── platform/          # Platform-specific
    └── darwin/        # macOS scripts
```

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

## Automated Hooks

This project uses Claude Code hooks for automated validation and verification:

- **PostToolUse**: Syntax validation after file edits (shell, elisp, python)
- **Stop**: Session end verification (uncommitted changes, commit format)
- **Pre-commit**: Blocks `.elc` files, warns on potential issues

Hook scripts are stored in `.claude/hooks/` and committed to the repository.

See `hooks.md` for detailed documentation of hook behavior and configuration.

## Coding Conventions

Detailed conventions in `.claude/rules/`:

### Universal Standards
- `coding-style.md` — Line length (80 chars), file headers, documentation
- `quality.md` — Quality standards for AI-assisted code generation
- `patterns.md` — Design patterns, immutability principle
- `security.md` — Secrets management, input validation

### Workflow & Process
- `dev-workflow.md` — Research-first approach, verification phases
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

| Task              | Command                                  |
|-------------------|------------------------------------------|
| First-time setup  | See Getting Started section above        |
| Prerequisites     | `brew install stow`                      |
| Claude Code setup | See `claude/setup.md`                    |
| Full setup        | `./setup.sh install --all`               |
| Stow package      | `./setup.sh install <package>`           |
| Restow            | `./setup.sh install --force <package>`   |
| Preview           | `./setup.sh install --dry-run <package>` |
| Status            | `./setup.sh status`                      |
| Unstow            | `./setup.sh uninstall <package>`         |
| Hook validation   | Automatic (see Automated Hooks section)  |

## Troubleshooting

### Common Issues

**Stow conflict: file already exists**

GNU Stow refuses to create a symlink if the target already exists as a real file.

```bash
# Backup and remove conflicting file
cp ~/.zshrc ~/.zshrc.bak
rm ~/.zshrc

# Now stow succeeds
./setup.sh install zsh
```

**Emacs not loading changes**

Emacs always prefers `.elc` files over `.el` files when both exist. Editing `.el` files without removing stale `.elc` files means Emacs loads the old compiled version.

```bash
# Clean stale .elc files (see lang/elisp.md for details)
find ~/.config/emacs/ -name '*.elc' -delete

# Restart Emacs to load the .el source files
```

**Symlink points to wrong location**

If a symlink is broken or points to the wrong location, restow the package.

```bash
# Remove broken symlink
rm ~/.config/zsh/conf.d/aliases.zsh

# Restow package to recreate symlink
./setup.sh install --force zsh
```

**Git hooks not running**

Git hooks must be executable and in the correct location.

```bash
# Verify hook is executable
ls -la .git/hooks/pre-commit

# Make executable if needed
chmod +x .git/hooks/pre-commit
```

**Setup script fails with permission error**

Some operations require elevated permissions or correct file ownership.

```bash
# Check current permissions
ls -la ~/ | grep -E "^\."

# Fix ownership if needed
sudo chown -R $(whoami) ~/.config ~/.local
```

### Getting Help

- **Rule documentation**: Check `.claude/rules/` directory for detailed standards
- **Setup help**: Run `./setup.sh help` for command reference
- **Project README**: See `README.md` for project overview and setup guide
- **Claude Code setup**: See `claude/setup.md` for Claude Code environment setup

---

For detailed coding standards and workflows, see `.claude/rules/` directory.
