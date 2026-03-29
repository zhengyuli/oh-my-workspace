# CLAUDE.md

macOS dotfiles repository using GNU Stow for XDG-compliant configuration management.

## Project Scope

**This is a configuration management repository**, not a code repository.

**Contains**: Configuration files (`.toml`, `.yml`, `.conf`, `.el`, `.sh`, `.zsh`)
**Purpose**: Manage dotfiles and tool configurations across machines

## Getting Started

For new users setting up the dotfiles repository:

1. **Clone the repository:**
   ```bash
   git clone https://github.com/zhengyuli/oh-my-workspace.git
   cd oh-my-workspace
   ```

2. **Preview changes (recommended):**
   ```bash
   ./setup.sh install --dry-run --all
   ```

3. **Run full setup:**
   ```bash
   ./setup.sh install --all
   ```

4. **Verify installation:**
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

**XDG Base Directory compliance**: Use `$XDG_CONFIG_HOME` (`~/.config`), `$XDG_DATA_HOME`, `$XDG_CACHE_HOME`, `$XDG_STATE_HOME` for all paths — never hardcode user-specific directories.

### Directory Structure

```
oh-my-workspace/
├── setup.sh           # Main setup script
├── CLAUDE.md
├── LICENSE
├── README.md
│
├── claude/            # Claude Code environment
│   └── setup.md       # Setup guide for Claude Code
│
├── docs/              # Documentation
│
├── shell/             # Shell configurations
│   ├── starship/      # Starship prompt config (~/.config/starship.toml)
│   └── zsh/           # Zsh config (~/.config/zsh/, ~/.zshenv)
│
├── editor/            # Text editors
│   ├── emacs/         # Emacs modular config (~/.config/emacs/)
│   └── vim/           # Vim config (~/.config/vim/)
│
├── term/              # Terminal emulators
│   └── ghostty/       # Ghostty config (~/.config/ghostty/)
│
├── tool/              # CLI utilities
│   ├── git/           # Git config (~/.config/git/)
│   ├── lazygit/       # Lazygit config (~/.config/lazygit/)
│   ├── ripgrep/       # Ripgrep config (~/.config/ripgrep/)
│   └── yazi/          # Yazi file manager (~/.config/yazi/)
│
├── lang/              # Language runtimes
│   ├── python/uv/     # uv config (~/.config/uv/)
│   └── typescript/bun/# bun config (~/.config/bun/)
│
├── platform/          # Platform-specific
│   └── darwin/        # macOS scripts (defaults.sh)
│
└── pkg/               # Package management
    └── homebrew/      # Brewfile
```

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

- `bash.md` — Bash shell scripting conventions
- `zsh.md` — Zsh-specific features and conventions
- `elisp.md` — Emacs Lisp conventions
- `config.md` — Configuration files (no extension)
- `toml.md` — TOML configuration files
- `yaml.md` — YAML configuration files

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

---

For detailed coding standards, see `.claude/rules/`.
