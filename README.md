# Oh My Dotfiles

A modern, XDG-compliant dotfiles repository for macOS using GNU Stow for symlink management.

- **Unified theme**: Doom One across Emacs, Ghostty, Starship, and fzf
- **Modern toolchain**: bun (JS/TS), uv (Python), no version managers
- **XDG-compliant**: Clean separation of config, cache, data, and state
- **Interactive setup**: Single script for install, update, and uninstall

## Directory Structure

```
oh-my-dotfiles/
├── setup.sh           # Interactive setup script
├── zsh/               # Zsh configuration
├── git/               # Git configuration
├── vim/               # Vim configuration
├── emacs/             # Emacs configuration (>= 30.2)
├── ghostty/           # Ghostty terminal
├── ripgrep/           # Ripgrep configuration
├── uv/                # UV package manager
├── bun/               # Bun configuration
├── starship/          # Starship prompt
├── homebrew/          # Brewfile for dependencies
└── macos/             # macOS-specific settings
```

## Quick Start

### Prerequisites

- **macOS** (Apple Silicon or Intel)
- **Xcode Command Line Tools**: `xcode-select --install`

### Installation

```bash
# Clone the repository
git clone https://github.com/zhengyuli/oh-my-dotfiles.git ~/oh-my-dotfiles
cd ~/oh-my-dotfiles

# Run full installation (prerequisites + brew bundle + all packages)
./setup.sh install --all

# Or install specific packages (prerequisites must be present)
./setup.sh install zsh git vim emacs

# Apply changes
source ~/.zshenv
```

The setup script will:
1. Install Xcode Command Line Tools (if needed)
2. Install Homebrew (if needed)
3. Install packages from Brewfile (including GNU Stow)
4. Stow configuration packages via symlinks

### Update

```bash
# Update everything (prerequisites + brew bundle + restow stowed packages)
./setup.sh update --all

# Or restow only currently stowed packages (skip prerequisites/brew)
./setup.sh update --pkgs
```

### Uninstall

```bash
# Uninstall all stowed packages (prompts for confirmation)
./setup.sh uninstall --all

# Or uninstall specific packages
./setup.sh uninstall vim emacs
```

### Status

```bash
# View installation status for all packages
./setup.sh status

# Or check specific packages
./setup.sh status zsh git
```

### Restore

```bash
# Restore backed-up files for a package (unstows first if needed)
./setup.sh restore vim
```

### macOS Defaults

```bash
# Apply macOS system preferences (prompts for confirmation)
./setup.sh defaults
```

### All Commands

| Command | Description |
|---------|-------------|
| `./setup.sh install --all` | Full install: prerequisites + brew + all packages |
| `./setup.sh install <pkg>...` | Install specific packages |
| `./setup.sh uninstall --all` | Uninstall all stowed packages |
| `./setup.sh uninstall <pkg>...` | Uninstall specific packages |
| `./setup.sh update --all` | Update prerequisites + brew + restow all |
| `./setup.sh update --pkgs` | Restow stowed packages only |
| `./setup.sh update <pkg>...` | Restow specific packages |
| `./setup.sh restore <pkg>...` | Restore backups for packages |
| `./setup.sh status [<pkg>...]` | Show installation status |
| `./setup.sh defaults` | Apply macOS system defaults |
| `./setup.sh help` | Show help message |

## Stow Package Management

Packages are managed via GNU Stow, which creates symlinks from the repository to your home directory.

| Package | Symlinks |
|---------|----------|
| `zsh` | `~/.zshenv` → `~/oh-my-dotfiles/zsh/.zshenv` |
| `zsh` | `~/.config/zsh/` → `~/oh-my-dotfiles/zsh/.config/zsh/` |
| `git` | `~/.config/git/` → `~/oh-my-dotfiles/git/.config/git/` |
| `vim` | `~/.config/vim/` → `~/oh-my-dotfiles/vim/.config/vim/` |
| `emacs` | `~/.config/emacs/` → `~/oh-my-dotfiles/emacs/.config/emacs/` |
| `ghostty` | `~/.config/ghostty/` → `~/oh-my-dotfiles/ghostty/.config/ghostty/` |
| `ripgrep` | `~/.config/ripgrep/` → `~/oh-my-dotfiles/ripgrep/.config/ripgrep/` |
| `uv` | `~/.config/uv/` → `~/oh-my-dotfiles/uv/.config/uv/` |
| `bun` | `~/.config/bun/` → `~/oh-my-dotfiles/bun/.config/bun/` |
| `starship` | `~/.config/starship.toml` → `~/oh-my-dotfiles/starship/.config/starship.toml` |

### Manual Stow Commands

| Command | Description |
|---------|-------------|
| `stow zsh git` | Stow specific packages |
| `stow -D zsh` | Unstow (remove symlinks) |
| `stow -R zsh` | Restow (refresh symlinks) |
| `stow -n zsh` | Dry-run preview |

## Toolchain

Modern replacements for traditional version managers:

| Tool | Purpose | Replaces |
|------|---------|----------|
| **bun** | JS/TS runtime + package manager | node, npm, yarn, pnpm, fnm |
| **uv** | Python package manager | pip, pipenv, poetry, pyenv |
| **go** | Go programming language | (standard install) |

### LSP Servers

```bash
# JS/TS ecosystem (bun)
bun install -g typescript-language-server typescript
bun install -g vscode-langservers-extracted   # html/css/json/eslint
bun install -g bash-language-server
bun install -g @tailwindcss/language-server
bun install -g prettier

# Python ecosystem (uv)
uv tool install basedpyright
uv tool install ruff

# Go (standard install)
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest

# System-level (via Homebrew)
# clangd, lua-language-server, rust-analyzer
```

## Theme & Terminal

Unified **Doom One** theme across all tools:

| Tool | Configuration |
|------|---------------|
| Emacs | `doom-one` via `doom-themes` package |
| Ghostty | Custom color palette matching Doom One |
| fzf | Doom One colors in `FZF_DEFAULT_OPTS` |
| Starship | Prompt configuration |

**Terminal**: [Ghostty](https://ghostty.org/) - fast, native, XDG-compliant terminal emulator.

## XDG Base Directory

This repository follows the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html):

| Variable | Default | Purpose |
|----------|---------|---------|
| `XDG_CONFIG_HOME` | `~/.config` | Configuration files |
| `XDG_CACHE_HOME` | `~/.cache` | Cache files |
| `XDG_DATA_HOME` | `~/.local/share` | Data files |
| `XDG_STATE_HOME` | `~/.local/state` | State files |

## Verification

```bash
# Verify symlinks
ls -la ~/.zshenv ~/.config/zsh/.zshrc ~/.config/git/config

# Test shell configuration
zsh -c 'echo $ZDOTDIR'

# Test Emacs configuration
emacs --debug-init

# Check XDG paths
zsh -c 'echo $XDG_CONFIG_HOME'
```

## Troubleshooting

### Stow conflicts

If stow reports conflicts with existing files:

```bash
# Remove existing files and restow
stow -D zsh
rm ~/.zshenv
stow zsh
```

The setup script automatically backs up conflicting files with `.bak.N` suffix.

### XDG paths not set

Ensure `~/.zshenv` is loaded first (it should be a symlink):

```bash
ls -la ~/.zshenv
# Should show: ~/.zshenv -> ~/oh-my-dotfiles/zsh/.zshenv

# Verify
zsh -c 'echo $XDG_CONFIG_HOME'
# Should output: /Users/yourname/.config
```

### Shell not loading configuration

Restart your shell or source manually:

```bash
source ~/.zshenv
```

## License

[MIT](LICENSE) - Copyright (c) 2026 Zhengyu Li
