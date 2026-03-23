# Oh My Workspace

A comprehensive macOS development environment configuration repository featuring XDG-compliant dotfiles, Homebrew package management, shell/editor/IDE configurations, and system preferences automation.

- **Unified theme**: Doom One across Emacs, Ghostty, Starship, and fzf
- **Modern toolchain**: bun (JS/TS), uv (Python), no version managers
- **XDG-compliant**: Clean separation of config, cache, data, and state
- **Interactive setup**: Single script for install, update, and uninstall
- **Category-organized**: Directories organized by type for easy extension

## Directory Structure

Directories are organized by category for better organization and cross-platform extensibility:

```
oh-my-workspace/
├── setup.sh           # Interactive setup script
├── editor/            # Editor configurations
│   ├── emacs/         # Emacs (>= 30.2)
│   └── vim/           # Vim
├── shell/             # Shell configurations
│   ├── zsh/           # Zsh
│   └── starship/      # Starship prompt
├── runtime/           # Language runtimes
│   ├── bun/           # Bun JS/TS runtime
│   └── uv/            # UV Python package manager
├── terminal/          # Terminal emulators
│   └── ghostty/       # Ghostty terminal
├── vcs/               # Version control
│   └── git/           # Git
├── tools/             # CLI tools
│   └── ripgrep/       # Ripgrep
├── packages/          # System package managers
│   └── homebrew/      # Brewfile (NOT stowed)
└── platform/          # OS configurations
    └── macos/         # macOS settings (NOT stowed)
```

### Category Definitions

| Category | Purpose | Future Extensions |
|----------|---------|-------------------|
| `editor/` | Text editors | neovim, helix |
| `shell/` | Shell and prompt | fish, nushell |
| `runtime/` | Language runtimes | go, rust, deno |
| `terminal/` | Terminal emulators | alacritty, kitty, wezterm |
| `vcs/` | Version control | mercurial |
| `tools/` | CLI utilities | fd, bat, eza |
| `packages/` | System packages | apt, pacman, dnf (Linux) |
| `platform/` | OS configurations | linux |

## Quick Start

### Prerequisites

- **macOS** (Apple Silicon or Intel)
- **Xcode Command Line Tools**: `xcode-select --install`

### Installation

```bash
# Clone the repository
git clone https://github.com/zhengyuli/oh-my-workspace.git ~/oh-my-workspace
cd ~/oh-my-workspace

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

| Package | Category | Symlinks |
|---------|----------|----------|
| `zsh` | shell | `~/.zshenv` → `~/oh-my-workspace/shell/zsh/.zshenv` |
| `zsh` | shell | `~/.config/zsh/` → `~/oh-my-workspace/shell/zsh/.config/zsh/` |
| `starship` | shell | `~/.config/starship.toml` → `~/oh-my-workspace/shell/starship/.config/starship.toml` |
| `git` | vcs | `~/.config/git/` → `~/oh-my-workspace/vcs/git/.config/git/` |
| `vim` | editor | `~/.config/vim/` → `~/oh-my-workspace/editor/vim/.config/vim/` |
| `emacs` | editor | `~/.config/emacs/` → `~/oh-my-workspace/editor/emacs/.config/emacs/` |
| `ghostty` | terminal | `~/.config/ghostty/` → `~/oh-my-workspace/terminal/ghostty/.config/ghostty/` |
| `ripgrep` | tools | `~/.config/ripgrep/` → `~/oh-my-workspace/tools/ripgrep/.config/ripgrep/` |
| `uv` | runtime | `~/.config/uv/` → `~/oh-my-workspace/runtime/uv/.config/uv/` |
| `bun` | runtime | `~/.config/bun/` → `~/oh-my-workspace/runtime/bun/.config/bun/` |

### Manual Stow Commands

With the category-organized structure, use `-d` to specify the category directory:

| Command | Description |
|---------|-------------|
| `stow -d shell zsh` | Stow zsh from shell category |
| `stow -d editor emacs vim` | Stow multiple packages from editor category |
| `stow -D zsh` | Unstow (remove symlinks) |
| `stow -R zsh` | Restow (refresh symlinks) |
| `stow -n zsh` | Dry-run preview |

Or use the setup script which handles the category paths automatically:

```bash
./setup.sh install zsh git vim  # Works with just package names
```

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
stow -d shell zsh
```

The setup script automatically backs up conflicting files with `.bak.N` suffix.

### XDG paths not set

Ensure `~/.zshenv` is loaded first (it should be a symlink):

```bash
ls -la ~/.zshenv
# Should show: ~/.zshenv -> ~/oh-my-workspace/shell/zsh/.zshenv

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
