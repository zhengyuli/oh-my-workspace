# oh-my-workspace

> A curated macOS development environment with Zsh, Neovim/Emacs, and modern CLI tools.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![macOS](https://img.shields.io/badge/Platform-macOS-lightgrey.svg)]()
[![Shell: Zsh](https://img.shields.io/badge/Shell-Zsh-green.svg)]()
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)]()

## Quick Start

```bash
# Clone the repository
git clone https://github.com/zhengyuli/oh-my-workspace.git ~/oh-my-workspace
cd ~/oh-my-workspace

# Install everything (prerequisites + packages + symlinks)
./setup.sh install --all

# Open a new terminal or source zshenv
source ~/.zshenv
```

That's it. Your development environment is ready.

**What this does:**
- Installs Xcode CLI, Homebrew, and GNU Stow
- Installs all packages from `Brewfile` (see [Package Reference](#package-reference))
- Creates symlinks for all dotfiles using GNU Stow
- Offers to switch your default shell to Zsh

## Features

- **Modern Shell** — Zsh with Starship prompt, zoxide (smart cd), direnv, and carapace completions
- **Dual Editor Setup** — Neovim and Emacs configurations included
- **Fast Terminal** — Ghostty with tmux multiplexing
- **Powerful Search** — ripgrep, fd, fzf, and eza for file operations
- **Language Runtimes** — Pre-configured for Python (uv), TypeScript (bun), Go, and Rust
- **Git Workflow** — git + lazygit + git-delta + GPG signing support
- **One-Command Setup** — `./setup.sh install --all` handles everything
- **Clean Symlinks** — GNU Stow manages dotfiles without cluttering `$HOME`

## Installation

### Prerequisites

| Requirement | How to Check      | Auto-Installed? |
|-------------|-------------------|-----------------|
| macOS       | `uname -s`        | —               |
| Bash 4.3+   | `bash --version`  | —               |
| Xcode CLI   | `xcode-select -p` | Yes             |
| Homebrew    | `brew --version`  | Yes             |
| GNU Stow    | `stow --version`  | Yes             |

### Full Installation

```bash
./setup.sh install --all
```

This command:
1. Installs Xcode Command Line Tools (prompts for confirmation)
2. Installs Homebrew (if not present)
3. Installs GNU Stow via Homebrew
4. Runs `brew bundle` to install all packages from `Brewfile`
5. Stows all dotfile packages using GNU Stow
6. Offers to switch your default shell to Zsh

### Partial Installation

Install specific packages only (prerequisites must already be installed):

```bash
# Stow specific packages
./setup.sh install zsh git vim

# Preview changes without modifying files
./setup.sh install --dry-run zsh
```

### Updating

After adding new dotfiles to a package:

```bash
# Restow a specific package to pick up changes
./setup.sh install --force zsh

# Restow everything
./setup.sh install --force --all
```

### Uninstalling

```bash
# Remove all symlinks
./setup.sh uninstall --all

# Remove specific packages
./setup.sh uninstall zsh git
```

### Checking Status

```bash
./setup.sh status
```

Output shows:
- Prerequisites status (installed/missing)
- Which packages are stowed
- Symlink paths for each stowed package

## Directory Structure

```
oh-my-workspace/
├── setup.sh              # Main setup script
├── CLAUDE.md
├── LICENSE
├── README.md
│
├── claude/               # Claude Code environment
│   └── setup.md          # Setup guide for Claude Code
│
├── docs/                 # Documentation
│
├── shell/                # Shell configuration
│   ├── zsh/              # ~/.config/zsh/, ~/.zshenv
│   └── starship/         # ~/.config/starship.toml
│
├── editor/               # Text editors
│   ├── vim/              # ~/.config/vim/
│   └── emacs/            # ~/.config/emacs/
│
├── term/                 # Terminal emulators
│   └── ghostty/          # ~/.config/ghostty/
│
├── tool/                 # CLI tools
│   ├── git/              # ~/.config/git/
│   ├── lazygit/          # ~/.config/lazygit/
│   ├── ripgrep/          # ~/.config/ripgrep/
│   └── yazi/             # ~/.config/yazi/
│
├── lang/                 # Language runtimes
│   ├── python/uv/        # ~/.config/uv/
│   └── typescript/bun/   # ~/.config/bun/
│
├── platform/             # Platform-specific configs
│   └── darwin/           # Darwin/macOS preferences
│
└── pkg/                  # Package management
    └── homebrew/         # Brewfile
```

Each package directory follows the [GNU Stow](https://www.gnu.org/software/stow/manual/) convention: files are placed as they would appear in `$HOME`.

## Customization

### Fork and Adapt

1. **Fork this repository** on GitHub
2. **Clone your fork:**
   ```bash
   git clone https://github.com/YOUR_USERNAME/oh-my-workspace.git ~/oh-my-workspace
   ```
3. **Remove packages you don't need** — delete directories from `PKG_ALL` in `setup.sh`
4. **Add your own dotfiles** — create new package directories following the structure
5. **Customize the Brewfile** — edit `pkg/homebrew/Brewfile` to add/remove packages

### Adding a New Package

```bash
# Create package directory structure
mkdir -p tool/mytool/.config/mytool

# Add your config files
echo "my-setting = value" > tool/mytool/.config/mytool/config.conf

# Register in setup.sh
# Add "tool/mytool" to PKG_ALL array

# Stow the new package
./setup.sh install mytool
```

### Package Naming Convention

| Category    | Purpose            | Examples                  |
|-------------|--------------------|---------------------------|
| `shell/`    | Shell and prompt   | zsh, starship             |
| `editor/`   | Text editors       | vim, emacs                |
| `term/`     | Terminal emulators | ghostty                   |
| `tool/`     | CLI utilities      | git, ripgrep, yazi        |
| `lang/`     | Language runtimes  | python/uv, typescript/bun |
| `platform/` | Platform-specific  | darwin                    |

### Overriding Defaults

To override a specific config without modifying the stowed file:

```bash
# Create a local override (not tracked by git)
mkdir -p ~/.config/zsh/local
echo "alias mycmd='echo hello'" > ~/.config/zsh/local/aliases.zsh
```

## Package Reference

### Shell Environment

| Package                                         | Description                                                   |
|-------------------------------------------------|---------------------------------------------------------------|
| [zsh](https://www.zsh.org/)                     | Powerful shell with advanced completion and scripting         |
| [starship](https://starship.rs/)                | Fast, customizable cross-shell prompt                         |
| [direnv](https://direnv.net/)                   | Automatically load/unload environment variables per directory |
| [zoxide](https://github.com/ajeetdsouza/zoxide) | Smart `cd` command with frecency tracking                     |
| [carapace](https://carapace.sh/)                | Multi-shell completion system                                 |

### Editors

| Package                                      | Description                          |
|----------------------------------------------|--------------------------------------|
| [neovim](https://neovim.io/)                 | Hyperextensible Vim-based editor     |
| [emacs](https://www.gnu.org/software/emacs/) | Extensible, customizable text editor |

### Terminal

| Package                              | Description                    |
|--------------------------------------|--------------------------------|
| [ghostty](https://ghostty.org/)      | Fast, native terminal emulator |
| [tmux](https://github.com/tmux/tmux) | Terminal multiplexer           |

### File Operations

| Package                                          | Description                            |
|--------------------------------------------------|----------------------------------------|
| [ripgrep](https://github.com/BurntSushi/ripgrep) | Fast recursive grep (`rg`)             |
| [fd](https://github.com/sharkdp/fd)              | Fast, user-friendly `find` alternative |
| [fzf](https://github.com/junegunn/fzf)           | Command-line fuzzy finder              |
| [eza](https://github.com/eza-community/eza)      | Modern `ls` with icons and git status  |
| [bat](https://github.com/sharkdp/bat)            | `cat` clone with syntax highlighting   |
| [yazi](https://yazi-rs.github.io/)               | Terminal file manager                  |

### Development Tools

| Package                                             | Description                          |
|-----------------------------------------------------|--------------------------------------|
| [git](https://git-scm.com/)                         | Distributed version control          |
| [lazygit](https://github.com/jesseduffield/lazygit) | Simple terminal UI for git           |
| [git-delta](https://github.com/dandavison/delta)    | Syntax-highlighting pager for git    |
| [gnupg](https://www.gnupg.org/)                     | GNU Privacy Guard for commit signing |
| [pass](https://www.passwordstore.org/)              | Standard Unix password manager       |

### Language Runtimes

| Package                          | Description                                       |
|----------------------------------|---------------------------------------------------|
| [uv](https://docs.astral.sh/uv/) | Fast Python package manager (replaces pip/pipenv) |
| [bun](https://bun.sh/)           | JavaScript/TypeScript runtime and package manager |
| [go](https://go.dev/)            | Go programming language                           |
| [rustup](https://rustup.rs/)     | Rust toolchain installer                          |

### Utilities

| Package                                    | Description                  |
|--------------------------------------------|------------------------------|
| [jq](https://stedolan.github.io/jq/)       | Command-line JSON processor  |
| [yq](https://github.com/mikefarah/yq)      | Command-line YAML processor  |
| [wget](https://www.gnu.org/software/wget/) | Network downloader           |
| [htop](https://htop.dev/)                  | Interactive process viewer   |
| [pandoc](https://pandoc.org/)              | Universal document converter |

### GUI Applications

| Package                                       | Description               |
|-----------------------------------------------|---------------------------|
| [Emacs](https://www.gnu.org/software/emacs/)  | Emacs as macOS application |
| [Rectangle](https://rectangleapp.com/)        | Window management          |

## Migration Notes

### Switching from Other Dotfiles

If you're migrating from another dotfiles setup:

1. **Backup existing configs:**
   ```bash
   # Create backup directory
   mkdir -p ~/.config/backup

   # Move conflicting files
   mv ~/.config/zsh ~/.config/backup/
   mv ~/.config/git ~/.config/backup/
   ```

2. **Run the installer:**
   ```bash
   ./setup.sh install --all
   ```

3. **Compare and merge:** Review backed-up configs and copy any personal customizations to the new setup.

### Adopting Specific Packages Only

You don't have to use everything. Install only what you need:

```bash
# Just the shell setup
./setup.sh install zsh starship

# Just git configuration
./setup.sh install git

# Preview before applying
./setup.sh install --dry-run zsh
```

### Preserving Existing Homebrew Packages

The installer runs `brew bundle`, which:
- Installs missing packages
- Skips already-installed packages
- Does NOT remove packages not in the Brewfile

Your existing Homebrew packages are safe.

### Manual Symlink Management

If you prefer manual control over symlinks:

```bash
# Check what a package would link
stow -n -v -d "$WORKSPACE_DIR/shell" -t ~ zsh

# Stow manually
stow -v -d "$WORKSPACE_DIR/shell" -t ~ zsh

# Unstow manually
stow -D -v -d "$WORKSPACE_DIR/shell" -t ~ zsh
```

## Troubleshooting

### Common Issues

**"Xcode CLI installation failed"**
```bash
# Manual installation
xcode-select --install
```

**"Homebrew not found after installation"**
```bash
# Add Homebrew to PATH (Apple Silicon)
eval "$(/opt/homebrew/bin/brew shellenv)"
```

**"Stow conflict: existing file"**
```bash
# Option 1: Preview what would be linked
./setup.sh install --dry-run zsh

# Option 2: Force restow (replaces conflicting symlinks)
./setup.sh install --force zsh
```

**"Changes not appearing in new terminal"**
```bash
# Source zshenv manually
source ~/.zshenv

# Or verify symlinks exist
./setup.sh status
```

### Getting Help

```bash
# Check status of all packages
./setup.sh status

# View available commands
./setup.sh help
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'feat: add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.

## Acknowledgments

Inspired by:
- [mathiasbynens/dotfiles](https://github.com/mathiasbynens/dotfiles)
- [holman/dotfiles](https://github.com/holman/dotfiles)
- [thoughtbot/dotfiles](https://github.com/thoughtbot/dotfiles)
