# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **workspace configuration repository** (dotfiles) that provides comprehensive development environment setup for macOS. It contains configurations for multiple tools:

- **Emacs** - Text editor with LSP, completion, and project management
- **Vim** - Lightweight modal editor with modern enhancements
- **Zsh** - Shell with Oh My Zsh framework, plugins, and completions
- **Development tools** - Language runtimes (Go, Python, Rust), GPG, Pass, etc.

This is a **monorepo** containing configurations for multiple independent tools, not a single application.

## Repository Structure

```
oh-my-workspace/
├── emacs/           # Emacs configuration (lisp/, init.el, setup.sh)
├── vim/             # Vim configuration (vimrc, setup.sh)
├── zsh/             # Zsh configuration (zshrc, aliases, functions)
├── README.md        # Comprehensive setup guide for all tools
└── CLAUDE.md        # This file (AI assistant guidance)
```

## Setup Commands

```bash
# Emacs configuration (creates ~/.emacs symlink)
./emacs/setup.sh

# Vim configuration (creates ~/.vimrc symlink)
./vim/setup.sh

# Zsh configuration is managed separately (source ~/.zshrc from this repo)
```

Each setup script:
1. Creates symbolic links to configuration files
2. Displays dependency installation guides
3. Validates environment setup

## Common Development Commands

### Emacs
| Command | Function |
|---------|----------|
| `M-x config-dependency-validate` | Verify external dependencies (LSP servers, tools) |
| `M-x cleanup-config-timers` | Clean up configuration timers |
| `C-c p p` | Switch project (Projectile) |
| `C-c p f` | Find file in project |
| `C-x b` | Switch buffer (Vertico) |
| `C-s` | Search in buffer (Consult) |

### Zsh
| Command | Function |
|---------|----------|
| `menu` | Browse command history with fzf |
| `j` | Autojump to frequently visited directories |
| `gcd` | Go to git repository root |

## Quick Start

### First-time Emacs Setup

```bash
cd ~/oh-my-workspace/emacs
./setup.sh
emacs

# Inside Emacs, verify dependencies
M-x config-dependency-validate
```

### Verify Installation

```bash
# Test Emacs config loads without errors
emacs --debug-init

# Inside Emacs, run dependency validation
M-x config-dependency-validate
```

## Emacs Configuration Architecture

The Emacs configuration (`emacs/`) is modular with a specific loading order defined in `init.el`:

### Module Loading Order

1. **Core**: `init-packages` → `init-funcs` → `init-base`
2. **UI**: `init-fonts` → `init-ui`
3. **Editor**: `init-completion` → `init-editing` → `init-dired` → `init-projects`
4. **Tools**: `init-vc` → `init-terminal` → `init-ai` → `init-auth`
5. **Languages**: `init-prog` → language-specific modules (Python, Go, C/C++, etc.)

### Key Architecture Patterns

- **Dependency Injection**: All modules `require 'init-funcs` for utilities
- **Validation System**: `config-dependency-validate` checks external tools via registered validators
- **Timer Management**: `run-config-timer` defers expensive operations for faster startup
- **Package Management**: Uses `straight.el` with `use-package` macros for package management
- **Advisable System**: Many hooks use `advice-add` for extension rather than redefinition

### Adding New Modules

1. Create file in appropriate location:
   - `lisp/` - Core editor features
   - `lisp/tools/` - External tool integrations
   - `lisp/lang/` - Language-specific configurations
2. Add `(require 'init-funcs)` if using utilities
3. Add `(provide 'module-name)` at end
4. Add `(require 'module-name)` in `init.el` at correct position based on loading order

### Customization

User-specific settings go in `emacs/custom_settings.el` (git-ignored). This file is automatically created on first run and can override any defcustom variable.

## Vim Configuration

The Vim configuration (`vim/`) is managed through `vimrc` with:
- Plugin management via vim-plug
- Custom color schemes and themes
- NERDTree for file navigation
- FZF for fuzzy finding
- Custom keybindings for productivity

## Zsh Configuration

The Zsh configuration is spread across:
- `zsh/zshrc` - Main configuration file
- `zsh/aliases` - Command aliases
- `zsh/functions` - Custom functions
- `zsh/completions` - Custom completions

Key features:
- **fzf integration**: Menu-based command history browsing
- **autojump**: Directory jumping based on frequency
- **zsh-autosuggestions**: Command suggestions based on history
- **syntax-highlighting**: Real-time syntax highlighting as you type
- **zsh-completions**: Enhanced completions for various commands

## External Dependencies

### Verify Dependencies
```bash
# Run in Emacs
M-x config-dependency-validate
```

### Core Tools (P0 - Required)
| Tool | Purpose | Install |
|------|---------|---------|
| git | Version control | brew install git |
| ripgrep | Code search | brew install ripgrep |
| the_silver_searcher | Fallback search | brew install the_silver_searcher |
| fd | Fast file find | brew install fd |
| coreutils | macOS GNU ls | brew install coreutils |

### LSP Servers (P1 - Development)
| Language | Server | Install |
|----------|--------|---------|
| Python | pylsp | pip install python-lsp-server[all] |
| Go | gopls | go install golang.org/x/tools/gopls@latest |
| C/C++ | clangd | Xcode Command Line Tools |
| YAML | yaml-language-server | npm install -g yaml-language-server |
| Bash | bash-language-server | npm install -g bash-language-server |
| Dockerfile | docker-langserver | npm install -g dockerfile-language-server-nodejs |
| CMake | cmake-language-server | pip install cmake-language-server |

### Formatters (P1 - Development)
| Language | Formatter | Install |
|----------|-----------|---------|
| Python | black | pip install black |
| Go | gofumpt | go install mvdan.cc/gofumpt@latest |
| C/C++ | clang-format | Xcode Command Line Tools |

### Auxiliary Tools (P2 - Optional)
| Tool | Purpose | Install |
|------|---------|---------|
| aspell | Spell checking | brew install aspell |
| hunspell | Spell checking alternative | brew install hunspell |
| pandoc | Document conversion | brew install pandoc |
| marksman | Markdown LSP | brew install marksman |
| libvterm | Emacs vterm dependency | brew install libvterm |

### Fonts
Run `M-x nerd-icons-install-fonts` after setup to install icon fonts.

## Validation

```bash
# Verify Emacs config loads without errors
emacs --debug-init

# Inside Emacs, validate dependencies
M-x config-dependency-validate
```

## Troubleshooting

### Slow Emacs Startup
1. Check for byte-compile cache issues
   ```bash
   rm -rf ~/.emacs.d/eln-cache/
   ```
2. Run dependency validation
   ```bash
   M-x config-dependency-validate
   ```
3. Review startup logs
   ```bash
   C-h e  ; View *Messages* buffer
   ```

### Package Installation Fails
1. Check network and proxy settings
   ```elisp
   M-x show-http-proxy  ; Check current proxy
   M-x set-http-proxy   ; Set proxy if needed
   ```
2. Refresh package contents
   ```bash
   M-x package-refresh-contents
   ```
3. Verify use-package is installed
   ```bash
   M-x package-install RET use-package
   ```

### LSP Not Working
1. Confirm LSP server is installed
   ```bash
   M-x config-dependency-validate
   ```
2. Check eglot status
   ```bash
   M-x eglot  ; Start LSP manually
   ```
3. Review LSP events log
   ```bash
   C-h b  ; Switch to *eglot-events* buffer
   ```

## Git Workflow

### Commit Messages
Follow conventional commit format:
```
<type>: <description>

<optional body>

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
```

Types: `feat`, `fix`, `refactor`, `docs`, `test`, `chore`, `perf`, `ci`, `style`

### Branches
- **master** - Main development branch
- Feature branches are typically created for specific tool improvements

## Important Notes

- **Multi-tool repository**: This is NOT a single application but configurations for multiple independent tools
- **macOS-focused**: Most configurations assume macOS as the primary OS
- **Symlink-based setup**: Configuration files are symlinked rather than copied for easier updates
- **Validation-driven**: Use `M-x config-dependency-validate` to check if all required tools are installed
- **Emacs-centric**: The Emacs configuration is the most complex and has the most extensive validation system
