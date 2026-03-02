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
├── emacs/           # Emacs configuration
│   ├── init.el      # Main entry point (symlinked to ~/.emacs)
│   ├── lisp/        # Configuration modules
│   │   ├── init-*.el            # Core feature modules
│   │   ├── lang/                # Language-specific configs
│   │   └── tools/               # External tool integrations
│   ├── site-packages/ # Custom Emacs packages
│   └── setup.sh      # Installation script
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
| `M-x emacs-config-validate-all` | Verify all external dependencies (LSP servers, tools) |
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
M-x emacs-config-validate-all
```

### Verify Installation

```bash
# Test Emacs config loads without errors
emacs --debug-init

# Inside Emacs, validate dependencies
M-x emacs-config-validate-all
```

## Emacs Configuration Architecture

The Emacs configuration (`emacs/`) is modular with a specific loading order defined in `init.el`:

### Module Loading Order

Core modules (loaded in order):
1. **UI**: `init-ui` → `init-fonts`
2. **Editor**: `init-editing` → `init-completion` → `init-dired`
3. **Tools**: `init-vc` → `init-terminal` → `init-ai` → `init-auth`
4. **Languages**: `init-prog` → language-specific modules in `lisp/lang/`

Language modules (loaded via dolist):
- `init-elisp`, `init-cc`, `init-python`, `init-go`
- `init-typescript`, `init-dockerfile`, `init-cmake`, `init-yaml`, `init-markdown`

### Key Architecture Patterns

- **use-package**: All package configurations use `use-package` macros with `:ensure t`
- **Deferred Loading**: Most packages use `:defer t` for faster startup
- **Package Management**: Uses built-in `package.el` with MELPA, non-GNU, and GNU ELPA archives
- **Custom Packages**: Local packages in `site-packages/` are automatically added to load-path
- **Customization**: User settings go in `custom.el` (auto-generated, git-ignored)

### Directory Structure

- **`lisp/`**: Core Emacs Lisp modules (init-*.el)
- **`lisp/lang/`**: Language-specific configurations (LSP, formatting, etc.)
- **`lisp/tools/`**: External tool integrations (AI, authentication, terminal, etc.)
- **`site-packages/`**: Custom/locally-installed Emacs packages
  - `dired-single-0.3.1/`: Single-buffer dired
  - `dired-custom-extension-0.0.1/`: Custom dired enhancements
- **`templates/`**: File templates for new files
- **`snippets/`**: YASnippet code snippets
- **`banners/`**: Dashboard banner images

### Adding New Modules

1. Create file in appropriate location:
   - `lisp/` - Core editor features (init-*.el)
   - `lisp/lang/` - Language-specific configurations
   - `lisp/tools/` - External tool integrations
2. Add `(provide 'module-name)` at end
3. Add `(require 'module-name)` in `init.el` at correct position based on loading order

### Custom Packages

The `site-packages/` directory contains custom Emacs packages that are automatically added to `load-path`. When adding new custom packages:

1. Create a subdirectory in `site-packages/package-name/`
2. Add a `.el` file with `(provide 'package-name)`
3. Reference in other modules with `(require 'package-name)`

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
M-x emacs-config-validate-all
```

### Core Tools (P0 - Required)
| Tool | Purpose | Install |
|------|---------|---------|
| git | Version control | brew install git |
| ripgrep | Code search | brew install ripgrep |
| the_silver_searcher | Fallback search | brew install the_silver_searcher |
| fd | Fast file find | brew install fd |
| coreutils | macOS GNU ls | brew install coreutils |
| aspell | Spell checking | brew install aspell |
| pandoc | Document conversion | brew install pandoc |
| libvterm | Emacs vterm dependency | brew install libvterm |

### LSP Servers (P1 - Development)
| Language | Server | Install |
|----------|--------|---------|
| Python | pylsp | pip install python-lsp-server[all] |
| Go | gopls | go install golang.org/x/tools/gopls@latest |
| C/C++ | clangd | Xcode Command Line Tools |
| TypeScript | typescript-language-server | npm install -g typescript-language-server typescript |
| YAML | yaml-language-server | npm install -g yaml-language-server |
| Bash | bash-language-server | npm install -g bash-language-server |
| Dockerfile | docker-langserver | npm install -g dockerfile-language-server-nodejs |
| CMake | cmake-language-server | pip install cmake-language-server |
| Markdown | marksman | brew install marksman |

### Formatters (P1 - Development)
| Language | Formatter | Install |
|----------|-----------|---------|
| Python | black, isort | pip install black black-macchiato isort |
| Go | gofumpt | go install mvdan.cc/gofumpt@latest |
| C/C++ | clang-format | Xcode Command Line Tools |

### Debug Tools (P1 - Development)
| Tool | Purpose | Install |
|------|---------|---------|
| debugpy | Python debugger | pip install debugpy |
| pylint | Python linter | pip install pylint |

### Fonts
Run `M-x nerd-icons-install-fonts` after setup to install icon fonts.

## Validation

```bash
# Verify Emacs config loads without errors
emacs --debug-init

# Inside Emacs, validate dependencies
M-x emacs-config-validate-all
```

## Troubleshooting

### Slow Emacs Startup
1. Check for byte-compile cache issues
   ```bash
   rm -rf ~/.emacs.d/eln-cache/
   ```
2. Run dependency validation
   ```bash
   M-x emacs-config-validate-all
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

### LSP Not Working
1. Confirm LSP server is installed
   ```bash
   M-x emacs-config-validate-all
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

Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>
```

Types: `feat`, `fix`, `refactor`, `docs`, `test`, `chore`, `perf`, `ci`, `style`

### Branches
- **master** - Main development branch
- Feature branches are typically created for specific tool improvements

## Important Notes

- **Multi-tool repository**: This is NOT a single application but configurations for multiple independent tools
- **macOS-focused**: Most configurations assume macOS as the primary OS
- **Symlink-based setup**: Configuration files are symlinked rather than copied for easier updates
- **Validation-driven**: Use `M-x emacs-config-validate-all` to check if all required tools are installed
- **Emacs-centric**: The Emacs configuration is the most complex and has the most extensive validation system
- **Minimum Emacs version**: Requires Emacs >= 30.2
