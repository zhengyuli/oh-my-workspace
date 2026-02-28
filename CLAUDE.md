# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **workspace configuration repository** (dotfiles) that provides comprehensive development environment setup for macOS. It includes configurations for Emacs, Vim, Zsh, and various development tools.

## Setup Commands

```bash
# Emacs configuration
./emacs/setup.sh    # Creates symlink ~/.emacs -> emacs/init.el

# Vim configuration
./vim/setup.sh      # Creates symlink ~/.vimrc -> vim/vimrc
```

## Emacs Configuration Architecture

The Emacs configuration is modular and loads in a specific order defined in `emacs/init.el`:

### Directory Structure

```
emacs/
├── init.el              # Entry point, early init, module loading
├── custom_settings.el   # User-specific settings (git-ignored)
├── lisp/
│   ├── init-packages.el # Package management (straight.el)
│   ├── init-funcs.el    # Utilities, timer system, validation
│   ├── init-base.el     # Core settings, GC management
│   ├── init-env.el      # Environment, proxy, macOS settings
│   ├── init-fonts.el    # Font configuration
│   ├── init-ui.el       # Theme, modeline, tabs, dashboard
│   ├── init-completion.el # Vertico, Corfu, Consult, etc.
│   ├── init-editing.el  # Editing enhancements
│   ├── init-dired.el    # File manager
│   ├── init-projects.el # Projectile
│   ├── tools/           # External tool integration
│   │   ├── init-vc.el       # Magit
│   │   ├── init-terminal.el # Vterm
│   │   ├── init-ai.el       # Claude Code IDE
│   │   └── init-auth.el     # Auth-source
│   └── lang/            # Language support
│       ├── init-prog.el     # Base programming mode
│       ├── init-python.el   # Python + LSP
│       ├── init-go.el       # Go + LSP
│       └── ...              # Other languages
```

### Module Loading Order

1. **Core**: `init-packages` → `init-funcs` → `init-base` → `init-env`
2. **UI**: `init-fonts` → `init-ui`
3. **Editor**: `init-completion` → `init-editing` → `init-dired` → `init-projects`
4. **Tools**: `init-vc` → `init-terminal` → `init-ai` → `init-auth`
5. **Languages**: `init-prog` → language-specific modules

### Key Patterns

- **Dependencies**: All modules `require 'init-funcs` for utilities
- **Validation**: `M-x config-dependency-validate` checks external dependencies
- **Timer System**: `run-config-timer` defers initialization for faster startup
- **Package Management**: Uses `straight.el` with `use-package` macros
- **Emacs Version**: Requires Emacs 30.2+

### Adding New Modules

1. Create file in appropriate location (`lisp/`, `lisp/tools/`, or `lisp/lang/`)
2. Add `(require 'init-funcs)` if using utilities
3. Add `(provide 'init-module-name)` at end
4. Add `(require 'init-module-name)` in `init.el` at correct position

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

### Auxiliary Tools (P2 - Optional)
| Tool | Purpose | Install |
|------|---------|---------|
| aspell | Spell checking | brew install aspell |
| hunspell | Spell checking alternative | brew install hunspell |
| pandoc | Document conversion | brew install pandoc |
| marksman | Markdown LSP | brew install marksman |
| libvterm | Emacs vterm dependency | brew install libvterm |

### Formatters (P1 - Development)
| Language | Formatter | Install |
|----------|-----------|---------|
| Python | black | pip install black |
| Go | gofumpt | go install mvdan.cc/gofumpt@latest |
| C/C++ | clang-format | Xcode Command Line Tools |

### Fonts
Run `M-x nerd-icons-install-fonts` after setup to install icon fonts.

## Validation

```bash
# Verify Emacs config loads without errors
emacs --debug-init

# Inside Emacs, validate dependencies
M-x config-dependency-validate
```
