# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **workspace configuration repository** (dotfiles) that provides comprehensive development environment setup for macOS. It contains configurations for multiple tools:

- **Emacs** - Text editor with LSP, completion, and project management
- **Vim** - Lightweight modal editor with modern enhancements

This is a **monorepo** containing configurations for multiple independent tools, not a single application.

## Repository Structure

```
oh-my-workspace/
├── emacs/           # Emacs configuration
│   ├── init.el      # Main entry point (symlinked to ~/.emacs)
│   ├── lisp/        # Configuration modules
│   │   ├── init-*.el            # Core feature modules
│   │   ├── tools/               # Tool integrations (Magit, vterm, AI, auth)
│   │   └── lang/                # Language-specific configs
│   ├── site-packages/ # Custom Emacs packages
│   └── setup.sh      # Installation script
├── vim/             # Vim configuration
│   ├── vimrc        # Main configuration file
│   └── setup.sh     # Installation script
├── README.md        # Comprehensive setup guide for all tools
└── CLAUDE.md        # This file (AI assistant guidance)
```

## Setup Commands

```bash
# Emacs configuration (creates ~/.emacs symlink)
./emacs/setup.sh

# Vim configuration (creates ~/.vimrc symlink)
./vim/setup.sh
```

Each setup script:
1. Creates symbolic links to configuration files
2. Displays dependency installation guides
3. Validates environment setup

## Common Development Commands

### Emacs
| Command | Function |
|---------|----------|
| `C-x b` | Switch buffer (Consult) |
| `C-x B` | Open recent file (Consult) |
| `C-s` | Search in buffer (Consult) |
| `C-x f` | Find file in directory (Consult) |
| `C-x g` | Search files with grep (Consult) |
| `M-y` | Browse kill ring (Consult) |
| `M-g g` | Go to line |
| `C-.` | Embark actions |
| `M-p` / `M-n` | Previous/Next tab (Centaur Tabs) |
| `M-.` | Find definition (xref) |
| `M-,` | Pop xref marker |
| `M-x claude-code-ide` | Start Claude Code IDE |

### Vim
| Command | Function |
|---------|----------|
| `:e` | Edit file |
| `:b` | Switch buffer |
| `:tabn` / `:tabp` | Next/Previous tab |
| `:ls` | List buffers |
| `gd` | Go to definition |
| `Ctrl-n` / `Ctrl-p` | Auto-complete |

## Quick Start

### First-time Emacs Setup

```bash
cd ~/oh-my-workspace/emacs
./setup.sh
emacs

# Test configuration loads without errors
emacs --debug-init
```

### First-time Vim Setup

```bash
cd ~/oh-my-workspace/vim
./setup.sh
vim
```

## Emacs Configuration Architecture

The Emacs configuration (`emacs/`) is modular with a specific loading order defined in `init.el`:

### Module Loading Order

**Core modules** (loaded in order):
1. **UI**: `init-ui` → `init-fonts`
2. **Editor**: `init-editing` → `init-completion` → `init-dired`
3. **Tools**: `init-vc` → `init-terminal` → `init-ai` → `init-auth`
4. **Languages**: `init-prog` → language-specific modules in `lisp/lang/`

**Language modules** (loaded via dolist):
- `init-elisp` - Emacs Lisp configuration
- `init-cc` - C/C++ (Google C++ style, compile_commands.json generation)
- `init-python` - Python (pyvenv + Poetry integration, mode line indicators)
- `init-go` - Go (gopls LSP)
- `init-typescript` - TypeScript/JavaScript
- `init-dockerfile` - Dockerfile
- `init-cmake` - CMake
- `init-yaml` - YAML
- `init-markdown` - Markdown (table alignment, formatting)

### Key Architecture Patterns

- **use-package**: All package configurations use `use-package` macros with `:ensure t`
- **Deferred Loading**: Most packages use `:defer t` for faster startup
- **Package Management**: Uses built-in `package.el` with MELPA, non-GNU, and GNU ELPA archives
- **Custom Packages**: Local packages in `site-packages/` are automatically added to load-path
- **Customization**: User settings go in `custom.el` (auto-generated, git-ignored)

### Directory Structure

- **`lisp/`**: Core Emacs Lisp modules (init-*.el)
- **`lisp/tools/`**: External tool integrations
- **`lisp/lang/`**: Language-specific configurations (LSP, formatting, etc.)
- **`site-packages/`**: Custom/locally-installed Emacs packages
  - `dired-single-0.3.1/`: Single-buffer dired
  - `dired-custom-extension-0.0.1/`: Custom dired enhancements

### Special Features

**Mode Line Indicators**:
- Python virtual environment: `🐍[venv-name]` (displayed via pyvenv)
- Poetry project: `📦[project-name]` (displayed via poetry)

**Programming Save Hooks** (`my/prog-save-mode`):
- Automatically delete trailing whitespace
- Convert tabs to spaces
- Update copyright years

### Adding New Modules

1. Create file in appropriate location:
   - `lisp/init-*.el` - Core editor features
   - `lisp/lang/` - Language-specific configurations
   - `lisp/tools/` - External tool integrations
2. Add `(provide 'module-name)` at end
3. Add `(require 'module-name)` in `init.el` at correct position based on loading order

### Custom Packages

The `site-packages/` directory contains custom Emacs packages that are automatically added to `load-path`. When adding new custom packages:

1. Create a subdirectory in `site-packages/package-name/`
2. Add a `.el` file with `(provide 'package-name)`
3. Reference in other modules with `(require 'package-name)`

## Emacs Module Details

### Core Modules (lisp/)

**init-ui.el** - User Interface
- Doom themes (doom-xcode)
- Doom modeline (enriched mode line)
- Centaur tabs (tab bar)
- Nerd icons (file icons)
- Pulsar (cursor position highlighting)
- Dashboard (startup screen)

**init-fonts.el** - Font Configuration
- Monospace font configuration
- Chinese character fallback
- Dynamic text scaling

**init-editing.el** - Editing Enhancements
- Smart text operations (indent, copy, kill)
- Vundo (visual undo history)
- Expand region (semantic selection)
- Browse kill ring
- File templates (auto-insert)

**init-completion.el** - Modern Completion Framework
- Vertico (vertical completion UI)
- Orderless (flexible matching)
- Marginalia (rich annotations)
- Consult (enhanced commands)
- Embark (context actions)

**init-dired.el** - File Management
- Dirvish (enhanced dired)
- Single-buffer navigation
- Async operations

### Tool Modules (lisp/tools/)

**init-vc.el** - Version Control
- Magit (Git operations)

**init-terminal.el** - Terminal
- vterm (native PTY support)

**init-ai.el** - AI Integration
- Claude Code IDE (AI coding assistant)

**init-auth.el** - Authentication
- GPG integration
- pass password store
- pinentry

### Language Modules (lisp/lang/)

**init-prog.el** - Base Programming Configuration
- Smartparens (automatic parenthesis pairing)
- Rainbow delimiters (colorized brackets)
- Flycheck (syntax checking)
- Eglot (LSP client) - hooks into all supported modes
- Quickrun (execute code quickly)
- Dumb jump (code navigation)
- Copyright updates
- `my/prog-save-mode` (save hooks for cleanup)

**init-python.el** - Python
- pyvenv (virtual environment tracking)
- Poetry (Python packaging manager)
- Mode line indicators: 🐍[venv-name] and 📦[project-name]

**init-go.el** - Go
- gopls LSP configuration

**init-cc.el** - C/C++
- Google C++ style
- clang-format integration
- compile_commands.json generation

**init-typescript.el** - TypeScript/JavaScript
- typescript-language-server integration

**init-dockerfile.el** - Dockerfile
- docker-langserver integration

**init-cmake.el** - CMake
- cmake-language-server integration

**init-yaml.el** - YAML
- yaml-language-server integration

**init-markdown.el** - Markdown
- Table alignment
- Format checking
- marksman LSP integration

**init-elisp.el** - Emacs Lisp
- Emacs Lisp enhancements

## Vim Configuration

The Vim configuration (`vim/`) is managed through `vimrc` with:
- Modern Vim 8+ settings with true color support
- Native package management support (`~/.vim/pack/plugins/start/`)
- Custom keybindings for productivity
- Status line with file info and cursor position
- Persistent undo history
- Optional vim-plug plugin support (commented out in vimrc)

## External Dependencies

### Core Tools (P0 - Required)
| Tool | Purpose | Install |
|------|---------|---------|
| git | Version control | `brew install git` |
| ripgrep | Code search | `brew install ripgrep` |
| the_silver_searcher | Fallback search | `brew install the_silver_searcher` |
| fd | Fast file find | `brew install fd` |
| coreutils | macOS GNU ls | `brew install coreutils` |
| aspell | Spell checking | `brew install aspell` |
| pandoc | Document conversion | `brew install pandoc` |
| libvterm | Emacs vterm dependency | `brew install libvterm` |

### LSP Servers (P1 - Development)
| Language | Server | Install |
|----------|--------|---------|
| Python | pylsp | `pip install python-lsp-server[all]` |
| Go | gopls | `go install golang.org/x/tools/gopls@latest` |
| C/C++ | clangd | Xcode Command Line Tools |
| TypeScript | typescript-language-server | `npm install -g typescript-language-server typescript` |
| YAML | yaml-language-server | `npm install -g yaml-language-server` |
| Bash | bash-language-server | `npm install -g bash-language-server` |
| Dockerfile | docker-langserver | `npm install -g dockerfile-language-server-nodejs` |
| CMake | cmake-language-server | `pip install cmake-language-server` |
| Markdown | marksman | `brew install marksman` |

### Formatters (P1 - Development)
| Language | Formatter | Install |
|----------|-----------|---------|
| Python | black, isort | `pip install black black-macchiato isort` |
| Go | gofumpt | `go install mvdan.cc/gofumpt@latest` |
| C/C++ | clang-format | Xcode Command Line Tools |
| Markdown | markdownfmt | `npm install -g markdownfmt` |

### Debug Tools (P1 - Development)
| Tool | Purpose | Install |
|------|---------|---------|
| debugpy | Python debugger | `pip install debugpy` |
| pylint | Python linter | `pip install pylint` |

### Fonts
Run `M-x nerd-icons-install-fonts` after setup to install icon fonts (GUI mode only).

## Validation and Troubleshooting

### Emacs Startup Issues

Test that configuration loads without errors:
```bash
emacs --debug-init
```

Check *Messages* buffer for startup logs:
```
C-h e  ; View *Messages* buffer
```

### LSP Not Working

1. Confirm LSP server is installed:
   ```bash
   # Check if server is in PATH
   which pylsp gopls clangd typescript-language-server
   ```

2. Check Eglot status:
   ```
   M-x eglot  ; Start LSP manually
   ```

3. Review LSP events log:
   ```
   C-h b  ; Switch to *eglot-events* buffer
   ```

### Package Installation Fails

1. Check network and proxy settings:
   ```elisp
   M-x show-http-proxy  ; Check current proxy
   M-x set-http-proxy   ; Set proxy if needed
   ```

2. Refresh package contents:
   ```bash
   M-x package-refresh-contents
   ```

### Slow Emacs Startup

1. Check for byte-compile cache issues:
   ```bash
   rm -rf ~/.emacs.d/eln-cache/
   ```

2. Review startup logs:
   ```bash
   C-h e  ; View *Messages* buffer
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

## Emacs Lisp Coding Standards

### File Header Template

All `.el` files must include this header (replace placeholders):

```elisp
;;; filename.el -*- lexical-binding: t; -*-
;; Time-stamp: <YYYY-MM-DD HH:MM:SS Weekday by zhengyu.li>

;; Copyright (C) 2021-2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: keyword1, keyword2
;; Dependencies: (none) or (module1 module2)

;; GPL license text...

;;; Commentary:
;;
;; One or two sentences describing file purpose.

;;; Code:

;; ==================================================================================
;; Section name - description
(code...)
```

**Required**: `-*- lexical-binding: t; -*-`, Time-stamp, Copyright (current year), Keywords, Dependencies

### Naming Conventions

| Type | Prefix | Example |
|------|--------|---------|
| Custom functions | `my/` | `my/prog-before-save` |
| Custom variables | `emacs-` | `emacs-user-name` |
| Custom minor modes | `my/` | `my/prog-save-mode` |

### use-package Keywords

| Keyword | Usage | Example |
|----------|-------|---------|
| `:ensure t` | Auto-install (external) | Most packages |
| `:ensure nil` | Built-in package | `dired`, `emacs` |
| `:defer t` | Lazy loading | Most packages |
| `:demand t` | Immediate load | Core packages |
| `:after` | Load after | `:after init-prog` |
| `:hook` | Add hooks | `:hook (prog-mode . flycheck-mode)` |
| `:bind` | Keybindings | See below |
| `:config` | Config code | After package loads |
| `:init` | Init code | Before package loads |
| `:commands` | Declare commands | Enables autoloading |

### Code Format

**Indentation**: 2 spaces (standard Emacs Lisp)

**setq multi-line**:
```elisp
(setq var1 value1  ; Comment
      var2 value2  ; Comment
      var3 value3) ; Comment
```

**Keybindings**:
```elisp
:bind
(("C-c p" . command)         ; Global
 (:map mode-map              ; Mode-local
       ("C-c a" . cmd1)
       ("C-c b" . cmd2)))
```

**Functions**:
```elisp
(defun my/function-name (arg)
  "One-line summary for simple functions."
  (interactive)
  (body))

(defun my/complex-function (arg)
  "Multi-line summary when complex.
Details with - bullets for key points."
  (let ((var value))
    (when condition
      (do-something))))
```

### Hook Guidelines

**✅ Preferred: Use `:hook` in use-package**
```elisp
(use-package package-name
  :hook (mode-symbol . function))

;; Multiple hooks
(use-package package-name
  :hook ((mode1 . function1)
         (mode2 . function2)))
```

**❌ Avoid: Global `add-hook` outside use-package**
```elisp
;; 不推荐
(add-hook 'prog-mode-hook #'my-function)

;; 推荐封装到 use-package 中
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . my-function))
```

**Hook naming**: Use `mode-symbol` (without `-hook` suffix):
- `:hook (prog-mode . function)` ✓
- `:hook (python-mode . function)` ✓
- `:hook (markdown-mode . function)` ✓

use-package automatically adds `-hook` suffix internally.

### Block Separators

```elisp
;; ==================================================================================
;; Package/Feature - Description
;; Optional second line
(use-package ...)
```

**Required**: Separator before each major use-package block, with blank line before/after

### Comments

| Type | Format | Example |
|------|--------|---------|
| Block | `;; text` | `;; Smartparens config` |
| Section | `;; ===` | `;; ==================================================================================` |
| Line-end | `; text` | `:defer t  ; Lazy load` |
| Docstring | `"text"` | `"Function summary."` |

**Line-end comments**: 2+ spaces before semicolon

### File Footer

```elisp
;; ==================================================================================
;;; Provide features
(provide 'init-filename)

;;; init-filename.el ends here
```

### Checklist

- [ ] `-*- lexical-binding: t; -*-` in first line
- [ ] Time-stamp current, Copyright includes current year
- [ ] Custom functions use `my/` prefix
- [ ] Custom variables use `emacs-` prefix
- [ ] use-package with correct `:ensure` (t/nil)
- [ ] Most packages use `:defer t`
- [ ] Hooks defined in `:hook` (not global `add-hook`)
- [ ] Functions have docstrings
- [ ] Line-end comments have 2+ spaces before `;`
- [ ] Block separators with blank lines before/after
- [ ] File ends with `(provide '...)` and end comment

## Important Notes

- **Multi-tool repository**: This is NOT a single application but configurations for multiple independent tools
- **macOS-focused**: Most configurations assume macOS as the primary OS
- **Symlink-based setup**: Configuration files are symlinked rather than copied for easier updates
- **Emacs-centric**: The Emacs configuration is the most complex
- **Minimum Emacs version**: Requires Emacs >= 30.2
- **Minimum Vim version**: Requires Vim >= 8.0
- **Zsh configuration**: This repository does NOT contain Zsh configuration files. The README.md provides manual Oh My Zsh setup instructions
- **No `emacs-config-validate-all` function**: The setup.sh mentions this function but it is not implemented in the Lisp code. Use the `check_lsp_servers()` function in setup.sh to verify LSP server installation
