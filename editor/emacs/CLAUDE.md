---
lastUpdated: 2026-03-22
maintainer: zhengyu.li
---

# CLAUDE.md - Emacs Configuration

This file provides guidance to Claude Code when working with the Emacs configuration.

## Project Overview

**Emacs >= 30.2** configuration for macOS with LSP support, completion system, and project management.

## Directory Structure

```
emacs/.config/emacs/
├── early-init.el        # Early initialization (before package system)
├── init.el              # Main entry point
├── banners/             # Dashboard banners
├── lisp/
│   ├── editor/          # Editor behavior (appearance, completion, edit, etc.)
│   ├── lib/             # Utility library (proxy, shared utils)
│   ├── lang/            # Language-specific configs
│   ├── text/            # Text/document modes
│   └── tool/            # Tool integrations (git, ai, pdf, term)
├── site-packages/       # Custom packages (auto-added to load-path)
└── templates/           # File templates (YASnippet + auto-insert)
```

## File Templates

Templates in `templates/` are auto-inserted via YASnippet + auto-insert:

| File Type | Template | Trigger |
|-----------|----------|---------|
| Emacs Lisp | `template.el` | `\.el$` |
| C | `template.c` | `\.c$` |
| C Header | `template.h` | `\.h$` |
| Python | `template.py` | `\.py$` |
| Go | `template.go` | `\.go$` |
| Shell | `template.sh` | `\.sh$` |

## Setup

```bash
emacs --debug-init   # Test configuration
brew bundle --file homebrew/Brewfile
bun install -g typescript-language-server yaml-language-server bash-language-server
uv tool install basedpyright ruff
go install golang.org/x/tools/gopls@latest mvdan.cc/gofumpt@latest
```

## Architecture

### Module Loading Order

**Critical**: `init.el` loads modules in this order. Dependencies MUST be respected.

1. **Early boot**: omw-proxy
2. **Editor**: omw-font → omw-appearance → omw-edit → omw-search → omw-template → omw-completion → omw-explorer
3. **Tools**: omw-pass → omw-git → omw-term → omw-pdf → omw-ai
4. **Languages**: omw-prog → omw-cc → omw-go → omw-python → omw-typescript → omw-elisp → omw-shell
5. **Config/build**: omw-cmake → omw-yaml → omw-dockerfile → omw-vimrc → omw-gitconfig
6. **Text**: omw-markdown

When adding modules: add `(provide 'omw-xxx)` at end of file, add `(require 'omw-xxx)` in `init.el` at the correct position.

### Key Architecture Patterns

**Centralized LSP**: All language servers are configured in `omw-prog.el` via a single `eglot` block. Language modules must NOT configure LSP servers.

**Minimal Language Modules**: `lisp/lang/omw-*.el` files are 3-10 lines typical — only install the major mode package.

**Custom Packages**: `site-packages/` is auto-added to `load-path`. Require with `(require 'package-name)`.

**Custom File**: Emacs customizations go in `~/.emacs.d/custom.el`, never `init.el`.

### Module Guidelines

| Type | Path | Characteristics |
|------|------|-----------------|
| Editor | `lisp/editor/omw-*.el` | May use `:demand t`, complex configs OK |
| Lib | `lisp/lib/omw-*.el` | No UI, utilities only, loaded early |
| Tool | `lisp/tool/omw-*.el` | May use `:vc`, platform-specific OK |
| Language | `lisp/lang/omw-*.el` | Concise, no LSP config, `:ensure t :defer t` |

### Tool Installation

- **Brewfile**: runtimes (`go`, `rust`, `bun`, `uv`), system tools (`llvm`→clangd, `aspell`, `pandoc`), fonts, Emacs itself
- **`omw/install-<lang>-tools`** (in each lang module): ecosystem-managed LSP servers

## Coding Standards

See `.claude/rules/elisp/coding-style.md` for: `omw/` prefix convention, use-package keyword order, setq grouping, docstring style, section separators, alignment rules.

## Validation

```bash
# Full load test
emacs --debug-init

# Batch test
emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") (message "OK"))'

# Audit naming
grep -rn "defun (?!omw/)" lisp --include="*.el" | grep -v "^;;"
```

## Keybindings Reference

| Binding | Function |
|---------|----------|
| `C-x b` | Switch buffer (Consult) |
| `C-x f` | Find file (Consult) |
| `C-x g` | Grep files (Consult) |
| `C-s` | Search in buffer |
| `M-y` | Browse kill ring |
| `C-.` | Embark actions |
| `M-.` / `M-,` | Go to definition / pop marker |
| `<f11>` | Toggle fullscreen |
