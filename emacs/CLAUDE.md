# CLAUDE.md - Emacs Configuration

This file provides guidance to Claude Code when working with the Emacs configuration in this directory.

## Project Overview

This is an **Emacs configuration** (>= 30.2) for macOS, featuring LSP support, completion system, and project management.

## Directory Structure

```
emacs/.config/emacs/
├── early-init.el        # Early initialization (before package system)
├── init.el              # Main entry point (symlinked to ~/.config/emacs/init.el)
├── banners/             # Dashboard banners (octopus.png, totoro.png)
├── lisp/
│   ├── editor/          # Editor behavior (appearance, completion, edit, etc.)
│   ├── lib/             # Utility library (proxy, shared utils)
│   ├── lang/            # Language-specific configs (cc, python, go, cmake, yaml, etc.)
│   ├── text/            # Text/document modes (markdown)
│   └── tool/            # Tool integrations (git, ai, pdf, term)
├── site-packages/       # Custom Emacs packages (auto-added to load-path)
└── templates/           # File templates (YASnippet + auto-insert)
```

## File Templates

Automatic file header templates are located in the `templates/` directory, integrated via YASnippet + auto-insert. New files automatically get standards-compliant headers inserted.

### Available Templates

| File Type | Template File | Trigger Condition |
|---------|---------|---------|
| Emacs Lisp | `template.el` | `\.el$` |
| C | `template.c` | `\.c$` |
| C Header | `template.h` | `\.h$` |
| Python | `template.py` | `\.py$` |
| Go | `template.go` | `\.go$` |
| Shell | `template.sh` | `\.sh$` |

### Usage

- **Automatic**: Triggered when creating matching files
- **Manual**: `M-x auto-insert` to re-insert template

## Quick Start

### Setup

```bash
# Run root setup script (creates symlinks)
./setup.sh link

# Test configuration loads without errors
emacs --debug-init

# Install all dependencies via Homebrew
brew bundle --file homebrew/Brewfile

# Install LSP servers (language-specific package managers)
bun install -g typescript-language-server yaml-language-server bash-language-server dockerfile-language-server-nodejs
uv tool install basedpyright ruff
go install golang.org/x/tools/gopls@latest mvdan.cc/gofumpt@latest
```

### Common Commands

| Command                        | Function                         |
|--------------------------------|----------------------------------|
| `C-x b`                        | Switch buffer (Consult)          |
| `C-x f`                        | Find file in directory (Consult) |
| `C-x g`                        | Search files with grep (Consult) |
| `C-s`                          | Search in buffer (Consult)       |
| `M-y`                          | Browse kill ring (Consult)       |
| `C-.`                          | Embark actions                   |
| `M-.` / `M-,`                  | Go to definition / pop marker    |
| `M-x eglot`                    | Start LSP manually               |
| `M-x nerd-icons-install-fonts` | Install icon fonts (GUI mode)    |
| `<f11>`                        | Toggle fullscreen                |

### Quick Validation

```bash
# Full initialization test
emacs --debug-init

# Batch mode test (faster for CI/CD)
emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") (message "Configuration loaded successfully"))'
```

## Architecture

### Module Loading System

**Critical**: The `init.el` file loads modules in a specific order. Dependencies MUST be respected.

**Loading order** (search `;; Load modules` in init.el):
1. **Early boot** (before package init): omw-proxy
2. **Editor**: omw-font → omw-appearance → omw-edit → omw-search → omw-template → omw-completion → omw-explorer
3. **Tools**: omw-pass → omw-git → omw-term → omw-pdf → omw-ai
4. **Languages**: omw-prog → omw-cc → omw-go → omw-python → omw-typescript → omw-elisp → omw-shell
5. **Config/build languages**: omw-cmake → omw-yaml → omw-dockerfile → omw-vimrc → omw-gitconfig
6. **Text**: omw-markdown

**When adding new modules**:
- Add `(provide 'omw-xxx)` at end of file (where xxx is the module name)
- Add `(require 'omw-xxx)` in `init.el` at correct position
- Language modules are explicitly required in init.el (not auto-loaded)

### Key Architecture Patterns

**Centralized LSP Configuration**: All language LSP servers are configured in `omw-prog.el` via a single `eglot` use-package block. Language modules should NOT configure LSP servers themselves.

**Minimal Language Modules**: Language modules in `lisp/lang/` (named `omw-xxx.el`) should be minimal (3-10 lines typical). They only install the major mode package.

**Custom Packages**: The `site-packages/` directory is automatically added to `load-path` by `init.el`. Custom packages there can be required with `(require 'package-name)`.

**Custom File Separation**: Emacs customizations are stored in `~/.emacs.d/custom.el`, not in init.el. The `custom-file` variable is set early (before package initialization) to prevent Emacs from writing customizations to init.el.

### Module Guidelines

**Editor Modules** (`lisp/editor/omw-*.el`)
- Purpose: Fundamental editor features (appearance, editing, completion, fonts)
- Characteristics: May use `:demand t` for critical packages, complex configurations acceptable

**Lib Modules** (`lisp/lib/omw-*.el`)
- Purpose: Utility/library modules shared across other modules
- Characteristics: Loaded early or on-demand; no UI, just utilities

**Tool Modules** (`lisp/tool/omw-*.el`)
- Purpose: External tool integration (Git, AI, PDF, terminal)
- Characteristics: May use `:vc` for Git-based packages, platform-specific code acceptable

**Language Modules** (`lisp/lang/omw-*.el` and `lisp/lang/config/omw-*.el`)
- Purpose: Language-specific configuration
- Characteristics: Should be concise and focused, MUST NOT configure LSP servers (centralized in omw-prog.el)
- Simple `:ensure t :defer t` preferred for most cases

### Tool Installation Convention

Development tools are installed via two mechanisms:

**Brewfile** (`homebrew/Brewfile`) installs:
- Language runtimes: `go`, `rust` (rustup), `bun`, `uv`
- System-level tools with no language-ecosystem alternative: `llvm` (→ clangd), `aspell`, `pandoc`
- Fonts: `font-sauce-code-pro-nerd-font`
- Emacs itself: `emacs-app` (cask)

**Emacs `omw/install-<lang>-tools`** (in each lang module) installs:
- Ecosystem-managed LSP servers: `gopls`, `gofumpt` (go), `basedpyright`, `ruff` (uv), etc.

## Coding Standards

See `.claude/rules/dotfiles/emacs.md` for:
- Naming Convention (omw/ prefix)
- use-package Patterns (keyword ordering)
- Function & Variable Patterns
- Code Formatting

## Code Quality

### Validation Commands

```bash
# Full initialization test
emacs --debug-init

# Batch mode test
emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") (message "OK"))'

# Find functions without omw/ prefix
grep -rn "defun (?!omw/)" lisp --include="*.el" | grep -v "^;;"
```

### Compliance Checklist

- [ ] All custom functions use `omw/` prefix
- [ ] All defcustom use `:group 'omw-emacs`
- [ ] File headers complete (Time-stamp, Author, Copyright, Dependencies, History, Commentary)
- [ ] All files have complete MIT license text
- [ ] All setup functions have docstrings
- [ ] use-package keywords follow correct order
- [ ] Section separators (`;; ============================================================================`) used correctly (79 chars)
- [ ] Files end with `(provide 'omw-xxx)` and `;;; omw-xxx.el ends here`
- [ ] All comments in English

### Troubleshooting

**Emacs fails to load:**
```bash
emacs --debug-init  # Check error messages
```

**LSP issues:**
1. Verify server installed: `which pylsp gopls clangd`
2. Start LSP manually: `M-x eglot`
3. Check events log: Switch to `*eglot-events*` buffer

## Best Practices

### 1. Language Module Cohesion

**Principle**: Keep language-related code together, even if it means more lines.

**Why this works better than artificial limits:**
- **Cohesion**: All code for Language X is in one place
- **Discoverability**: Developers know where to find language-specific features
- **Maintainability**: Language features are not scattered across multiple files

### 2. Docstring Style

**Use paragraph form, not bullet lists:**

```elisp
;; AVOID: Bullet list in docstrings
"Do X.
- Step 1
- Step 2"

;; CORRECT: Paragraph form
"Do X by first doing step 1, then step 2."
```

### 3. Testing Before Committing

```bash
# 1. Syntax check
emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") (message "OK"))'

# 2. Debug check
emacs --debug-init

# 3. Quick audit
grep -rn "defun (?!omw/)" lisp --include="*.el"
```

## Quick Reference Card

### Essential Rules

1. **File Header:** Must include Time-stamp, Author, Copyright, Dependencies, History, Commentary
2. **use-package Order:** `:ensure → :when → :defer/:demand → :after → :hook → :bind → :custom-face → :config`; never use `:defer t` with `:after`
3. **Naming Prefix:** Always `omw/` for custom functions/variables
4. **Setup Functions:** Use `setq-local` for variables, mode functions for toggles
5. **Comments:** Minimal, only for non-obvious code
6. **Separators:** `;; ============================================================================` (79 chars)
7. **Language Modules:** Keep language-related code together
8. **LSP Configuration:** Centralize all LSP in omw-prog.el
9. **Face Customization:** Use `:custom-face` with `fixed-pitch` inheritance
10. **Tracking Packages:** Use `:demand t` for environment tracking (pyvenv, poetry)
11. **Alignment Spaces:** Prohibited (single space only)
12. **Comment Language:** English only

### Pre-Commit Checklist

- [ ] Configuration loads: `emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") ...)'`
- [ ] No naming violations: All functions use `omw/` prefix
- [ ] No variable violations: All defcustom use `:group 'omw-emacs`
- [ ] File headers complete: Time-stamp, Author, Copyright, Dependencies, History, Commentary, MIT
- [ ] Docstrings present: All setup functions have documentation
- [ ] use-package order correct: Keywords in standard order
- [ ] Section separators 79 chars
- [ ] All comments in English

### Common Patterns

```elisp
;; Minimal language module
(use-package <lang>-mode
  :ensure t
  :defer t)

;; Setup function pattern
(defun omw/<mode>-setup ()
  "Apply custom settings for <mode>."
  (setq-local var1 value1)
  (mode-enable))

(use-package <mode>
  :hook (<mode> . omw/<mode>-setup))

;; Custom variable
(defcustom omw/<var> <default>
  "Description."
  :type '<type>
  :group 'omw-emacs)

;; Minor mode
(define-minor-mode omw/<feature>-mode
  "Description."
  :lighter " Indicator"
  :global nil
  (if omw/<feature>-mode
      (add-hook 'hook-name #'omw/function nil t)
    (remove-hook 'hook-name #'omw/function t)))

;; Environment tracking package (use :demand t)
(use-package pyvenv
  :ensure t
  :demand t
  :config
  (pyvenv-tracking-mode 1))
```
