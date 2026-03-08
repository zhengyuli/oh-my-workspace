# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **workspace configuration repository** (dotfiles) providing comprehensive development environment setup for macOS. It contains configurations for multiple independent tools:

- **Emacs** (>= 30.2) - Text editor with LSP, completion, and project management
- **Vim** (>= 8.0) - Lightweight modal editor with modern enhancements

This is a **monorepo** of configurations, not a single application. Each tool's configuration is independent.

## Repository Structure

```
oh-my-workspace/
├── emacs/
│   ├── init.el              # Main entry point (symlinked to ~/.emacs)
│   ├── lisp/
│   │   ├── init-*.el        # Core feature modules
│   │   ├── tools/           # Tool integrations (Magit, vterm, AI, auth)
│   │   └── lang/            # Language-specific configs
│   ├── site-packages/       # Custom Emacs packages (auto-added to load-path)
│   └── setup.sh             # Installation script with dependency validation
├── vim/
│   ├── vimrc                # Main configuration (symlinked to ~/.vimrc)
│   └── setup.sh             # Installation script
├── README.md                # Comprehensive setup guide
└── CLAUDE.md                # This file
```

## Setup and Validation

```bash
# Emacs setup
./emacs/setup.sh

# Test configuration loads without errors
emacs --debug-init

# Install all dependencies (macOS)
brew install git aspell pandoc the_silver_searcher ripgrep coreutils libvterm fd marksman
npm install -g typescript-language-server yaml-language-server bash-language-server dockerfile-language-server-nodejs
pip install "python-lsp-server[all]" black black-macchiato isort pylint debugpy cmake-language-server
go install golang.org/x/tools/gopls@latest mvdan.cc/gofumpt@latest

# Vim setup
./vim/setup.sh
```

The `emacs/setup.sh` script validates dependencies and displays installation commands for missing tools. Use it to check what's missing.

## Emacs Configuration Architecture

### Module Loading System

**Critical**: The `init.el` file loads modules in a specific order. Dependencies MUST be respected.

**Loading order** (lines 207-228 of init.el):
1. **Core**: init-editing → init-completion → init-auth → init-proxy → init-fonts → init-ui
2. **Tools**: init-dired → init-pdf → init-magit → init-terminal → init-agent
3. **Languages**: init-prog → [language modules in lang/]

**When adding new modules**:
- Add `(provide 'init-module-name)` at end of file
- Add `(require 'init-module-name)` in `init.el` at correct position
- For language modules: just place in `lisp/lang/` - they're auto-loaded via dolist

### Key Architecture Patterns

**Centralized LSP Configuration**: All language LSP servers are configured in `init-prog.el` via a single `eglot` use-package block. Language modules should NOT configure LSP servers themselves.

**Minimal Language Modules**: Language modules in `lisp/lang/` should be minimal (3-10 lines typical). They only install the major mode package. Example:
```elisp
(use-package go-mode
  :ensure t
  :defer t)
; LSP server (gopls) is configured in init-prog.el
```

**Custom Packages**: The `site-packages/` directory is automatically added to `load-path` by `init.el`. Custom packages there can be required with `(require 'package-name)`.

**Custom File Separation**: Emacs customizations are stored in `~/.emacs.d/custom.el`, not in init.el. The `custom-file` variable is set early (before package initialization) to prevent Emacs from writing customizations to init.el:

```elisp
;; Set custom file early (before package-initialize)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load custom file after all modules are loaded
(when (file-readable-p custom-file)
  (load custom-file nil 'nomessage))
```

**Path Resolution**: The config uses `omw/emacs-config-root-path` to find the actual config directory, resolving symlinks correctly.

## Emacs Lisp Coding Standards

### Naming Convention (CRITICAL)

**All custom functions and variables MUST use `omw/` prefix** (not `my/`):

```elisp
;; ✅ CORRECT
(defun omw/prog-mode-setup () ...)
(defcustom omw/http-proxy nil ...)

;; ❌ WRONG
(defun my/prog-mode-setup () ...)
```

The `omw/` prefix stands for "oh-my-workspace" and is used consistently throughout the codebase.

#### Function Naming Patterns

**Pattern A: Setup functions** (for mode configuration)
```
omw/<mode>-setup
```
- Examples: `omw/prog-mode-setup`, `omw/markdown-mode-setup`, `omw/vterm-mode-setup`
- Used when: 3+ buffer-local settings for the same mode

**Pattern B: Action functions** (for operations)
```
omw/<noun>-<verb>
```
- Examples: `omw/indent-entire-buffer`, `omw/show-http-proxy`, `omw/set-http-proxy`

**Pattern C: Mode indicators** (for mode-line display)
```
omw/<feature>-mode-line-indicator
```
- Examples: `omw/pyvenv-mode-line-indicator`, `omw/poetry-mode-line-indicator`

#### Variable Naming Patterns

**Format:** `omw/<category>-<item>`

**Examples:**
- `omw/font-monospace-list` (list of items)
- `omw/font-size-default` (simple value)
- `omw/emacs-config-root-path` (file path - use `-path` suffix)
- `omw/http-proxy`
- `omw/markdown-colors`

**Variable Type:** Must use `defcustom` for user-configurable variables with `:group 'omw-emacs`

#### Minor Mode Naming

**Format:** `omw/<feature>-mode`

**Examples:**
- `omw/prog-before-save-mode`

### File Header Format (MANDATORY)

Every `.el` file must start with:

```elisp
;;; init-module.el -*- lexical-binding: t; -*-
;; Time-stamp: <YYYY-MM-DD HH:MM:SS Weekday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: keyword1, keyword2, keyword3
;; Dependencies: (none) or module-name

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; One or two sentence description of module purpose.

;;; Code:

;; ==================================================================================
```

**Required fields checklist:**
- ✅ `-*- lexical-binding: t; -*-` on line 1
- ✅ Time-stamp with **current date**
- ✅ Copyright must include **current year** (2026)
- ✅ Keywords: 2-4 relevant tags
- ✅ Dependencies: list if any, otherwise "(none)"
- ✅ Commentary: Brief description (1-2 sentences)

### use-package Keyword Order (MANDATORY)

**Keywords MUST appear in this exact order:**

```
1. :ensure or :vc          # Package source
2. :when or :if            # Conditional loading (optional)
3. :defer or :demand       # Loading strategy
4. :after                  # Dependencies (optional)
5. :hook                   # Mode hooks
6. :bind                   # Keybindings
7. :custom-face            # Face customization (optional)
8. :config                 # Configuration
```

#### :ensure/:vc Rules

**Rule A: External packages from ELPA/MELPA**
```elisp
(use-package <package>
  :ensure t)  ; ✅ CORRECT
```

**Rule B: Built-in Emacs packages**
```elisp
(use-package <built-in>
  :ensure nil)  ; ✅ CORRECT
```

**Rule C: Packages from Git/VCS**
```elisp
(use-package <package>
  :vc (:url "https://github.com/<user>/<repo>" :rev :newest))  ; ✅ CORRECT
```

#### :defer vs :demand Rules

**Rule A: Default behavior - Use :defer t**
```elisp
(use-package <package>
  :ensure t
  :defer t)  ; ✅ CORRECT for 95% of packages
```

**Rule B: Critical packages requiring immediate load**
```elisp
(use-package <critical-package>
  :ensure t
  :demand t  ; ✅ Use for: theme system, completion framework core
  :config
  ...)
```

**When to use :demand t:**
- Theme packages (doom-themes)
- Completion framework core (vertico, orderless)
- Packages that must be available at startup
- Environment tracking packages (pyvenv, poetry) - need to track state immediately

**Rule C: Environment tracking packages**
```elisp
;; ✅ CORRECT: Tracking packages need :demand t to activate immediately
(use-package pyvenv
  :ensure t
  :demand t
  :config
  (pyvenv-tracking-mode 1))

;; ❌ AVOID: :defer t prevents tracking from working correctly
(use-package pyvenv
  :ensure t
  :defer t
  :hook (python-mode . pyvenv-tracking-mode))  ; May miss environment changes
```

**When to use :defer t:**
- All language modes
- All tool integrations
- All enhancement packages

#### :hook Rules

**Rule A: Single hook - Simple form**
```elisp
:hook (prog-mode . smartparens-mode)  ; ✅ CORRECT
```

**Rule B: Multiple hooks - List form**
```elisp
:hook ((c-mode . eglot-ensure)       ; ✅ CORRECT
       (c++-mode . eglot-ensure)
       (python-mode . eglot-ensure))
```

**Rule C: Setup function for complex configurations**
```elisp
;; Define setup function when multiple buffer-local settings needed
(defun omw/prog-mode-setup ()
  "Apply custom settings for programming modes."
  (setq-local tab-width 4)
  (display-line-numbers-mode 1)
  (hs-minor-mode 1))

(use-package prog-mode
  :hook (prog-mode . omw/prog-mode-setup))  ; ✅ CORRECT
```

**Setup function naming:** `omw/<mode>-setup`
- Examples: `omw/prog-mode-setup`, `omw/markdown-mode-setup`, `omw/vterm-mode-setup`

**When to use setup functions:**
- 3+ buffer-local settings for the same mode
- Settings that should always be applied together
- Improves maintainability

**Prefer built-in functions over custom wrappers:**
```elisp
;; ✅ CORRECT: Use built-in function directly
:hook (pdf-view-mode . pdf-view-fit-height-to-window)

;; ❌ AVOID: Unnecessary wrapper function
(defun omw/pdf-view-mode-setup ()
  (setq-local pdf-view-display-size 'fit-height))
:hook (pdf-view-mode . omw/pdf-view-mode-setup)
```

#### :bind Rules

**Rule A: Global keybindings only**
```elisp
:bind (("C-x C-t" . multi-vterm)    ; ✅ CORRECT
       ("C-c g s" . magit-status))
```

**Rule B: Mode-local keybindings only**
```elisp
:bind (:map vterm-mode-map          ; ✅ CORRECT
            ("M-1" . nil)
            ("C-g" . vterm--self-insert))
```

**Rule C: Both global and mode-local**
```elisp
:bind (("C-x j" . dired-jump)       ; ✅ CORRECT
       (:map dired-mode-map
             ("C-c C-c" . dired-do-copy)
             ("C-c C-r" . dired-do-rename)))
```

**Keybinding prefix guidelines:**
- `C-c <letter>` - For mode-specific commands
- `C-c <letter> <letter>` - For sub-modes (e.g., `C-c g s` for git status)
- Avoid overriding Emacs defaults unless documented

#### :custom-face Rules

**Rule A: Use `:custom-face` instead of `custom-set-faces`**
```elisp
:custom-face
(centaur-tabs-selected ((t (:inherit fixed-pitch :bold t :foreground "#28cd41"))))  ; ✅ CORRECT
```

**Rule B: Use explicit face attributes to prevent buffer-local remapping issues**
```elisp
;; ✅ CORRECT: Explicit attributes prevent inheritance from buffer-local remapping
:custom-face
(centaur-tabs-display-line ((t (:inherit fixed-pitch :box nil :underline nil))))

;; ❌ AVOID: Inheriting from 'default can pick up buffer-local face remapping
:custom-face
(some-face ((t (:inherit default))))  ; May be affected by markdown-mode scaling
```

**Why use `:inherit fixed-pitch` instead of `:inherit default`:**
- Buffer-local face remapping (e.g., `face-remap-add-relative` in markdown-mode) affects faces that inherit from `default`
- Using `fixed-pitch` or explicit attributes prevents unintended font scaling in UI elements like tab bars

#### :config Rules

**Rule A: Simple configuration**
```elisp
:config
(setq var1 value1
      var2 value2)  ; ✅ CORRECT
```

**Rule B: Complex configuration**
```elisp
:config
(setq var1 value1)
(setq var2 value2)
(function-call arg1 arg2)  ; ✅ CORRECT
```

**Rule C: Configuration with requires**
```elisp
:config
(require 'dired-x)
(require 'dired-async)  ; ✅ CORRECT
(setq dired-dwim-target t)
```

### Function and Variable Patterns

#### Function Definition Patterns

**Pattern A: Simple function (1-10 lines)**
```elisp
(defun omw/simple-function ()
  "One-line summary of what function does."
  (interactive)
  (body))
```

**Pattern B: Medium complexity (10-30 lines)**
```elisp
(defun omw/medium-function (arg)
  "One-line summary with additional context.
Details about behavior, arguments, or side effects."
  (body))
```

**Pattern C: Complex function (30+ lines) or complex behavior**
```elisp
(defun omw/complex-function (arg)
  "Multi-line summary with details.

Argument ARG is used for...

Behavior:
- First action
- Second action
- Third action

Returns: Description of return value."
  (body))
```

**Docstring guidelines:**
- Simple functions (1-10 lines): One-line docstring
- Medium complexity (10-30 lines): One-line with brief context
- Complex (30+ lines) or non-obvious behavior: Multi-line with details
- ❌ Never use numbered steps in docstrings
- ❌ Don't state the obvious (e.g., "Function to do X")

#### Variable Definition Patterns

**Pattern A: Simple custom variable**
```elisp
(defcustom omw/font-monospace-list '("JetBrains Mono" "Menlo")
  "Priority list of monospace fonts for coding."
  :type '(repeat string)
  :group 'omw-emacs)  ; ✅ REQUIRED
```

**Pattern A2: Integer variable**
```elisp
(defcustom omw/font-size-default 160
  "Default font height in 1/10pt units (160 = 16pt)."
  :type 'integer
  :group 'omw-emacs)  ; ✅ REQUIRED
```

**Pattern B: Choice type variable**
```elisp
(defcustom omw/http-proxy nil
  "HTTP proxy for Emacs and subprocesses.
Format: \"127.0.0.1:7890\" or \"http://127.0.0.1:7890\""
  :type '(choice (const :tag "No proxy" nil)
                 (string :tag "Proxy address"))
  :group 'omw-emacs)  ; ✅ REQUIRED
```

**Pattern C: Alist type variable**
```elisp
(defcustom omw/markdown-colors
  '((header . "#46dcb0")
    (code-bg . "#293134")
    (code-fg . "#e0e2e4"))
  "Colors for Markdown syntax highlighting."
  :type 'alist
  :group 'omw-emacs)  ; ✅ REQUIRED
```

#### Minor Mode Definition Pattern

**Template:**
```elisp
(define-minor-mode omw/<feature>-mode
  "Description of minor mode."
  :lighter " ModeLineIndicator"
  :global nil
  (if omw/<feature>-mode
      (add-hook 'hook-name #'omw/function nil t)
    (remove-hook 'hook-name #'omw/function t)))
```

**Example:**
```elisp
(define-minor-mode omw/prog-before-save-mode
  "Minor mode for programming buffers to run custom before-save hooks."
  :lighter " SaveHook"
  :global nil
  (if omw/prog-before-save-mode
      (add-hook 'before-save-hook #'omw/prog-before-save nil t)
    (remove-hook 'before-save-hook #'omw/prog-before-save t)))
```

### Comment Style Guidelines

#### Inline Comments

**Rule: Use inline comments ONLY when:**
- Explaining "why" not "what"
- Documenting non-obvious behavior
- Providing context for complex logic

**✅ Good inline comment:**
```elisp
(setq dirvish-attributes
      (append dirvish-attributes
              '(git-msg file-modes)))  ; Add custom attributes not in default set
```

**❌ Unnecessary inline comment:**
```elisp
(setq tab-width 4)  ; Set tab width to 4  ; ❌ Obvious from code
```

#### Section Separators

**Required format (with blank lines before/after):**
```elisp
;; ==================================================================================

(use-package ...)

;; ==================================================================================
```

**Usage:**
- ✅ Before each major use-package block
- ✅ Before each major function definition (if file has multiple sections)
- ✅ Before "Provide features" section
- ✅ With blank line before and after

### Code Formatting

**Indentation:** 2 spaces (Emacs Lisp default)

**Multi-line setq:**
```elisp
(setq var1 value1
      var2 value2
      var3 value3)
```

**Multi-line setq with comments:**
```elisp
(setq var1 value1      ; Comment for var1
      var2 value2      ; Comment for var2
      var3 value3)     ; Comment for var3
```

**Line-end comments:** 2+ spaces before semicolon

### File Footer

```elisp
;; ==================================================================================
(provide 'init-module)

;;; init-module.el ends here
```

## Module Type Guidelines

### Core Modules (lisp/init-*.el)

**Purpose:** Fundamental editor features (UI, editing, completion)

**Characteristics:**
- May use `:demand t` for critical packages
- Complex configurations acceptable
- Multiple packages per file
- Cross-cutting concerns

**Examples:** init-ui.el, init-completion.el, init-editing.el

### Tool Modules (lisp/tools/init-*.el)

**Purpose:** External tool integration (Magit, vterm, AI tools)

**Characteristics:**
- May use `:vc` for Git-based packages
- Platform-specific code acceptable
- External executable detection
- Custom setup functions for tool behavior

**Examples:** init-magit.el, init-terminal.el, init-dired.el, init-pdf.el

### Language Modules (lisp/lang/init-*.el)

**Purpose:** Language-specific configuration

**Characteristics:**
- **MUST be minimal** - 3 to 10 lines typical
- **MUST NOT configure LSP servers** - LSP is centralized in init-prog.el
- Simple `:ensure t :defer t` preferred
- Use setup functions only for language-specific buffer-local settings

**Example minimal language module:**
```elisp
;;; init-go.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-06 19:29:50 Friday by zhengyu.li>

;; Copyright (C) 2021, 2022, 2023, 2024, 2025, 2026 zhengyu li
;;
;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: go, golang
;; Dependencies: (none)

;; [GPL license text]

;;; Commentary:
;;
;; Go language configuration.

;;; Code:

;; ==================================================================================
(use-package go-mode
  :ensure t
  :defer t)
;; LSP server (gopls) is configured in init-prog.el.

;; ==================================================================================
(provide 'init-go)

;;; init-go.el ends here
```

## Common Emacs Commands

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

### Dired/Dirvish Keybindings (in dired-mode)

| Key     | Command                      | Description          |
|---------|------------------------------|----------------------|
| `C-x d` | dired                        | Open dired           |
| `C-x j` | dired-jump                   | Jump to dired        |
| `o`     | omw/dired-open-externally    | Open file externally |
| `k`     | omw/smart-kill-buffer        | Kill buffer          |
| `TAB`   | dirvish-subtree-toggle       | Toggle subtree       |

### PDF View Keybindings (in pdf-view-mode)

| Key | Command                                      | Description        |
|-----|----------------------------------------------|--------------------|
| `j` | pdf-view-next-line-or-next-page              | Next line/page     |
| `k` | pdf-view-previous-line-or-previous-page      | Previous line/page |
| `+` | pdf-view-enlarge                             | Zoom in            |
| `-` | pdf-view-shrink                              | Zoom out           |

## Validation and Troubleshooting

**Test Emacs loads without errors:**
```bash
emacs --debug-init
```

**Check for LSP issues:**
1. Verify server installed: `which pylsp gopls clangd`
2. Start LSP manually: `M-x eglot`
3. Check events log: Switch to `*eglot-events*` buffer

**Package installation issues:**
```elisp
M-x package-refresh-contents  ; Refresh package list
```

**Slow startup**: Check `*Messages*` buffer with `C-h e`

## Important Notes

- **macOS-focused**: Configuration assumes macOS as primary OS
- **Symlink-based**: Config files are symlinked, not copied
- **Emacs minimum version**: 30.2 (hard check in init.el)
- **No Zsh config**: Repository doesn't contain Zsh files (see README.md for setup)
- **Prefix correction**: Code uses `omw/` prefix, not `my/`

## Quick Reference Card

### Essential Rules

1. **File Header:** Must include Time-stamp, current copyright year, Dependencies
2. **use-package Order:** `:ensure → :when → :defer → :after → :hook → :bind → :custom-face → :config`
3. **Naming Prefix:** Always `omw/` for custom functions/variables
4. **Setup Functions:** Use for 3+ buffer-local settings; prefer built-in functions over wrappers
5. **Comments:** Minimal, only for non-obvious code
6. **Separators:** `;; ==================================================================================`
7. **Language Modules:** Keep minimal, LSP in init-prog.el
8. **Face Customization:** Use `:custom-face` with `fixed-pitch` inheritance to avoid buffer-local remapping
9. **Tracking Packages:** Use `:demand t` for environment tracking (pyvenv, poetry)

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
(define-minor-mode omw/<mode>-mode
  "Description."
  :lighter " Indicator"
  :global nil
  (if omw/<mode>-mode
      (add-hook 'hook-name #'omw/function nil t)
    (remove-hook 'hook-name #'omw/function t)))

;; Package with custom faces (use fixed-pitch to avoid buffer-local remapping)
(use-package centaur-tabs
  :ensure t
  :custom-face
  (centaur-tabs-selected ((t (:inherit fixed-pitch :bold t :foreground "#28cd41")))))

;; Environment tracking package (use :demand t for immediate activation)
(use-package pyvenv
  :ensure t
  :demand t
  :config
  (pyvenv-tracking-mode 1))
```
