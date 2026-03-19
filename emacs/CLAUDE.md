# CLAUDE.md - Emacs Configuration

This file provides guidance for Claude Code when working with the Emacs configuration in this directory.

## Project Overview

This is an **Emacs configuration** (>= 30.2) for macOS, featuring LSP support, completion system, and project management.

## Directory Structure

The Emacs configuration is located at `.config/emacs/` within the stow package:

```
emacs/.config/emacs/
├── early-init.el        # Early initialization (before package system)
├── init.el              # Main entry point (symlinked to ~/.config/emacs/init.el)
├── banners/             # Dashboard banners (octopus.png, totoro.png)
├── lisp/
│   ├── editor/          # Editor behavior (appearance, completion, edit, etc.)
│   ├── lib/             # Utility library (proxy, shared utils)
│   ├── lang/            # Programming languages
│   │   ├── *.el         # Language-specific configs (cc, python, go, etc.)
│   │   └── config/      # Config/build languages (cmake, dockerfile, yaml)
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

### Template Placeholders

Using `template.el` as an example, it includes these placeholders:
- `${1:keyword1, keyword2}` - Keywords
- `${2:(none)}` - Dependencies
- `${3:One or two sentence description...}` - Module description

**Note**: When modifying template format, update `templates/template.el` file accordingly.

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

**Dired/Dirvish Keybindings** (in dired-mode):

| Key     | Command                   | Description          |
|---------|---------------------------|----------------------|
| `C-x d` | dired                     | Open dired           |
| `C-x j` | dired-jump                | Jump to dired        |
| `o`     | omw/dired-open-externally | Open file externally |
| `k`     | omw/smart-kill-buffer     | Kill buffer          |
| `TAB`   | dirvish-subtree-toggle    | Toggle subtree       |

**PDF View Keybindings** (in pdf-view-mode):

| Key | Command                                 | Description        |
|-----|-----------------------------------------|--------------------|
| `j` | pdf-view-next-line-or-next-page         | Next line/page     |
| `k` | pdf-view-previous-line-or-previous-page | Previous line/page |
| `+` | pdf-view-enlarge                        | Zoom in            |
| `-` | pdf-view-shrink                         | Zoom out           |

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
4. **Languages**: omw-prog → omw-cc → omw-go → omw-python → omw-typescript → omw-elisp → omw-shell → omw-cmake → omw-yaml → omw-dockerfile
5. **Text**: omw-markdown

**When adding new modules**:
- Add `(provide 'omw-xxx)` at end of file (where xxx is the module name)
- Add `(require 'omw-xxx)` in `init.el` at correct position
- Language modules are explicitly required in init.el (not auto-loaded)

### Key Architecture Patterns

**Centralized LSP Configuration**: All language LSP servers are configured in `omw-prog.el` via a single `eglot` use-package block. Language modules should NOT configure LSP servers themselves.

**Minimal Language Modules**: Language modules in `lisp/lang/` (named `omw-xxx.el`) should be minimal (3-10 lines typical). They only install the major mode package.

**Custom Packages**: The `site-packages/` directory is automatically added to `load-path` by `init.el`. Custom packages there can be required with `(require 'package-name)`.

**Custom File Separation**: Emacs customizations are stored in `~/.emacs.d/custom.el`, not in init.el. The `custom-file` variable is set early (before package initialization) to prevent Emacs from writing customizations to init.el.

**Path Resolution**: The config uses `omw/emacs-config-root-path` to find the actual config directory, resolving symlinks correctly.

### Module Guidelines

**Editor Modules** (`lisp/editor/omw-*.el`)
- Purpose: Fundamental editor features (appearance, editing, completion, fonts)
- Characteristics: May use `:demand t` for critical packages, complex configurations acceptable
- Examples: omw-appearance.el, omw-completion.el, omw-edit.el, omw-font.el

**Lib Modules** (`lisp/lib/omw-*.el`)
- Purpose: Utility/library modules shared across other modules
- Characteristics: Loaded early or on-demand; no UI, just utilities
- Examples: omw-proxy.el, omw-utils.el

**Tool Modules** (`lisp/tool/omw-*.el`)
- Purpose: External tool integration (Git, AI, PDF, terminal)
- Characteristics: May use `:vc` for Git-based packages, platform-specific code acceptable
- Examples: omw-git.el, omw-ai.el, omw-pdf.el, omw-term.el

**Language Modules** (`lisp/lang/omw-*.el` and `lisp/lang/config/omw-*.el`)
- Purpose: Language-specific configuration
- Characteristics: Should be concise and focused, MUST NOT configure LSP servers (centralized in omw-prog.el)
- Simple `:ensure t :defer t` preferred for most cases
- Use setup functions only for language-specific buffer-local settings
- Keep language-related code together for module cohesion

## Coding Standards

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

**Exception: `site-packages/` directory**

The `site-packages/` directory holds custom packages maintained as if they were
external packages (potential future MELPA candidates). Functions in this
directory follow Emacs ecosystem naming conventions for their respective
subsystem (e.g., `dired-` prefix for dired-ecosystem extensions) rather than
the `omw/` prefix. This exception is intentional to preserve compatibility with
external package conventions.

**Function Naming Patterns:**
- **Setup functions** (mode configuration): `omw/<mode>-setup`
  - Examples: `omw/prog-mode-setup`, `omw/markdown-mode-setup`
- **Tool installer functions** (install dependencies): `omw/install-<lang>-tools`
  - Examples: `omw/install-python-tools`, `omw/install-go-tools`, `omw/install-bash-tools`
- **Action functions** (operations): `omw/<noun>-<verb>`
  - Examples: `omw/indent-entire-buffer`, `omw/set-http-proxy`
- **Mode indicators** (mode-line display): `omw/<feature>-mode-line-indicator`
  - Examples: `omw/pyvenv-mode-line-indicator`

**Variable Naming Patterns:**
- Format: `omw/<category>-<item>`
- Examples: `omw/font-monospace-list`, `omw/font-size-default`, `omw/http-proxy`
- Must use `defcustom` with `:group 'omw-emacs`

**Minor Mode Naming:**
- Format: `omw/<feature>-mode`
- Examples: `omw/prog-before-save-mode`

### File Header Format (MANDATORY)

Template file: `templates/template.el` (auto-inserted via auto-insert)

**Structure**:

| Line | Content | Description |
|-----|------|------|
| 1 | `;;; filename.el -*- lexical-binding: t; -*-` | Filename + lexical binding |
| 2 | `;; Time-stamp: <YYYY-MM-DD HH:MM:SS Day by Author>` | Last modification timestamp |
| 4-6 | Author, Keywords, Dependencies | Metadata |
| 8 | Copyright | Current year |
| 10-22 | MIT License | Full license text |
| 24-26 | History | Creation date |
| 28-30 | Commentary | Module description |

**Required fields checklist:**
- ✅ `-*- lexical-binding: t; -*-` on line 1
- ✅ Time-stamp: `<YYYY-MM-DD HH:MM:SS Day by Author>` on line 2
- ✅ Author line with name and email
- ✅ Keywords: 2-4 relevant tags
- ✅ Dependencies: list if any, otherwise "(none)"
- ✅ Copyright with current year
- ✅ MIT license full text
- ✅ History section with creation date
- ✅ Commentary: Brief description (1-2 sentences)

### File Footer (MANDATORY)

```elisp
;; ============================================================================
;;; Provide features
(provide 'omw-module)

;;; omw-module.el ends here
```


### use-package Patterns (MANDATORY)

**Keywords MUST appear in this exact order:**

```
1. :ensure or :vc          # Package source
2. :when or :if            # Conditional loading (optional)
3. :defer or :demand       # Loading strategy - OMIT when :after is present
4. :after                  # Dependencies (optional) - implies deferral
5. :hook                   # Mode hooks
6. :bind                   # Keybindings
7. :custom-face            # Face customization (optional)
8. :config                 # Configuration
```

**CRITICAL: `:after` implies deferral - never use `:defer t` together with `:after`.**

`:after` waits for the dependency to load before loading this package, which
already defers loading. Adding `:defer t` is redundant and incorrect.

```elisp
;; ✅ CORRECT - :after provides deferral, no :defer t needed
(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; ❌ WRONG - :defer t and :after together
(use-package corfu-terminal
  :ensure t
  :defer t
  :after corfu
  :config
  (corfu-terminal-mode 1))
```

#### :ensure/:vc Rules

**External packages from ELPA/MELPA:**
```elisp
(use-package <package>
  :ensure t)
```

**Built-in Emacs packages:**
```elisp
(use-package <built-in>
  :ensure nil)
```

**Packages from Git/VCS:**
```elisp
(use-package <package>
  :vc (:url "https://github.com/<user>/<repo>" :rev :newest))
```

#### :defer vs :demand Rules

**Default behavior - Use `:defer t` for 95% of packages:**
```elisp
(use-package <package>
  :ensure t
  :defer t)
```

**Critical packages requiring immediate load** (themes, completion core, environment tracking):
```elisp
(use-package <critical-package>
  :ensure t
  :demand t
  :config
  ...)
```

**Environment tracking packages** (pyvenv, poetry) need `:demand t` to activate immediately:
```elisp
(use-package pyvenv
  :ensure t
  :demand t
  :config
  (pyvenv-tracking-mode 1))
```

#### :hook Rules

**Single hook - Simple form:**
```elisp
:hook (prog-mode . smartparens-mode)
```

**Multiple hooks - List form:**
```elisp
:hook ((c-mode . eglot-ensure)
       (c++-mode . eglot-ensure)
       (python-mode . eglot-ensure))
```

**Setup function for complex configurations** (3+ buffer-local settings):
```elisp
(defun omw/prog-mode-setup ()
  "Apply custom settings for programming modes."
  (setq-local tab-width 4)
  (display-line-numbers-mode 1)
  (hs-minor-mode 1))

(use-package prog-mode
  :hook (prog-mode . omw/prog-mode-setup))
```

**Setup function rules:**
- Use `setq-local` for variables, mode functions for toggles
- Naming: `omw/<mode>-setup`
- Prefer built-in functions over custom wrappers

#### :bind Rules

**Global keybindings only:**
```elisp
:bind (("C-x C-t" . multi-vterm)
       ("C-c g s" . magit-status))
```

**Mode-local keybindings only:**
```elisp
:bind (:map vterm-mode-map
            ("M-1" . nil)
            ("C-g" . vterm--self-insert))
```

**Both global and mode-local:**
```elisp
:bind (("C-x j" . dired-jump)
       (:map dired-mode-map
             ("C-c C-c" . dired-do-copy)
             ("C-c C-r" . dired-do-rename)))
```

**Keybinding prefix guidelines:**
- `C-c <letter>` - For mode-specific commands
- `C-c <letter> <letter>` - For sub-modes
- Avoid overriding Emacs defaults unless documented

#### :custom-face Rules

**Use `:custom-face` instead of `custom-set-faces`:**
```elisp
:custom-face
(centaur-tabs-selected ((t (:inherit fixed-pitch :bold t :foreground "#28cd41"))))
```

**Use `fixed-pitch` inheritance to prevent buffer-local remapping issues:**
```elisp
;; ✅ CORRECT: Explicit attributes prevent inheritance from buffer-local remapping
:custom-face
(centaur-tabs-display-line ((t (:inherit fixed-pitch :box nil :underline nil))))

;; ❌ AVOID: Inheriting from 'default can pick up buffer-local face remapping
:custom-face
(some-face ((t (:inherit default))))
```

**Why:** Buffer-local face remapping affects faces that inherit from `default`. Using `fixed-pitch` or explicit attributes prevents unintended font scaling in UI elements.

#### :config Rules

**Simple configuration (single category):**
```elisp
:config
(setq var1 value1
      var2 value2)
```

**Categorized configuration (multiple categories) - PREFERRED:**
```elisp
:config
;; Category 1
(setq var1 value1
      var2 value2)

;; Category 2
(setq var3 value3
      var4 value4)
```

**Category naming guidelines:**
- Use brief, descriptive names based on **functionality**, not implementation details
- Group related settings together
- Separate categories with **blank lines** (one empty line between categories)
- Each category gets its own `setq` block
- Single-variable categories are acceptable when the variable is distinct enough

**Common category examples:**
- `Autosave` - auto-save-list-file-prefix
- `Native compilation` - native-compile-target-directory
- `Backup and version control` - backup-directory-alist, backup-by-copying, version-control, delete-old-versions
- `File and buffer management` - recentf-max-saved-items, uniquify-buffer-name-style, uniquify-separator
- `User identity and timestamps` - user-full-name, user-mail-address, time-stamp-format
- `Startup behavior` - inhibit-default-init, inhibit-startup-echo-area-message, inhibit-startup-screen
- `UI and interaction` - use-short-answers, ring-bell-function
- `macOS key modifiers` - mac-command-modifier, mac-option-modifier (platform-specific)

**AVOID grouping by implementation detail:**
```elisp
;; ❌ WRONG: Grouping by "XDG paths" (implementation detail)
;; XDG paths
(setq auto-save-list-file-prefix ...
      native-compile-target-directory ...
      backup-directory-alist ...)

;; ✅ CORRECT: Grouping by functionality
;; Autosave
(setq auto-save-list-file-prefix ...)

;; Native compilation
(setq native-compile-target-directory ...)

;; Backup and version control
(setq backup-directory-alist ...
      backup-by-copying t
      version-control t)
```

**Configuration with requires:**
```elisp
:config
(require 'dired-x)
(require 'dired-async)
(setq dired-dwim-target t)
```


### Function & Variable Patterns

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

### Code Style

#### Inline Comments

> **See [Root CLAUDE.md - Inline Comments](../CLAUDE.md#inline-comments-critical) for repository-wide rule.**

Emacs Lisp follows the repository-wide inline comment prohibition with one exception: let binding comments.

**Exception: Comments in let bindings**

Comments within `let`/`let*` bindings may use `;;` prefix on the same line as the variable they describe:

```elisp
(let* (;; Normalize proxy URL: add http:// prefix if missing
       (proxy-url (if (string-match-p "\\`https?://" proxy)
                      proxy
                    (concat "http://" proxy)))
       ;; Parse proxy URL to extract host and port
       (parsed (url-generic-parse-url proxy-url)))
  ...)
```

This exception exists because:
1. Let binding comments describe the variable being defined on that line
2. Moving them above would break the binding structure
3. They are scoped to the binding, not general code

**❌ End-of-line comments still prohibited for regular code:**
```elisp
;; ❌ End-of-line comments cause alignment issues
(setq tab-width 4)  ; Set tab width to 4
```

**❌ Unnecessary comment:**
```elisp
;; ❌ Obvious from code
(setq tab-width 4)
```

#### Section Separators

**Required format (with blank lines before/after):**
```elisp
;; ============================================================================

(use-package ...)

;; ============================================================================
```

**Usage:**
- ✅ Before each major use-package block
- ✅ Before each major function definition (if file has multiple sections)
- ✅ Before "Provide features" section
- ✅ With blank line before and after
- ✅ Width: 79 characters (matching repository-wide standard)

#### Alignment Spaces (CRITICAL)

> **See [Root CLAUDE.md - Alignment Spaces](../CLAUDE.md#alignment-spaces-critical) for repository-wide rule.**

Emacs Lisp follows the repository-wide alignment space prohibition. Here are Emacs-specific examples:

**❌ AVOID - Alignment spaces in cons cells:**
```elisp
'(("short"   . value1)
  ("medium"  . value2)
  ("long-key" . value3))
```

**✅ CORRECT - Single space before dot:**
```elisp
'(("short" . value1)
  ("medium" . value2)
  ("long-key" . value3))
```

**❌ AVOID - Alignment spaces in :hook/:bind lists:**
```elisp
:hook ((c-mode      . eglot-ensure)
       (python-mode . eglot-ensure))
```

**✅ CORRECT - Single space:**
```elisp
:hook ((c-mode . eglot-ensure)
       (python-mode . eglot-ensure))
```

#### Code Formatting

**Indentation:** 2 spaces (Emacs Lisp default)

**Multi-line setq:**
```elisp
(setq var1 value1
      var2 value2
      var3 value3)
```

**setq grouping and comment rules (CRITICAL):**

1. **Group related variables into a single `setq`** - variables that configure
   the same feature or concern belong together.
2. **Put a single group-level comment above the `setq`** - describe the group as
   a whole, not individual variables.
3. **Never add per-variable inline comments inside a `setq` body.**

```elisp
;; ✅ CORRECT - group comment above, single setq, no per-variable comments
;; Omit filter rules:
;; - dired-omit-files: hidden files/dirs (.), common project dirs
;; - dired-omit-extensions: compiled artifacts and lock files
(setq dired-omit-files (concat "^\\.\\|" "…")
      dired-omit-extensions (append dired-omit-extensions '(".pyc" ".elc")))

;; ✅ CORRECT - no comment needed when purpose is obvious
(setq var1 value1
      var2 value2
      var3 value3)

;; ❌ WRONG - per-variable inline comments inside setq body
(setq var1 value1
      ;; Set tab width
      var2 4
      ;; Enable this feature
      var3 t)

;; ❌ WRONG - split related variables into separate setq blocks needlessly
(setq dired-omit-files "…")
;; Supplementary extension filter
(setq dired-omit-extensions (append …))
```

**Comment prefix rules:**
- Use `;;` for all inline code comments
- Use `;;;` only for file-level section headers (;; Commentary:, ;;; Code:)

#### TODO/FIXME Format

Standardize comment markers for tracking work:

```elisp
;; TODO(author): Description of what needs to be done
;; FIXME(author): Description of the bug or issue
;; NOTE: Important information for maintainers
;; HACK: Temporary workaround with explanation
```

**Examples:**
```elisp
;; TODO(zhengyu.li): Add support for tree-sitter fontification
;; FIXME(zhengyu.li): Company backends conflict with corfu
;; NOTE: This requires Emacs 30+
```

#### Comment Language

**Rule: All comments must be in English.**

This includes:
- File headers (Commentary, History)
- Inline comments
- TODO/FIXME markers
- Docstrings

## Code Quality

### Validation Commands

**Test configuration loads without errors:**
```bash
# Full initialization test
emacs --debug-init

# Batch mode test (faster for CI/CD)
emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") (message "Configuration loaded successfully"))'
```

**Check naming conventions:**
```bash
# Find functions without omw/ prefix
grep -rn "defun (?!omw/)" lisp --include="*.el" | grep -v "^;;"

# Find defcustom without :group 'omw-emacs
grep -rn "defcustom.*:group" lisp --include="*.el" | grep -v "omw-emacs" | grep -v "^;;"
```

**Check file header completeness:**
```bash
# Verify Author line presence
grep -rn "^;; Author:" lisp --include="*.el" | wc -l

# Verify current copyright year (2026)
grep -rn "Copyright (C) 2026" lisp --include="*.el" | wc -l

# Verify History section
grep -rn "^;;; History:" lisp --include="*.el" | wc -l

# Verify MIT license text
for file in lisp/**/*.el lisp/*.el; do
  grep -q "Permission is hereby granted" "$file" || echo "Missing MIT: $file"
done
```

**Check use-package keyword order:**
```bash
# Find potential violations (visual inspection needed)
grep -A10 "use-package" lisp --include="*.el" | grep -E ":hook|:bind" | head -20
```

### Compliance Checklist

- [ ] All custom functions use `omw/` prefix
- [ ] All defcustom use `:group 'omw-emacs`
- [ ] All files have Time-stamp, Author, Copyright, Dependencies, History, Commentary
- [ ] All files have complete MIT license text
- [ ] All setup functions have docstrings
- [ ] use-package keywords follow correct order
- [ ] Section separators (`;; ============================================================================`) used correctly (79 chars)
- [ ] Files end with `(provide 'omw-xxx)` and `;;; omw-xxx.el ends here`
- [ ] All comments in English

### Current Status

**Compliance Level:** 100% (as of 2026-03-18)

**Key metrics:**
- Naming convention violations: 0
- File header violations: 0
- Documentation gaps: 0
- use-package order violations: 0
- Configuration load errors: 0

### Troubleshooting

**Emacs fails to load:**
```bash
emacs --debug-init  # Check error messages
```

**LSP issues:**
1. Verify server installed: `which pylsp gopls clangd`
2. Start LSP manually: `M-x eglot`
3. Check events log: Switch to `*eglot-events*` buffer

**Package installation issues:**
```elisp
M-x package-refresh-contents  ; Refresh package list
```

**Slow startup:** Check `*Messages*` buffer with `C-h e`

### Important Notes

- **macOS-focused**: Configuration assumes macOS as primary OS
- **Symlink-based**: Config files are symlinked, not copied
- **Emacs minimum version**: 30.2 (hard check in init.el)
- **Prefix correction**: Code uses `omw/` prefix, not `my/`
- **Quality assurance**: Codebase maintains 98%+ compliance with documented standards through regular audits

## Best Practices

### 1. Language Module Cohesion

**Principle**: Keep language-related code together, even if it means more lines.

**Examples from the codebase:**
- `omw-python.el` (46 lines) - Includes pyvenv and poetry tracking because these are essential for Python development
- `omw-markdown.el` (82 lines) - Includes visual editing tools (valign, olivetti, visual-fill-column) because they're part of the Markdown writing experience
- `omw-elisp.el` (19 lines) - Includes enhancement tools (elisp-slime-nav, lisp-extra-font-lock, rainbow-mode) for better Lisp development

**Why this works better than artificial limits:**
- ✅ **Cohesion**: All code for Language X is in one place
- ✅ **Discoverability**: Developers know where to find language-specific features
- ✅ **Maintainability**: Language features are not scattered across multiple files
- ✅ **Pragmatism**: Real-world languages have different complexity levels

### 2. Docstring Style

**Use paragraph form, not bullet lists:**

```elisp
;; ❌ AVOID: Bullet list in docstrings
"Do X.
- Step 1
- Step 2
- Step 3"

;; ✅ CORRECT: Paragraph form
"Do X by first doing step 1, then step 2.
Finally complete step 3."
```

**Why**: Bullet lists are hard to read in docstrings and violate Emacs conventions.

### 3. Testing Before Committing

**Always run these commands before committing changes:**

```bash
# 1. Syntax check
emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") (message "✅ OK"))'

# 2. Debug check
emacs --debug-init

# 3. Quick audit
grep -rn "defun (?!omw/)" lisp --include="*.el"
grep -rn "defcustom.*:group" lisp --include="*.el" | grep -v "omw-emacs"
```

**Why**: Catches common issues before they reach the repository.

### 4. File Header Consistency

**Use template file** `templates/template.el` to ensure consistency.

**Why**: Templates auto-generate standard format, avoiding inconsistencies from manual editing.

### 5. Special Cases

**Environment tracking packages** (pyvenv, poetry) need `:demand t`:
```elisp
(use-package pyvenv
  :ensure t
  :demand t
  :config
  (pyvenv-tracking-mode 1))
```

**Face customization** uses `:custom-face` with `fixed-pitch`:
```elisp
(use-package centaur-tabs
  :ensure t
  :custom-face
  (centaur-tabs-selected ((t (:inherit fixed-pitch :bold t :foreground "#28cd41")))))
```

**Why**: Prevents buffer-local face remapping issues and ensures tracking activates immediately.

---

## Quick Reference Card

### Essential Rules

1. **File Header:** Must include Time-stamp, Author, Copyright, Dependencies, History, Commentary
2. **use-package Order:** `:ensure → :when → :defer/:demand → :after → :hook → :bind → :custom-face → :config`; never use `:defer t` with `:after`
3. **Naming Prefix:** Always `omw/` for custom functions/variables
4. **Setup Functions:** Use `setq-local` for variables, mode functions for toggles; prefer built-in functions
5. **Comments:** Minimal, only for non-obvious code
6. **Separators:** `;; ============================================================================` (79 chars)
7. **Language Modules:** Keep language-related code together, don't artificially limit lines
8. **LSP Configuration:** Centralize all LSP in omw-prog.el, never in language modules
9. **Face Customization:** Use `:custom-face` with `fixed-pitch` inheritance to avoid buffer-local remapping
10. **Tracking Packages:** Use `:demand t` for environment tracking (pyvenv, poetry)
11. **Comment Language:** English only

### Pre-Commit Checklist

- [ ] Configuration loads: `emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") ...)'`
- [ ] No naming violations: All functions use `omw/` prefix
- [ ] No variable violations: All defcustom use `:group 'omw-emacs`
- [ ] File headers complete: Time-stamp, Author, Copyright, Dependencies, History, Commentary, MIT
- [ ] Docstrings present: All setup functions have documentation
- [ ] use-package order correct: Keywords in standard order
- [ ] Section separators 79 chars: `;; ============================================================================`
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

;; Section separator (79 chars)
;; ============================================================================
```
