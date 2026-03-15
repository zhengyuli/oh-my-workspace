# CLAUDE.md - Emacs Configuration

This file provides guidance for Claude Code when working with the Emacs configuration in this directory.

## Project Overview

This is an **Emacs configuration** (>= 30.2) for macOS, featuring LSP support, completion system, and project management.

## Directory Structure

```
emacs/
├── init.el              # Main entry point (symlinked to ~/.emacs)
├── lisp/
│   ├── editor/          # Editor behavior (appearance, completion, edit, etc.)
│   ├── system/          # System/environment (credential, proxy)
│   ├── lang/            # Programming languages
│   │   ├── *.el         # Language-specific configs (cc, python, go, etc.)
│   │   └── markup/      # Markup/config languages (cmake, dockerfile, yaml)
│   ├── text/            # Text/document modes (markdown)
│   └── tool/            # Tool integrations (git, ai, pdf, term)
├── site-packages/       # Custom Emacs packages (auto-added to load-path)
└── templates/           # File templates
```

## Quick Start

### Setup

```bash
# Run root setup script (creates symlinks)
./setup.sh link

# Test configuration loads without errors
emacs --debug-init

# Install all dependencies (macOS)
brew install git aspell pandoc the_silver_searcher ripgrep coreutils libvterm fd marksman llvm
npm install -g typescript-language-server yaml-language-server bash-language-server dockerfile-language-server-nodejs
pip install "python-lsp-server[all]" black black-macchiato isort pylint debugpy cmake-language-server
go install golang.org/x/tools/gopls@latest mvdan.cc/gofumpt@latest
```

Dependencies are managed via `homebrew/Brewfile` at the repository root.

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
emacs --batch --eval '(progn (load-file "emacs/init.el") (message "Configuration loaded successfully"))'
```

## Architecture

### Module Loading System

**Critical**: The `init.el` file loads modules in a specific order. Dependencies MUST be respected.

**Loading order** (lines 209-236 of init.el):
1. **Editor**: init-font → init-appearance → init-edit → init-search → init-template → init-completion → init-explorer
2. **System**: init-credential → init-proxy
3. **Tools**: init-git → init-term → init-pdf → init-ai
4. **Languages**: init-prog → init-cc → init-go → init-python → init-javascript → init-elisp → init-shell → init-cmake → init-yaml → init-dockerfile
5. **Text**: init-markdown

**When adding new modules**:
- Add `(provide 'init-xxx)` at end of file (where xxx is the module name)
- Add `(require 'init-xxx)` in `init.el` at correct position
- Language modules are explicitly required in init.el (not auto-loaded)

### Key Architecture Patterns

**Centralized LSP Configuration**: All language LSP servers are configured in `init-prog.el` via a single `eglot` use-package block. Language modules should NOT configure LSP servers themselves.

**Minimal Language Modules**: Language modules in `lisp/lang/` (named `init-xxx.el`) should be minimal (3-10 lines typical). They only install the major mode package.

**Custom Packages**: The `site-packages/` directory is automatically added to `load-path` by `init.el`. Custom packages there can be required with `(require 'package-name)`.

**Custom File Separation**: Emacs customizations are stored in `~/.emacs.d/custom.el`, not in init.el. The `custom-file` variable is set early (before package initialization) to prevent Emacs from writing customizations to init.el.

**Path Resolution**: The config uses `omw/emacs-config-root-path` to find the actual config directory, resolving symlinks correctly.

### Module Guidelines

**Editor Modules** (`lisp/editor/init-*.el`)
- Purpose: Fundamental editor features (appearance, editing, completion, fonts)
- Characteristics: May use `:demand t` for critical packages, complex configurations acceptable
- Examples: init-appearance.el, init-completion.el, init-edit.el, init-font.el

**System Modules** (`lisp/system/init-*.el`)
- Purpose: System/environment configuration (credentials, proxy settings)
- Characteristics: Environment-specific, may contain sensitive configuration
- Examples: init-credential.el, init-proxy.el

**Tool Modules** (`lisp/tool/init-*.el`)
- Purpose: External tool integration (Git, AI, PDF, terminal)
- Characteristics: May use `:vc` for Git-based packages, platform-specific code acceptable
- Examples: init-git.el, init-ai.el, init-pdf.el, init-term.el

**Language Modules** (`lisp/lang/init-*.el` and `lisp/lang/markup/init-*.el`)
- Purpose: Language-specific configuration
- Characteristics: Should be concise and focused, MUST NOT configure LSP servers (centralized in init-prog.el)
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

**Function Naming Patterns:**
- **Setup functions** (mode configuration): `omw/<mode>-setup`
  - Examples: `omw/prog-mode-setup`, `omw/markdown-mode-setup`
- **Tool installer functions** (ensure dependencies): `omw/ensure-<lang>-tools`
  - Examples: `omw/ensure-python-tools`, `omw/ensure-go-tools`, `omw/ensure-bash-tools`
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

Every `.el` file must start with:

```elisp
;;; init-module.el -*- lexical-binding: t; -*-

;; Author: chieftain <lizhengyu419@outlook.com>
;; Keywords: keyword1, keyword2
;; Dependencies: (none) or module-name

;; Copyright (C) 2026 zhengyu li

;; Licensed under the GPL License version 3.0

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; History:
;;
;; YYYY-MM-DD HH:MM chieftain <lizhengyu419@outlook.com> created.

;;; Commentary:
;;
;; One or two sentence description of module purpose.

;;; Code:

;; ==================================================================================
```

**Required fields checklist:**
- ✅ `-*- lexical-binding: t; -*-` on line 1
- ✅ Author line with name and email
- ✅ Keywords: 2-4 relevant tags
- ✅ Dependencies: list if any, otherwise "(none)"
- ✅ Copyright with current year
- ✅ GPL license short notice
- ✅ Full GPL license text
- ✅ History section with creation date
- ✅ Commentary: Brief description (1-2 sentences)

### File Footer (MANDATORY)

```elisp
;; ==================================================================================
;;; Provide features
(provide 'init-module)

;;; init-module.el ends here
```


### use-package Patterns (MANDATORY)

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

**Simple configuration:**
```elisp
:config
(setq var1 value1
      var2 value2)
```

**Complex configuration:**
```elisp
:config
(setq var1 value1)
(setq var2 value2)
(function-call arg1 arg2)
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

**Rule: Use inline comments ONLY when:**
- Explaining "why" not "what"
- Documenting non-obvious behavior
- Providing context for complex logic

**Comment style guidelines:**
- Use standalone line comments with `;;` prefix
- Do NOT use end-of-line comments (alignment issues)
- Comments in let bindings use `;;` prefix on same line as variable

**✅ Good inline comment (standalone):**
```elisp
;; Add custom attributes not in default set
(setq dirvish-attributes
      (append dirvish-attributes
              '(git-msg file-modes)))
```

**✅ Good let binding comment:**
```elisp
(let* (;; Normalize proxy URL: add http:// prefix if missing
       (proxy-url (if (string-match-p "\\`https?://" proxy)
                      proxy
                    (concat "http://" proxy)))
       ;; Parse proxy URL to extract host and port
       (parsed (url-generic-parse-url proxy-url)))
  ...)
```

**❌ Avoid end-of-line comments:**
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
;; ==================================================================================

(use-package ...)

;; ==================================================================================
```

**Usage:**
- ✅ Before each major use-package block
- ✅ Before each major function definition (if file has multiple sections)
- ✅ Before "Provide features" section
- ✅ With blank line before and after

#### Code Formatting

**Indentation:** 2 spaces (Emacs Lisp default)

**Multi-line setq:**
```elisp
(setq var1 value1
      var2 value2
      var3 value3)
```

**Multi-line setq with comments (use standalone comments above):**
```elisp
;; Configure variables for feature X
(setq var1 value1
      var2 value2
      var3 value3)
```

**Comment prefix rules:**
- Use `;;` for all inline code comments
- Use `;;;` only for file-level section headers (;; Commentary:, ;;; Code:)

## Code Quality

### Validation Commands

**Test configuration loads without errors:**
```bash
# Full initialization test
emacs --debug-init

# Batch mode test (faster for CI/CD)
emacs --batch --eval '(progn (load-file "emacs/init.el") (message "Configuration loaded successfully"))'
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

# Verify GPL license text
for file in lisp/**/*.el lisp/*.el; do
  grep -q "This program is free software" "$file" || echo "Missing GPL: $file"
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
- [ ] All files have Author, Copyright, Dependencies, History, Commentary
- [ ] All files have complete GPL license text
- [ ] All setup functions have docstrings
- [ ] use-package keywords follow correct order
- [ ] Section separators (`;; ==================================================================================`) used correctly
- [ ] Files end with `(provide 'init-xxx)` and `;;; init-xxx.el ends here`

### Current Status

**Compliance Level:** 98%+ (as of 2026-03-13)

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
- `init-python.el` (46 lines) - Includes pyvenv and poetry tracking because these are essential for Python development
- `init-markdown.el` (82 lines) - Includes visual editing tools (valign, olivetti, visual-fill-column) because they're part of the Markdown writing experience
- `init-elisp.el` (19 lines) - Includes enhancement tools (elisp-slime-nav, lisp-extra-font-lock, rainbow-mode) for better Lisp development

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
emacs --batch --eval '(progn (load-file "emacs/init.el") (message "✅ OK"))'

# 2. Debug check
emacs --debug-init

# 3. Quick audit
grep -rn "defun (?!omw/)" lisp --include="*.el"
grep -rn "defcustom.*:group" lisp --include="*.el" | grep -v "omw-emacs"
```

**Why**: Catches common issues before they reach the repository.

### 4. File Header Consistency

**Keep file headers consistent** - use the exact same format:

- Line 1: `;;; init-filename.el -*- lexical-binding: t; -*-`
- Line 3: Author with name and email
- Line 4: Keywords (2-4 tags)
- Line 5: Dependencies ((none) or module-name)
- Line 7: Copyright with current year
- Line 9: GPL license short notice
- Lines 11-23: Full GPL license text
- Lines 25-27: History section with creation date
- Lines 29-31: Commentary (1-2 sentence description)

**Why**: Consistency makes the codebase easier to navigate and understand.

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

1. **File Header:** Must include Author, Copyright, Dependencies, History, Commentary
2. **use-package Order:** `:ensure → :when → :defer → :after → :hook → :bind → :custom-face → :config`
3. **Naming Prefix:** Always `omw/` for custom functions/variables
4. **Setup Functions:** Use `setq-local` for variables, mode functions for toggles; prefer built-in functions
5. **Comments:** Minimal, only for non-obvious code
6. **Separators:** `;; ==================================================================================`
7. **Language Modules:** Keep language-related code together, don't artificially limit lines
8. **LSP Configuration:** Centralize all LSP in init-prog.el, never in language modules
9. **Face Customization:** Use `:custom-face` with `fixed-pitch` inheritance to avoid buffer-local remapping
10. **Tracking Packages:** Use `:demand t` for environment tracking (pyvenv, poetry)

### Pre-Commit Checklist

- [ ] Configuration loads: `emacs --batch --eval '(progn (load-file "emacs/init.el") ...)'`
- [ ] No naming violations: All functions use `omw/` prefix
- [ ] No variable violations: All defcustom use `:group 'omw-emacs`
- [ ] File headers complete: Author, Copyright, Dependencies, History, Commentary, GPL
- [ ] Docstrings present: All setup functions have documentation
- [ ] use-package order correct: Keywords in standard order

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
```
