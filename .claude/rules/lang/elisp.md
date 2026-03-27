---
globs: ["**/*.el"]
version: "1.0.0"
last-updated: "2026-03-26"
maintainer: "zhengyu.li"
---
# Emacs Lisp Conventions

Coding standards for Emacs Lisp in oh-my-workspace.

## References

- [Emacs Lisp Style Guide](https://github.com/bbatsov/emacs-lisp-style-guide) - Community standard
- [Emacs Lisp Manual](https://www.gnu.org/software/emacs/manual/elisp.html) - Official documentation

## Naming Conventions

### Use kebab-case

```elisp
;; Good
(defun omw-buffer-empty-p ()
  ...)

;; Bad
(defun omwBufferEmptyP ()
  ...)
```

### Package Prefix

Prefix all names with package namespace:

```elisp
;; For oh-my-workspace, use omw- prefix
(defvar omw-config-directory nil)
(defun omw-setup-environment () ...)
(defmacro omw-with-temp-buffer (&rest body) ...)
```

### Predicates

End predicate functions with `-p`:

```elisp
(defun omw-buffer-empty-p ()
  "Return t if buffer is empty."
  (zerop (buffer-size)))

(defun omw-file-exists-p (filepath)
  "Return t if FILEPATH exists."
  (file-exists-p filepath))
```

## File Structure

### Modular init.el

Structure `init.el` as a thin loader — all configuration lives in
feature modules:

```elisp
;;; init.el -*- lexical-binding: t; -*-
;; Main entry point — load feature modules

;; Core modules (always required)
(require 'omw-base)
(require 'omw-packages)
(require 'omw-keybindings)

;; Feature modules
(require 'omw-shell)
(require 'omw-git)
(require 'omw-project)

;; Local overrides (not in git)
(require 'omw-local nil t)
```

Each module file must:
- Be < 400 lines
- Have single responsibility (e.g., `omw-git.el` only configures git)
- Be documented with a commentary section
- Handle missing dependencies gracefully

### Commentary Section

Start with commentary:

```elisp
;;; omw-shell.el --- Shell configuration for oh-my-workspace -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zhengyu.li

;; Author: zhengyu.li
;; URL: https://github.com/zhengyu-li/oh-my-workspace

;;; Commentary:

;; This module provides shell integration for oh-my-workspace.
;; It configures shell-mode, term-mode, and related utilities.

;;; Code:
```

### Autoloads

Use `;;;###autoload` for public functions:

```elisp
;;;###autoload
(defun omw-open-terminal ()
  "Open terminal in current directory."
  (interactive)
  ...)
```

### Provide at End

```elisp
(provide 'omw-shell)
;;; omw-shell.el ends here
```

## Documentation

### Docstrings

Every public function needs a docstring:

```elisp
(defun omw-setup-aliases (aliases)
  "Set up shell ALIASES in the current buffer.
ALIASES should be an alist of (NAME . COMMAND) pairs.

Example:
  (omw-setup-aliases '((\"ll\" . \"ls -la\")
                       (\"gs\" . \"git status\")))"
  ...)
```

### First Line

First line should be a complete sentence:

```elisp
;; Good
"Return the absolute path to the workspace directory."

;; Bad
"Returns absolute path"  ;; Not a sentence
"Get workspace dir"       ;; Too terse
```

### Parameters

Document parameters in docstring:

```elisp
(defun omw-find-config (name &optional directory)
  "Find config file with NAME in DIRECTORY or default location.
NAME should be a string without extension.
DIRECTORY defaults to `omw-config-directory'.

Returns the full path to the config file, or nil if not found."
  ...)
```

## Key Conventions

### Use kbd Macro

```elisp
;; Good - readable
(define-key shell-mode-map (kbd "C-c C-s") 'omw-shell-sync)

;; Bad - hard to read
(define-key shell-mode-map "\C-c\C-s" 'omw-shell-sync)
```

### Follow Emacs Conventions

Common prefixes:
- `C-c` - User bindings (C-c C-<letter> for major modes)
- `C-x` - Global bindings
- `C-h` - Help

## Package Management

### use-package

Use `use-package` for dependencies:

```elisp
(use-package shell
  :ensure nil  ; Built-in package
  :config
  (setq explicit-shell-file-name "/usr/bin/env zsh")
  :bind
  (:map shell-mode-map
        ("C-c C-s" . omw-shell-sync)))
```

### Declare Dependencies

```elisp
;; Package-Requires header
;; Package-Requires: ((emacs "27.1") (dash "2.19") (s "1.12"))
```

## Best Practices

### Immutability

Prefer creating new lists over mutating existing ones:

```elisp
;; WRONG: Mutates the existing list in-place
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; CORRECT: Creates a new list — safe, side-effect-free
(setq auto-mode-alist
      (cons '("\\.py\\'" . python-mode) auto-mode-alist))
```

Use `defconst` for values that must never change:

```elisp
(defconst omw-version "1.0.0"
  "Current version of oh-my-workspace.")
```

### Lexical Binding

Always use lexical binding:

```elisp
;;; filename.el -*- lexical-binding: t; -*-
```

### Avoid Global Variables

Prefer `defvar-local` for buffer-local:

```elisp
(defvar-local omw-shell-buffer-name nil
  "Name of the current shell buffer.")
```

### Use cl-lib

For modern Common Lisp features:

```elisp
(require 'cl-lib)

(cl-defun omw-process-output (&key input timeout)
  "Process INPUT with TIMEOUT."
  ...)
```

### Error Handling

```elisp
(defun omw-safe-load (file)
  "Safely load FILE, returning t on success."
  (condition-case err
      (progn
        (load file)
        t)
    (error
     (message "Failed to load %s: %s" file err)
     nil)))
```

### Byte Compilation

Always ensure modules byte-compile cleanly:

```bash
# Compile a single file
emacs --batch -f batch-byte-compile omw-shell.el

# Compile all files in a directory
emacs --batch --eval \
  "(byte-recompile-directory \"~/.config/emacs/lisp\" 0)"
```

A clean compile produces no warnings. Treat warnings as errors in CI.

### Stale .elc Files — Critical Pitfall

**Emacs always prefers `foo.elc` over `foo.el`** when both exist in the
same directory. Editing `foo.el` without removing the stale `foo.elc`
means Emacs silently loads the *old* compiled version — the most common
cause of "my change has no effect" confusion.

#### Rules

1. **Never commit `.elc` files.** They are in `.gitignore`. If
   `git status` shows any `.elc`, run the cleanup below before
   staging anything.
2. **Clean stale `.elc` before committing any `emacs/` change.** The
   pre-commit hook (see `hooks.md`) blocks commits that stage `.elc`
   files.
3. **Clean the stow target too.** `.gitignore` only covers the repo;
   stale `.elc` files living in `~/.config/emacs/` are equally
   dangerous and are not tracked by git.

#### Cleanup Commands

```bash
# Remove all .elc from the repo working tree
find emacs/ -name '*.elc' -delete

# Remove all .elc from the stow target (live Emacs config)
find ~/.config/emacs/ -name '*.elc' -delete

# Verify none remain in either location
find emacs/ ~/.config/emacs/ -name '*.elc'
```

After cleaning, restart Emacs (or `M-x load-file`) to confirm the
`.el` source is being loaded.

### Lazy Loading with use-package

Defer loading until the feature is actually needed:

```elisp
;; Defer loading until first use of the command
(use-package magit
  :defer t
  :bind ("C-c g" . magit-status))

;; Load only when opening a matching file
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode))

;; Load after another package is loaded
(use-package company-jedi
  :after (company python))
```

Prefer `:defer t` for packages not needed at startup to reduce
Emacs startup time.

### Testing with ERT

Use the built-in Emacs Lisp Regression Testing framework:

```elisp
;;; omw-shell-test.el --- Tests for omw-shell -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'omw-shell)

(ert-deftest omw-shell-buffer-empty-p-test ()
  "Test omw-buffer-empty-p returns t for empty buffer."
  (with-temp-buffer
    (should (omw-buffer-empty-p))))

(ert-deftest omw-safe-load-missing-file-test ()
  "Test omw-safe-load returns nil for missing file."
  (should-not (omw-safe-load "/nonexistent/path.el")))

(provide 'omw-shell-test)
;;; omw-shell-test.el ends here
```

Run tests from the command line:

```bash
# Run all tests matching a pattern
emacs --batch -l omw-shell.el -l omw-shell-test.el \
  -f ert-run-tests-batch-and-exit
```
## Enhanced File Headers (Based on Project Standards)

All Emacs Lisp files MUST include comprehensive headers:

```elisp
;;; filename.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>

;; Author: Your Name <email@example.com>
;; Keywords: keyword1, keyword2
;; Dependencies: (none) or list of packages

;; Copyright (C) 2026 Your Name

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; History:
;;
;; 2026-03-27 00:00 Your Name <email@example.com> created.

;;; Commentary:
;;
;; Brief description of what this module does.
;; Explain the purpose and how it fits into the larger configuration.

;;; Code:

;; ============================================================================
;; Section title
;; ============================================================================

;;; filename.el ends here
```

### Header Components

**Mode Line**: `;;; filename.el -*- lexical-binding: t; -*-`
- Triple semicolon for file-level
- Always use `lexical-binding: t`

**Timestamp**: `Time-stamp: <...>`
- Format: `<YYYY-MM-DD HH:MM:SS Day by username>`
- Auto-updated by Emacs `time-stamp` package

**Metadata**: Author, Keywords, Dependencies
- Author: Name and email
- Keywords: Relevant categories for finder-by-keyword
- Dependencies: Required packages (or "none")

**License**: MIT License (project standard)
- Full license text in header
- Adjust copyright holder as needed

**History**: Change log
- Date, author, brief description
- Most recent first

**Commentary**: Module description
- Purpose and context
- How it integrates with other modules
- Usage examples if complex

**Code Section**: `;;; Code:`
- Delimiter between documentation and implementation
- Use `;;` for section headers (double semicolon)

**Footer**: `;;; filename.el ends here`
- Marks end of file
- Triple semicolon

## Section Delimiters

Organize code into logical sections with clear delimiters:

```elisp
;; ============================================================================
;; Section Title
;; ============================================================================

(defun section-function ()
  "Documentation."
  ;; Implementation
  )
```

### Delimiter Format

- **Top line**: `;; ` + `=` * 76 (79 total with `;;`)
- **Title**: `;; Section Title`
- **Bottom line**: Same as top line

### Common Sections

Organize by functionality, not alphabetically:

1. **Dependencies** - `require` statements
2. **Customization** - `defgroup` and `defcustom`
3. **Faces** - `defface` definitions
4. **Variables** - `defvar`, `defconst`
5. **Utility Functions** - Helper functions
6. **Main Functions** - Core functionality
7. **Keybindings** - `define-key` calls
8. **Hooks** - `add-hook` calls
9. **Mode Definition** - `define-derived-mode`

### Example Structure

```elisp
;;; omw-git.el --- Git integration -*- lexical-binding: t; -*-

;; [Header details...]

;;; Code:

;; ============================================================================
;; Dependencies
;; ============================================================================

(require 'magit)
(require 'diff-hl)

;; ============================================================================
;; Customization
;; ============================================================================

(defgroup omw-git nil
  "Git integration settings."
  :group 'tools
  :prefix "omw-git-")

(defcustom omw-git-auto-revert t
  "Automatically revert buffers when git status changes."
  :type 'boolean
  :group 'omw-git)

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defun omw-git--current-branch ()
  "Return current git branch name."
  (magit-get-current-branch))

;; ============================================================================
;; Main Functions
;; ============================================================================

(defun omw-git-status ()
  "Show git status for current repository."
  (interactive)
  (magit-status-setup-buffer))

;; ============================================================================
;; Keybindings
;; ============================================================================

(global-set-key (kbd "C-c g s") 'omw-git-status)

;; ============================================================================
;; Hooks
;; ============================================================================

(add-hook 'after-save-hook 'omw-git-auto-commit-changed-files)

;;; omw-git.el ends here
```

## Module Integration

### Feature Requirements

Declare dependencies clearly:

```elisp
;; At top of file
(require 'magit)

;; In Commentary section
;;; Commentary:
;;
;; Git integration for oh-my-workspace.
;; Requires: magit, diff-hl
```

### Provide Feature

Always provide the feature at end:

```elisp
(provide 'omw-git)
;;; omw-git.el ends here
```

### Lazy Loading

Use `with-eval-after-load` for deferred loading:

```elisp
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-c C-f") 'omw-git-fetch-all))
```

## Use-Package Patterns

### Standard Pattern

```elisp
(use-package feature-name
  :ensure t                    ; Install if missing
  :defer t                     ; Lazy load
  :init                        ; Run before load
  (setq variable t)
  :config                      ; Run after load
  (define-key map (kbd "k") 'fn)
  :bind                        ; Keybindings
  ("C-c f" . feature-command))
```

### Grouping Related Packages

```elisp
;; ============================================================================
;; Git Integration
;; ============================================================================

(use-package magit
  :ensure t
  :defer t
  :bind ("C-c g s" . magit-status))

(use-package diff-hl
  :ensure t
  :defer t
  :hook (prog-mode . diff-hl-mode))
```

## Byte Compilation

### Compile Commands

Always ensure files byte-compile cleanly:

```bash
# Single file
emacs --batch -f batch-byte-compile omw-module.el

# Directory
emacs --batch --eval \
  "(byte-recompile-directory \"~/.config/emacs/lisp\" 0)"
```

### Compilation Warnings

Treat warnings as errors:
- Resolve all byte-compile warnings
- Check for unused variables
- Verify function definitions
- Ensure proper requires

### Clean Compilation

```bash
# Remove all .elc files
find ~/.config/emacs/ -name '*.elc' -delete

# Recompile everything
emacs --batch --eval \
  "(byte-recompile-directory \"~/.config/emacs/lisp\" 0)"
```

## Testing Integration

### ERT Test Files

Create corresponding test files:

```elisp
;;; omw-module-test.el --- Tests for omw-module -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'omw-module)

(ert-deftest omw-module-function-test ()
  "Test omw-module-function behavior."
  (should (eq (omw-module-function input) expected)))

(provide 'omw-module-test)
;;; omw-module-test.el ends here
```

### Run Tests

```bash
emacs --batch -l omw-module.el -l omw-module-test.el \
  -f ert-run-tests-batch-and-exit
```

## Delimiter Hierarchy (MANDATORY)

All Emacs Lisp files MUST use a three-level delimiter system:

### Level 0: File Header Delimiter

**Format**: `;; ` + `=` * 76 (79 total characters)
**Purpose**: Mark file-level metadata area
**Character**: `=` (equals sign - strongest visual emphasis)

```elisp
;;; filename.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>

;; Author: Your Name <email@example.com>
;; Keywords: keyword1, keyword2

;; Copyright (C) 2026 Your Name

;; [License text...]

;;; History:
;;
;; 2026-03-27 00:00 Your Name created.

;;; Commentary:
;;
;; Module description.

;;; Code:

;; ============================================================================
;; Section Title
;; ============================================================================

;;; filename.el ends here
```

**Position**: 
- Not used as a wrapper (file header uses `;;;` semicolons)
- Section delimiters start after `;;; Code:`

### Level 1: Primary Section Delimiter

**Format**: `;; ` + `=` * 76 (79 total characters)
**Purpose**: Mark major functional categories
**Character**: `=` (equals sign - strong visual emphasis)

```elisp
;; ============================================================================
;; Primary Category
;; ============================================================================
```

**Characteristics**:
- Appears in pairs (top and bottom boundary)
- Single-level category (no hyphen in title)
- Uses double semicolon (`;;`) for section level

### Level 2: Subsection Delimiter

**Format**: `;; ` + `-` * 76 (79 total characters) + " - " in title
**Purpose**: Mark subcategories within primary categories
**Character**: `-` (hyphen - medium visual emphasis)
**Distinction**: Title uses " - " (space-hyphen-space) to indicate hierarchy

```elisp
;; ----------------------------------------------------------------------------
;; Primary Category - Subcategory
;; ----------------------------------------------------------------------------
```

**Characteristics**:
- Appears in pairs (top and bottom boundary)
- Title contains " - " to show parent-child relationship
- Uses different delimiter character (`-`) to distinguish from Level 1

### Hierarchy Example

```
File Header (Level 0 - uses ;;; semicolons)
├── Dependencies (Level 1)
├── Customization (Level 1)
│   ├── Customization - Variables (Level 2)
│   └── Customization - Faces (Level 2)
├── Functions (Level 1)
│   ├── Functions - Utility (Level 2)
│   └── Functions - Main (Level 2)
├── Keybindings (Level 1)
│   ├── Keybindings - Global (Level 2)
│   └── Keybindings - Mode-Specific (Level 2)
└── Hooks (Level 1)
```

### Complete Example

```elisp
;;; omw-git.el --- Git integration -*- lexical-binding: t; -*-
;; Time-stamp: <2026-03-27 00:00:00 Thursday by zhengyu.li>

;; Author: zhengyu li <lizhengyu419@outlook.com>
;; Keywords: git, tools

;; Copyright (C) 2026 zhengyu li

;; [MIT License text...]

;;; History:
;;
;; 2026-03-27 00:00 zhengyu li created.

;;; Commentary:
;;
;; Git integration for oh-my-workspace.

;;; Code:

;; ============================================================================
;; Dependencies
;; ============================================================================

(require 'magit)
(require 'diff-hl)

;; ============================================================================
;; Customization
;; ============================================================================

(defgroup omw-git nil
  "Git integration settings."
  :group 'tools)

;; ----------------------------------------------------------------------------
;; Customization - Variables
;; ----------------------------------------------------------------------------

(defcustom omw-git-auto-revert t
  "Automatically revert buffers when git status changes."
  :type 'boolean
  :group 'omw-git)

;; ----------------------------------------------------------------------------
;; Customization - Faces
;; ----------------------------------------------------------------------------

(defface omw-git-highlight-face
  '((t :inherit highlight))
  "Face for git highlights."
  :group 'omw-git)

;; ============================================================================
;; Functions
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Functions - Utility
;; ----------------------------------------------------------------------------

(defun omw-git--current-branch ()
  "Return current git branch name."
  (magit-get-current-branch))

;; ----------------------------------------------------------------------------
;; Functions - Main
;; ----------------------------------------------------------------------------

(defun omw-git-status ()
  "Show git status for current repository."
  (interactive)
  (magit-status-setup-buffer))

;; ============================================================================
;; Keybindings
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Keybindings - Global
;; ----------------------------------------------------------------------------

(global-set-key (kbd "C-c g s") 'omw-git-status)

;; ----------------------------------------------------------------------------
;; Keybindings - Mode-Specific
;; ----------------------------------------------------------------------------

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-c C-f") 'omw-git-fetch-all))

;; ============================================================================
;; Hooks
;; ============================================================================

(add-hook 'after-save-hook 'omw-git-auto-commit-changed-files)

(provide 'omw-git)
;;; omw-git.el ends here
```

### Delimiter Specifications

**Width Calculation**:
- Total width: 79 characters (80-char line - 1 newline)
- Format: `;; ` (3 chars) + delimiter character * 76

**Character Selection**:
- **Level 1**: `=` (equals) - strong emphasis for major sections
- **Level 2**: `-` (hyphen) - medium emphasis for subsections

**Title Format**:
- **Level 1**: `Primary Category` (single phrase)
- **Level 2**: `Primary Category - Subcategory` (connected with " - ")

**Semicolon Convention**:
- `;;;` (triple) - File-level (headers, provide, ends here)
- `;;` (double) - Section level (delimiters, major sections)
- `;` (single) - Line level (inline comments)

### Comment Grouping (Optional)

Within a section, use simple comments to group related items:

```elisp
;; ============================================================================
;; Customization
;; ============================================================================

;; User-configurable variables
(defcustom omw-git-auto-revert t
  "Automatically revert buffers."
  :type 'boolean
  :group 'omw-git)

;; Visual settings
(defface omw-git-highlight-face
  '((t :inherit highlight))
  "Face for highlights."
  :group 'omw-git)
```

### Use-Package Integration

Group related package configurations:

```elisp
;; ============================================================================
;; Git Integration
;; ============================================================================

;; Core git interface
(use-package magit
  :ensure t
  :defer t
  :bind ("C-c g s" . magit-status))

;; Diff highlighting
(use-package diff-hl
  :ensure t
  :defer t
  :hook (prog-mode . diff-hl-mode))
```

### Module Organization Pattern

For complex modules, use consistent section ordering:

1. **Dependencies** - `require` statements
2. **Customization** - `defgroup`, `defcustom`, `defface`
3. **Variables** - `defvar`, `defconst`
4. **Functions** - Utility first, then main
5. **Keybindings** - Global first, then mode-specific
6. **Hooks** - Mode setup and teardown
7. **Mode Definition** - `define-derived-mode` (if applicable)
