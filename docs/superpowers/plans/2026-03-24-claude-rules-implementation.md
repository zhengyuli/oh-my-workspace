# Claude Rules Directory Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Create a comprehensive `.claude/rules` directory structure with coding standards, git conventions, security guidelines, and AI constraints for the oh-my-workspace dotfiles repository.

**Architecture:** 4 core rule files at root level + 5 language-specific files in `lang/` subdirectory. Each file references authoritative style guides (Google, Conventional Commits, community standards) and provides practical conventions for dotfiles development.

**Tech Stack:** Markdown, Claude Code rules system

---

## File Structure

```
.claude/rules/
├── coding-style.md           # Universal coding standards
├── git-workflow.md           # Git conventions
├── security.md               # Security best practices
├── ai-generation.md          # AI-specific constraints
└── lang/
    ├── shell.md              # Common shell practices
    ├── bash.md               # Bash-specific extensions
    ├── zsh.md                # Zsh-specific extensions
    ├── elisp.md              # Emacs Lisp conventions
    └── python.md             # Python conventions
```

---

### Task 1: Create Directory Structure

**Files:**
- Create: `.claude/rules/lang/` (directory)

- [ ] **Step 1: Create lang subdirectory**

Run: `mkdir -p .claude/rules/lang`

Expected: Directory created successfully

- [ ] **Step 2: Verify structure**

Run: `ls -la .claude/rules/`

Expected: `lang` directory visible

---

### Task 2: Create coding-style.md

**Files:**
- Create: `.claude/rules/coding-style.md`

- [ ] **Step 1: Write coding-style.md**

```markdown
# Coding Style

Universal coding standards for the oh-my-workspace repository.

## Style Guide References

Follow these authoritative style guides:

- **Shell (Bash/Zsh):** [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html)
- **Python:** [Google Python Style Guide](https://google.github.io/styleguide/pyguide.html)
- **Emacs Lisp:** [Emacs Lisp Style Guide](https://github.com/bbatsov/emacs-lisp-style-guide)

## Line Length

- **Maximum:** 80 characters (all languages)
- **Rationale:** Improves readability in side-by-side diffs and terminal windows

## Universal Rules

### File Headers

Every file should include a header with:
- Purpose/description
- Usage information
- Author and date (optional but recommended)

Example:
```bash
# setup.sh -*- mode: sh; -*-
# =============================================================================
# oh-my-workspace Setup Script
#
# Location: $WORKSPACE_DIR/setup.sh
# Usage:    ./setup.sh help
# =============================================================================
```

### Function Documentation

Document all functions with:
- Purpose description
- Parameters (`@param`)
- Return values (`@return`)
- Exit codes for shell functions

Example:
```bash
# Validate package name exists in PKG_ALL array.
#
# @param $1 - Package name to validate
# @return 0 if valid, 1 if not found
_validate_package() {
  local -r pkg="$1"
  ...
}
```

### Comments

- Explain **why**, not **what**
- Use comments for non-obvious logic
- Keep comments up-to-date with code changes
- Remove commented-out code before committing

### Code Quality

- **No dead code** - Remove unused functions and variables
- **Single responsibility** - Each function does one thing well
- **DRY** - Don't repeat yourself; extract common logic
- **YAGNI** - Don't add features you don't need yet

## Language-Specific Extensions

For language-specific rules, see:
- `lang/shell.md` - Common shell practices
- `lang/bash.md` - Bash-specific features
- `lang/zsh.md` - Zsh-specific features
- `lang/elisp.md` - Emacs Lisp conventions
- `lang/python.md` - Python conventions
```

Run: Write to `.claude/rules/coding-style.md`

- [ ] **Step 2: Verify file created**

Run: `cat .claude/rules/coding-style.md | head -20`

Expected: File content visible

---

### Task 3: Create git-workflow.md

**Files:**
- Create: `.claude/rules/git-workflow.md`

- [ ] **Step 1: Write git-workflow.md**

```markdown
# Git Workflow

Git conventions based on industry best practices.

## Commit Message Format

Follow [Conventional Commits](https://www.conventionalcommits.org/) v1.0.0.

### Format

```
type(scope): description

[optional body]

[optional footer(s)]
```

### Types

| Type | Description |
|------|-------------|
| `feat` | New feature |
| `fix` | Bug fix |
| `docs` | Documentation changes |
| `style` | Code style changes (formatting, semicolons) |
| `refactor` | Code refactoring without feature changes |
| `perf` | Performance improvements |
| `test` | Adding or modifying tests |
| `build` | Build system changes |
| `ci` | CI/CD changes |
| `chore` | Maintenance tasks |
| `revert` | Revert a previous commit |

### Examples

```bash
feat(zsh): add git aliases for status and log
fix(emacs): resolve startup error with missing package
docs: update README with new package structure
refactor(setup): simplify stow conflict resolution
```

### Rules

- **Subject line:** 50 characters max
- **Imperative mood:** "add feature" not "added feature"
- **No period:** Don't end with `.`
- **Body (optional):** Wrap at 72 characters, explain what and why
- **Issues:** Reference with `#issue-number`

## Branch Naming

Follow [GitHub Flow](https://docs.github.com/en/get-started/quickstart/github-flow).

### Format

```
type-description
type/description
```

### Examples

```
add-tmux-config
fix-emacs-startup
update-readme
refactor/stow-logic
```

### Rules

- Use lowercase
- Separate words with hyphens
- Keep it short and descriptive
- Delete branches after merge

## Pull Requests

- **One logical change** per PR
- **Descriptive title** matching commit format
- **Reference issues** in description
- **Small PRs** - easier to review

## Dotfiles-Specific Conventions

- Group related config changes together
- Test stow operations before committing
- Update documentation when adding new packages
- Verify symlinks work after changes
```

Run: Write to `.claude/rules/git-workflow.md`

- [ ] **Step 2: Verify file created**

Run: `cat .claude/rules/git-workflow.md | head -20`

Expected: File content visible

---

### Task 4: Create security.md

**Files:**
- Create: `.claude/rules/security.md`

- [ ] **Step 1: Write security.md**

```markdown
# Security

Security best practices for dotfiles management.

## Secrets Management

### Never Commit Secrets

- API keys
- Access tokens
- Passwords
- Private keys
- Database credentials

### Use Environment Variables

Store secrets in environment variables, not files:

```bash
# Bad - hardcoded secret
API_KEY="sk-1234567890"

# Good - environment variable
API_KEY="${API_KEY:-}"
```

### .gitignore Patterns

Ensure these patterns are in `.gitignore`:

```
.env
*.key
*.pem
*_secret
credentials
config.local
```

## Input Validation

### Validate User Input

Always validate external input:

```bash
# Validate package name
_validate_package() {
  local -r pkg="$1"
  if [[ -z "$pkg" ]]; then
    printf 'error: package name required\n' >&2
    return 1
  fi
}
```

### Sanitize Variables

Quote variables to prevent word splitting and glob expansion:

```bash
# Bad - unquoted
rm -rf $dir

# Good - quoted
rm -rf "$dir"
```

### Avoid eval

Never use `eval` on untrusted input:

```bash
# Dangerous
eval "$user_input"

# Safe alternative
case "$user_input" in
  option1) do_something ;;
  option2) do_other ;;
  *) printf 'invalid option\n' >&2 ;;
esac
```

## File Permissions

### Recommended Permissions

| File Type | Permission | Octal |
|-----------|------------|-------|
| Scripts | rwxr-xr-x | 755 |
| Config files | rw-r--r-- | 644 |
| SSH keys | rw------- | 600 |
| Secrets | rw------- | 600 |

### Check Permissions

```bash
# Verify script is executable
[[ -x "$script" ]] || chmod 755 "$script"

# Verify secret file permissions
[[ $(stat -f %Lp "$secret_file") == "600" ]]
```

## Dotfiles-Specific Security

### Review Before Stowing

Check configs for:
- Hardcoded paths (use `$HOME` or `$XDG_*`)
- Internal network addresses
- Machine-specific settings
- Username references

### Shell History

Be careful with:
- Commands containing secrets
- API calls with tokens
- Database connection strings

### Safe Defaults

```bash
# Don't store secrets in history
setopt HIST_IGNORE_SPACE  # zsh
export HISTCONTROL=ignorespace  # bash
```

## Incident Response

If secrets are accidentally committed:

1. **Rotate immediately** - Generate new credentials
2. **Remove from history** - Use `git filter-branch` or BFG Repo-Cleaner
3. **Force push** - Only if necessary and coordinated with team
4. **Audit** - Check for unauthorized access
```

Run: Write to `.claude/rules/security.md`

- [ ] **Step 2: Verify file created**

Run: `cat .claude/rules/security.md | head -20`

Expected: File content visible

---

### Task 5: Create ai-generation.md

**Files:**
- Create: `.claude/rules/ai-generation.md`

- [ ] **Step 1: Write ai-generation.md**

```markdown
# AI Generation Constraints

Rules for AI-assisted development (Claude-specific).

## Documentation Requirements

### Comments

AI must add comments explaining:
- Complex logic or algorithms
- Non-obvious design decisions
- Business rules and constraints
- Workarounds and their reasons

```bash
# Good - explains WHY
# Use printf instead of echo for portability (POSIX compliance)
printf '%s\n' "$message"

# Bad - explains WHAT
# Print the message
printf '%s\n' "$message"
```

### Function Documentation

Every function must have:
- Purpose description
- Parameter documentation
- Return value/exit code documentation

```bash
# Install packages using GNU Stow.
#
# Creates symlinks in $HOME for each package directory.
# Handles conflicts by removing existing files (with confirmation).
#
# @param $1 - Space-separated list of package names
# @return 0 on success, 1 on failure
_install_packages() {
  ...
}
```

## Code Quality

### Follow Project Rules

AI must follow:
- `coding-style.md` - Formatting and documentation
- `git-workflow.md` - Commit message format
- `security.md` - No hardcoded secrets

### Naming Conventions

Use meaningful names:

```bash
# Bad - cryptic
x=$(pwd)

# Good - descriptive
workspace_dir=$(pwd)
```

### No Dead Code

- Remove unused functions
- Delete commented-out code
- Clean up debugging statements

## Safety Checks

### Secrets

AI must not generate code that:
- Hardcodes API keys or tokens
- Embeds passwords in scripts
- Exposes sensitive configuration

### Input Validation

AI must validate:
- User-provided parameters
- Environment variables
- File paths before operations

```bash
# Check before overwriting
if [[ -f "$target" ]]; then
  printf 'warning: %s exists, skipping\n' "$target"
  return 1
fi
```

### File Operations

Safe file operations:

```bash
# Check directory exists
[[ -d "$dir" ]] || mkdir -p "$dir"

# Verify before symlink
[[ -L "$link" ]] || ln -s "$target" "$link"

# Backup before modify
cp "$file" "${file}.bak"
```

## Dotfiles-Specific Rules

### Stow Operations

Before suggesting changes:
1. Test with `stow -n -v` (dry-run)
2. Verify no conflicts
3. Check symlink paths are correct

### Package Structure

- Respect existing category organization
- Follow `category/package` naming
- Update `PKG_ALL` array in `setup.sh`

### Documentation Updates

When adding packages:
- Update README if it exists
- Add inline comments explaining purpose
- Document any dependencies

## Quality Checklist

Before completing any task, verify:

- [ ] All functions documented
- [ ] Complex logic explained
- [ ] No hardcoded secrets
- [ ] Input validation present
- [ ] Safe file operations
- [ ] Follows project conventions
- [ ] Commit message follows Conventional Commits
```

Run: Write to `.claude/rules/ai-generation.md`

- [ ] **Step 2: Verify file created**

Run: `cat .claude/rules/ai-generation.md | head -20`

Expected: File content visible

---

### Task 6: Create lang/shell.md

**Files:**
- Create: `.claude/rules/lang/shell.md`

- [ ] **Step 1: Write lang/shell.md**

```markdown
# Shell Best Practices

Universal shell conventions shared by Bash and Zsh.

## References

- [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html) - Primary reference

## Error Handling

### Strict Mode

Always use strict mode at the start of scripts:

```bash
set -euo pipefail
```

- `-e` - Exit immediately if a command exits with non-zero status
- `-u` - Treat unset variables as an error
- `-o pipefail` - Return value of a pipeline is the status of the last command to exit with non-zero status

### ERR Trap

Implement error handler for cleanup:

```bash
_err_handler() {
  local -r code=$?
  printf '[error] %s() line %d: exit %d\n' \
    "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
}
trap '_err_handler' ERR
```

### Exit Codes

Use meaningful exit codes:
- `0` - Success
- `1` - General error
- `2` - Misuse of shell command
- `126` - Command not executable
- `127` - Command not found

## Function Design

### Single Responsibility

Each function should do one thing:

```bash
# Good - single purpose
_validate_package() { ... }
_install_package() { ... }
_cleanup_backup() { ... }

# Bad - multiple responsibilities
_install_and_validate_and_cleanup() { ... }
```

### Local Variables

Always use `local` for function variables:

```bash
_process_file() {
  local -r input="$1"
  local output
  output="${input%.txt}.processed"
  ...
}
```

### Parameter Validation

Validate at function start:

```bash
_validate_package() {
  local -r pkg="$1"

  if [[ -z "$pkg" ]]; then
    printf 'error: package name required\n' >&2
    return 1
  fi

  if [[ ! -v PKG_ALL ]]; then
    printf 'error: PKG_ALL array not defined\n' >&2
    return 1
  fi
}
```

## Parameter Handling

### getopts for Flags

Use `getopts` for option parsing:

```bash
while getopts ":hd" opt; do
  case "$opt" in
    h) _show_help; exit 0 ;;
    d) DRY_RUN=true ;;
    \?) printf 'error: invalid option -%s\n' "$OPTARG" >&2; exit 1 ;;
    :) printf 'error: option -%s requires argument\n' "$OPTARG" >&2; exit 1 ;;
  esac
done
shift $((OPTIND - 1))
```

### Help and Version

Always provide:

```bash
_show_help() {
  cat <<'EOF'
Usage: setup.sh [OPTIONS] [COMMAND]

Commands:
  status    Show current stow status
  install   Install packages
  help      Show this help

Options:
  -h, --help     Show help
  -d, --dry-run  Preview changes
  -v, --version  Show version
EOF
}
```

### Default Values

Use sensible defaults:

```bash
# Use environment variable or default
WORKSPACE_DIR="${WORKSPACE_DIR:-$(pwd)}"
NETWORK_TIMEOUT="${NETWORK_TIMEOUT:-60}"
```

## Testing and Quoting

### Prefer [[ ]] over [ ]

```bash
# Good - more robust
if [[ "$var" == "value" ]]; then
  ...
fi

# Less preferred - traditional
if [ "$var" = "value" ]; then
  ...
fi
```

### Arithmetic with (( ))

```bash
# Good - arithmetic context
if (( count > 0 )); then
  ...
fi

# Less preferred
if [[ $count -gt 0 ]]; then
  ...
fi
```

### Always Quote Variables

```bash
# Good - prevents word splitting
rm -rf "$dir"
printf '%s\n' "$message"

# Bad - unsafe
rm -rf $dir
printf '%s\n' $message
```

## Language-Specific Extensions

For Bash-specific features, see `bash.md`.
For Zsh-specific features, see `zsh.md`.
```

Run: Write to `.claude/rules/lang/shell.md`

- [ ] **Step 2: Verify file created**

Run: `cat .claude/rules/lang/shell.md | head -20`

Expected: File content visible

---

### Task 7: Create lang/bash.md

**Files:**
- Create: `.claude/rules/lang/bash.md`

- [ ] **Step 1: Write lang/bash.md**

```markdown
# Bash-Specific Conventions

Bash-specific features extending the common shell practices.

## Base Requirements

This file extends `shell.md`. Read that first.

## Version Requirements

- **Minimum:** Bash 4.3+ (for namerefs and associative arrays)
- **Check version:** `echo "${BASH_VERSION}"`

## Shebang

Always use `#!/usr/bin/env bash`:

```bash
#!/usr/bin/env bash
```

**Rationale:**
- Works across different systems
- Respects user's PATH
- Not hardcoded to `/bin/bash`

## Advanced Features

### Namerefs (Bash 4.3+)

Use `local -n` for pass-by-reference:

```bash
# Pass array by reference
_process_array() {
  local -n arr="$1"  # nameref
  local item

  for item in "${arr[@]}"; do
    printf '%s\n' "$item"
  done
}

declare -a my_array=(one two three)
_process_array my_array
```

### Associative Arrays

Use `declare -A` for key-value lookups:

```bash
declare -A config=(
  [editor]="emacs"
  [shell]="zsh"
  [term]="ghostty"
)

# Access
printf 'Editor: %s\n' "${config[editor]}"

# Iterate
for key in "${!config[@]}"; do
  printf '%s = %s\n' "$key" "${config[$key]}"
done
```

### Readonly Arrays

Use `readonly -a` for constant arrays:

```bash
readonly -a PKG_ALL=(
  shell/zsh
  editor/emacs
  term/ghostty
)
```

## Bash-Specific Syntax

### Parameter Expansion

```bash
# Default value
dir="${path:-/default/path}"

# Error if unset
file="${config:?config is required}"

# Substring
version="${BASH_VERSION:0:3}"

# String replacement
path="${filepath//\//\\}"  # Replace all / with \
```

### Process Substitution

```bash
# Compare outputs
diff <(cmd1) <(cmd2)

# Read into array
mapfile -t lines < <(find . -name '*.sh')
```

### Coproc

For bidirectional communication:

```bash
coproc backend {
  ./server.sh
}

printf 'request\n' >&${backend[1]}
read -r response <&${backend[0]}
```

## Debugging

### Bash-Specific Options

```bash
# Verbose - show commands as read
set -v

# xtrace - show commands after expansion
set -x

# Combined with strict mode
set -euxo pipefail
```

### Debugging Functions

```bash
_debug_caller() {
  printf '[debug] %s called from %s at line %d\n' \
    "${FUNCNAME[1]}" "${BASH_SOURCE[2]}" "${BASH_LINENO[1]}"
}
```

## Compatibility Notes

### Avoid Bash-Only Features for Portable Scripts

If portability is required:
- Avoid `[[ ]]` (use `[ ]`)
- Avoid `(( ))` (use `$(( ))`)
- Avoid `local -n`
- Avoid associative arrays
- Use POSIX shell instead

### Check Bash Version

```bash
if (( BASH_VERSINFO[0] < 4 ||
      ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] < 3 ) )); then
  printf 'error: bash 4.3+ required\n' >&2
  exit 1
fi
```
```

Run: Write to `.claude/rules/lang/bash.md`

- [ ] **Step 2: Verify file created**

Run: `cat .claude/rules/lang/bash.md | head -20`

Expected: File content visible

---

### Task 8: Create lang/zsh.md

**Files:**
- Create: `.claude/rules/lang/zsh.md`

- [ ] **Step 1: Write lang/zsh.md**

```markdown
# Zsh-Specific Conventions

Zsh-specific features extending the common shell practices.

## Base Requirements

This file extends `shell.md`. Read that first.

## Shebang

Always use `#!/usr/bin/env zsh`:

```zsh
#!/usr/bin/env zsh
```

## Key Differences from Bash

### Array Indexing

**Arrays start at 1, not 0:**

```zsh
arr=(one two three)
echo $arr[1]     # "one" (not "two" as in bash)
echo $arr[2]     # "two"
echo $arr[-1]    # "three" (last element)
```

### typeset vs declare

Zsh prefers `typeset`:

```zsh
# Both work, but typeset is more idiomatic in zsh
typeset -a my_array
typeset -A my_hash

# Also valid
declare -a my_array
declare -A my_hash
```

### Globbing Extensions

Zsh has powerful globbing:

```zsh
# Recursive glob
files=(**/*.sh)

# Exclude pattern
files=(^*.test.sh)

# Numeric sort
files=(*(n))

# By modification time
files=*(.om[1,5])   # 5 most recently modified files
```

### Parameter Expansion

Additional features:

```zsh
# Array length
arr=(a b c)
echo ${#arr}        # 3 (number of elements)
echo ${#arr[1]}     # 1 (length of first element)

# Modify on expansion
echo ${arr:u}       # Uppercase
echo ${arr:l}       # Lowercase

# Split/join
str="a:b:c"
arr=(${(s/:/)str})  # Split by :
echo ${(j/-/)arr}   # Join with -
```

## Zsh-Specific Features

### Options

```zsh
# Better globbing
setopt EXTENDED_GLOB

# No match is error (not pass-through)
setopt NOMATCH

# Auto cd (type directory name)
setopt AUTO_CD

# Correct typos
setopt CORRECT

# Share history
setopt SHARE_HISTORY

# Ignore dupes
setopt HIST_IGNORE_ALL_DUPS
```

### Associative Arrays

```zsh
typeset -A config
config=(
  editor emacs
  shell zsh
  term ghostty
)

# Access
echo $config[editor]

# Iterate
for key value in ${(kv)config}; do
  echo "$key = $value"
done
```

### Functions with Explicit Arguments

```zsh
# Named arguments (zsh 5.4+)
func() {
  local -A opts
  zparseopts -A opts -- -help h -verbose v

  (( ${+opts[--help]} )) && { show_help; return }
  (( ${+opts[-v]} )) && verbose=1
}
```

### Hooks

```zsh
# Run before each command
precmd() {
  # Update prompt, etc.
}

# Run after each command
preexec() {
  # Log command, etc.
}

# Chpwd - on directory change
chpwd() {
  ls -la
}
```

## Prompt Customization

Zsh has rich prompt support:

```zsh
# Simple
PROMPT='%n@%m %# '

# With git info (requires vcs_info)
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
PROMPT='%~ %F{green}${vcs_info_msg_0_}%f %# '
```

## Compatibility

### Writing Bash-Compatible Zsh

For scripts that should work in both:

```zsh
# Avoid zsh-specific features
# Use POSIX-compatible syntax
# Test in both shells
```

### Zsh Emulation

```zsh
# Emulate sh/ksh
emulate -L sh
```
```

Run: Write to `.claude/rules/lang/zsh.md`

- [ ] **Step 2: Verify file created**

Run: `cat .claude/rules/lang/zsh.md | head -20`

Expected: File content visible

---

### Task 9: Create lang/elisp.md

**Files:**
- Create: `.claude/rules/lang/elisp.md`

- [ ] **Step 1: Write lang/elisp.md**

```markdown
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
```

Run: Write to `.claude/rules/lang/elisp.md`

- [ ] **Step 2: Verify file created**

Run: `cat .claude/rules/lang/elisp.md | head -20`

Expected: File content visible

---

### Task 10: Create lang/python.md

**Files:**
- Create: `.claude/rules/lang/python.md`

- [ ] **Step 1: Write lang/python.md**

```markdown
# Python Conventions

Python coding standards for oh-my-workspace.

## References

- [Google Python Style Guide](https://google.github.io/styleguide/pyguide.html) - Primary reference
- [PEP 8](https://peps.python.org/pep-0008/) - Base standard

## Code Style

### Line Length

- **Maximum:** 80 characters
- **Docstrings:** 72 characters for content

### Indentation

- **Spaces:** 4 spaces (no tabs)
- **Continuation:** Align with opening delimiter or 4-space indent

```python
# Good - align with opening delimiter
result = some_function(
    arg1, arg2,
    arg3, arg4)

# Good - 4-space indent
def long_function_name(
        var_one, var_two, var_three,
        var_four):
    ...
```

### Naming

| Type | Convention | Example |
|------|------------|---------|
| Module | snake_case | `dotfiles_manager.py` |
| Class | PascalCase | `DotfilesManager` |
| Function | snake_case | `install_package` |
| Variable | snake_case | `package_name` |
| Constant | UPPER_SNAKE_CASE | `DEFAULT_TIMEOUT` |
| Private | _leading_underscore | `_internal_func` |

### Imports

Order: stdlib → third-party → local

```python
# Standard library
import os
import sys
from pathlib import Path

# Third-party
import click
from rich.console import Console

# Local
from .utils import symlink
from .config import Config
```

## Type Hints

### Always Use Type Hints

```python
from typing import Optional, List

def install_packages(
    packages: List[str],
    dry_run: bool = False,
) -> bool:
    """Install packages using stow."""
    ...

def get_config(key: str) -> Optional[str]:
    """Get config value or None if not found."""
    ...
```

### Modern Syntax (Python 3.9+)

```python
# Good - modern
def process(items: list[str]) -> dict[str, int]:
    ...

# Avoid - old style
from typing import List, Dict
def process(items: List[str]) -> Dict[str, int]:
    ...
```

### Union Types (Python 3.10+)

```python
# Good - modern
def get_value(key: str) -> str | None:
    ...

# Avoid - old style
from typing import Optional
def get_value(key: str) -> Optional[str]:
    ...
```

## Documentation

### Docstrings

Use Google style docstrings:

```python
def install_package(name: str, force: bool = False) -> bool:
    """Install a dotfiles package using GNU Stow.

    Creates symlinks in the user's home directory for the
    specified package. Handles conflicts according to the
    force parameter.

    Args:
        name: Package name (e.g., "shell/zsh").
        force: If True, overwrite existing files without prompting.

    Returns:
        True if installation succeeded, False otherwise.

    Raises:
        ValueError: If package name is invalid.
        FileNotFoundError: If package directory doesn't exist.

    Example:
        >>> install_package("shell/zsh")
        True
    """
    ...
```

### Module Docstrings

```python
"""Package management for oh-my-workspace dotfiles.

This module provides utilities for installing, updating, and
managing dotfiles packages using GNU Stow.

Example usage:
    from dotfiles import install_packages
    install_packages(["shell/zsh", "editor/emacs"])
"""
```

## Error Handling

### Specific Exceptions

```python
# Good - specific exception
try:
    config = load_config(path)
except FileNotFoundError:
    print(f"Config not found: {path}")
    return None
except json.JSONDecodeError as e:
    print(f"Invalid JSON in config: {e}")
    return None

# Bad - bare except
try:
    config = load_config(path)
except:
    print("Something went wrong")
    return None
```

### Context Managers

```python
# Good - automatic cleanup
with open(config_file) as f:
    config = json.load(f)

# Also good - custom context manager
from contextlib import contextmanager

@contextmanager
def stow_context(package: str):
    """Context manager for stow operations."""
    prepare_stow(package)
    try:
        yield
    finally:
        cleanup_stow(package)
```

### Raise Meaningful Exceptions

```python
def validate_package(name: str) -> None:
    """Validate package name.

    Raises:
        ValueError: If name is empty or contains invalid characters.
    """
    if not name:
        raise ValueError("Package name cannot be empty")
    if not re.match(r'^[a-z0-9/-]+$', name):
        raise ValueError(f"Invalid package name: {name}")
```

## Dependencies

### Use uv

This project uses `uv` for package management:

```bash
# Add dependency
uv add requests

# Add dev dependency
uv add --dev pytest

# Install from requirements
uv sync
```

### pyproject.toml

Declare dependencies in `pyproject.toml`:

```toml
[project]
name = "oh-my-workspace"
version = "1.0.0"
requires-python = ">=3.10"
dependencies = [
    "click>=8.0",
    "rich>=13.0",
]

[project.optional-dependencies]
dev = [
    "pytest>=7.0",
    "black>=23.0",
    "ruff>=0.1.0",
]
```

## Testing

### pytest

Use pytest for tests:

```python
# tests/test_install.py
import pytest
from dotfiles import install_package

def test_install_valid_package():
    """Test installing a valid package."""
    result = install_package("shell/zsh")
    assert result is True

def test_install_invalid_package():
    """Test installing an invalid package raises error."""
    with pytest.raises(ValueError):
        install_package("invalid/package")

@pytest.fixture
def temp_home(tmp_path):
    """Create a temporary home directory."""
    home = tmp_path / "home"
    home.mkdir()
    return home
```

### Run Tests

```bash
# Run all tests
uv run pytest

# Run specific test
uv run pytest tests/test_install.py::test_install_valid_package

# Run with coverage
uv run pytest --cov=dotfiles
```
```

Run: Write to `.claude/rules/lang/python.md`

- [ ] **Step 2: Verify file created**

Run: `cat .claude/rules/lang/python.md | head -20`

Expected: File content visible

---

### Task 11: Verify Complete Structure

**Files:**
- Verify: `.claude/rules/` structure

- [ ] **Step 1: Verify all files created**

Run: `find .claude/rules -type f | sort`

Expected output:
```
.claude/rules/ai-generation.md
.claude/rules/coding-style.md
.claude/rules/git-workflow.md
.claude/rules/lang/bash.md
.claude/rules/lang/elisp.md
.claude/rules/lang/python.md
.claude/rules/lang/shell.md
.claude/rules/lang/zsh.md
.claude/rules/security.md
```

- [ ] **Step 2: Verify file count**

Run: `find .claude/rules -type f | wc -l`

Expected: `9`

- [ ] **Step 3: Commit all changes**

```bash
git add .claude/rules/
git commit -m "feat(rules): add comprehensive .claude/rules directory structure

- Add core rules: coding-style, git-workflow, security, ai-generation
- Add language-specific rules: shell, bash, zsh, elisp, python
- Reference authoritative style guides (Google, Conventional Commits)
- Include dotfiles-specific conventions"
```

Expected: Commit created successfully
