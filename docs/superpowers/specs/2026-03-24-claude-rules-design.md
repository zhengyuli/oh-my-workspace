# Claude Rules Directory Structure Design

**Date:** 2026-03-24
**Status:** Draft
**Author:** zhengyu.li

## Overview

Design a comprehensive `.claude/rules` directory structure for the oh-my-workspace dotfiles repository. This structure will provide coding standards, conventions, and best practices for AI-assisted development.

## Goals

1. Establish clear coding standards for shell scripts (bash/zsh), Emacs Lisp, and Python
2. Define git workflow conventions following industry best practices
3. Provide security guidelines for dotfiles management
4. Set AI generation constraints for Claude-assisted development
5. Reference authoritative style guides rather than inventing custom standards

## Directory Structure

```
.claude/rules/
├── coding-style.md           # Universal coding standards
├── git-workflow.md           # Git commit/branch/PR rules
├── security.md               # Security best practices
├── ai-generation.md          # AI-specific constraints
└── lang/
    ├── shell.md              # Common shell practices (bash + zsh)
    ├── bash.md               # Bash-specific extensions
    ├── zsh.md                # Zsh-specific extensions
    ├── elisp.md              # Emacs Lisp conventions
    └── python.md             # Python conventions
```

**Total: 9 files** (4 core + 5 language-specific)

## File Contents

### Core Rules

#### 1. coding-style.md

**Purpose:** Universal coding standards referencing authoritative style guides

**Content:**
- Style guide references:
  - Shell: Google Shell Style Guide
  - Python: Google Python Style Guide
  - Emacs Lisp: Emacs Lisp Style Guide (community standard)
- Line length: 80 characters (all languages)
- Universal rules:
  - File header: purpose, usage, author, date
  - Function documentation: purpose, parameters, return values
  - Comments: explain "why" not "what"
  - No dead code or commented-out code
  - Single responsibility principle
- Language-specific overrides: see `lang/*.md` files

#### 2. git-workflow.md

**Purpose:** Git conventions based on industry standards

**Content:**
- Commit message format: Conventional Commits v1.0.0
  - Format: `type(scope): description`
  - Types: feat, fix, docs, style, refactor, perf, test, build, ci, chore, revert
  - Examples: `feat(zsh): add git aliases`, `fix(emacs): resolve startup error`
- Branch naming: GitHub Flow
  - Format: `type-description` or `type/description`
  - Examples: `add-tmux-config`, `fix-emacs-startup`
- Best practices:
  - Atomic commits (one logical change per commit)
  - Reference issues in commits
  - Write commits as if explaining to a colleague

#### 3. security.md

**Purpose:** Security best practices for dotfiles

**Content:**
- Secrets management:
  - Never commit secrets (API keys, tokens, passwords)
  - Use environment variables for sensitive data
  - Use `.gitignore` patterns for secret files
  - Examples: `.env`, `*.key`, `*_secret`
- Input validation:
  - Validate user input in scripts
  - Sanitize variables used in commands
  - Avoid eval on untrusted input
- File permissions:
  - Scripts should not be world-writable
  - SSH keys: 600 permissions
  - Config files with secrets: 600 permissions
- Dotfiles-specific:
  - Review configs before stowing
  - Don't expose internal network configurations
  - Be careful with shell history settings

#### 4. ai-generation.md

**Purpose:** Constraints for AI-assisted development (Claude-specific)

**Content:**
- Documentation requirements:
  - AI must add comments explaining complex logic
  - AI must document functions (purpose, parameters, returns)
  - AI must explain non-obvious design decisions
- Code quality:
  - AI must follow coding-style.md rules
  - AI must follow git-workflow.md commit format
  - AI must not generate dead code or commented-out code
  - AI must use meaningful variable/function names
- Safety checks:
  - AI must not generate code that exposes secrets
  - AI must validate user input in generated scripts
  - AI must use safe file operations (check before overwrite)
- Dotfiles-specific:
  - AI must test stow operations before suggesting changes
  - AI must respect existing package structure
  - AI must update documentation when adding packages

### Language-Specific Rules

#### 5. lang/shell.md

**Purpose:** Universal shell conventions shared by bash and zsh

**Content:**
- References: Google Shell Style Guide (base standard)
- Error handling:
  - Use `set -euo pipefail` for strict mode
  - Implement ERR trap for cleanup
  - Check exit codes explicitly when needed
  - Provide meaningful error messages
- Function design:
  - Single responsibility per function
  - Use `local` for all variables
  - Validate parameters at function start
  - Return meaningful exit codes (0=success, 1=failure)
- Parameter handling:
  - Use `getopts` for flag parsing
  - Provide `--help` and `--version` options
  - Validate required parameters
  - Use sensible defaults
- Testing & quoting:
  - Prefer `[[ ]]` over `[ ]` for tests
  - Use `(( ))` for arithmetic
  - Quote all variables: `"$var"`

#### 6. lang/bash.md

**Purpose:** Bash-specific features extending shell.md

**Content:**
- Version requirement: Bash 4.3+ (for namerefs, associative arrays)
- Shebang: `#!/usr/bin/env bash`
- Advanced features:
  - `local -n` for namerefs
  - Associative arrays: `declare -A`
  - `readonly -a` for constant arrays
- Bash-specific traps: note any bash-specific signal handling

#### 7. lang/zsh.md

**Purpose:** Zsh-specific features extending shell.md

**Content:**
- Shebang: `#!/usr/bin/env zsh`
- Zsh-specific features:
  - Array indexing starts at 1
  - `typeset` instead of `declare`
  - Globbing extensions
  - Zsh-specific parameter expansion

#### 8. lang/elisp.md

**Purpose:** Emacs Lisp conventions

**Content:**
- References:
  - Emacs Lisp Style Guide (community standard)
  - Emacs Lisp Manual
- Naming conventions:
  - Use `kebab-case` for function/variable names
  - Prefix with package namespace (e.g., `omw-`)
  - Predicates end with `-p` (e.g., `omw-buffer-empty-p`)
- File structure:
  - Commentary section at top
  - `;;;###autoload` for autoloads
  - Provide at end: `(provide 'package-name)`
- Documentation:
  - Every function should have a docstring
  - First line: complete sentence describing purpose
  - Document parameters and return values
- Key conventions:
  - Use `define-key` with `kbd` macro
  - Follow Emacs keybinding conventions
- Package dependencies:
  - Use `use-package` for package management

#### 9. lang/python.md

**Purpose:** Python conventions extending Google Python Style Guide

**Content:**
- References:
  - Google Python Style Guide (primary)
  - PEP 8 (base standard)
- Code style:
  - Line length: 80 characters
  - Indentation: 4 spaces (no tabs)
  - Naming: `snake_case` for functions/variables, `PascalCase` for classes
  - Imports: stdlib → third-party → local (alphabetical within groups)
- Type hints:
  - Use type hints for all function signatures
  - Prefer `list[str]` over `List[str]` (Python 3.9+)
  - Use `|` for unions (Python 3.10+)
- Documentation:
  - Docstrings for all public modules/functions/classes
  - Use triple double quotes: `"""`
  - Follow Google docstring format
- Error handling:
  - Prefer specific exceptions over bare `except:`
  - Use context managers (`with` statements)
- Dependencies:
  - Use `uv` for package management
  - Declare dependencies in `pyproject.toml`

## Design Decisions

### 1. Reference Authoritative Guides

**Decision:** Use Google Style Guides, Conventional Commits, and community standards rather than inventing custom rules.

**Rationale:**
- Industry-proven standards reduce cognitive load
- Better tooling support (linters, formatters)
- Easier for contributors familiar with these standards

### 2. Separate Common Shell from Language-Specific

**Decision:** Create `shell.md` for common practices, `bash.md` and `zsh.md` for language-specific features.

**Rationale:**
- Avoids duplication of shared concepts (error handling, functions)
- Makes it clear which rules apply to all shells vs specific ones
- Easier to maintain and update

### 3. Include AI Generation Constraints

**Decision:** Create `ai-generation.md` specifically for Claude-assisted development.

**Rationale:**
- This repo is actively developed with Claude Code assistance
- Ensures consistent quality of AI-generated code
- Prevents common AI mistakes (missing comments, hardcoded paths)

### 4. Skip Testing and Performance Rules

**Decision:** Omit testing.md and performance.md from the structure.

**Rationale:**
- Dotfiles repos typically don't have extensive test suites
- Performance is rarely critical for configuration scripts
- Keeps the rules focused and practical

### 5. Merge Naming into Coding Style

**Decision:** Include naming conventions in `coding-style.md` rather than a separate file.

**Rationale:**
- Naming is tightly coupled with coding style
- Reduces file count and navigation overhead
- Follows Google Style Guide organization

## Implementation Plan

1. Create `.claude/rules/` directory structure
2. Write core rule files (coding-style.md, git-workflow.md, security.md, ai-generation.md)
3. Create `lang/` subdirectory
4. Write language-specific rule files (shell.md, bash.md, zsh.md, elisp.md, python.md)
5. Commit with message: `feat(rules): add .claude/rules directory structure`

## Success Criteria

- All 9 files created with complete content
- Each file references appropriate style guides
- Rules are practical and relevant to dotfiles development
- Structure is clear and maintainable
- Claude Code can use these rules to assist development effectively
