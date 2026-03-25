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
- **KISS** - Keep it simple, avoid over-engineering
- **Composition over inheritance** - Prefer composing behavior

## Immutability Principle

### Prefer Immutability

Immutable data is easier to reason about and test.

#### Shell Variables

```bash
# Good - readonly prevents accidental modification
local -r config_path="$HOME/.config"
readonly -a REQUIRED_PACKAGES=(zsh git emacs)

# Avoid - mutable without reason
local config_path="$HOME/.config"
```

#### Python Variables

```python
# Good - use tuples for immutable sequences
REQUIRED_PACKAGES: tuple[str, ...] = ("zsh", "git", "emacs")

# Avoid - lists can be modified
required_packages: list[str] = ["zsh", "git", "emacs"]
```

### When to Use Immutability

**Use immutable structures for:**
- Configuration values
- Constants
- Function arguments that shouldn't change
- Default values

**Use mutable structures for:**
- Accumulators
- State that must change
- Performance-critical hot paths (with justification)

## File Organization

### File Size Targets

Keep files focused and manageable:

| File Type | Target Lines | Max Lines |
|-----------|-------------|-----------|
| Shell scripts | 100-300 | 500 |
| Python modules | 150-400 | 600 |
| Emacs Lisp | 100-300 | 500 |

### When to Split Files

**Split a file when:**
- Exceeds maximum line count
- Contains multiple unrelated concerns
- Has deeply nested conditionals (>3 levels)
- Functions have high cognitive complexity

**Splitting strategy:**
1. Identify cohesive groups of functions
2. Extract to new file with descriptive name
3. Maintain single responsibility per file
4. Update imports/requires in dependent files

### File Structure Template

```bash
# 1. Header (purpose, usage, author)
# 2. Constants and configuration
# 3. Utility functions (private)
# 4. Core functions (public)
# 5. Main entry point (if executable)
```

## Language-Specific Extensions

The following rule files are **conditionally loaded** (path-scoped via
`globs`) and injected into context only when working on matching files.
They extend — never replace — this file.

| Rule file | Loaded when working on |
|-----------|------------------------|
| `lang/shell.md` | `**/*.sh`, `**/*.bash`, `shell/**` |
| `lang/bash.md` | `**/*.sh`, `**/*.bash` |
| `lang/zsh.md` | `**/*.zsh`, `zsh/**` |
| `lang/elisp.md` | `**/*.el`, `emacs/**` |
| `lang/python.md` | `**/*.py` |

## Code Quality Checklist

Before completing any task, verify:

- [ ] All functions documented
- [ ] Complex logic explained
- [ ] No hardcoded secrets
- [ ] Input validation present
- [ ] Safe file operations
- [ ] Follows project conventions
- [ ] Commit message follows Conventional Commits
- [ ] Immutable structures used where appropriate
- [ ] File size within target limits