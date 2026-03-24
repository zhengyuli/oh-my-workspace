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