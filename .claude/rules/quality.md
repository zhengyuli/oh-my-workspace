---
version: "1.0.0"
last-updated: "2026-03-26"
maintainer: "zhengyu.li"
---
# Quality Standards

Quality standards for AI-assisted development code generation.

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

## Configuration Code Standards

### File Headers (MANDATORY)

Every configuration file MUST include a standard header with location and usage:

**Emacs Lisp header format:**
```elisp
;;; filename.el --- Brief description -*- lexical-binding: t; -*-

;; Location: ~/.config/emacs/lisp/filename.el
;; Usage:    (require 'filename)
```

**Shell script header format:**
```bash
#!/usr/bin/env bash
# =============================================================================
# Script Name - Brief description
#
# Location: ~/.local/bin/script-name
# Usage:    script-name [options] [args]
# =============================================================================
```

AI must ensure all generated configuration files include these headers.

### Configuration Comments

Configuration comments must follow these standards:

1. **Explain WHY, not WHAT**
   ```bash
   # Good - explains WHY
   # Use printf for POSIX compliance (portable across shells)
   printf '%s\n' "$message"

   # Bad - explains WHAT
   # Print message
   printf '%s\n' "$message"
   ```

2. **Document dependencies**
   ```elisp
   ;; Requires: dash.el, s.el
   ;; These provide functional utilities for list processing
   ```

3. **Explain non-obvious choices with references**
   ```bash
   # Using -print0 and -0 for safe handling of filenames with spaces
   # Reference: https://mywiki.wooledge.org/BashFAQ/020
   find . -type f -print0 | xargs -0 rm
   ```

### Configuration Code Style

Follow these principles when writing configuration code:

1. **Modular over monolithic**
   - Split large configs into logical modules
   - Use `source` (shell) or `require` (Emacs) to compose
   - Each module should have a single responsibility

2. **Explicit over implicit**
   - Declare all variables at the top
   - Use descriptive names even if longer
   - Avoid magic numbers - use named constants

3. **Version control friendly**
   - Keep files under per-type max lines (see `coding-style.md`)
   - Minimize generated content in tracked files
   - Use comments to separate logical sections

4. **Immutable patterns**
   - Create new versions instead of modifying in-place
   - Use symlinks to switch between config versions
   - Always backup before modification: `cp file file.bak`

## Quality Checklist

Before completing any task, verify the universal checklist in
`coding-style.md`. Additional quality-specific checks:

### Quality Extra Checks

- [ ] Standard file header present (location and usage) — see header
  format above
- [ ] Comments explain WHY, not WHAT — no "print the message" comments
- [ ] Dependencies documented in file header
- [ ] Non-obvious choices include references/URLs
- [ ] No hardcoded machine-specific paths (use `$HOME`, `$XDG_*`)
- [ ] Backup created before any in-place modification (`cp file file.bak`)