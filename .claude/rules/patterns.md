---
version: "1.0.0"
last-updated: "2026-03-27"
maintainer: "zhengyu.li"
---
# Common Patterns

Design patterns and principles for oh-my-workspace configuration code.

## Immutability Principle

**ALWAYS create new configurations, NEVER modify existing ones in-place.**

Rationale:
- Enables easy rollback if changes break
- Prevents unintended side effects
- Makes debugging easier
- Maintains history of changes

For language-specific immutability examples, see:
- Shell: in-place edits vs backup-and-replace — `lang/shell.md`
- Emacs Lisp: `cons` over `add-to-list` — `lang/elisp.md`
- Python: `tuple` over `list` for constants — `lang/python.md`

## File Organization

**MANY SMALL FILES > FEW LARGE FILES:**

- **Organization:** By feature/domain, not by type
- **Cohesion:** High within files, low between files
- **Size targets by file type:** See `coding-style.md` for authoritative limits

**When to split configuration files:**

- Split when a file exceeds its max line count
- Split when it contains multiple unrelated concerns
- Split when functions have high cognitive complexity

For language-specific file layout examples, see:
- Emacs modular config structure — `lang/elisp.md`
- Zsh conf.d split layout — `lang/zsh.md`
- Shell script ordering — `lang/shell.md`

## Safe Defaults

Always provide fallback values so configs work even when optional
environment variables are not set. Avoid hard failures caused by
unset variables.

For concrete examples, see `lang/shell.md`.

## Design Patterns

### Validation Pattern

Always validate configs and inputs before use:

1. Check the resource exists and is readable
2. Validate syntax
3. Only proceed on success; emit a clear error and return on failure

For a concrete implementation example, see `lang/shell.md`.

### Repository Pattern for Configs

Encapsulate config access behind a consistent interface with standard
operations: load, save, validate, backup. This enables easy swapping
of config locations, simplifies testing, and centralises error handling.

## Anti-Patterns to Avoid

### Deep Nesting

Limit nesting to 3 levels maximum. Use early-return guards to flatten
conditional structures. For a code example, see `lang/shell.md`.

### Magic Numbers

Never use bare numeric literals — define named constants instead.
This makes intent clear and centralises values that may need updating.
For a code example, see `lang/shell.md`.

### Hardcoded Paths

Never hardcode absolute paths to home directories or system locations.
Use `$HOME` / `$XDG_*` variables with sensible defaults.
For a code example, see `lang/shell.md`.

### Large Monolithic Configs

Don't put everything in one file. Split by feature/domain into focused
files that each have a single responsibility.
For a code example, see `lang/shell.md`.
