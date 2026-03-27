---
version: "1.0.0"
last-updated: "2026-03-26"
maintainer: "zhengyu.li"
---
# Coding Style

Universal coding standards for the oh-my-workspace repository.

## Style Guide References

Authoritative style guides are listed in each language rule file.
Quick reference:

- **Shell/Bash/Zsh:** `lang/bash.md` / `lang/zsh.md`
- **Python:** `lang/python.md`
- **Emacs Lisp:** `lang/elisp.md`

## Line Length

- **Maximum:** 80 characters (all languages)
- **Rationale:** Improves readability in side-by-side diffs and terminal windows

## Universal Rules

### File Headers (MANDATORY)

Every configuration file MUST include a standard header with location and
usage. AI must ensure all generated files include these headers.

Required header elements:
- File location path
- Usage / invocation instructions
- Dependencies (packages, tools, modules this file relies on)
- Non-obvious design choices — include a reference URL where helpful

For language-specific header formats, see:
- Bash — `lang/bash.md`, Zsh — `lang/zsh.md`
- Emacs Lisp — `lang/elisp.md`
- Python — `lang/python.md`

### Function Documentation

Every function MUST have:
- Purpose description
- Parameters
- Return values / exit codes

For language-specific documentation patterns, see the appropriate `lang/` rule.

### Comments

- Explain **why**, not **what**
- Use comments for non-obvious logic
- Keep comments up-to-date with code changes
- Remove commented-out code before committing

AI must add comments explaining:
- Complex logic or algorithms
- Non-obvious design decisions
- Business rules and constraints
- Workarounds and their reasons

For language-specific comment examples, see the appropriate `lang/` rule.

### Code Quality

- **No dead code** - Remove unused functions and variables
- **Single responsibility** - Each function does one thing well
- **DRY** - Don't repeat yourself; extract common logic
- **YAGNI** - Don't add features you don't need yet
- **KISS** - Keep it simple, avoid over-engineering
- **Composition over inheritance** - Prefer composing behavior

### Naming Conventions

Use meaningful, descriptive names. Avoid single-letter or cryptic
abbreviations. For language-specific naming conventions, see the
appropriate `lang/` rule.

## Immutability Principle

### Prefer Immutability

Immutable data is easier to reason about and test. Use read-only or
constant constructs wherever your language supports them:

- Shell: `local -r`, `readonly -a` — see `lang/bash.md` / `lang/zsh.md`
- Python: `tuple` instead of `list` for constants — see `lang/python.md`
- Emacs Lisp: `defconst`, prefer `cons` over `add-to-list` — see
  `lang/elisp.md`

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

| File Type      | Target Lines | Max Lines |
|----------------|--------------|-----------|
| Shell scripts  | 100-300      | 500       |
| Python modules | 150-400      | 600       |
| Emacs Lisp     | 100-300      | 500       |

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

### File Structure

Each file type has an idiomatic internal ordering. For concrete
templates, see the appropriate `lang/` rule.

## Language-Specific Extensions

The following language-specific rule files are **conditionally loaded**
via `globs` frontmatter. They extend — never replace — this file.

| Rule file        | Globs                                                                       |
|------------------|-----------------------------------------------------------------------------|
| `lang/bash.md`   | `**/*.sh`, `**/*.bash`                                                      |
| `lang/zsh.md`    | `**/*.zsh`, `**/.zsh*`, `**/zshrc`, `**/zprofile`, `**/zshenv`, `**/zlogin` |
| `lang/elisp.md`  | `**/*.el`                                                                   |
| `lang/python.md` | `**/*.py`                                                                   |

## Code Quality Checklist

Before completing any task, verify:

- [ ] Standard file header present (location and usage)
- [ ] All functions documented with purpose, params, return
- [ ] Complex logic explained (comments say WHY, not WHAT)
- [ ] Dependencies documented in file header
- [ ] Non-obvious choices include references/URLs
- [ ] No hardcoded secrets or machine-specific paths (use `$HOME`, `$XDG_*`)
- [ ] Input validation present
- [ ] Safe file operations (check before overwrite; backup before modify)
- [ ] Follows project conventions
- [ ] Commit message follows Conventional Commits
- [ ] Run `./setup.sh clean` to remove stale compiled files before commit
- [ ] Immutable structures used where appropriate
- [ ] File size within target limits
