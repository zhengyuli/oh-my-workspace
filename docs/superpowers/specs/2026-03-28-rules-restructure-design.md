# .claude/rules Restructure Design

Comprehensive rewrite of project-level rules for the oh-my-workspace
dotfiles repository.

## Background

### Current Problems

1. **Broken conditional loading**: Lang rules use `globs:` as
   frontmatter key. Official Claude Code uses `paths:`. This may cause
   all 1,206 lines of lang rules to load unconditionally on every
   session.
2. **Context bloat**: 1,354 total lines across 8 files. When lang rules
   load unconditionally, a typical session editing a `.toml` file still
   loads elisp, bash, and zsh rules.
3. **Duplication**: `coding-style.md` and `patterns.md` overlap with
   lang rules on formatting, comments, and code quality principles.
4. **Generic content**: Rules include DRY/KISS/YAGNI explanations that
   Claude already knows, consuming context without adding value.
5. **Nested directory**: `lang/` subdirectory adds hierarchy without
   benefit — Claude Code discovers all `.md` files recursively
   regardless.

### Design Decisions

- **Approach**: Full rewrite (Approach C) with mandatory rules preserved
- **Scope**: Project-level `.claude/rules/` only; global rules untouched
- **Target**: Minimize unconditional context while preserving all
  repo-specific constraints

## Target Structure

```
.claude/rules/
├── standards.md       # ~40 lines, unconditional
├── bash.md            # ~150 lines, paths-scoped
├── zsh.md             # ~160 lines, paths-scoped
├── elisp.md           # ~180 lines, paths-scoped
├── config.md          # ~80 lines, paths-scoped
├── toml.md            # ~75 lines, paths-scoped
└── yaml.md            # ~80 lines, paths-scoped
```

**Removed**: `lang/` directory, `coding-style.md`, `patterns.md`

## File Specifications

### standards.md (~40 lines, unconditional)

Baseline rules applying to ALL files in the repo. No path-specific
frontmatter — loads every session.

**Content:**

```markdown
# Standards

Universal baseline for the oh-my-workspace repository.

## Formatting

- 2-space indentation (never tabs)
- Never align values with spaces
- Prefer separate-line comments over inline

## File Organization

- Many small files > few large files; organize by feature/domain
- Every file must include a standard header:
  - File location path
  - Usage / invocation instructions
  - Dependencies
  - Non-obvious design choices with reference URLs

## Comments

- Explain WHY, not WHAT
- Keep comments up-to-date with code changes
- Remove commented-out code before committing

## Principles

- Immutability: create new configurations, never modify in-place
- XDG paths: use `$HOME` / `$XDG_*` variables, never hardcode
- Safe defaults: provide fallback values for optional variables
- DRY / KISS / YAGNI / Single Responsibility
```

**Design rationale:**

- Principles listed as keywords only (no explanation) — Claude knows
  these concepts; the list serves as a reminder to prevent drift
- Immutability and XDG paths included because they are repo-specific
  conventions not standard in general programming
- Line length NOT included here — each lang rule specifies its own

### bash.md (~150 lines, paths-scoped)

**Frontmatter:**

```yaml
---
paths:
  - "**/*.sh"
  - "**/*.bash"
---
```

**Content sections (preserving mandatory rules):**

1. File Header — shebang `#!/usr/bin/env bash`, header template
2. Delimiter Hierarchy — Level 0 (79 `=` chars), Level 1 (79 `-`)
3. Line Length — 79 characters maximum
4. Error Handling — `set -euo pipefail` (MANDATORY for scripts),
   ERR trap, exit codes
5. Code Patterns — variable handling, conditionals `[[ ]]` / `(( ))`,
   output via `printf`, naming conventions (UPPER_SNAKE constants,
   lower_snake locals)
6. Functions — single responsibility, parameter validation, main()
   wrapper, local command substitution (declare/assign on separate
   lines)
7. Anti-Patterns — eval for user input, local masking exit codes,
   short-circuit operators
8. Security — code injection prevention, permission management,
   secrets management
9. References — Google Shell Style Guide, Bash Manual
10. Validation — `bash -n`, `shellcheck`

**Changes from current:**

- `globs:` → `paths:` frontmatter
- Remove duplicate formatting rules (now in standards.md)
- Remove duplicate comment rules (now in standards.md)
- Remove generic code quality principles
- Add: line length 79 (migrated from standards.md)
- Add: safe defaults `${VAR:-default}` (migrated from standards.md)
- Add: validation at system boundaries (migrated from patterns.md)
- Add: nesting limit 3 levels (migrated from patterns.md)
- Add: no magic numbers (migrated from patterns.md)
- Keep: ALL mandatory rules (strict mode, short-circuit ban, etc.)

### zsh.md (~160 lines, paths-scoped)

**Frontmatter:**

```yaml
---
paths:
  - "**/*.zsh"
  - "**/.zshrc"
  - "**/.zprofile"
  - "**/.zshenv"
  - "**/.zlogin"
  - "**/zshrc"
  - "**/zprofile"
  - "**/zshenv"
---
```

**Content**: Same structure as bash.md with zsh-specific differences:

- Shebang: `#!/usr/bin/env zsh`
- `print` built-in instead of `printf`
- Zsh-specific ERR trap using `${funcstack[2]}` and
  `${funcfiletrace[1]}`
- Note: interactive `.zshrc` files must NOT use `set -e`
- Same mandatory rules as bash (strict mode, short-circuit ban, etc.)
- Same migrated rules (safe defaults, validation, nesting, magic
  numbers)
- Line length: 79 characters

### elisp.md (~180 lines, paths-scoped)

**Frontmatter:**

```yaml
---
paths:
  - "**/*.el"
---
```

**Content sections:**

1. File Header — `lexical-binding: t`, copyright, Commentary section
2. Delimiter Hierarchy — Level 0 (79 `;=` chars), Level 1 (79 `;-`)
3. Line Length — 79 characters maximum
4. Error Handling — `ignore-errors` (side effects only),
   `condition-case` (specific failures), `unwind-protect` (cleanup)
5. Code Patterns — docstrings (mandatory for public functions),
   naming (kebab-case, `omw-` prefix, `-p` predicates),
   buffer-local vars (`defvar-local`), key bindings (`kbd` macro)
6. use-package Declaration — mandatory keyword order (13 keywords),
   lazy loading via `:defer t`
7. Anti-Patterns — mutate shared lists, raw key strings, inline
   explanations
8. Functions — interactive declaration, parameter validation,
   optional dependency guard
9. Security — GNU ELPA/MELPA Stable only, no `.elc` commits
10. References — Emacs Lisp Manual, use-package docs
11. Validation — `emacs --batch -f batch-byte-compile`

**Changes from current:**

- `globs:` → `paths:` frontmatter
- Remove duplicate formatting/comment rules
- Add: line length 79 (migrated from standards.md)
- Add: validation at function boundaries (migrated from patterns.md)
- Add: nesting limit 3 levels (migrated from patterns.md)
- Add: no magic numbers (migrated from patterns.md)
- Keep: ALL mandatory rules (lexical-binding, use-package order,
  defvar-local, kbd macro, etc.)

### config.md (~80 lines, paths-scoped)

**Frontmatter:**

```yaml
---
paths:
  - "**/config"
  - "**/*.conf"
  - "**/*.cfg"
  - "**/rc"
  - "**/.gitconfig"
  - "**/git/config"
---
```

**Content sections:**

1. File Header
2. Delimiter Hierarchy
3. Line Length — 79 characters
4. Code Patterns — comments (why not what), value types, formatting,
   includes for machine-specific overrides
5. Anti-Patterns — inline explanations, aligned values
6. Security — split-file strategy for secrets
7. References — Git config docs, XDG spec
8. Validation — `git config --list`

**Changes from current:**

- `globs:` → `paths:` frontmatter
- Remove duplicate formatting rules
- Add: line length 79 (migrated from standards.md)

### toml.md (~75 lines, paths-scoped)

**Frontmatter:**

```yaml
---
paths:
  - "**/*.toml"
---
```

**Content sections:**

1. File Header
2. Delimiter Hierarchy
3. Line Length — 79 characters
4. Code Patterns — comments, value types (strings always quoted,
   booleans unquoted), formatting
5. Anti-Patterns — unquoted strings, inline explanations
6. Security — split-file strategy
7. References — TOML spec
8. Validation — `python3 -c "import toml; toml.load(...)"`

**Changes from current:**

- `globs:` → `paths:` frontmatter
- Remove duplicate formatting rules
- Add: line length 79 (migrated from standards.md)

### yaml.md (~80 lines, paths-scoped)

**Frontmatter:**

```yaml
---
paths:
  - "**/*.yml"
  - "**/*.yaml"
---
```

**Content sections:**

1. File Header
2. Delimiter Hierarchy
3. Line Length — 79 characters
4. Code Patterns — comments, value types, formatting
5. Anti-Patterns — unquoted special characters, aligned values
6. Security — split-file strategy
7. References — YAML spec, yamllint
8. Validation — `python3 -c "import yaml; yaml.safe_load(...)"`

**Changes from current:**

- `globs:` → `paths:` frontmatter
- Remove duplicate formatting rules
- Add: line length 79 (migrated from standards.md)

## Context Usage Comparison

| Scenario | Before (lines) | After (lines) | Savings |
|----------|---------------|---------------|---------|
| Unconditional | 1,354* or 148 | ~40 | 73-97% |
| Edit .zsh file | 1,354 or 419 | ~200 | 52-85% |
| Edit .el file | 1,354 or 443 | ~220 | 50-84% |
| Edit .toml file | 1,354 or 270 | ~115 | 57-92% |

*If `globs:` frontmatter is ignored, all 1,206 lang lines load
unconditionally.

## Migration Plan

### Phase 1: Create new files

1. Write `standards.md`
2. Write each lang rule with `paths:` frontmatter

### Phase 2: Remove old files

1. Delete `coding-style.md`
2. Delete `patterns.md`
3. Delete `lang/` directory (all 6 files)

### Phase 3: Update references

1. Update `CLAUDE.md` if it references old rule structure
2. Verify no other files reference `lang/` paths

### Validation

- Confirm `paths:` frontmatter works: create a test file and verify
  only matching rules load
- Run `bash -n` on any modified shell scripts
- Confirm line counts match targets

## Out of Scope

- Global `~/.claude/rules/` — user has already handled this
- `CLAUDE.md` content changes (only reference updates if needed)
- `.claude/settings.json` or hooks configuration
