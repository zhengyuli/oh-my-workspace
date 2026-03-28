# .claude/rules Restructure Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restructure `.claude/rules/` to fix conditional loading, reduce context bloat, and preserve all mandatory rules.

**Architecture:** Replace 8 files (2 universal + 6 lang in nested `lang/`) with 7 flat files (1 unconditional `standards.md` + 6 paths-scoped lang rules). Each lang rule preserves ALL mandatory/CRITICAL rules while removing generic content Claude already knows.

**Tech Stack:** Claude Code rules system, `.md` files, `paths:` frontmatter (replacing `globs:`)

**Spec document:** `docs/superpowers/specs/2026-03-28-rules-restructure-design.md`

---

## File Map

**Files created:**
- `.claude/rules/standards.md` (~40 lines, unconditional)
- `.claude/rules/bash.md` (~150 lines, paths-scoped)
- `.claude/rules/zsh.md` (~160 lines, paths-scoped)
- `.claude/rules/elisp.md` (~180 lines, paths-scoped)
- `.claude/rules/config.md` (~80 lines, paths-scoped)
- `.claude/rules/toml.md` (~75 lines, paths-scoped)
- `.claude/rules/yaml.md` (~80 lines, paths-scoped)

**Files deleted:**
- `.claude/rules/coding-style.md`
- `.claude/rules/patterns.md`
- `.claude/rules/lang/` (entire directory: bash.md, zsh.md, elisp.md, config.md, toml.md, yaml.md)

**Files modified:**
- `CLAUDE.md` (Coding Conventions section references, ~lines 146-160)

**Parallelism:** Tasks 1-7 are independent of each other. Task 8 depends on Tasks 1-7. Task 9 depends on Task 8. Task 10 depends on Task 9.

---

## Task 1: Create `standards.md`

**Files:**
- Create: `.claude/rules/standards.md`

- [ ] **Step 1: Write `standards.md`**

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
- No dead code or commented-out code
- DRY / KISS / YAGNI / Single Responsibility
```

- [ ] **Step 2: Verify file**

Run: `wc -l .claude/rules/standards.md`
Expected: ~40 lines (acceptable: 35-45)

- [ ] **Step 3: Commit**

```bash
git add .claude/rules/standards.md
git commit -m "feat(rules): add standards.md as universal baseline"
```

---

## Task 2: Create `bash.md`

**Files:**
- Create: `.claude/rules/bash.md`

**Source:** `.claude/rules/lang/bash.md` (current, 264 lines)

**Instructions:** Rewrite the current file with these changes:
- Change frontmatter from `globs:` to `paths:`
- Add "Line Length — 79 characters maximum" section after Delimiter Hierarchy
- Remove "Comments" subsection (explain WHY not WHAT — now in standards.md)
- Remove "Formatting" subsection rules duplicated by standards.md (2-space, align values)
- Keep "Split long pipelines" formatting rule (bash-specific)
- Remove generic code quality principles
- Add "Validation at Boundaries" subsection under Code Patterns
- Add "Nesting Limit" subsection (max 3 levels, early-return guards)
- Add "No Magic Numbers" subsection (named constants only)
- Preserve ALL mandatory rules: strict mode, ERR trap, exit codes, all
  anti-patterns (eval, local masks, short-circuit), all security rules,
  all function patterns, all references, all validation commands

- [ ] **Step 1: Write `bash.md`**

```markdown
---
paths:
  - "**/*.sh"
  - "**/*.bash"
---

# Bash Conventions

Bash-specific features and universal shell practices.

# =============================================================================
## File Header
# =============================================================================

```bash
#!/usr/bin/env bash
# script.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Script Title - Brief Description
#
# Location: $WORKSPACE_DIR/path/to/script.sh
# Usage: ./script.sh [options]
# Dependencies: bash 4.3+
# =============================================================================
```

**Shebang**: `#!/usr/bin/env bash` (use `env` for portability)

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...` (79 chars)
**Level 1** (Primary Section): `# -----------...` (79 chars)
**Level 2** (Subsection): `# --- Title ---`

## Line Length — 79 characters maximum.

# -----------------------------------------------------------------------------
## Error Handling
# -----------------------------------------------------------------------------

### Strict Mode (MANDATORY for scripts)

```bash
set -euo pipefail
```

- `-e` - Exit on non-zero status
- `-u` - Treat unset variables as error
- `-o pipefail` - Pipeline fails on first error

**Note**: For interactive `.bashrc` files, **do not** use `set -e`.

### ERR Trap

```bash
_err_handler() {
  local -r code=$?
  printf '[error] %s() line %d: exit %d\n' \
    "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
}
trap '_err_handler' ERR
```

### Exit Codes

`0` success, `1` error, `2` misuse, `126` not executable, `127` not found

# -----------------------------------------------------------------------------
## Code Patterns
# -----------------------------------------------------------------------------

### Variable Handling

Quote all variables, use `local` scope in functions.

```bash
_process_file() {
  local -r input="$1"
  rm -rf "$dir"
}
```

### Conditionals

Use `[[ ]]` for strings and `(( ))` for arithmetic.

```bash
if [[ "$var" == "value" ]]; then  # String
if (( count > 0 )); then          # Arithmetic
```

### Default Values

Use `${VAR:-default}` pattern.

```bash
WORKSPACE_DIR="${WORKSPACE_DIR:-$(pwd)}"
```

### Validation at Boundaries

Validate all inputs at system boundaries (user input, file paths,
external data).

```bash
_validate_package() {
  local -r pkg="$1"
  if [[ -z "$pkg" ]]; then
    printf 'error: package required\n' >&2
    return 1
  fi
}
```

### Nesting Limit

Maximum 3 levels of nesting. Use early-return guards to flatten.

```bash
# WRONG — 4 levels deep
if [[ -n "$var" ]]; then
  if [[ "$type" == "file" ]]; then
    if [[ -f "$var" ]]; then
      process "$var"
    fi
  fi
fi

# CORRECT — early return
if [[ -z "$var" ]]; then return 1; fi
if [[ "$type" != "file" ]]; then return 1; fi
if [[ ! -f "$var" ]]; then return 1; fi
process "$var"
```

### No Magic Numbers

Never use bare numeric literals — define named constants.

```bash
readonly MAX_RETRIES=3
readonly TIMEOUT=30
```

### Output Standards

Use `printf` over `echo` (POSIX-compliant). Direct errors to stderr.

```bash
printf 'error: %s not found\n' "$pkg" >&2
```

### Naming Conventions

Constants: `UPPER_SNAKE_CASE`, Local: `lower_snake_case`

```bash
readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
local temp_file
```

### Formatting

- Split long pipelines at `|` with each stage on its own line

# -----------------------------------------------------------------------------
## Functions
# -----------------------------------------------------------------------------

### Parameter Validation

Validate required parameters at the start.

```bash
_validate_package() {
  local -r pkg="$1"
  if [[ -z "$pkg" ]]; then
    printf 'error: package required\n' >&2
    return 1
  fi
}
```

### Main Function

For scripts longer than ~20 lines, wrap all logic in `main()`.

```bash
main() {
  local -r pkg="$1"
  _validate_package "$pkg"
  _install_package "$pkg"
}

main "$@"
```

### Local Command Substitution

Declare and assign on separate lines when RHS is command substitution.

```bash
# CORRECT — exit code preserved
local dir
dir="$(dirname "$file")"
```

# -----------------------------------------------------------------------------
## Anti-Patterns
# -----------------------------------------------------------------------------

### Don't: eval for User Input

```bash
# WRONG
eval "$user_input"

# CORRECT — explicit dispatch
case "$user_input" in
  install)   _install ;;
  uninstall) _uninstall ;;
  *)         printf 'error: invalid command: %s\n' "$user_input" >&2; exit 1 ;;
esac
```

### Don't: local Masks Exit Codes

```bash
# WRONG — local always exits 0
local dir="$(dirname "$file")"

# CORRECT — exit code preserved
local dir
dir="$(dirname "$file")"
```

### Don't: Short-Circuit Operators

```bash
# WRONG — unclear intent
[[ -f "$file" ]] && cat "$file"
[[ ! -d "$dir" ]] || mkdir -p "$dir"

# CORRECT — explicit conditionals
if [[ -f "$file" ]]; then
  cat "$file"
fi

if [[ ! -d "$dir" ]]; then
  mkdir -p "$dir"
fi
```

# -----------------------------------------------------------------------------
## Security
# -----------------------------------------------------------------------------

### Code Injection Prevention

Never use `eval` to execute user input. Use explicit `case` dispatch.

### Permission Management

| File Type    | Mode | Rationale                        |
|--------------|------|----------------------------------|
| Scripts      | 755  | Executable by owner/group/other  |
| Config files | 644  | Readable by all, writable by owner |
| SSH keys     | 600  | Accessible only by owner         |
| Secret files | 600  | Accessible only by owner         |

### Secrets Management

Read secrets from environment variables with safe defaults.

```bash
API_KEY="${API_KEY:-}"
```

# -----------------------------------------------------------------------------
## References
# -----------------------------------------------------------------------------

1. [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html)
2. [Bash Manual](https://www.gnu.org/software/bash/manual/)

# -----------------------------------------------------------------------------
## Validation
# -----------------------------------------------------------------------------

```bash
bash -n script.sh      # Syntax check
shellcheck script.sh   # Lint (if installed)
```
```

- [ ] **Step 2: Verify file**

Run: `wc -l .claude/rules/bash.md`
Expected: ~150 lines (acceptable: 140-160)

- [ ] **Step 3: Commit**

```bash
git add .claude/rules/bash.md
git commit -m "feat(rules): rewrite bash.md with paths frontmatter"
```

---

## Task 3: Create `zsh.md`

**Files:**
- Create: `.claude/rules/zsh.md`

**Source:** `.claude/rules/lang/zsh.md` (current, 271 lines)

**Instructions:** Mirror bash.md structure exactly with these zsh-specific substitutions:
- Frontmatter `paths:` with all zsh globs (including `.zlogin`)
- Shebang: `#!/usr/bin/env zsh`
- `print` built-in instead of `printf` throughout
- ERR trap uses `${funcstack[2]}` and `${funcfiletrace[1]}`
- Note: interactive `.zshrc` must NOT use `set -e`
- Same mandatory, migrated, and anti-pattern rules as bash.md
- Line length: 79 characters

- [ ] **Step 1: Write `zsh.md`**

(Implementation mirrors bash.md with zsh-specific substitutions listed above.)

- [ ] **Step 2: Verify file**

Run: `wc -l .claude/rules/zsh.md`
Expected: ~160 lines (acceptable: 150-170)

- [ ] **Step 3: Commit**

```bash
git add .claude/rules/zsh.md
git commit -m "feat(rules): rewrite zsh.md with paths frontmatter"
```

---

## Task 4: Create `elisp.md`

**Files:**
- Create: `.claude/rules/elisp.md`

**Source:** `.claude/rules/lang/elisp.md` (current, 295 lines)

**Instructions:** Rewrite the current file with these changes:
- Change frontmatter from `globs:` to `paths:`
- Add "Line Length — 79 characters maximum" section after Delimiter Hierarchy
- Remove "Comments" subsection (now in standards.md)
- Remove generic code quality principles
- Add "Validation at Boundaries" subsection
- Add "Nesting Limit" subsection (max 3 levels)
- Add "No Magic Numbers" subsection
- KEEP: lexical-binding `t;` MANDATORY
- KEEP: use-package keyword order (13 keywords)
- KEEP: defvar-local, kbd macro, kebab-case, omw- prefix, -p predicates
- KEEP: ALL error handling (ignore-errors, condition-case, unwind-protect)
- KEEP: ALL anti-patterns (immutable lists, raw keys, inline comments)
- KEEP: ALL function patterns (interactive, parameter validation, dependency guard)
- KEEP: ALL security (ELPA/MELPA only, no .elc)
- KEEP: file header, delimiter hierarchy, semicolon convention, docstrings
- KEEP: validation commands, references

- [ ] **Step 1: Write `elisp.md`**

(Implementation preserves all sections from current file. Add line length,
migrated patterns, remove generic principles and duplicate comment rules.)

- [ ] **Step 2: Verify file**

Run: `wc -l .claude/rules/elisp.md`
Expected: ~180 lines (acceptable: 170-190)

- [ ] **Step 3: Commit**

```bash
git add .claude/rules/elisp.md
git commit -m "feat(rules): rewrite elisp.md with paths frontmatter"
```

---

## Task 5: Create `config.md`

**Files:**
- Create: `.claude/rules/config.md`

**Source:** `.claude/rules/lang/config.md` (current, 127 lines)

**Instructions:** Rewrite the current file with these changes:
- Change frontmatter from `globs:` to `paths:`
- Add "Line Length — 79 characters" section
- Remove duplicate formatting rules (now in standards.md)
- KEEP: file header, delimiter hierarchy, all code patterns, all
  anti-patterns, security section, references, validation

- [ ] **Step 1: Write `config.md`**

(Implementation mirrors current file with `paths:` frontmatter,
line length section, duplicate formatting rules removed.)

- [ ] **Step 2: Verify file**

Run: `wc -l .claude/rules/config.md`
Expected: ~80 lines (acceptable: 75-85)

- [ ] **Step 3: Commit**

```bash
git add .claude/rules/config.md
git commit -m "feat(rules): rewrite config.md with paths frontmatter"
```

---

## Task 6: Create `toml.md`

**Files:**
- Create: `.claude/rules/toml.md`

**Source:** `.claude/rules/lang/toml.md` (current, 122 lines)

**Instructions:** Rewrite the current file with these changes:
- Change frontmatter from `globs:` to `paths:`
- Add "Line Length — 79 characters" section
- Remove duplicate formatting rules
- KEEP: file header, delimiter hierarchy, value types (strings always
  quoted), all anti-patterns, security, references, validation

- [ ] **Step 1: Write `toml.md`**

(Implementation mirrors current file with `paths:` frontmatter,
line length section, duplicate formatting rules removed.)

- [ ] **Step 2: Verify file**

Run: `wc -l .claude/rules/toml.md`
Expected: ~75 lines (acceptable: 70-80)

- [ ] **Step 3: Commit**

```bash
git add .claude/rules/toml.md
git commit -m "feat(rules): rewrite toml.md with paths frontmatter"
```

---

## Task 7: Create `yaml.md`

**Files:**
- Create: `.claude/rules/yaml.md`

**Source:** `.claude/rules/lang/yaml.md` (current, 127 lines)

**Instructions:** Rewrite the current file with these changes:
- Change frontmatter from `globs:` to `paths:`
- Add "Line Length — 79 characters" section
- Remove duplicate formatting rules
- KEEP: file header, delimiter hierarchy, value types, all
  anti-patterns, security, references, validation

- [ ] **Step 1: Write `yaml.md`**

(Implementation mirrors current file with `paths:` frontmatter,
line length section, duplicate formatting rules removed.)

- [ ] **Step 2: Verify file**

Run: `wc -l .claude/rules/yaml.md`
Expected: ~80 lines (acceptable: 75-85)

- [ ] **Step 3: Commit**

```bash
git add .claude/rules/yaml.md
git commit -m "feat(rules): rewrite yaml.md with paths frontmatter"
```

---

## Task 8: Delete old files

**Files:**
- Delete: `.claude/rules/coding-style.md`
- Delete: `.claude/rules/patterns.md`
- Delete: `.claude/rules/lang/` (entire directory)

- [ ] **Step 1: Delete old files**

```bash
git rm .claude/rules/coding-style.md .claude/rules/patterns.md
git rm .claude/rules/lang/bash.md .claude/rules/lang/zsh.md \
  .claude/rules/lang/elisp.md .claude/rules/lang/config.md \
  .claude/rules/lang/toml.md .claude/rules/lang/yaml.md
```

- [ ] **Step 2: Verify deletion**

Run: `ls .claude/rules/`
Expected: only new files (standards.md, bash.md, zsh.md, elisp.md,
config.md, toml.md, yaml.md)

Run: `ls .claude/rules/lang/ 2>&1`
Expected: "No such file or directory"

- [ ] **Step 3: Commit**

```bash
git commit -m "refactor(rules): remove old rules structure"
```

---

## Task 9: Update `CLAUDE.md`

**Files:**
- Modify: `CLAUDE.md` (Coding Conventions section, ~lines 146-160)

The Coding Conventions section MUST be updated to reflect the new flat
structure. The old references to `coding-style.md`, `patterns.md`, and
`lang/` directory are invalid after Task 8.

- [ ] **Step 1: Update Coding Conventions section**

Replace the old content (lines ~150-160):

```markdown
### Universal Standards
- `coding-style.md` — Line length (80 chars), file headers, documentation
- `patterns.md` — Design patterns, immutability principle

### Language-Specific (`.claude/rules/lang/`)
- `lang/bash.md` — Bash-specific features (namerefs, associative arrays)
- `lang/zsh.md` — Zsh-specific features (globbing, hooks)
- `lang/elisp.md` — Emacs Lisp conventions (lexical binding, ERT testing)
- `lang/config.md` — Configuration files (no extension)
- `lang/toml.md` — TOML configuration files
- `lang/yaml.md` — YAML configuration files
```

With new content:

```markdown
### Universal Standards
- `standards.md` — Universal baseline (formatting, file organization, comments, principles)

### Language-Specific (`.claude/rules/`)
- `bash.md` — Bash shell scripting conventions
- `zsh.md` — Zsh-specific features and conventions
- `elisp.md` — Emacs Lisp conventions
- `config.md` — Configuration files (no extension)
- `toml.md` — TOML configuration files
- `yaml.md` — YAML configuration files
```

- [ ] **Step 2: Verify no stale references anywhere**

Run: `grep -rn 'coding-style\|patterns\.md\|lang/' CLAUDE.md`
Expected: no matches

- [ ] **Step 3: Commit**

```bash
git add CLAUDE.md
git commit -m "docs: update coding conventions references to new rules structure"
```

---

## Task 10: Final validation

- [ ] **Step 1: Verify file structure**

Run: `ls .claude/rules/`
Expected: 7 `.md` files, no `lang/` directory, no `coding-style.md`,
no `patterns.md`

- [ ] **Step 2: Verify line counts**

Run: `wc -l .claude/rules/*.md`
Expected:
- standards.md: ~40
- bash.md: ~150
- zsh.md: ~160
- elisp.md: ~180
- config.md: ~80
- toml.md: ~75
- yaml.md: ~80

- [ ] **Step 3: Verify frontmatter**

Run: `head -5 .claude/rules/bash.md .claude/rules/zsh.md .claude/rules/elisp.md .claude/rules/config.md .claude/rules/toml.md .claude/rules/yaml.md`
Expected: all start with `---` followed by `paths:` (not `globs:`)

- [ ] **Step 4: Verify standards.md has no frontmatter**

Run: `head -3 .claude/rules/standards.md`
Expected: starts with `# Standards` (no `---` YAML frontmatter)

- [ ] **Step 5: Verify mandatory rules preserved**

Run: `grep -l 'MANDATORY\|mandatory' .claude/rules/bash.md .claude/rules/zsh.md`
Expected: both files contain MANDATORY references

Run: `grep -c 'lexical-binding' .claude/rules/elisp.md`
Expected: >= 2 (header + mandatory reference)

Run: `grep 'defvar-local' .claude/rules/elisp.md`
Expected: present

Run: `grep 'kbd' .claude/rules/elisp.md`
Expected: present

- [ ] **Step 6: Manual verification of paths: frontmatter**

Open a Claude Code session, reference a `.toml` file, and verify that
only `standards.md` and `toml.md` rules load (not bash, zsh, elisp, etc.).
