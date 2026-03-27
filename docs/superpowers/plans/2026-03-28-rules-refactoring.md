# Universal Rules Refactoring Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Refactor universal rules to be general, move workflow files to skills/, and clean up broken cross-references.

**Architecture:** Two-file universal rules structure (coding-style.md for format, patterns.md for design), workflow files moved to skills/ directory, all broken references removed from CLAUDE.md.

**Tech Stack:** Markdown documentation files, Claude Code rule loading system

---

## File Structure

### Files to Modify

| File | Action | Purpose |
|------|--------|---------|
| `.claude/rules/coding-style.md` | Refactor | Format conventions (remove frontmatter, restructure) |
| `.claude/rules/patterns.md` | Refactor | Design principles (remove frontmatter, restructure) |
| `CLAUDE.md` | Modify | Update/remove cross-references (lines 161-180) |
| `.claude/skills/dev-workflow.md` | Create (moved) | Development workflow |
| `.claude/skills/git-workflow.md` | Create (moved) | Git workflow |

### Files to Delete

| File | Reason |
|------|--------|
| `.claude/rules/README.md` | Rules are self-documenting |
| `.claude/rules/dev-workflow.md` | Moved to skills/ |
| `.claude/rules/git-workflow.md` | Moved to skills/ |

### Files to Create

| File | Purpose |
|------|---------|
| `.claude/skills/` | New directory for workflow files |

---

## Task 1: Pre-Flight Verification

**Goal:** Verify current state matches expected before making changes.

- [ ] **Step 1: List current rules directory**

Run: `find .claude/rules -maxdepth 1 -name "*.md"`

Expected output:
```
.claude/rules/README.md
.claude/rules/coding-style.md
.claude/rules/dev-workflow.md
.claude/rules/git-workflow.md
.claude/rules/patterns.md
```

- [ ] **Step 2: Verify no skills directory exists**

Run: `ls -la .claude/skills/ 2>&1 || echo "Directory does not exist (expected)"`

Expected output:
```
Directory does not exist (expected)
```

- [ ] **Step 3: Document non-existent file references**

Run: `grep -nE "security\.md|testing\.md|shell\.md|python\.md|hooks\.md" CLAUDE.md .claude/rules/*.md`

Expected output:
```
CLAUDE.md:166:- `security.md` — Secrets management, input validation
CLAUDE.md:171:- `testing.md` — Syntax validation, functional and integration testing
CLAUDE.md:174:- `lang/shell.md` — Universal shell practices (base)
CLAUDE.md:178:- `lang/python.md` — Python standards (type hints, pytest)
.claude/rules/dev-workflow.md:233:- `hooks.md` — Claude Code hooks integration for automated validation
```

---

## Task 2: Create Backup Branch and Skills Directory

**Goal:** Create safety net and prepare target directory.

- [ ] **Step 1: Create backup branch**

Run: `git checkout -b refactor/rules-cleanup`

Expected output:
```
Switched to a new branch 'refactor/rules-cleanup'
```

- [ ] **Step 2: Create skills directory**

Run: `mkdir -p .claude/skills`

- [ ] **Step 3: Verify directory created**

Run: `ls -la .claude/skills/`

Expected output:
```
total 0
drwxr-xr-x   2 user  staff    64 Mar 28 00:00 .
drwxr-xr-x  10 user  staff   320 Mar 28 00:00 ..
```

- [ ] **Step 4: Skip empty directory commit**

Note: Empty directories cannot be committed to git. The skills/ directory will be created and populated in Task 6 when we move dev-workflow.md into it.

No commit needed for this step.

---

## Task 3: Refactor coding-style.md

**Goal:** Remove YAML frontmatter, restructure sections, simplify content.

**Files:**
- Modify: `.claude/rules/coding-style.md`

- [ ] **Step 1: Write refactored coding-style.md**

Replace entire file content with:

```markdown
# Coding Style

Universal coding standards for the oh-my-workspace repository.

## Line Length

Maximum 80 characters for all files.

Rationale: Improves readability in side-by-side diffs and terminal windows.

## File Headers

Every configuration file must include a standard header with:
- File location path
- Usage / invocation instructions
- Dependencies (packages, tools, modules this file relies on)
- Non-obvious design choices with reference URLs where helpful

## Comment Standards

### Content Principles

Explain rationale (WHY), not mechanics (WHAT). Document non-obvious
decisions and constraints.

### Delimiter Hierarchy

**Level 0** (File Header):
```
# ============================================================================
# Title
# ============================================================================
```

**Level 1** (Primary Section):
```
# -----------------------------------------------------------------------------
# Section Name
# -----------------------------------------------------------------------------
```

**Level 2** (Subsection): Inline style
```
# --- Subsection Name ---
```

### Placement Rules

- Use separate comment lines for explanations
- Inline comments acceptable only for brief notes
- Never inline comments for rationale/documentation

## Naming Conventions

Use meaningful, descriptive names. Avoid single-letter or cryptic
abbreviations.

## Formatting Rules

- Never align values with spaces
- Prefer separate-line comments over inline
- 2-space indentation for applicable languages

## Code Quality Principles

- **No dead code** - Remove unused functions and variables
- **Single responsibility** - Each function does one thing well
- **DRY** - Don't repeat yourself; extract common logic
- **YAGNI** - Don't add features you don't need yet
- **KISS** - Keep it simple, avoid over-engineering

## Language-Specific Extensions

Language-specific rules are conditionally loaded via `globs` frontmatter.
They extend these universal rules with language-specific conventions.

| Rule file       | Globs                                                                       |
|-----------------|-----------------------------------------------------------------------------|
| `lang/bash.md`  | `**/*.sh`, `**/*.bash`                                                      |
| `lang/zsh.md`   | `**/*.zsh`, `**/.zsh*`, `**/zshrc`, `**/zprofile`, `**/zshenv`, `**/zlogin` |
| `lang/elisp.md` | `**/*.el`                                                                   |

## Code Quality Checklist

Before completing any task, verify:

- [ ] Standard file header present (location and usage)
- [ ] All functions documented with purpose, params, return
- [ ] Complex logic explained (comments say WHY, not WHAT)
- [ ] Dependencies documented in file header
- [ ] Non-obvious choices include references/URLs
- [ ] No hardcoded secrets or machine-specific paths
- [ ] Input validation present
- [ ] Safe file operations (check before overwrite; backup before modify)
- [ ] File size within target limits
```

- [ ] **Step 2: Verify file syntax**

Run: `head -20 .claude/rules/coding-style.md`

Expected: No YAML frontmatter, starts with `# Coding Style`

- [ ] **Step 3: Commit refactored coding-style.md**

Run: `git add .claude/rules/coding-style.md && git commit -m "$(cat <<'EOF'
refactor(rules): simplify coding-style.md structure

- Remove YAML frontmatter (universal rules auto-load)
- Merge Comment Standards and Delimiter Hierarchy sections
- Simplify to principle statements (remove "when to use" lists)
- Remove tool-specific checklist items
- Focus on format conventions only

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
EOF
)"`

---

## Task 4: Refactor patterns.md

**Goal:** Remove YAML frontmatter, ensure focus is design/organization.

**Files:**
- Modify: `.claude/rules/patterns.md`

- [ ] **Step 1: Write refactored patterns.md**

Replace entire file content with:

```markdown
# Design Patterns

Design patterns and principles for oh-my-workspace configuration code.

## Immutability Principle

Always create new configurations, never modify existing ones in-place.

Rationale:
- Enables easy rollback if changes break
- Prevents unintended side effects
- Makes debugging easier
- Maintains history of changes

## File Organization

Many small files are preferred over few large files.

- Organization: By feature/domain, not by type
- Cohesion: High within files, low between files
- Size targets: Refer to language-specific rules for limits

Split configuration files when:
- File exceeds maximum line count
- File contains multiple unrelated concerns
- Functions have high cognitive complexity

## Safe Defaults

Always provide fallback values so configs work even when optional
environment variables are not set. Avoid hard failures caused by
unset variables.

## Design Patterns

### Validation Pattern

Always validate configs and inputs before use:

1. Check the resource exists and is readable
2. Validate syntax
3. Only proceed on success; emit a clear error and return on failure

### Repository Pattern

Encapsulate config access behind a consistent interface with standard
operations: load, save, validate, backup. This enables easy swapping
of config locations, simplifies testing, and centralizes error handling.

## Anti-Patterns

### Deep Nesting

Limit nesting to 3 levels maximum. Use early-return guards to flatten
conditional structures.

### Magic Numbers

Never use bare numeric literals. Define named constants instead.
This makes intent clear and centralizes values that may need updating.

### Hardcoded Paths

Never hardcode absolute paths to home directories or system locations.
Use `$HOME` / `$XDG_*` variables with sensible defaults.

### Large Monolithic Files

Do not put everything in one file. Split by feature/domain into focused
files that each have a single responsibility.
```

- [ ] **Step 2: Verify file syntax**

Run: `head -10 .claude/rules/patterns.md`

Expected: No YAML frontmatter, starts with `# Design Patterns`

- [ ] **Step 3: Verify no overlap with coding-style.md**

Run: `grep -E "Line Length|File Headers|Comment Standards|Naming Conventions|Formatting Rules" .claude/rules/patterns.md`

Expected: No matches (these belong in coding-style.md)

- [ ] **Step 4: Commit refactored patterns.md**

Run: `git add .claude/rules/patterns.md && git commit -m "$(cat <<'EOF'
refactor(rules): simplify patterns.md structure

- Remove YAML frontmatter (universal rules auto-load)
- Simplify to principle statements
- Focus on design: organization, patterns, anti-patterns
- Remove reference to coding-style.md (keep self-contained)

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
EOF
)"`

---

## Task 5: Remove README.md

**Goal:** Delete rules/README.md as rules are now self-documenting.

**Files:**
- Delete: `.claude/rules/README.md`

- [ ] **Step 1: Remove README.md**

Run: `rm .claude/rules/README.md`

- [ ] **Step 2: Verify file removed**

Run: `ls .claude/rules/README.md 2>&1`

Expected output:
```
No such file or directory
```

- [ ] **Step 3: Commit removal**

Run: `git add -A .claude/rules/README.md && git commit -m "$(cat <<'EOF'
chore(rules): remove README.md

Rules are self-documenting after refactoring. Each file has clear
headers and structure, making a separate README unnecessary.

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
EOF
)"`

---

## Task 6: Move dev-workflow.md to skills/

**Goal:** Move workflow file to skills/ directory and update cross-references.

**Files:**
- Move: `.claude/rules/dev-workflow.md` → `.claude/skills/dev-workflow.md`

- [ ] **Step 1: Move file**

Run: `mv .claude/rules/dev-workflow.md .claude/skills/dev-workflow.md`

- [ ] **Step 2: Update "See Also" section in moved file**

Read the file and update the "See Also" section (lines 231-235).

Replace:
```markdown
## See Also

- `hooks.md` — Claude Code hooks integration for automated validation
- `git-workflow.md` — Commit message conventions
- `coding-style.md` — Code formatting standards
```

With:
```markdown
## See Also

- `git-workflow.md` — Commit message conventions
- `../rules/coding-style.md` — Code formatting standards
```

- [ ] **Step 3: Verify file moved**

Run: `ls .claude/rules/dev-workflow.md .claude/skills/dev-workflow.md 2>&1`

Expected output:
```
ls: .claude/rules/dev-workflow.md: No such file or directory
.claude/skills/dev-workflow.md
```

- [ ] **Step 4: Commit move**

Run: `git add -A && git commit -m "$(cat <<'EOF'
refactor: move dev-workflow.md to skills directory

- Move from rules/ to skills/ (workflow guidance, not code rules)
- Update See Also section: remove non-existent hooks.md
- Update paths to relative references from skills/

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
EOF
)"`

---

## Task 7: Move git-workflow.md to skills/

**Goal:** Move workflow file to skills/ directory.

**Files:**
- Move: `.claude/rules/git-workflow.md` → `.claude/skills/git-workflow.md`

- [ ] **Step 1: Move file**

Run: `mv .claude/rules/git-workflow.md .claude/skills/git-workflow.md`

- [ ] **Step 2: Verify file moved**

Run: `ls .claude/rules/git-workflow.md .claude/skills/git-workflow.md 2>&1`

Expected output:
```
ls: .claude/rules/git-workflow.md: No such file or directory
.claude/skills/git-workflow.md
```

- [ ] **Step 3: Commit move**

Run: `git add -A && git commit -m "$(cat <<'EOF'
refactor: move git-workflow.md to skills directory

Move from rules/ to skills/ (workflow guidance, not code rules).

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
EOF
)"`

---

## Task 8: Update CLAUDE.md Cross-References

**Goal:** Update workflow file paths and remove references to non-existent files.

**Files:**
- Modify: `CLAUDE.md` (lines 159-180)

- [ ] **Step 1: Read current CLAUDE.md section**

Run: `sed -n '159,180p' CLAUDE.md`

Expected current content:
```markdown
## Coding Conventions

Detailed conventions in `.claude/rules/`:

### Universal Standards
- `coding-style.md` — Line length (80 chars), file headers, documentation
- `patterns.md` — Design patterns, immutability principle
- `security.md` — Secrets management, input validation

### Workflow & Process
- `dev-workflow.md` — Research-first approach, verification phases
- `git-workflow.md` — Conventional Commits, branch naming
- `testing.md` — Syntax validation, functional and integration testing

### Language-Specific (conditionally loaded)
- `lang/shell.md` — Universal shell practices (base)
- `lang/bash.md` — Bash-specific features (namerefs, associative arrays)
- `lang/zsh.md` — Zsh-specific features (globbing, hooks)
- `lang/elisp.md` — Emacs Lisp conventions (lexical binding, ERT testing)
- `lang/python.md` — Python standards (type hints, pytest)

**See `.claude/rules/README.md` for complete rule documentation.**
```

- [ ] **Step 2: Update CLAUDE.md Coding Conventions section**

Replace lines 159-180 with:

```markdown
## Coding Conventions

Detailed conventions in `.claude/rules/` and `.claude/skills/`:

### Universal Standards (`.claude/rules/`)
- `coding-style.md` — Line length (80 chars), file headers, documentation
- `patterns.md` — Design patterns, immutability principle

### Workflow & Process (`.claude/skills/`)
- `dev-workflow.md` — Research-first approach, verification phases
- `git-workflow.md` — Conventional Commits, branch naming

### Language-Specific (`.claude/rules/lang/`)
- `lang/bash.md` — Bash-specific features (namerefs, associative arrays)
- `lang/zsh.md` — Zsh-specific features (globbing, hooks)
- `lang/elisp.md` — Emacs Lisp conventions (lexical binding, ERT testing)
- `lang/config.md` — Configuration files (no extension)
- `lang/toml.md` — TOML configuration files
- `lang/yaml.md` — YAML configuration files
```

- [ ] **Step 3: Verify no broken references remain**

Run: `grep -nE "security\.md|testing\.md|shell\.md|python\.md|hooks\.md|README\.md" CLAUDE.md`

Expected: No output (all non-existent references removed)

- [ ] **Step 4: Verify workflow paths updated**

Run: `grep -n "dev-workflow\|git-workflow" CLAUDE.md`

Expected output:
```
169:- `dev-workflow.md` — Research-first approach, verification phases
170:- `git-workflow.md` — Conventional Commits, branch naming
```
(Paths should reference files in skills/ via the section header)

- [ ] **Step 5: Commit CLAUDE.md updates**

Run: `git add CLAUDE.md && git commit -m "$(cat <<'EOF'
docs: update CLAUDE.md coding conventions section

- Update workflow file paths to skills/ directory
- Remove references to non-existent files:
  - security.md (no such file)
  - testing.md (no such file)
  - lang/shell.md (no such file)
  - lang/python.md (no such file)
- Remove reference to deleted README.md
- Add lang/config.md, lang/toml.md, lang/yaml.md (actually exist)

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
EOF
)"`

---

## Task 9: Final Verification

**Goal:** Verify all changes are correct and no broken references remain.

- [ ] **Step 1: Verify rules directory structure**

Run: `ls -la .claude/rules/`

Expected output:
```
coding-style.md
patterns.md
lang/
```

- [ ] **Step 2: Verify skills directory structure**

Run: `ls -la .claude/skills/`

Expected output:
```
dev-workflow.md
git-workflow.md
```

- [ ] **Step 3: Verify no broken references**

Run: `grep -rE "security\.md|testing\.md|shell\.md|python\.md|hooks\.md" CLAUDE.md .claude/rules/coding-style.md .claude/skills/*.md 2>/dev/null || echo "No broken references found (expected)"`

Expected output:
```
No broken references found (expected)
```

Note: Also verify coding-style.md does not reference non-existent lang/python.md:
```bash
grep "lang/python" .claude/rules/coding-style.md
```
Expected: No output (reference removed in refactored version)

- [ ] **Step 4: Verify universal rules have no YAML frontmatter**

Run: `head -5 .claude/rules/coding-style.md .claude/rules/patterns.md`

Expected: Both files start with `#` (not `---`)

- [ ] **Step 5: Verify git status**

Run: `git status`

Expected: Working tree clean (all changes committed)

---

## Task 10: Merge to Master

**Goal:** Merge refactoring branch to master and push.

- [ ] **Step 1: Switch to master**

Run: `git checkout master`

- [ ] **Step 2: Merge refactoring branch**

Run: `git merge --squash refactor/rules-cleanup`

- [ ] **Step 3: Create final commit**

Run: `git commit -m "$(cat <<'EOF'
refactor(rules): restructure universal rules and move workflows

- Remove YAML frontmatter from universal rules (auto-loaded)
- Simplify coding-style.md to format conventions only
- Simplify patterns.md to design principles only
- Move dev-workflow.md and git-workflow.md to skills/
- Remove README.md (rules are self-documenting)
- Clean up CLAUDE.md references to non-existent files

BREAKING CHANGE: workflow files moved from rules/ to skills/

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
EOF
)"`

- [ ] **Step 4: Push to remote**

Run: `git push origin master`

- [ ] **Step 5: Delete feature branch**

Run: `git branch -d refactor/rules-cleanup`

---

## Success Criteria Verification

After completing all tasks, verify:

- [ ] Universal rules have no YAML frontmatter
- [ ] `coding-style.md` focuses on format (not design)
- [ ] `patterns.md` focuses on design (not format)
- [ ] No overlap between coding-style.md and patterns.md
- [ ] Workflow files are in `.claude/skills/` directory
- [ ] README.md is removed
- [ ] CLAUDE.md references only existing files
- [ ] No references to non-existent files (security.md, testing.md, etc.)
- [ ] All changes committed and pushed to master

---

## Rollback Procedure

If any issues are discovered during implementation, use these commands to restore the previous state:

### Rollback to Pre-Refactoring State

```bash
# Discard all uncommitted changes
git checkout .

# Switch back to master and delete the feature branch
git checkout master
git branch -D refactor/rules-cleanup

# If already committed to master, revert the merge
git reset --hard origin/master
```

### Partial Rollback (Undo Specific Task)

If only one task caused issues:

```bash
# Find the commit for that task
git log --oneline -10

# Revert that specific commit
git revert <commit-hash>
```

### Verification After Rollback

After rollback, verify the system is in a clean state:

```bash
# Verify rules directory is unchanged
find .claude/rules -maxdepth 1 -name "*.md"

# Verify CLAUDE.md has original references
grep -n "security\.md\|testing\.md" CLAUDE.md

# Start a new Claude Code session to verify rules load correctly
```
