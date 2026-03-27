# Universal Rules Refactoring Design

**Date:** 2026-03-28
**Author:** zhengyu.li
**Status:** Approved

## Overview

Refactor the universal rules in `.claude/rules/` (excluding `lang/` directory) to follow the structure and content presentation of `lang/elisp.md`. Make universal rules more general and remove tool-specific details. Move workflow files to `skills/` directory as they describe processes rather than static code rules.

## Problem Statement

Current universal rules have several issues:
1. **Mixed focus**: Universal rules contain tool-specific details (e.g., GNU Stow operations in `dev-workflow.md`)
2. **Inconsistent structure**: Universal rules don't follow the same clear hierarchical structure as `lang/elisp.md`
3. **Overlap**: Content is duplicated across files (e.g., Immutability appears in both `coding-style.md` and `patterns.md`)
4. **Misplaced files**: `*-workflow.md` files describe workflows/processes, not code rules, yet live in `rules/`

## Design Goals

1. **Universal rules must be general** - no tool-specific details
2. **Follow `lang/elisp.md` template** - hierarchical structure, clear sections
3. **Clear separation of concerns** - each file has a single, well-defined purpose
4. **Simplify content** - principle statements over detailed "when to use" guidance

## File Structure Changes

```
.claude/rules/
├── coding-style.md    # Format conventions (refactored)
├── patterns.md        # Design principles (refactored)
└── lang/              # Language-specific rules (unchanged)
    ├── elisp.md
    ├── bash.md
    ├── zsh.md
    ├── config.md
    ├── toml.md
    └── yaml.md

.claude/skills/        # New directory
├── dev-workflow.md    # Moved from rules/
└── git-workflow.md    # Moved from rules/
```

**Removed:**
- `.claude/rules/README.md` - Rules are self-documenting

## Detailed Changes

### 1. `coding-style.md` - Format Conventions

**Purpose:** Code formatting and style rules (how to write)

**Responsibility:** Format and style, not design or organization

**Section Structure:**

```markdown
# Coding Style

## Line Length
Maximum 80 characters, with rationale

## File Headers
Standard header format (location, usage, dependencies)

## Comment Standards
### Content Principles
Explain WHY not WHAT

### Delimiter Hierarchy
Level 0: # ============================================================================
Level 1: # -----------------------------------------------------------------------------
Level 2: # --- Title ---

### Placement Rules
Separate lines for explanations

## Naming Conventions
Meaningful names, avoid abbreviations

## Formatting Rules
No alignment, 2-space indent (for applicable languages)

## Code Quality
DRY, KISS, YAGNI, Single Responsibility
```

**Key Changes:**
- Remove YAML frontmatter
- Merge "Comment Standards" and "Delimiter Hierarchy" into single section
- Simplify to principle statements (no "when to use" lists)
- Focus on format: line length, headers, comments, naming

### 2. `patterns.md` - Design Principles

**Purpose:** Code organization and design patterns (how to organize)

**Responsibility:** Architecture and organization, not format

**Section Structure:**

```markdown
# Design Patterns

## Immutability Principle
Always create new, never modify in-place

## File Organization
MANY SMALL FILES > FEW LARGE FILES
High cohesion, low coupling

## Safe Defaults
Always provide fallback values

## Design Patterns
### Validation Pattern
Check exists -> validate syntax -> proceed or error

### Repository Pattern
Encapsulate access behind consistent interface

## Anti-Patterns
### Deep Nesting
Limit to 3 levels

### Magic Numbers
Define named constants

### Hardcoded Paths
Use $HOME / $XDG_*

### Large Monolithic Files
Split by feature/domain
```

**Key Changes:**
- Remove YAML frontmatter
- Simplify to principle statements (no "when to use" lists)
- Focus on design: organization, patterns, anti-patterns
- No language-specific examples

### 3. Workflow Files → `skills/` Directory

**Files to move:**
- `dev-workflow.md` → `skills/dev-workflow.md`
- `git-workflow.md` → `skills/git-workflow.md`

**Rationale:**
- These files describe workflows/processes, not static code rules
- Skills are about "how to work", rules are about "how to write"
- Better aligns with user's mental model

### 4. Language-Specific Rules (Unchanged)

**Files in `lang/` directory:**
- `elisp.md`
- `bash.md`
- `zsh.md`
- `config.md`
- `toml.md`
- `yaml.md`

**No changes to:**
- YAML frontmatter with `globs` for conditional loading
- Language-specific content (functions, error handling, validation)
- Code examples showing CORRECT/WRONG patterns

## Template Reference: `lang/elisp.md`

Key structural elements to adopt:

1. **Hierarchical section organization** - Clear top-level headers
2. **Delimiter Hierarchy** - Visual comment conventions
3. **CORRECT/WRONG examples** - Show both good and bad patterns
4. **Clear section separation** - Each concern has its own section
5. **"WHY not WHAT" commentary** - Explain rationale

### Adapting Template for Universal Rules

`lang/elisp.md` uses Elisp comment syntax (`;`), but universal rules use Markdown:

| Element | elisp.md | Universal Rules |
|---------|----------|-----------------|
| File comment | `;; ===` | `# ===` |
| Section comment | `;; ---` | `# ---` |
| Inline comment | `; text` | `# text` |
| Example format | Elisp code | Config/pseudo-code |

We adopt the **structure** (hierarchy levels) but adapt the **syntax** for Markdown.

### Delimiter Usage Examples

Level 0 (File boundaries):
```markdown
# ============================================================================
# Coding Style
# ============================================================================
```

Level 1 (Major sections):
```markdown
# -----------------------------------------------------------------------------
# Comment Standards
# -----------------------------------------------------------------------------
```

Level 2 (Subsections within a section):
```markdown
# --- Content Principles ---
```

## Cross-Reference Inventory

### Files Being Modified/Moved

| File | Action | New Path | References to Update |
|------|--------|----------|---------------------|
| `coding-style.md` | Refactor | `.claude/rules/coding-style.md` (unchanged) | None |
| `patterns.md` | Refactor | `.claude/rules/patterns.md` (unchanged) | Line 26: self-ref (no change) |
| `dev-workflow.md` | Move | `.claude/skills/dev-workflow.md` | See below |
| `git-workflow.md` | Move | `.claude/skills/git-workflow.md` | See below |
| `README.md` | Delete | N/A | N/A |

### References Requiring Updates

| Source File | Lines | Reference | New Value |
|-------------|-------|-----------|-----------|
| `CLAUDE.md` | 169 | `.claude/rules/dev-workflow.md` | `.claude/skills/dev-workflow.md` |
| `CLAUDE.md` | 170 | `.claude/rules/git-workflow.md` | `.claude/skills/git-workflow.md` |
| `dev-workflow.md` | 234-235 | `git-workflow.md`, `coding-style.md` | Update to relative paths from `skills/` |
| `CLAUDE.md` | 180 | `.claude/rules/README.md` | Remove line (README deleted) |

### Non-Existent File References (Cleanup Required)

| Source File | Lines | Missing Reference | Action |
|-------------|-------|-------------------|--------|
| `CLAUDE.md` | 166 | `security.md` | **Remove line** (no such file exists) |
| `CLAUDE.md` | 171 | `testing.md` | **Remove line** (no such file exists) |
| `CLAUDE.md` | 174 | `lang/shell.md` | **Remove line** (no such file exists) |
| `CLAUDE.md` | 178 | `lang/python.md` | **Remove line** (no such file exists) |
| `dev-workflow.md` | 233 | `hooks.md` | **Remove line** (no such file exists) |

**Note:** These references are documentation bugs - they reference files that were planned but never created. This refactoring should clean them up.

### Search Patterns for Verification

```bash
# Find references to workflow files (should find only in skills/ after migration)
grep -r "dev-workflow\|git-workflow" .claude/

# Find references to non-existent files (should return nothing after cleanup)
grep -E "security\.md|testing\.md|shell\.md|python\.md|hooks\.md" CLAUDE.md .claude/

# Find all .md file references (for manual verification)
grep -oE "[a-z-]+\.md" CLAUDE.md .claude/rules/*.md | sort -u
```

## Content Principles

### Universal Rules vs Language-Specific Rules

| Aspect | Universal Rules | Language-Specific Rules |
|--------|----------------|------------------------|
| Scope | All files | Specific file types |
| Content | Format, design principles | Implementation details |
| Examples | Pseudo-code, config examples | Real code in that language |
| Loading | Auto-loaded (no globs) | Conditionally loaded via globs |

### What NOT to Include in Universal Rules

| Topic | Reason |
|-------|--------|
| Functions | Too language-specific |
| Error Handling | Varies between languages |
| Security | Already covered in lang rules |
| Validation | Tool-specific commands |
| Tool-specific operations | e.g., GNU Stow, Git commands |

### Security Content Handling

**Note:** There is no separate `security.md` file. Security content is distributed:

- **Universal rules**: "No hardcoded secrets" in checklist (keep this)
- **Language-specific rules**: Each `lang/*.md` has Security section (unchanged)

Security guidance remains in place, just not as a separate universal file.

### Code Quality Checklist Handling

Current `coding-style.md` contains an actionable checklist (lines 119-136). This is useful for pre-commit verification.

**Decision:** Keep the checklist, but simplify it to focus on universal concerns (format, structure) rather than implementation details (e.g., "Run `./setup.sh clean`" is tool-specific and should be removed).

## Migration Plan

### Phase 0: Pre-Flight Check
1. Verify current state: `ls -la .claude/rules/`
2. Document all files that reference rules:
   ```bash
   grep -r "\.claude/rules/" CLAUDE.md .claude/
   grep -rE "coding-style|patterns\.md|dev-workflow|git-workflow" CLAUDE.md .claude/
   ```
3. Identify all non-existent file references:
   ```bash
   grep -E "security\.md|testing\.md|shell\.md|python\.md|hooks\.md" CLAUDE.md .claude/
   ```

### Phase 1: Prepare
1. Create backup branch: `git checkout -b refactor/rules-cleanup`
2. Create `.claude/skills/` directory
3. Verify current file list matches expected state

### Phase 2: Refactor Universal Rules
1. Refactor `coding-style.md`:
   - Remove YAML frontmatter (lines 1-5)
   - Restructure sections per template (hierarchy)
   - Merge Comment/Delimiter sections
   - Simplify to principle statements (remove "when to use" lists)
   - Keep simplified checklist:
     - **Keep**: File headers, comments, naming, immutability, file size
     - **Remove**: "Run `./setup.sh clean`", "Commit message follows Conventional Commits"
2. Refactor `patterns.md`:
   - Remove YAML frontmatter (lines 1-5)
   - Ensure focus is design/organization (not format)
   - Simplify to principle statements
   - Verify no overlap with `coding-style.md`
3. Remove `README.md` (rules are self-documenting)

### Phase 2: Refactor Universal Rules
1. Refactor `coding-style.md`:
   - Remove YAML frontmatter
   - Restructure sections per template
   - Merge Comment/Delimiter sections
   - Simplify to principle statements
   - Keep simplified checklist (remove tool-specific items)
2. Refactor `patterns.md`:
   - Remove YAML frontmatter
   - Ensure focus is design/organization (not format)
   - Simplify to principle statements
3. Remove `README.md` (rules are self-documenting)

### Phase 3: Move Workflow Files
1. Create `.claude/skills/` directory
2. Move `dev-workflow.md` → `skills/dev-workflow.md`
3. Move `git-workflow.md` → `skills/git-workflow.md`
4. Update "See Also" section in moved files (new paths)

### Phase 4: Update Cross-References
1. Update `CLAUDE.md`:
   - **Lines 161-180**: Update workflow file paths to `skills/` directory
   - **Line 166**: Remove `security.md` reference (file doesn't exist)
   - **Line 171**: Remove `testing.md` reference (file doesn't exist)
   - **Line 174**: Remove `lang/shell.md` reference (file doesn't exist)
   - **Line 178**: Remove `lang/python.md` reference (file doesn't exist)
   - **Line 180**: Remove reference to `README.md` (file being deleted)
   - **Lines 264, 271**: Verify description still accurate after changes
2. Update moved files (`skills/dev-workflow.md`, `skills/git-workflow.md`):
   - Update "See Also" section paths to relative references
   - Remove `hooks.md` reference from `dev-workflow.md` (line 233)
3. Verify no broken references:
   ```bash
   # Should return nothing after cleanup
   grep -r "dev-workflow\|git-workflow" .claude/ --exclude-dir=skills
   grep -E "security\.md|testing\.md|shell\.md|python\.md|hooks\.md" CLAUDE.md .claude/
   ```

### Phase 5: Verification Protocol

#### 5.1 Syntax Validation
```bash
# Markdown linting (if available)
markdownlint .claude/rules/*.md .claude/skills/*.md

# Check for broken links
grep -r "\[.*\](.*\.md)" .claude/rules/ .claude/skills/
```

#### 5.2 Content Review
- [ ] Universal rules have no YAML frontmatter
- [ ] No tool-specific content in universal rules
- [ ] `coding-style.md` focuses on format (not design)
- [ ] `patterns.md` focuses on design (not format)
- [ ] No overlap between the two files
- [ ] Workflow files in `skills/` directory
- [ ] All cross-references updated
- [ ] CLAUDE.md references only existing files
- [ ] No dangling references to deleted files (security.md, testing.md, etc.)

#### 5.3 Rule Loading Verification
Since Claude Code loads rules based on file structure and settings.json, verify:

1. Check `.claude/settings.json` for any explicit rule paths
2. Test by starting a new Claude Code session
3. Ask a simple question to verify rules are loaded
4. If rules fail to load, restore from backup branch

#### 5.4 Rollback Trigger
If any of these occur, abort and restore from backup:
- Claude reports missing rules
- Syntax errors in any file
- Broken cross-references detected
- Universal rules contain tool-specific content

## Success Criteria

1. ✅ Universal rules have no YAML frontmatter
2. ✅ Universal rules are general (no tool-specific details)
3. ✅ `coding-style.md` and `patterns.md` have clear, non-overlapping responsibilities
4. ✅ Workflow files are in `skills/` directory
5. ✅ Structure follows `lang/elisp.md` template (adapted for Markdown)
6. ✅ Content is simplified (principle statements, not detailed guidance)
7. ✅ All cross-references updated and verified
8. ✅ No dangling references to old file locations
9. ✅ Security content preserved (distributed across files)
10. ✅ CLAUDE.md references only existing files
11. ✅ No references to non-existent files (security.md, testing.md, shell.md, python.md, hooks.md)

## References

- Template: `.claude/rules/lang/elisp.md`
- Current rules: `.claude/rules/coding-style.md`, `.claude/rules/patterns.md`
- Workflow files: `.claude/rules/dev-workflow.md`, `.claude/rules/git-workflow.md`
