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
2. **Delimiter Hierarchy** - Visual comment conventions (adapted for `#`)
3. **CORRECT/WRONG examples** - Show both good and bad patterns
4. **Clear section separation** - Each concern has its own section
5. **"WHY not WHAT" commentary** - Explain rationale

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

## Migration Plan

### Phase 1: Prepare
1. Create `skills/` directory
2. Create backup of current rules

### Phase 2: Refactor Universal Rules
1. Refactor `coding-style.md`
2. Refactor `patterns.md`
3. Remove `README.md`

### Phase 3: Move Workflow Files
1. Move `dev-workflow.md` to `skills/`
2. Move `git-workflow.md` to `skills/`

### Phase 4: Update References
1. Update CLAUDE.md if it references these files
2. Update any cross-references between files

### Phase 5: Verify
1. Syntax validation
2. Content review
3. Test that Claude loads rules correctly

## Success Criteria

1. ✅ Universal rules have no YAML frontmatter
2. ✅ Universal rules are general (no tool-specific details)
3. ✅ `coding-style.md` and `patterns.md` have clear, non-overlapping responsibilities
4. ✅ Workflow files are in `skills/` directory
5. ✅ Structure follows `lang/elisp.md` template
6. ✅ Content is simplified (principle statements, not detailed guidance)

## References

- Template: `.claude/rules/lang/elisp.md`
- Current rules: `.claude/rules/coding-style.md`, `.claude/rules/patterns.md`
- Workflow files: `.claude/rules/dev-workflow.md`, `.claude/rules/git-workflow.md`
