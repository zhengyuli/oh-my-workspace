# elisp.md Rule Refinement Design

**Date**: 2026-03-27
**Status**: Approved
**Purpose**: Enhance `.claude/rules/lang/elisp.md` to serve as the template for all language rule files

## Overview

This document outlines refinements to the Emacs Lisp rule file (`elisp.md`) to improve content organization, description style, and adherence to Claude Code rule best practices. The refined `elisp.md` will serve as the structural template for optimizing other language rules in the project.

## Problem Statement

The current `elisp.md` has several areas for improvement:

1. **Inconsistent section structure** - Security and Validation chapters use bullet points instead of subheadings
2. **Mixed semantic content** - Validation chapter mixes bash commands with Elisp-specific workflow guidance
3. **Unclear hierarchy** - Functions chapter uses bold prefixes instead of proper subheadings
4. **Ambiguous descriptions** - Delimiter Hierarchy uses unclear character count notation
5. **Inconsistent mandatory markers** - Some sections use `(MANDATORY)` suffix, others do not

## Design Goals

1. **Consistency** - Align all chapter structures to use subheadings where appropriate
2. **Clarity** - Remove ambiguous descriptions, provide clear visual examples
3. **Professionalism** - Use prescriptive language consistently throughout
4. **Template-ready** - Create a structure that can be applied to other language rules

## Detailed Changes

### Change 1: Security Chapter Structure

**Before**:
```markdown
## Security

**Package sources**: Only install packages...
**Local overrides**: Keep machine-specific...
**Stale `.elc` files**: Never commit...
```

**After**:
```markdown
## Security

### Package Sources

Only install packages from GNU ELPA or MELPA Stable; never from unverified
third-party repositories.

### Local Overrides

Keep machine-specific or sensitive settings in `omw-local.el`, excluded from
version control via `.gitignore`:

### Bytecompile Artifacts

Never commit `.elc` files; always delete them before committing.
```

**Rationale**: Subheadings improve scanability and align with other chapters like Error Handling.

---

### Change 2: Validation Chapter Separation

**Before**:
```markdown
## Validation

**Byte compilation**: Zero warnings required...

**Before committing**: After verification passes, delete all `.elc` files:
```

**After**:
```markdown
## Validation

### Byte Compilation

Zero warnings required — treat every warning as an error:

### Pre-Commit Workflow

After verification passes, delete all `.elc` files before committing:
```

**Rationale**: Separates verification commands from workflow guidance; provides clearer semantic distinction.

---

### Change 3: Functions Chapter Subheadings

**Before**:
```markdown
## Functions

**Single responsibility**: Each function does exactly one thing.

**`interactive` declaration**: All user-facing commands...
```

**After**:
```markdown
## Functions

### Single Responsibility

Each function does exactly one thing.

### Interactive Declaration

All user-facing commands (key-bound or M-x callable) must declare
`(interactive)`; internal helpers must not:
```

**Rationale**: Proper subheadings are visible in table of contents and improve document structure.

---

### Change 4: Documentation & Code Patterns Subheadings

**Before**:
```markdown
## Documentation & Code Patterns

**Comments:**
- Explain rationale (WHY), not mechanics (WHAT)

**Docstrings**: Every public function requires a docstring.
```

**After**:
```markdown
## Documentation & Code Patterns

### Comments

Explain rationale (WHY), not mechanics (WHAT). Document non-obvious
decisions and constraints. Use separate comment lines; never inline
explanations.

### Docstrings

Every public function requires a docstring. First line must be a complete
sentence; document all parameters and return value.
```

**Rationale**: Consistency with other chapters; better visual hierarchy.

---

### Change 5: Remove (MANDATORY) Suffix

**Before**:
```markdown
## File Header (MANDATORY)
## Delimiter Hierarchy (MANDATORY)
```

**After**:
```markdown
## File Header
## Delimiter Hierarchy
```

**Rationale**: Importance is conveyed through heading level and content; the suffix is redundant.

---

### Change 6: use-package Promoted to Subsection

**Before**:
```markdown
**use-package (MANDATORY)**: All package configuration must use `use-package`.
```

**After**:
```markdown
### use-package Declaration

All package configuration must use `use-package`.
```

**Rationale**: Aligns with other major rules in the document.

---

### Change 7: Delimiter Hierarchy Description Precision

**Before**:
```markdown
**Level 0** (File Header): `;; ===` × 76 (79 chars total)
**Level 1** (Primary Section): `;; ---` × 76 (79 chars total)
```

**After**:
```markdown
**Level 0** (File Header):
```
;; ============================================================================
```

**Level 1** (Primary Section):
```
;; ----------------------------------------------------------------------------
```
```

**Rationale**: Visual examples eliminate ambiguity; users can copy/paste directly.

---

## Style Guidelines

### Tone and Language

1. **Imperative mood** - Use `must`, `must not`, `always`, `never`
2. **Prescriptive** - State rules directly, avoid hedging language
3. **First-word capitalization** - List items and descriptions begin with lowercase unless proper noun or sentence start

### Structural Patterns

1. **Chapters** (`##`) - Major topic areas
2. **Sections** (`###`) - Specific rules or subtopics within chapters
3. **Code examples** - Follow each rule with `CORRECT` / `WRONG` examples where applicable
4. **Rationale** - Include brief explanations for non-obvious rules

### Markdown Conventions

- **Bold** for emphasis in running text
- **Code blocks** for all examples
- **CORRECT/WRONG** labels in all caps for contrast
- No alignment with spaces (prevents diff noise)

## Implementation Plan

1. Update `elisp.md` with all changes outlined above
2. Review against current structure to ensure no content was lost
3. Verify markdown rendering and code block syntax
4. Commit with conventional commit message

## Success Criteria

- All chapters use consistent heading structure
- No `(MANDATORY)` suffixes remain
- All code examples are properly formatted
- Document is scannable with clear visual hierarchy
- Structure can be applied to other language rules (`bash.md`, `zsh.md`, etc.)

## Future Work

After `elisp.md` is finalized, apply the same structural improvements to:
- `lang/bash.md`
- `lang/zsh.md`
- `lang/python.md`
- `lang/config.md`
- `lang/toml.md`
- `lang/yaml.md`
