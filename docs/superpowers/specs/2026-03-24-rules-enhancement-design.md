# Rules Enhancement Design Specification

**Date:** 2026-03-24
**Status:** Approved
**Scope:** Add missing rules from reference project adapted for oh-my-workspace configuration management

## Overview

This specification defines the enhancement of oh-my-workspace's `.claude/rules/` directory by adding three new rule files and enhancing two existing files. The enhancement focuses on maintaining consistency in configuration code (Emacs, Zsh, and other dotfiles) through strong constraints on file headers, comments, and coding style.

## Problem Statement

oh-my-workspace is a mixed project containing:
- Dotfiles (configuration files)
- Utility scripts and tools
- Complex configuration code (Emacs configs, Zsh configs, etc.)

**Current gaps:**
1. No formal workflow for researching and implementing configuration changes
2. No design patterns or principles for writing maintainable configuration code
3. No hooks system for tracking multi-file configuration changes
4. Missing immutability principle in coding standards
5. Missing file organization standards (size limits)
6. Weak constraints on configuration code documentation

## Goals

1. **Establish consistency** in file headers, comments, and coding style across all configuration files
2. **Prevent config bloat** through file organization standards
3. **Enable safe changes** through immutability principles and backup strategies
4. **Improve workflow** through research-first approach and TodoWrite integration
5. **Track complex changes** through hooks system

## Non-Goals

1. Adding testing rules (project uses manual verification)
2. Adding performance optimization rules (not priority for config work)
3. Adding agent orchestration rules (user already proficient with agents)
4. Restructuring directory layout (keep existing structure)
5. Application-focused patterns (keep config-focused)

## Design

### Architecture

**Directory structure (no changes):**
```
.claude/rules/
├── coding-style.md (enhanced)
├── git-workflow.md (unchanged)
├── security.md (unchanged)
├── ai-generation.md (enhanced)
├── patterns.md (NEW)
├── development-workflow.md (NEW)
├── hooks.md (NEW)
└── lang/ (unchanged)
    ├── shell.md
    ├── bash.md
    ├── zsh.md
    ├── elisp.md
    └── python.md
```

**Rule Interaction:**
- New common rules (patterns.md, development-workflow.md, hooks.md) apply to ALL configuration code
- Language-specific rules in `lang/` extend (not replace) common rules
- If conflicts occur: `lang/*` rules take precedence for that language (specific overrides general)
- Example: `lang/python.md` may specify different file size limits for Python utilities vs. Python configs

### New Files

#### 1. patterns.md (~150 lines)

**Purpose:** Define design patterns and principles for configuration code

**Sections:**

1. **Immutability Principle**
   - Always create new configurations, never mutate in-place
   - Rationale: prevents side effects, enables rollback, simplifies debugging
   - Examples for shell scripts, Emacs Lisp, and configs

2. **File Organization**
   - Target: 200-400 lines per file
   - Maximum: 800 lines (split if approaching)
   - Organize by feature/domain, not by type
   - When to split configuration files

3. **Configuration Patterns**
   - Layered configuration (base + local)
   - Safe defaults with environment variables
   - Modular Emacs configuration structure
   - Repository pattern for config access
   - Validation pattern for configs

4. **Anti-Patterns to Avoid**
   - Deep nesting (>4 levels)
   - Magic numbers without constants
   - Hardcoded paths
   - Large monolithic configs
   - Mutable operations without backups

#### 2. development-workflow.md (~120 lines)

**Purpose:** Define research-first workflow for configuration changes

**Sections:**

1. **Research First (Mandatory)**
   - Check existing implementations in oh-my-workspace
   - Review dotfiles repositories and GitHub for reference
   - Read package documentation
   - Understand dependencies

2. **Planning Phase**
   - Document the change (what, why, impact)
   - Create implementation plan (files, order, backup, rollback)
   - Use TodoWrite for complex changes
   - Track dependencies between steps

3. **Implementation Phase**
   - Backup before modifying (immutability principle)
   - Make changes incrementally
   - Test after each change
   - Document as you go
   - Follow immutability (create new versions, use symlinks)

4. **Verification Phase**
   - Syntax validation (bash -n, emacs --batch, python -m py_compile)
   - Functional testing (new terminal, restart Emacs, check symlinks)
   - Documentation check (README, inline comments, CLAUDE.md)

5. **Rollback Strategy**
   - Git rollback
   - Backup restoration
   - Symlink switch for versioned configs

6. **Config-Specific Workflows**
   - Adding new package
   - Modifying existing config
   - Merging upstream changes

#### 3. hooks.md (~100 lines)

**Purpose:** Define Claude Code hooks integration for configuration management

**Sections:**

1. **Hook Types**
   - PreToolUse (validation, parameter modification)
   - PostToolUse (auto-format, verification)
   - Stop (final verification)

2. **TodoWrite Best Practices**
   - When to use: multi-file updates, complex installs, breaking changes, refactoring
   - Benefits: reveals out of order steps, missing dependencies, extra changes, wrong granularity
   - Example usage for adding shell configuration

3. **Auto-Formatting Hooks**
   - PostToolUse syntax validation (bash -n, emacs --batch)
   - File size checks (warn if >800 lines)
   - Header presence verification

4. **Pre-Commit Hooks**
   - Verify all modified configs have valid syntax
   - Check for hardcoded secrets

5. **Session End Verification**
   - Verify all modified configs tested
   - Check for uncommitted critical files
   - Ensure README updated if needed

6. **Permission Management**
   - Auto-accept guidelines for config changes
   - Recommended allowedTools configuration

### Enhanced Files

#### 1. coding-style.md (+40 lines)

**Add after "Universal Rules" section:**

1. **Immutability Principle**
   - Always create new, never modify in-place
   - Examples for shell configs, Emacs configs
   - Rationale: rollback, debugging, side effects

2. **File Organization**
   - Many small files > few large files
   - Size targets and limits
   - When to split configuration files
   - Example: modular Emacs config structure

3. **Enhanced Code Quality Checklist**
   - Add: No mutation (immutable patterns used)
   - Add: File size is reasonable (split if >800 lines)

#### 2. ai-generation.md (+60 lines)

**Add after "Dotfiles-Specific Rules" section:**

1. **Configuration Code Standards**
   - **File Headers (MANDATORY):** Every config file must include standard header with location and usage
   - Emacs Lisp header format
   - Shell script header format
   - AI must ensure all generated configs include these headers

2. **Configuration Comments**
   - Explain WHY, not WHAT
   - Document dependencies
   - Explain non-obvious choices with references

3. **Configuration Code Style**
   - Modular over monolithic
   - Explicit over implicit
   - Version control friendly
   - Immutable patterns

4. **Enhanced Quality Checklist**
   - Add: Standard file header present
   - Add: Comments explain WHY, not WHAT
   - Add: Dependencies documented
   - Add: Non-obvious choices explained
   - Add: File size under 800 lines
   - Add: Modular structure
   - Add: No hardcoded machine-specific paths
   - Add: Backup created before modification

## Implementation Plan

### Phase 1: Create New Files (30 minutes)
1. Create `.claude/rules/patterns.md`
2. Create `.claude/rules/development-workflow.md`
3. Create `.claude/rules/hooks.md`

### Phase 2: Enhance Existing Files (20 minutes)
1. Enhance `.claude/rules/coding-style.md` with immutability and file organization
2. Enhance `.claude/rules/ai-generation.md` with configuration code standards

### Phase 3: Verification (10 minutes)
1. Verify all files follow project conventions
2. Check cross-references between files are correct
3. Ensure no conflicts with existing rules

## Success Criteria

1. ✅ All new files follow existing rule format and conventions
2. ✅ Enhanced files maintain backward compatibility
3. ✅ Immutability principle clearly defined and exemplified
4. ✅ File organization standards prevent config bloat
5. ✅ Development workflow ensures research-first approach
6. ✅ Hooks system enables tracking of complex changes
7. ✅ Configuration code has strong constraints on headers and comments
8. ✅ All rules are config-focused, not application-focused
9. ✅ Total additions ~470 lines (minimal overhead)

## Risks and Mitigations

**Risk:** New rules may feel restrictive for simple config changes
**Mitigation:** Rules emphasize when to apply (e.g., "for complex changes")

**Risk:** Immutability principle may slow down quick edits
**Mitigation:** Backup strategies are simple (cp file file.bak) and enable quick rollback

**Risk:** File size limits may require immediate refactoring of existing large configs
**Mitigation:** Rules are prospective - apply to new files, refactor existing files incrementally

**Risk:** Enhanced checklists may slow down workflow
**Mitigation:** Checklist items are quick boolean checks, not time-consuming tasks

## Alternatives Considered

### Alternative 1: Add Testing Rules
**Why rejected:** Project uses manual verification, no current test infrastructure

### Alternative 2: Add Performance Rules
**Why rejected:** Performance optimization not a priority for configuration work

### Alternative 3: Add Agent Orchestration Rules
**Why rejected:** User already proficient with agents, no need for formal rules

### Alternative 4: Full Restructuring (common + lang directories)
**Why rejected:** Too much overhead for current needs, keep existing structure

### Alternative 5: Copy Reference Rules Directly
**Why rejected:** Reference rules are application-focused, need adaptation for configuration context

## References

- Reference project: `~/MyProjects/everything-claude-code/rules/`
- Current rules: `.claude/rules/`
- Related specs:
  - `docs/superpowers/specs/2026-03-24-claude-rules-design.md`
