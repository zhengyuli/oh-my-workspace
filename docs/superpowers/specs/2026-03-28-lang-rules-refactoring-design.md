# Lang Rules Refactoring Design Specification

## Overview

Refactor `.claude/rules/lang/` directory to follow Claude Code community best practices for better AI instruction compliance and reduced token usage.

## Goals

1. **Improve AI compliance** - Follow community best practices for better instruction following
2. **Reduce token usage** - Target 15-20% reduction through consolidation and anti-patterns
3. **Unified structure** - Consistent section ordering across all lang rules
4. **Better maintainability** - Clear template for adding new language rules

## References

### Community Best Practices
1. [Claude Code Documentation - Custom Instructions](https://docs.anthropic.com/en/docs/claude-code/custom-instructions)
2. [Anthropic Guide to Claude Code Best Practices](https://www.anthropic.com/engineering/claude-code-best-practices)
3. [HumanLayer: Writing a Good CLAUDE.md](https://www.humanlayer.dev/blog/writing-a-good-claude-md)

## Current State Analysis

### Existing Files

| File | Lines | Issues |
|------|------|--------|
| `bash.md` | 345 | Redundant explanations, verbose examples |
| `zsh.md` | 340 | Nearly identical to bash.md, redundant content |
| `elisp.md` | 361 | Unique structure, verbose docstring examples |
| `config.md` | 156 | Minimal structure, missing anti-patterns |
| `toml.md` | 177 | Minimal structure. missing anti-patterns |
| `yaml.md` | 173 | Minimal structure. missing anti-patterns |

**Total: ~1,552 lines**

### Problems Identified

1. **Inconsistent section naming** - "Documentation & Code Patterns" vs "Code Patterns"
2. **Missing Anti-Patterns** - Explicit "Don't" sections improve compliance
3. **Missing References** - No links to official documentation
4. **Redundant explanations** - Multiple examples showing same concept
5. **Verbose comments** - Explanations for obvious patterns

### Dependency Analysis

**Universal Rules (already cover these topics):**
- `coding-style.md`: Line length (80 chars), file headers, documentation requirements
- `patterns.md`: Immutability principle, file organization, design patterns

**Lang Rules should NOT duplicate:**
- Line length requirements (reference coding-style.md)
- General immutability principle (reference patterns.md)
- General error handling philosophy (reference patterns.md)

**Lang Rules SHOULD include:**
- Language-specific syntax patterns
- Language-specific security concerns (e.g., code injection in shell)
- Language-specific validation commands
- Language-specific naming conventions
- Tool-specific configuration patterns

## Design

### Approach: Unified Template with Context-Specific Extensions

Each lang rule file is self-contained with consistent structure, optimized for AI comprehension.

### Directory Structure

```
.claude/rules/lang/
├── _template.md          # Shared structure (not loaded directly)
├── bash.md              # Bash-specific rules (~200 lines)
├── zsh.md              # Zsh-specific rules (~200 lines)
├── elisp.md            # Emacs Lisp rules (~220 lines)
├── config.md            # Config files (~130 lines)
├── toml.md              # TOML configs (~130 lines)
└── yaml.md             # YAML configs (~130 lines)
```

**Target: ~1,100 lines total (29% reduction)**

### Glob Pattern Strategy

**Current Patterns:**
| File | Globs | Overlap? |
|------|-------|----------|
| `bash.md` | `**/*.sh`, `**/*.bash` | No |
| `zsh.md` | `**/*.zsh`, `**/.zsh*`, `**/zshrc`, etc. | No |
| `elisp.md` | `**/*.el` | No |
| `config.md` | `**/config`, `**/*.conf`, etc. | No |
| `toml.md` | `**/*.toml` | No |
| `yaml.md` | `**/*.yml`, `**/*.yaml` | No |

**Decision:** Keep existing glob patterns unchanged. No overlaps detected.

### Unified Template Structure

Every lang rule follows this exact section order:

```markdown
---
globs:
  - "**/*.ext"
---

# [Language] Conventions

[Brief description - one line]

## File Header

[Template + explanation]

## Delimiter Hierarchy

[Standard delimiter levels - identical across all files]

## Code Patterns

### Comments
[WHY not WHAT principle]

### [Language-Specific Section 1]
[e.g., Naming Conventions, Formatting, etc.]

### [Language-Specific Section 2]
[e.g., Functions, Security, etc.]

## Anti-Patterns

### Don't: [Pattern Name]
[Brief explanation + correct alternative]

## References

1. [Official Documentation](url)
2. [Style Guide](url)

## Validation

[Tool-specific validation commands]
```

### Section Details

#### Required Sections (All Files)

1. **File Header** - Standard format with location, usage, dependencies
2. **Delimiter Hierarchy** - Consistent with `coding-style.md`
3. **Code Patterns / Comments** - WHY not WHAT principle
4. **Anti-Patterns** - Explicit "Don't" list
5. **References** - Links to authoritative documentation
6. **Validation** - Tool-specific validation commands

#### Language-Specific Sections

**Shell (bash.md, zsh.md):**
- Naming Conventions (UPPER_SNAKE_CASE vs lower_snake_case)
- Formatting (indentation, line length, pipelines)
- Functions (single responsibility, parameter validation)
- Security (code injection, secrets management)

**Elisp (elisp.md):**
- Naming (kebab-case, package prefix, predicates)
- Functions (docstrings, interactive, parameter validation)
- use-package (keyword order, lazy loading)
- Formatting (semicolon convention, alignment)

**Config Files (config.md, toml.md, yaml.md):**
- Value Types (strings, booleans, arrays)
- Formatting (indentation, alignment)
- Security (secrets management)

### Content Refactoring Guidelines

#### What to Keep

- File header templates (project-specific)
- Delimiter hierarchy (consistent with `coding-style.md`)
- Security patterns (secrets management, injection prevention)
- Validation commands (essential for quality)
- Language-specific best practices

#### What to Remove

- Redundant "CORRECT/WRONG" explanations (keep one example, show pattern)
- Duplicate explanations across similar sections
- Overly verbose comments
- Repeated content from official docs

#### What to Add

- **Anti-Patterns section** with explicit "Don't" list
- **References section** linking to official documentation
- **Brief header description** explaining WHY this file exists

#### Writing Style

- Prefer imperative mood ("Use", "Do", "Never")
- Avoid "CORRECT" label - show example directly
- Keep line length under 80 chars in rule files

## Implementation Plan

### Rollback Strategy

1. **Create a git branch before starting:** `git checkout -b refactor/lang-rules`
2. **Commit after each file is refactored** (atomic commits for easy rollback)
3. **If issues found:** `git revert <commit>` or `git checkout master -- .claude/rules/lang/`
4. **Test plan for each rollback:** Verify original files load correctly by restarting Claude Code session

### Phase 1: Create Template Documentation

1. Create `_template.md` (not loaded directly, documents the shared pattern)
2. Document the unified structure in `README.md`

### Phase 2: Refactor Each Rule File

Refactor in order of complexity:

1. `config.md` - Simplest, serves as template
2. `toml.md` - Build on config.md patterns
3. `yaml.md` - Build on config.md patterns
4. `bash.md` - Add Anti-Patterns, References
5. `zsh.md` - Add Anti-Patterns, References
6. `elisp.md` - Add Anti-Patterns, References

### Phase 3: Update Project Documentation

1. Update `README.md` with new structure
2. Update `coding-style.md` to reference new lang rule structure

### Phase 4: Verification

#### Syntax Validation
- Verify no malformed markdown in any rule file
- Verify YAML frontmatter is valid in each file
- Run `bash -n` on shell rule examples

#### Functional Testing
1. Create test files matching each glob pattern
2. Verify Claude Code loads correct rules when editing test files
3. Confirm no rule conflicts between lang rules and universal rules

#### Token Measurement
1. Count tokens before refactoring (baseline)
2. Count tokens after refactoring
3. Verify 15-20% reduction achieved
4. Use Claude Code's `/context` command to measure context usage

#### Information Preservation Checklist
- [ ] All security patterns preserved (code injection, secrets management)
- [ ] All validation commands preserved
- [ ] No loss of language-specific guidance
- [ ] References to official docs added

## Expected Outcomes

### Token Efficiency

- **Before**: ~1,552 lines across 6 files
- **After**: ~1,100 lines across 6 files
- **Line reduction**: ~29%
- **Expected token reduction**: 15-20% (conservative estimate)

**Note:** Line count != token count. Token reduction depends on content density. Conservative estimate based on removing redundant explanations while keeping essential patterns.

### AI Compliance Improvements
- Explicit anti-patterns improve instruction following
- References section provides authoritative backup
- Unified structure reduces cognitive load

### Maintainability Improvements
- Clear template for adding new languages
- Consistent section ordering across all files
- Self-contained files with no dependencies

## Success Criteria

1. All 6 lang rules follow unified template structure
2. Each file includes Anti-Patterns and References sections
3. Line reduction of 25-30% achieved (measured by `wc -l`)
4. No loss of essential information (verified by checklist)
5. `README.md` accurately documents new structure
6. All glob patterns unchanged (no breaking changes)
7. Each refactored file committed atomically (easy rollback)
