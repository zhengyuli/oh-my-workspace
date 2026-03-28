---
# This file is NOT loaded by Claude Code (prefix _ prevents loading)
# It documents the shared structure for all lang rule files
---

# Language Rule Template

This template documents the unified structure for all language-specific rule files in `.claude/rules/lang/`.

## Required Structure

Every lang rule MUST follow this exact section order:

```markdown
---
globs:
  - "**/*.ext"
---

# [Language] Conventions

[One-line description of what this file covers]

## File Header

[Template with: location, usage, dependencies, references]

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...`
**Level 1** (Primary Section): `# -----------...`
**Level 2** (Subsection): `# --- Title ---`

## Code Patterns

### Comments
[WHY not WHAT principle - brief example]

### [Language-Specific Section]
[Language-specific patterns]

## Anti-Patterns

### Don't: [Pattern Name]
[Brief explanation + correct alternative]

## References

1. [Official Documentation](url)
2. [Style Guide](url)

## Validation

[Tool-specific validation commands]
```

## Section Guidelines

### File Header
- Include: location, usage, dependencies, references
- Keep under 20 lines

### Delimiter Hierarchy
- Identical across ALL files (consistent with coding-style.md)
- Use exact character counts (80, 40, inline style)

### Code Patterns
- Use imperative mood ("Use", "Do", "Never")
- Show ONE example per pattern (not multiple)
- Remove "CORRECT/WRONG" labels - show pattern directly

### Anti-Patterns
- List 2-4 critical "Don't" patterns
- Brief explanation + correct alternative
- Focus on common mistakes

### References
- Link to official documentation
- Link to authoritative style guide
- Maximum 2-3 links

### Validation
- Include tool-specific syntax check command
- Include lint command if available
