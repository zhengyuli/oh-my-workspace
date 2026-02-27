# Documentation Optimization Design

**Date:** 2026-02-27
**Approach:** Conservative Refresh
**Scope:** All markdown files (README.md, CLAUDE.md, claudecode/README.md)

## Overview

Lightweight cleanup focusing on consistency, cross-references, and accuracy without restructuring content.

## Files Affected

| File | Purpose |
|------|---------|
| `README.md` | Main project documentation |
| `CLAUDE.md` | AI assistant guidance |
| `claudecode/README.md` | Claude Code setup instructions |

## Changes

### 1. Cross-File Linking

Add bidirectional links between all documentation files:

- **README.md:** Add link to `claudecode/README.md` in Quick Start section; add "Related Documentation" section at the end
- **CLAUDE.md:** Add "Related Files" section linking to README.md and claudecode/README.md
- **claudecode/README.md:** Add link back to main README.md at the top; add "Related Documentation" section

### 2. Consistency Improvements

- **TOC:** Add TOC to CLAUDE.md (currently missing); verify existing TOCs in README.md and claudecode/README.md
- **Code blocks:** Standardize language tags (use `shell` consistently)
- **Separators:** Consistent `---` between major sections

### 3. Factual Accuracy Check

- Verify setup script names match actual files
- Check Emacs font installation command
- Verify directory structure in CLAUDE.md matches actual layout
- Verify model names are current

## Implementation Summary

| File | Changes |
|------|---------|
| `README.md` | Add "Related Documentation" section, verify script names, fix TOC links |
| `CLAUDE.md` | Add TOC, add "Related Files" section, verify structure |
| `claudecode/README.md` | Add back-link to README, add "Related Documentation" section |

**No changes to:** Content substance, file locations, or overall document structure.

## Deliverable

- 3 updated markdown files
- Commit message: `docs: add cross-references and improve consistency`
