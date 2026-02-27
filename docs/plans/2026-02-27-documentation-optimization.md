# Documentation Optimization Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Improve documentation consistency, add cross-references, and fix factual inaccuracies across all markdown files.

**Architecture:** Conservative refresh - minimal changes to content, focus on linking and accuracy.

**Tech Stack:** Markdown, Git

---

## Task 1: Fix CLAUDE.md Script Names

**Files:**
- Modify: `CLAUDE.md:12-14`

**Step 1: Update incorrect script names**

Change lines 12-14 from:
```markdown
- **Claude Code Setup**: `./claudecode/claude-code-setup.sh` - Installs Node.js, Claude Code, and configures API settings with ZHIPU AI integration
- **Emacs Configuration**: `./emacs/install.sh` - Installs and configures Emacs with custom settings
- **Vim Configuration**: `./vim/install.sh` - Installs and configures Vim with custom settings
```

To:
```markdown
- **Claude Code Setup**: `./claudecode/claude-code-setup.sh` - Installs Node.js, Claude Code, and configures API settings with ZHIPU AI integration
- **Emacs Configuration**: `./emacs/setup.sh` - Installs and configures Emacs with custom settings
- **Vim Configuration**: `./vim/setup.sh` - Installs and configures Vim with custom settings
```

**Step 2: Verify changes**

Run: `grep -n "setup.sh\|install.sh" CLAUDE.md`
Expected: All three entries show `setup.sh`

**Step 3: Commit**

```bash
git add CLAUDE.md
git commit -m "docs(claude-md): fix script names to match actual files"
```

---

## Task 2: Fix CLAUDE.md Directory Structure

**Files:**
- Modify: `CLAUDE.md:29`

**Step 1: Fix README.org to README.md**

Change line 29 from:
```markdown
└── README.org           # Comprehensive documentation
```

To:
```markdown
└── README.md            # Comprehensive documentation
```

**Step 2: Verify changes**

Run: `grep -n "README" CLAUDE.md`
Expected: Shows `README.md` not `README.org`

**Step 3: Commit**

```bash
git add CLAUDE.md
git commit -m "docs(claude-md): fix documentation filename in directory structure"
```

---

## Task 3: Add TOC to CLAUDE.md

**Files:**
- Modify: `CLAUDE.md` (after line 4)

**Step 1: Add table of contents**

Insert after line 4 (after the description line):

```markdown

## Table of Contents

- [Project Overview](#project-overview)
- [Development Setup Commands](#development-setup-commands)
- [Architecture Overview](#architecture-overview)
- [Development Workflow](#development-workflow)
- [Configuration Philosophy](#configuration-philosophy)
- [Important Notes](#important-notes)
- [Related Files](#related-files)

```

**Step 2: Verify TOC renders correctly**

Run: `grep -n "## Table of Contents" CLAUDE.md`
Expected: Line appears after the header section

**Step 3: Commit**

```bash
git add CLAUDE.md
git commit -m "docs(claude-md): add table of contents"
```

---

## Task 4: Add Related Files Section to CLAUDE.md

**Files:**
- Modify: `CLAUDE.md` (at the end)

**Step 1: Add Related Files section**

Append at the end of CLAUDE.md:

```markdown

---

## Related Files

- [README.md](README.md) - Main project documentation with setup instructions
- [claudecode/README.md](claudecode/README.md) - Claude Code installation and configuration guide
```

**Step 2: Verify section added**

Run: `tail -10 CLAUDE.md`
Expected: Shows new "Related Files" section

**Step 3: Commit**

```bash
git add CLAUDE.md
git commit -m "docs(claude-md): add related files section"
```

---

## Task 5: Add Related Documentation Section to README.md

**Files:**
- Modify: `README.md` (at the end)

**Step 1: Add Related Documentation section**

Append at the end of README.md:

```markdown

---

## Related Documentation

- [CLAUDE.md](CLAUDE.md) - AI assistant guidance for working with this repository
- [claudecode/README.md](claudecode/README.md) - Detailed Claude Code setup guide (Chinese)
```

**Step 2: Verify section added**

Run: `tail -10 README.md`
Expected: Shows new "Related Documentation" section

**Step 3: Commit**

```bash
git add README.md
git commit -m "docs(readme): add related documentation section"
```

---

## Task 6: Add Back-link and Related Section to claudecode/README.md

**Files:**
- Modify: `claudecode/README.md` (at the beginning and end)

**Step 1: Add back-link at the top**

Insert after line 3 (after the title line):

```markdown

> **返回主文档**: [README.md](../README.md) - 项目主文档
```

**Step 2: Add Related Documentation section at the end**

Append at the end of claudecode/README.md:

```markdown

---

## 相关文档

- [README.md](../README.md) - 项目主文档
- [CLAUDE.md](../CLAUDE.md) - AI 助手指南
```

**Step 3: Verify changes**

Run: `head -10 claudecode/README.md && tail -10 claudecode/README.md`
Expected: Shows back-link at top and related section at bottom

**Step 4: Commit**

```bash
git add claudecode/README.md
git commit -m "docs(claudecode): add back-link and related documentation section"
```

---

## Task 7: Verify All Links Work

**Files:**
- None (verification only)

**Step 1: Check all markdown links**

Run: `grep -r "\[.*\](.*\.md)" --include="*.md" .`
Expected: All linked files exist

**Step 2: Manual verification**

Open each markdown file and verify:
- CLAUDE.md: TOC links work, Related Files links work
- README.md: Related Documentation links work
- claudecode/README.md: Back-link works, Related links work

**Step 3: Final status check**

Run: `git status`
Expected: All changes committed, working tree clean

---

## Summary

| Task | File | Change |
|------|------|--------|
| 1 | CLAUDE.md | Fix script names (install.sh → setup.sh) |
| 2 | CLAUDE.md | Fix README.org → README.md |
| 3 | CLAUDE.md | Add table of contents |
| 4 | CLAUDE.md | Add Related Files section |
| 5 | README.md | Add Related Documentation section |
| 6 | claudecode/README.md | Add back-link and Related section |
| 7 | - | Verify all links |

**Total commits:** 6
