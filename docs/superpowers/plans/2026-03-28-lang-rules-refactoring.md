# Lang Rules Refactoring Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Refactor `.claude/rules/lang/` directory to follow Claude Code community best practices with unified template structure.

**Architecture:** Each lang rule file is self-contained with consistent section ordering (File Header → Delimiter Hierarchy → Code Patterns → Anti-Patterns → References → Validation). Token reduction achieved through consolidation and explicit anti-patterns.

**Tech Stack:** Markdown, YAML frontmatter, Claude Code rules system

---

## Phase 0: Setup

### Task 0.1: Create Feature Branch

**Files:**
- N/A (git operation)

- [ ] **Step 1: Create and switch to feature branch**

```bash
git checkout -b refactor/lang-rules
```

Expected: Branch `refactor/lang-rules` created and checked out

### Task 0.2: Capture Baseline Metrics

**Files:**
- N/A (measurement)

- [ ] **Step 1: Count baseline lines in sections**

```bash
# Count lines in each file
wc -l .claude/rules/lang/{bash,zsh,elisp,config,toml,yaml}.md

# Count sections in each file
for f in .claude/rules/lang/*.md; do
  echo "$f: $(grep -c '^## ' $f) sections"
done
```

Expected: Baseline metrics recorded for comparison

- [ ] **Step 2: Verify no cross-references exist**

```bash
# Lang rules should be self-contained (no @import or file references)
grep -E "@|see" .claude/rules/lang/*.md
```

Expected: No output (files should be self-contained)

---

## Phase 1: Create Template Documentation

### Task 1.1: Create _template.md

**Files:**
- Create: `.claude/rules/lang/_template.md`

- [ ] **Step 1: Create the template documentation file**

```markdown
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
```

- [ ] **Step 2: Verify file is not loaded by Claude Code**

The `_` prefix prevents Claude Code from loading this file directly.

---

### Task 1.2: Update README.md

**Files:**
- Modify: `.claude/rules/README.md:29-38`

- [ ] **Step 1: Update the Language-Specific Rules table in README.md**

Replace the existing table with updated structure information:

```markdown
### Language-Specific Rules (conditionally loaded)

Located in the `lang/` subdirectory, these rules extend universal standards with language-specific features.

| Rule File | Globs | Description |
|-----------|-------|-------------|
| `lang/bash.md` | `**/*.sh`, `**/*.bash` | Bash shell scripting conventions |
| `lang/zsh.md` | `**/*.zsh`, `**/.zsh*`, `**/zshrc`, `**/zprofile`, `**/zshenv`, `**/zlogin` | Zsh shell scripting conventions |
| `lang/elisp.md` | `**/*.el` | Emacs Lisp conventions |
| `lang/config.md` | `**/config`, `**/*.conf`, `**/*.cfg`, `**/rc`, `**/.gitconfig`, `**/git/config` | Generic config files |
| `lang/toml.md` | `**/*.toml` | TOML configuration files |
| `lang/yaml.md` | `**/*.yml`, `**/*.yaml` | YAML configuration files |

**Unified Structure:** All lang rules follow a consistent 8-section template:
1. File Header
2. Delimiter Hierarchy
3. Code Patterns (with Comments subsection)
4. Language-Specific Patterns
5. Anti-Patterns
6. References
7. Validation

See `lang/_template.md` for the complete template structure.
```

- [ ] **Step 2: Commit template documentation**

```bash
git add .claude/rules/lang/_template.md .claude/rules/README.md
git commit -m "docs(rules): add unified template structure for lang rules

- Create _template.md documenting the shared structure
- Update README.md with unified structure documentation
- All lang rules will follow consistent 8-section template"
```

---

## Phase 2: Refactor Config File Rules

### Task 2.1: Refactor config.md

**Files:**
- Modify: `.claude/rules/lang/config.md`

- [ ] **Step 1: Read current config.md to understand existing content**

```bash
wc -l .claude/rules/lang/config.md
```

Expected: 156 lines

- [ ] **Step 2: Refactor config.md following unified template**

See full refactored content below:

```markdown
---
globs:
  - "**/config"
  - "**/*.conf"
  - "**/*.cfg"
  - "**/rc"
  - "**/.gitconfig"
  - "**/git/config"
---

# Configuration Files (No Extension)

Standards for configuration files without extensions (config, conf, rc).

## File Header

```
# filename -*- mode: xxx; -*-
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# Location: $WORKSPACE_DIR/path/to/file
# References:
#   1. Official documentation URL
# =============================================================================
```

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...`
**Level 1** (Primary Section): `# -----------...`
**Level 2** (Subsection): `# --- Title ---`

## Code Patterns

### Comments

Explain WHY, not WHAT. Use separate comment lines.

```conf
# Use delta for syntax-highlighted diffs (falls back to less)
pager = delta
```

### Value Types

- **Booleans**: `option = true` (not `"true"`)
- **Paths**: `~/.config/app/file` (XDG-compliant, not relative)
- **Includes**: `[include] path = config.local` (machine-specific overrides)

### Formatting

- Never align values with spaces
- Never use inline comments for explanations

## Anti-Patterns

### Don't: Inline Explanations

```conf
# WRONG
pager = delta  # syntax highlighting

# CORRECT
# Use delta for syntax-highlighted diffs
pager = delta
```

### Don't: Align Values

```conf
# WRONG
option1  = value1
option10 = value10

# CORRECT
option1 = value1
option10 = value10
```

## Security

### Secrets Management

Never commit sensitive data. Use split-file strategy:

```conf
# Main config (committed)
[core]
    editor = emacs
[include]
    path = ~/.config/git/config.local

# config.local (not committed, in .gitignore)
[user]
    email = your.email@company.com
```

**Sensitive types**: API keys, tokens, passwords, private keys, certificates

## References

1. [Git Configuration](https://git-scm.com/docs/git-config)
2. [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)

## Validation

```bash
# Git config
git config --list

# Ripgrep (no dedicated check - test a real search)
rg --version
```
```

- [ ] **Step 3: Verify line count reduction**

```bash
wc -l .claude/rules/lang/config.md
```

Expected: ~130 lines (16% reduction)

- [ ] **Step 4: Commit config.md refactor**

```bash
git add .claude/rules/lang/config.md
git commit -m "refactor(rules): unify config.md structure

- Add Anti-Patterns section with inline comments and alignment examples
- Add References section with Git and XDG docs
- Simplify Code Patterns section
- Remove redundant explanations
- Target: ~130 lines (16% reduction)"
```

---

### Task 2.2: Refactor toml.md

**Files:**
- Modify: `.claude/rules/lang/toml.md`

- [ ] **Step 1: Read current toml.md**

```bash
wc -l .claude/rules/lang/toml.md
```

Expected: 177 lines

- [ ] **Step 2: Refactor toml.md following unified template**

```markdown
---
globs:
  - "**/*.toml"
---

# TOML Configuration Files

Standards for TOML configuration files (starship, uv, bun, yazi, etc.).

## File Header

```toml
# filename.toml -*- mode: toml; -*-
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# Location: $WORKSPACE_DIR/path/to/file.toml
# References:
#   1. Official documentation URL
# =============================================================================
```

**Schema reference** (optional): `"$schema" = 'https://...'`

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...`
**Level 1** (Primary Section): `# -----------...`
**Level 2** (Subsection): `# --- Title ---`

## Code Patterns

### Comments

Explain WHY, not WHAT. Prefer separate lines over inline.

```toml
# Use Nerd Font symbols for better visual identification
symbol = " "
```

### Value Types

- **Strings**: Always quoted `"value"`
- **Numbers**: `timeout = 30` (int), `ratio = 0.8` (float)
- **Booleans**: `enabled = true` (not `"true"`)
- **Arrays**: `items = ["a", "b"]` or multiline
- **Tables**: `[section]` or inline `{ x = 1 }`

### Formatting

- Never align values with spaces
- Prefer separate-line comments over inline

## Anti-Patterns

### Don't: Unquoted Strings

```toml
# WRONG
name = value

# CORRECT
name = "value"
```

### Don't: Inline Explanations

```toml
# WRONG
symbol = " "  # Python icon

# CORRECT
# Python language icon
symbol = " "
```

## Security

### Secrets Management

Never hardcode sensitive data. Use split-file strategy:

```toml
# config.toml (committed)
[api]
# Set key in config.local.toml or environment

# config.local.toml (not committed, in .gitignore)
[api]
key = "sk-1234567890"
```

Add to `.gitignore`: `*.local.toml`, `*_secret.toml`

## References

1. [TOML Specification](https://toml.io/en/)
2. [TOML GitHub Wiki](https://github.com/toml-lang/toml/wiki)

## Validation

```bash
# Python validator
python3 -c "import toml; toml.load('config.toml')"

# Tool-specific
starship config --help
```
```

- [ ] **Step 3: Verify line count**

```bash
wc -l .claude/rules/lang/toml.md
```

Expected: ~130 lines (26% reduction)

- [ ] **Step 4: Commit toml.md refactor**

```bash
git add .claude/rules/lang/toml.md
git commit -m "refactor(rules): unify toml.md structure

- Add Anti-Patterns section with unquoted strings and inline examples
- Add References section with TOML spec and wiki
- Simplify Value Types and Formatting sections
- Remove redundant examples
- Target: ~130 lines (26% reduction)"
```

---

### Task 2.3: Refactor yaml.md

**Files:**
- Modify: `.claude/rules/lang/yaml.md`

- [ ] **Step 1: Read current yaml.md**

```bash
wc -l .claude/rules/lang/yaml.md
```

Expected: 173 lines

- [ ] **Step 2: Refactor yaml.md following unified template**

```markdown
---
globs:
  - "**/*.yml"
  - "**/*.yaml"
---

# YAML Configuration Files

Standards for YAML configuration files (lazygit, GitHub Actions, etc.).

## File Header

```yaml
# filename.yml -*- mode: yaml; -*-
# Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Title - Brief description
#
# Location: $WORKSPACE_DIR/path/to/file.yml
# References:
#   1. Official documentation URL
# =============================================================================
```

**Schema reference** (optional): `# yaml-language-server: $schema=https://...`

## Delimiter Hierarchy

**Level 0** (File Header): `# ============...`
**Level 1** (Primary Section): `# -----------...`
**Level 2** (Subsection): `# --- Title ---`

## Code Patterns

### Comments

Explain WHY, not WHAT. Prefer separate lines; inline acceptable for brief notes only.

```yaml
# Catppuccin Mocha Blue theme for visual consistency
theme:
  activeBorderColor:
    - '#89b4fa'
```

### Value Types

- **Strings**: Quote strings containing special characters
- **Booleans**: `enabled: true` (not `"true"`)
- **Lists**: `colors: [red, green]` or multiline
- **Maps**: Nested structure or inline `{x: 1, y: 2}`

### Formatting

- 2-space indentation (never tabs)
- Never align values with spaces

## Anti-Patterns

### Don't: Unquoted Special Characters

```yaml
# WRONG
message: error: file not found
pattern: *.txt

# CORRECT
message: "error: file not found"
pattern: "*.txt"
```

### Don't: Aligned Values

```yaml
# WRONG
nerdFontsVersion: "3"
showFileTree:     true

# CORRECT
nerdFontsVersion: "3"
showFileTree: true
```

## Security

### Secrets Management

Never hardcode sensitive data. Use split-file strategy:

```yaml
# config.yml (committed)
api:
  key: ""  # Set in config.local.yml

# config.local.yml (not committed, in .gitignore)
api:
  key: "sk-1234567890"
```

Add to `.gitignore`: `*.local.yml`, `*.local.yaml`, `*_secret.yml`

## References

1. [YAML Specification](https://yaml.org/spec/)
2. [YAMLlint Documentation](https://yamllint.readthedocs.io/)

## Validation

```bash
# Python validator
python3 -c "import yaml; yaml.safe_load(open('config.yml'))"

# Yamllint (if installed)
yamllint config.yml
```
```

- [ ] **Step 3: Verify line count**

```bash
wc -l .claude/rules/lang/yaml.md
```

Expected: ~130 lines (24% reduction)

- [ ] **Step 4: Commit yaml.md refactor**

```bash
git add .claude/rules/lang/yaml.md
git commit -m "refactor(rules): unify yaml.md structure

- Add Anti-Patterns section with special characters and alignment examples
- Add References section with YAML spec and yamllint docs
- Simplify Value Types and Formatting sections
- Remove redundant examples
- Target: ~130 lines (24% reduction)"
```

---

## Phase 3: Refactor Shell Rules

### Task 3.1: Refactor bash.md

**Files:**
- Modify: `.claude/rules/lang/bash.md`

- [ ] **Step 1: Read current bash.md**

```bash
wc -l .claude/rules/lang/bash.md
```

Expected: 345 lines

- [ ] **Step 2: Refactor bash.md following unified template**

Key changes:
- Keep: File Header, Delimiter Hierarchy, Security (code injection, secrets, permissions)
- Keep: Functions (single responsibility, parameter validation, main pattern)
- Simplify: Error Handling (keep strict mode, remove verbose explanations)
- Simplify: Documentation & Code Patterns → Code Patterns with Comments subsection
- Add: Anti-Patterns section (eval, local command substitution, short-circuit operators)
- Add: References section (Google Shell Style Guide, Bash Manual)
- Remove: Redundant CORRECT/WRONG examples (keep one per pattern)
- Remove: Parameter Handling section (merge into Functions)

Target: ~200 lines (42% reduction)

- [ ] **Step 3: Commit bash.md refactor**

```bash
git add .claude/rules/lang/bash.md
git commit -m "refactor(rules): unify bash.md structure

- Add Anti-Patterns section (eval, local masking, short-circuit operators)
- Add References section (Google Shell Style Guide, Bash Manual)
- Merge Parameter Handling into Functions section
- Simplify Error Handling (keep strict mode essentials)
- Remove redundant examples (keep one per pattern)
- Target: ~200 lines (42% reduction)"
```

---

### Task 3.2: Refactor zsh.md

**Files:**
- Modify: `.claude/rules/lang/zsh.md`

- [ ] **Step 1: Read current zsh.md**

```bash
wc -l .claude/rules/lang/zsh.md
```

Expected: 340 lines

- [ ] **Step 2: Refactor zsh.md following unified template**

Key changes (same as bash.md):
- Keep: File Header, Delimiter Hierarchy, Security, Functions
- Keep: Zsh-specific (print built-in, interactive .zshrc note)
- Simplify: Error Handling, Documentation & Code Patterns
- Add: Anti-Patterns section
- Add: References section
- Remove: Redundant examples

Target: ~200 lines (41% reduction)

- [ ] **Step 3: Commit zsh.md refactor**

```bash
git add .claude/rules/lang/zsh.md
git commit -m "refactor(rules): unify zsh.md structure

- Add Anti-Patterns section (eval, local masking, short-circuit operators)
- Add References section (Zsh Manual, Google Shell Style Guide)
- Keep Zsh-specific patterns (print built-in, interactive .zshrc note)
- Simplify Error Handling and Code Patterns sections
- Remove redundant examples
- Target: ~200 lines (41% reduction)"
```

---

### Task 3.3: Refactor elisp.md

**Files:**
- Modify: `.claude/rules/lang/elisp.md`

- [ ] **Step 1: Read current elisp.md**

```bash
wc -l .claude/rules/lang/elisp.md
```

Expected: 361 lines

- [ ] **Step 2: Refactor elisp.md following unified template**

Key changes:
- Keep: File Header (with full MIT license), Delimiter Hierarchy
- Keep: use-package keyword order (essential reference)
- Keep: Security (package sources, bytecompile artifacts)
- Simplify: Documentation & Code Patterns → Code Patterns
- Simplify: Functions (merge interactive, parameter validation)
- Add: Anti-Patterns section (mutating shared lists, raw key strings, inline comments)
- Add: References section (Emacs Lisp Manual, use-package docs)
- Remove: Verbose docstring examples (keep pattern, not full examples)

Target: ~220 lines (39% reduction)

- [ ] **Step 3: Commit elisp.md refactor**

```bash
git add .claude/rules/lang/elisp.md
git commit -m "refactor(rules): unify elisp.md structure

- Add Anti-Patterns section (mutating lists, raw key strings, inline comments)
- Add References section (Emacs Lisp Manual, use-package docs)
- Keep use-package keyword order reference (essential)
- Simplify Functions and Code Patterns sections
- Remove verbose docstring examples
- Target: ~220 lines (39% reduction)"
```

---

## Phase 4: Update Project Documentation

### Task 4.1: Update coding-style.md

**Files:**
- Modify: `.claude/rules/coding-style.md`

- [ ] **Step 1: Add reference to lang rule structure**

Add to the Language-Specific Extensions section:

```markdown
### Language-Specific Structure

All lang rules follow a unified 8-section template:
1. **File Header** - Location, usage, dependencies
2. **Delimiter Hierarchy** - Consistent with this file
3. **Code Patterns / Comments** - WHY not WHAT
4. **Language-Specific Patterns** - Syntax, naming, formatting
5. **Anti-Patterns** - Explicit "Don't" list
6. **Security** - Language-specific security concerns
7. **References** - Links to official docs
8. **Validation** - Tool-specific commands

See `.claude/rules/lang/_template.md` for the complete template.
```

- [ ] **Step 2: Commit coding-style.md update**

```bash
git add .claude/rules/coding-style.md
git commit -m "docs(rules): add lang rule structure reference to coding-style

- Document unified 8-section template for lang rules
- Reference _template.md for complete structure
- Clarify relationship between universal and lang rules"
```

---

## Phase 5: Verification

### Task 5.1: Verify Structure Consistency

- [ ] **Step 1: Verify all files have required sections**

```bash
for f in .claude/rules/lang/{bash,zsh,elisp,config,toml,yaml}.md; do
  echo "=== $f ==="
  grep -E "^## " "$f" | head -10
done
```

Expected: All files show same section structure (File Header, Delimiter Hierarchy, Code Patterns, Anti-Patterns, References, Validation)

- [ ] **Step 2: Verify glob patterns unchanged**

```bash
for f in .claude/rules/lang/{bash,zsh,elisp,config,toml,yaml}.md; do
  echo "=== $f ==="
  head -5 "$f" | grep -A10 "globs:"
done
```

Expected: All glob patterns match original spec

### Task 5.2: Verify Line Reduction

- [ ] **Step 1: Count total lines**

```bash
wc -l .claude/rules/lang/{bash,zsh,elisp,config,toml,yaml}.md
```

Expected: ~1,100 lines total (29% reduction from 1,552)

### Task 5.3: Verify Information Preservation

- [ ] **Step 1: Security patterns preserved**

```bash
grep -l "code injection\|secrets management\|Security" .claude/rules/lang/*.md
```

Expected: All shell files and config files have security sections

- [ ] **Step 2: Validation commands preserved**

```bash
grep -l "Validation" .claude/rules/lang/*.md
```

Expected: All files have Validation section

- [ ] **Step 3: References added**

```bash
grep -l "References" .claude/rules/lang/*.md
```

Expected: All files have References section

### Task 5.4: Functional Testing

- [ ] **Step 1: Create test files for each glob pattern**

```bash
# Create temporary test files
touch /tmp/test-bash.sh
touch /tmp/test-zsh.zsh
touch /tmp/test-config.conf
touch /tmp/test-toml.toml
touch /tmp/test-yaml.yml
touch /tmp/test-elisp.el
```

Expected: Test files created

- [ ] **Step 2: Verify Claude Code loads correct rules (manual check)**

Open each test file in an editor and verify the correct lang rule is loaded. This is a manual verification step.

### Task 5.5: Success Criteria Checklist

- [ ] **All 6 lang rules follow unified template structure**
- [ ] **Each file includes Anti-Patterns and References sections**
- [ ] **Line reduction of 25-30% achieved (measured by `wc -l`)**
- [ ] **No loss of essential information (security patterns, validation commands preserved)**
- [ ] **README.md accurately documents new structure**
- [ ] **All glob patterns unchanged (no breaking changes)**
- [ ] **Each refactored file committed atomically (easy rollback)**

---

## Phase 6: Final Commit

### Task 6.1: Merge to Master

- [ ] **Step 1: Review all commits**

```bash
git log --oneline master..HEAD
```

Expected: 8 commits (template + 6 refactors + coding-style update)

- [ ] **Step 2: Merge to master**

```bash
git checkout master
git merge --no-ff refactor/lang-rules -m "refactor(rules): unify lang rules structure

- Create _template.md documenting unified structure
- Refactor all 6 lang rules to follow 8-section template
- Add Anti-Patterns and References sections to all files
- Update README.md and coding-style.md with new structure
- Target: ~1,100 lines (29% reduction from 1,552)

BREAKING CHANGE: None (glob patterns unchanged)"
```

---

## Summary

| Metric | Before | After | Change |
|--------|--------|-------|--------|
| Total lines | 1,552 | 1,100 | -29% |
| bash.md | 345 | 200 | -42% |
| zsh.md | 340 | 200 | -41% |
| elisp.md | 361 | 220 | -39% |
| config.md | 156 | 130 | -16% |
| toml.md | 177 | 130 | -26% |
| yaml.md | 173 | 130 | -24% |

**Commits:** 8 atomic commits (easy rollback)
**Risk:** Low (glob patterns unchanged, self-contained files)
