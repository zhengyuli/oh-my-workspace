# elisp.md Rule Refinement Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Refine `.claude/rules/lang/elisp.md` to serve as the structural template for all language rule files, improving content organization and description style.

**Architecture:** Structural changes only — no logic modifications. Transform bullet-point sections into proper subheading structures, remove ambiguous character count descriptions, and standardize mandatory markers.

**Tech Stack:** Markdown documentation, Git for version control

---

> **IMPORTANT: Line Number Reference**
>
> All line numbers in this plan refer to the **original file state** before any changes. As you make changes sequentially, line numbers will shift. Use section headers (e.g., search for `^## Security`) as anchors rather than relying solely on line numbers.

---

**Single file modification:**
- Modify: `.claude/rules/lang/elisp.md`

This is a pure documentation update. The file contains:
1. YAML frontmatter (globs)
2. Section headers (##)
3. Code examples (elisp, bash)
4. CORRECT/WRONG comparison blocks

No new files will be created.

---

## Task 1: Remove (MANDATORY) Suffixes from Section Headers

**Files:**
- Modify: `.claude/rules/lang/elisp.md:10` (line 10)
- Modify: `.claude/rules/lang/elisp.md:65` (line 65)

- [ ] **Step 1: Update File Header section**

Find line 10:
```markdown
## File Header (MANDATORY)
```

Replace with:
```markdown
## File Header
```

- [ ] **Step 2: Update Delimiter Hierarchy section**

Find line 65:
```markdown
## Delimiter Hierarchy (MANDATORY)
```

Replace with:
```markdown
## Delimiter Hierarchy
```

- [ ] **Step 3: Verify changes**

Read the file and confirm both `(MANDATORY)` suffixes are removed from section headers.

- [ ] **Step 4: Intermediate verification**

```bash
grep -n "(MANDATORY)" .claude/rules/lang/elisp.md
```

Expected: No matches (all `(MANDATORY)` suffixes removed)

---

## Task 2: Fix Delimiter Hierarchy Description

**Files:**
- Modify: `.claude/rules/lang/elisp.md:67-68` (lines 67-68)

- [ ] **Step 1: Replace ambiguous character count notation**

Find lines 67-68:
```markdown
**Level 0** (File Header): `;; ===` × 76 (79 chars total)
**Level 1** (Primary Section): `;; ---` × 76 (79 chars total)
```

Replace with:
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

- [ ] **Step 2: Verify change**

Confirm the visual examples are clear and character count notation is removed.

> **Note**: For complete transformation details, see spec Change 7 at
> `docs/superpowers/specs/2026-03-27-elisp-rule-refinement-design.md`

---

## Task 3: Restructure Security Chapter

**Files:**
- Modify: `.claude/rules/lang/elisp.md:318-341` (lines 318-341)

- [ ] **Step 1: Transform bullet points to subheadings**

Find the Security chapter starting at line 318 and transform:

Current structure:
```markdown
## Security

**Package sources**: Only install packages...

**Local overrides**: Keep machine-specific...

**Stale `.elc` files**: Never commit...
```

Replace with:
```markdown
## Security

### Package Sources

Only install packages from GNU ELPA or MELPA Stable; never from unverified
third-party repositories.

### Local Overrides

Keep machine-specific or sensitive settings in `omw-local.el`, excluded from
version control via `.gitignore`:

```elisp
;; Main config (committed)
(setq user-full-name "Your Name")

;; Local overrides (not committed, in .gitignore)
;; File: ~/.config/emacs/lisp/omw-local.el
;; (setq user-mail-address "your.email@company.com")
;; (setq smtpmail-smtp-server "smtp.company.com")

;; Load local overrides if available
(require 'omw-local nil t)
```

### Bytecompile Artifacts

Never commit `.elc` files; always delete them before committing.
```

- [ ] **Step 2: Verify change**

Confirm Security chapter has proper subheadings and "Stale `.elc` files" was renamed to "Bytecompile Artifacts".

> **Note**: For complete transformation details, see spec Change 1 at
> `docs/superpowers/specs/2026-03-27-elisp-rule-refinement-design.md`

---

## Task 4: Restructure Validation Chapter

**Files:**
- Modify: `.claude/rules/lang/elisp.md:342-362` (lines 342-362)

- [ ] **Step 1: Separate into two sub-sections**

Find the Validation chapter starting at line 342 and transform:

Current structure:
```markdown
## Validation

**Byte compilation**: Zero warnings required...

**Before committing**: After verification passes, delete all `.elc` files:
```

Replace with:
```markdown
## Validation

### Byte Compilation

Zero warnings required — treat every warning as an error:

```bash
# Single file
emacs --batch -f batch-byte-compile omw-module.el

# Directory
emacs --batch --eval \
  "(byte-recompile-directory \"~/.config/emacs/lisp\" 0)"
```

### Pre-Commit Workflow

After verification passes, delete all `.elc` files before committing:

```bash
find emacs/ -name '*.elc' -delete
```
```

- [ ] **Step 2: Verify change**

Confirm Validation chapter has two subheadings and "Before committing" was renamed to "Pre-Commit Workflow".

> **Note**: For complete transformation details, see spec Change 2 at
> `docs/superpowers/specs/2026-03-27-elisp-rule-refinement-design.md`

---

## Task 5: Restructure Functions Chapter

**Files:**
- Modify: `.claude/rules/lang/elisp.md:276-316` (lines 276-316)

- [ ] **Step 1: Transform bold prefixes to subheadings**

Find the Functions chapter starting at line 276 and transform:

Current structure:
```markdown
## Functions

**Single responsibility**: Each function does exactly one thing.

**`interactive` declaration**: All user-facing commands...
```

Replace with:
```markdown
## Functions

### Single Responsibility

Each function does exactly one thing.

### Interactive Declaration

All user-facing commands (key-bound or M-x callable) must declare
`(interactive)`; internal helpers must not:

```elisp
;; CORRECT — user command
(defun omw/jump-to-matched-paren ()
  "Jump to the matched delimiter at point."
  (interactive)
  ...)

;; CORRECT — internal helper
(defun omw-find-config (name)
  "Return path to config NAME, or nil if not found."
  ...)
```

### Parameter Validation

Validate all required parameters at the start of the function body:

```elisp
(defun omw-load-module (name)
  "Load module with NAME."
  (unless (stringp name)
    (error "omw-load-module: NAME must be a string, got %S" name))
  ...)
```

### Optional Dependency Guard

Always check existence before calling into optional features:

```elisp
(when (featurep 'magit)
  (magit-auto-revert-mode -1))

(when (fboundp 'eglot-ensure)
  (eglot-ensure))
```
```

- [ ] **Step 2: Verify change**

Confirm Functions chapter has proper `###` subheadings for each rule.

> **Note**: For complete transformation details, see spec Change 3 at
> `docs/superpowers/specs/2026-03-27-elisp-rule-refinement-design.md`

---

## Task 6: Restructure Documentation & Code Patterns Chapter

**Files:**
- Modify: `.claude/rules/lang/elisp.md:132-274` (lines 132-274, original)

- [ ] **Step 1: Transform bold prefixes to subheadings**

Find the Documentation & Code Patterns chapter starting at line 132 (original).

Transform the following bold prefixes into `###` subheadings:
- `**Comments:**` → `### Comments`
- `**Docstrings**:` → `### Docstrings`
- `**Naming:**` → `### Naming`
- `**Buffer-local variables**:` → `### Buffer-local Variables`
- `**Key binding syntax**:` → `### Key Binding Syntax`
- `**Immutability**:` → `### Immutability`
- `**use-package (MANDATORY)**:` → `### use-package Declaration` (note: removes `(MANDATORY)` suffix)
- `**Lazy loading**:` → `### Lazy Loading`
- `**Formatting:**` → `### Formatting`

Example transformations:
```markdown
### Comments

Explain rationale (WHY), not mechanics (WHAT). Document non-obvious
decisions and constraints. Use separate comment lines; never inline
explanations.

### Docstrings

Every public function requires a docstring. First line must be a complete
sentence; document all parameters and return value.

### Naming

- kebab-case only: `omw-buffer-empty-p`, never `omwBufferEmptyP`
- All symbols must carry the `omw-` package prefix
- Predicate names must end with `-p`

### use-package Declaration

All package configuration must use `use-package`. Follow this keyword
order within each declaration:
```

> **Note**: For complete transformation details, see spec Change 4 at
> `docs/superpowers/specs/2026-03-27-elisp-rule-refinement-design.md`

- [ ] **Step 2: Verify change**

Confirm all bold prefixes are converted to proper `###` subheadings.

---

## Task 7: Final Verification

**Files:**
- Verify: `.claude/rules/lang/elisp.md`

- [ ] **Step 1: Read the entire file**

```bash
cat .claude/rules/lang/elisp.md
```

- [ ] **Step 2: Check git diff**

```bash
git diff .claude/rules/lang/elisp.md
```

Expected changes (search for section headers to locate):
- File Header section: `(MANDATORY)` removed from heading
- Delimiter Hierarchy section: `(MANDATORY)` removed, visual examples replace character counts
- Security chapter (near end of file): Has `### Package Sources`, `### Local Overrides`, `### Bytecompile Artifacts`
- Validation chapter (final chapter): Has `### Byte Compilation`, `### Pre-Commit Workflow`
- Functions chapter: Has `### Single Responsibility`, `### Interactive Declaration`, `### Parameter Validation`, `### Optional Dependency Guard`
- Documentation & Code Patterns chapter: Has `### Comments`, `### Docstrings`, `### Naming`, `### Buffer-local Variables`, `### Key Binding Syntax`, `### Immutability`, `### use-package Declaration`, `### Lazy Loading`, `### Formatting`

- [ ] **Step 3: Validate markdown syntax**

Ensure all code blocks are properly closed and headers use correct `##` / `###` syntax.

---

## Task 8: Commit Changes

**Files:**
- Commit: `.claude/rules/lang/elisp.md`

- [ ] **Step 1: Stage the file**

```bash
git add .claude/rules/lang/elisp.md
```

- [ ] **Step 2: Commit with conventional commit message**

```bash
git commit -m "$(cat <<'EOF'
style(rules): restructure elisp.md as template for language rules

Major structural improvements:
- Remove (MANDATORY) suffixes from section headers
- Replace character count notation with visual examples in Delimiter Hierarchy
- Convert Security chapter bullets to ### subheadings
- Split Validation into Byte Compilation and Pre-Commit Workflow sections
- Convert Functions chapter bold prefixes to ### subheadings
- Convert Documentation & Code Patterns bold prefixes to ### subheadings

These changes establish elisp.md as the structural template for all
language rule files in the project.

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
EOF
)"
```

- [ ] **Step 3: Verify commit**

```bash
git log -1 --stat
```

Expected: The commit should show only `.claude/rules/lang/elisp.md` modified with appropriate line changes.

---

## Success Criteria

After implementation, verify:

- [ ] All `(MANDATORY)` suffixes removed from section headers
- [ ] Delimiter Hierarchy uses visual examples, not character counts
- [ ] Security chapter has `### Package Sources`, `### Local Overrides`, `### Bytecompile Artifacts`
- [ ] Validation chapter has `### Byte Compilation`, `### Pre-Commit Workflow`
- [ ] Functions chapter has `### Single Responsibility`, `### Interactive Declaration`, `### Parameter Validation`, `### Optional Dependency Guard`
- [ ] Documentation & Code Patterns has proper `###` subheadings throughout
- [ ] File parses correctly as Markdown
- [ ] Git diff shows only intended structural changes

---

## Related Documentation

- Spec: `docs/superpowers/specs/2026-03-27-elisp-rule-refinement-design.md`
- Original file: `.claude/rules/lang/elisp.md`
