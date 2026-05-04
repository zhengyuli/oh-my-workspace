# Rules Refinement Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Refine all 7 rule files in `.claude/rules/` for cross-file consistency using a canonical section template, add missing sections, strengthen weak content, and fix CLAUDE.md.

**Architecture:** Each rule file is independently refactored to match a canonical section order. Changes are structural (reordering, promoting, grouping) and additive (new Indentation/Comments sections). No content is deleted — only moved, renamed, or strengthened.

**Tech Stack:** Markdown rule files, validated via `zsh -n` / `bash -n` / syntax checks on embedded code blocks.

**Spec:** `docs/superpowers/specs/2026-05-04-rules-refinement-design.md`

---

### Task 1: Refine bash.md

**Files:**
- Modify: `.claude/rules/bash.md`

The canonical section order for bash.md is:

```
---
paths: [...]
---
# Bash Conventions
## File Header
## Delimiter Hierarchy
  ### Blank Lines
## Indentation                    ← NEW
## Line Length
## Comments                       ← MOVED from position 9 to 7
## Error Handling
  ### Strict Mode
  ### ERR Trap
  ### EXIT Trap (Cleanup)
  ### Exit Codes
## Code Patterns
  ### Variable Handling
  ### Conditionals
  ### Default Values
  ### Output Standards
  ### ANSI Escape Sequences
  ### Naming Conventions
  ### Formatting
  ### Section Uniqueness
  ### Validation at Boundaries
  ### Nesting Limit
  ### No Magic Numbers
## Bash-Specific Patterns         ← RENAMED zone grouping
  ### Arrays                      ← MOVED from standalone section
  ### Line Continuation           ← MOVED from standalone section
  ### Dry-Run Pattern             ← MOVED from standalone section
## Functions
## Anti-Patterns
## Security
## References
## Validation
```

- [ ] **Step 1: Add `## Indentation` section after Delimiter Hierarchy**

Insert the following after the `## Delimiter Hierarchy` section (after the "Prohibited" line, before `## Line Length`):

```markdown
## Indentation

2-space indent for block bodies (`if`, `for`, `while`, `case`, functions).
4-space indent for continuation lines that do not align to an opening
delimiter.

```bash
# Block indent — 2 spaces
if [[ -f "$file" ]]; then
  process "$file"
fi

# Continuation — align under opening [[ when possible
if [[ "$mode" == "install" ]] \
   && [[ -n "$pkg" ]]; then
  _install_package "$pkg"
fi

# Continuation — 2-space indent when no alignment anchor
printf 'error: cannot install %s — missing dependency %s\n' \
  "$pkg" \
  "$dep" \
  >&2
```
```

- [ ] **Step 2: Move `## Comments` from position 9 to position 7 (after Line Length, before Error Handling)**

Cut the entire `## Comments` section (lines 482–496 in current file) and paste it between `## Line Length` and `## Error Handling`. Strengthen the content to match the canonical format:

```markdown
## Comments

Comments explain *why*, not *what*. The code itself should be readable
enough to show what it does.

**Comment syntax**: `#` followed by a single space.

**When to comment**:
- Non-obvious logic, workarounds, performance trade-offs
- Why a particular approach was chosen over alternatives
- Constraints or assumptions that are not self-evident

**When NOT to comment**:
- Self-documenting variable/function names
- Obvious operations (`# Increment count` before `(( count += 1 ))`)

**No end-of-line comments** — place comments on a separate line above
the code (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```bash
# WRONG — restates the code
# Increment count by 1
(( count += 1 ))

# CORRECT — explains the reasoning
# Retries are capped at 3 to avoid hammering a down service
(( count += 1 ))
```
```

- [ ] **Step 3: Group Arrays, Line Continuation, and Dry-Run Pattern under `## Bash-Specific Patterns`**

Rename the standalone `## Arrays` section to `### Arrays` and wrap all three sections under a new Level 1 heading. The current standalone sections `## Arrays`, `## Line Continuation`, and `## Dry-Run Pattern` become subsections:

```markdown
## Bash-Specific Patterns

### Arrays

[existing Arrays content unchanged]

### Line Continuation

[existing Line Continuation content unchanged]

### Dry-Run Pattern

[existing Dry-Run Pattern content unchanged]
```

- [ ] **Step 4: Verify the final section order matches the canonical template**

Scan the file top-to-bottom. The `##` headings must appear in this exact order:
1. File Header
2. Delimiter Hierarchy
3. Indentation
4. Line Length
5. Comments
6. Error Handling
7. Code Patterns
8. Bash-Specific Patterns
9. Functions
10. Anti-Patterns
11. Security
12. References
13. Validation

- [ ] **Step 5: Validate embedded code blocks**

Run:
```bash
bash -n setup.sh
```
Expected: no syntax errors (baseline check).

- [ ] **Step 6: Commit**

```bash
git add .claude/rules/bash.md
git commit -m "docs(rules): refine bash.md — canonical section order

- Add Indentation section (2-space block, 4-space continuation)
- Move Comments from position 9 to position 5 (before Error Handling)
- Group Arrays, Line Continuation, Dry-Run under Bash-Specific Patterns
- Strengthen Comments section with when-to/when-not-to guidance

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 2: Refine zsh.md

**Files:**
- Modify: `.claude/rules/zsh.md`

The canonical section order for zsh.md is:

```
---
paths: [...]
---
# Zsh Conventions
## File Header
## Delimiter Hierarchy
  ### Blank Lines
## Indentation                    ← NEW
## Line Length
## Comments                       ← MOVED from position 8 to 7
## Error Handling
## Code Patterns
  ### Variable Handling
  ### Conditionals
  ### Default Values
  ### Output Standards
  ### ANSI Escape Sequences
  ### Naming Conventions
  ### Formatting
  ### Line Continuation           ← MOVED from standalone to Code Patterns subsection
  ### Section Uniqueness
  ### Validation at Boundaries
  ### Nesting Limit
  ### No Magic Numbers
## Zsh-Specific Features          ← RENAMED grouping zone
  ### Glob Qualifiers             ← MOVED from Zsh Features
  ### Parameter Expansion Flags   ← MOVED from Zsh Features
  ### Anonymous Functions          ← MOVED from Zsh Features
  ### Array and Hash Types         ← MOVED from Zsh Features
  ### Autoload                     ← MOVED from Zsh Features
## Shell Options                  ← MOVED into grouped zone
## conf.d/ Organization           ← MOVED into grouped zone
## Plugin Management              ← MOVED into grouped zone
## Completion System              ← MOVED into grouped zone
## Functions
## Anti-Patterns
## Security
## References
## Validation
```

- [ ] **Step 1: Add `## Indentation` section after Delimiter Hierarchy**

Insert after the Delimiter Hierarchy section, before `## Line Length`:

```markdown
## Indentation

2-space indent for block bodies (`if`, `for`, `while`, `case`, functions).

```zsh
if [[ -f "$file" ]]; then
  process "$file"
fi

for pkg in "${pkgs[@]}"; do
  _install_package "$pkg"
done
```
```

- [ ] **Step 2: Move `## Comments` from position 8 to position 5 (after Line Length, before Error Handling)**

Cut the `## Comments` section (lines 496–510) and paste between `## Line Length` and `## Error Handling`. Strengthen:

```markdown
## Comments

Comments explain *why*, not *what*. The code itself should be readable
enough to show what it does.

**Comment syntax**: `#` followed by a single space.

**When to comment**:
- Non-obvious logic, workarounds, performance trade-offs
- Why a particular approach was chosen over alternatives
- Constraints or assumptions that are not self-evident

**When NOT to comment**:
- Self-documenting variable/function names
- Obvious operations (`# Export PATH` before `export PATH=...`)

**No end-of-line comments** — place comments on a separate line above
the code (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```zsh
# WRONG — restates the code
# Export PATH
export PATH="$HOME/.local/bin:$PATH"

# CORRECT — explains the reasoning
# Local bin takes precedence over system packages (personal builds)
export PATH="$HOME/.local/bin:$PATH"
```
```

- [ ] **Step 3: Move Line Continuation from standalone Code Patterns subsection — keep as-is (already a subsection)**

Line Continuation is already `### Line Continuation` inside Code Patterns. No move needed — verify it stays there.

- [ ] **Step 4: Rename `## Zsh Features` to `## Zsh-Specific Features`**

Change the heading from `## Zsh Features` to `## Zsh-Specific Features`. All subsections (Glob Qualifiers, Parameter Expansion Flags, Anonymous Functions, Array and Hash Types, Autoload) stay as-is under this heading.

- [ ] **Step 5: Verify Shell Options, conf.d/, Plugin Management, Completion System follow after Zsh-Specific Features**

These sections should appear in order after `## Zsh-Specific Features` and before `## Functions`. They are already in roughly this position — verify and adjust if needed.

- [ ] **Step 6: Verify the final section order matches the canonical template**

The `##` headings must appear in this exact order:
1. File Header
2. Delimiter Hierarchy
3. Indentation
4. Line Length
5. Comments
6. Error Handling
7. Code Patterns
8. Zsh-Specific Features
9. Shell Options
10. conf.d/ Organization
11. Plugin Management
12. Completion System
13. Functions
14. Anti-Patterns
15. Security
16. References
17. Validation

- [ ] **Step 7: Commit**

```bash
git add .claude/rules/zsh.md
git commit -m "docs(rules): refine zsh.md — canonical section order

- Add Indentation section (2-space block indent)
- Move Comments from position 8 to position 5
- Rename Zsh Features → Zsh-Specific Features
- Strengthen Comments with when-to/when-not-to guidance

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 3: Refine elisp.md

**Files:**
- Modify: `.claude/rules/elisp.md`

The canonical section order for elisp.md is:

```
---
paths: [...]
---
# Emacs Lisp Conventions
## File Header
## File Tail
## Module Loading
## Delimiter Hierarchy
  ### Blank Lines
## Indentation                    ← NEW
## Line Length
## Comments                       ← MOVED from position 9 to 7
## Error Handling
## Code Patterns
## use-package Declaration        ← language-specific zone
## LSP Configuration (eglot)      ← language-specific zone
## Functions                      ← MOVED after language-specific
## Anti-Patterns
## Security
## References
## Validation
```

- [ ] **Step 1: Add `## Indentation` section after Delimiter Hierarchy**

Insert between Delimiter Hierarchy and Line Length:

```markdown
## Indentation

2-space standard Lisp indent.  Let Emacs handle indentation via
`lisp-indent-function` — do not manually override indent levels.

Special forms (`defun`, `let`, `if`, `when`, `unless`, `condition-case`)
use their built-in indent rules.  For custom macros, declare indent with:

```elisp
(declare (indent defun))   ; body forms indent like defun
(declare (indent 1))       ; first arg is special, rest are body
```

Never use tabs in Emacs Lisp files — `indent-tabs-mode` must be `nil`.
```

- [ ] **Step 2: Move `## Comments` from position 9 to position 7 (after Line Length, before Error Handling)**

Cut the `## Comments` section (lines 442–460) and paste between `## Line Length` and `## Error Handling`. Strengthen:

```markdown
## Comments

Comments explain *why*, not *what*. The code itself should be readable
enough to show what it does.

**Comment syntax**:
- `;;` — standard inline comment (above a form)
- `;;;` — file-level section headers (recognized by `outline-minor-mode`)
- `;` — end-of-line (discouraged, see Anti-Patterns)

**When to comment**:
- Non-obvious logic, workarounds, Emacs quirks
- Why a particular approach was chosen over alternatives
- Package configuration rationale

**When NOT to comment**:
- Self-documenting function/variable names
- Obvious operations (`;; Set tab width` before `(setq tab-width 4)`)

**Docstrings vs comments**: docstrings describe public API contracts
(see Code Patterns > Docstrings). Do not duplicate docstring content
in inline comments.

**No end-of-line comments** — place comments on a separate `;;` line
above the form (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```elisp
;; WRONG — restates the code
;; Set tab width to 4
(setq tab-width 4)

;; CORRECT — explains the reasoning
;; Match project convention (Go, Python default indent)
(setq tab-width 4)
```
```

- [ ] **Step 3: Verify use-package Declaration and LSP Configuration are in language-specific zone**

These should appear after `## Code Patterns` and before `## Functions`. They are already in this position — verify.

- [ ] **Step 4: Verify `## Functions` appears after language-specific sections**

`## Functions` should come after `## LSP Configuration (eglot)` and before `## Anti-Patterns`. Currently it's at position 11 which is roughly correct — verify exact placement.

- [ ] **Step 5: Verify the final section order**

The `##` headings must appear in this exact order:
1. File Header
2. File Tail
3. Module Loading
4. Delimiter Hierarchy
5. Indentation
6. Line Length
7. Comments
8. Error Handling
9. Code Patterns
10. use-package Declaration
11. LSP Configuration (eglot)
12. Functions
13. Anti-Patterns
14. Security
15. References
16. Validation

- [ ] **Step 6: Commit**

```bash
git add .claude/rules/elisp.md
git commit -m "docs(rules): refine elisp.md — canonical section order

- Add Indentation section (2-space Lisp indent, Emacs auto-indent)
- Move Comments from position 9 to position 7
- Strengthen Comments with syntax guide and when-to/when-not-to
- Verify language-specific zone ordering

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 4: Refine config.md

**Files:**
- Modify: `.claude/rules/config.md`

The canonical section order for config.md is:

```
---
paths: [...]
---
# Configuration Files (No Extension)
## XDG Path Mapping               ← stays after scope description, before File Header
## File Header
## Delimiter Hierarchy
  ### Blank Lines
  ### INI Section Headers vs. Delimiter Hierarchy
## Indentation                    ← already exists
## Line Length
## Comments                       ← NEW top-level (extracted from Code Patterns)
## Code Patterns
  ### Value Types
  ### Per-Tool Syntax
  ### Section Uniqueness
## Anti-Patterns
## Security
## References
## Validation
```

- [ ] **Step 1: Add `## Comments` as top-level section (extract from Code Patterns)**

The current `### Comments` subsection under Code Patterns (lines 162–168) needs to be promoted to a top-level `## Comments` section. Place it between `## Line Length` and `## Code Patterns`. Strengthen:

```markdown
## Comments

Comments explain *why*, not *what*. Use separate comment lines above
the setting — never end-of-line.

**Comment syntax**: `#` followed by a single space (all three tools use `#`).

**When to comment**:
- Non-obvious setting values, workarounds, tool quirks
- Why a specific value was chosen over the default
- Dependencies between settings

**When NOT to comment**:
- Self-documenting key names (`pager = delta` needs no "set pager")
- Default values that match the tool's documented defaults

**No end-of-line comments** — place comments on a separate line above
the setting (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```conf
# WRONG — restates the setting
pager = delta  # set pager to delta

# CORRECT — explains reasoning
# Use delta for syntax-highlighted diffs (falls back to less)
pager = delta
```
```

- [ ] **Step 2: Remove the old `### Comments` subsection from Code Patterns**

Delete the `### Comments` subsection (lines 162–168) from under `## Code Patterns` since its content has been promoted. The Code Patterns section should now start with `### Value Types`.

- [ ] **Step 3: Verify the final section order**

The `##` headings must appear in this exact order:
1. XDG Path Mapping
2. File Header
3. Delimiter Hierarchy
4. Indentation
5. Line Length
6. Comments
7. Code Patterns
8. Anti-Patterns
9. Security
10. References
11. Validation

- [ ] **Step 4: Commit**

```bash
git add .claude/rules/config.md
git commit -m "docs(rules): refine config.md — canonical section order

- Promote Comments from Code Patterns subsection to top-level
- Strengthen Comments with when-to/when-not-to guidance
- Verify section order matches canonical template

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 5: Refine toml.md

**Files:**
- Modify: `.claude/rules/toml.md`

The canonical section order for toml.md is:

```
---
paths: [...]
---
# TOML Configuration Files
## File Header
## Delimiter Hierarchy
  ### Blank Lines                 ← absorb standalone Blank Line Rules
  ### TOML Table Headers vs. Delimiter Hierarchy
## Line Length
## Comments                       ← NEW top-level (extracted from Code Patterns)
## Code Patterns
  ### Value Types
  ### Formatting
  ### Section Uniqueness
## TOML Syntax Conventions        ← MOVED to language-specific zone
  ### Table Styles
  ### Dotted Keys
  ### Multiline Strings
## Per-Tool Notes
## Anti-Patterns
## Security
## References
## Validation
```

- [ ] **Step 1: Absorb `## Blank Line Rules` into `### Blank Lines` under Delimiter Hierarchy**

The standalone `## Blank Line Rules` section (lines 178–199) duplicates and extends the blank-line guidance. Merge its content into the existing `### Blank Lines` subsection under Delimiter Hierarchy. The merged subsection should cover:

- Around delimiters (Level 1: blank before/after; Level 2: no blank after)
- Between top-level tables: exactly one blank line
- Between `[[array_of_tables]]` entries: exactly one blank line
- Within a table (between key-value pairs): no blank lines
- Between logical groups inside a table: one blank line preceded by a comment
- Prohibited: two or more consecutive blank lines

Add a `### Blank Lines` subsection under Delimiter Hierarchy if one doesn't exist, then merge. Delete the standalone `## Blank Line Rules` section.

- [ ] **Step 2: Add `## Comments` as top-level section**

Extract `### Comments` from Code Patterns (lines 213–218) and promote to `## Comments` between `## Line Length` and `## Code Patterns`. Strengthen:

```markdown
## Comments

Comments explain *why*, not *what*. Use separate comment lines above
the setting — never end-of-line.

**Comment syntax**: `#` followed by a single space.

**When to comment**:
- Non-obvious values, format string syntax, workarounds
- Why a specific configuration was chosen
- Tool-specific quirks or limitations

**When NOT to comment**:
- Self-documenting key names (`symbol = " "` with a Nerd Font icon)
- Default values matching the tool's documentation

**No end-of-line comments** — place comments on a separate line above
the value (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```toml
# WRONG — end-of-line annotation
symbol = " "  # Python icon

# CORRECT — separate line explains reasoning
# Python language icon (Nerd Font)
symbol = " "
```
```

- [ ] **Step 3: Remove old `### Comments` from Code Patterns**

Delete the `### Comments` subsection from Code Patterns.

- [ ] **Step 4: Move `## TOML Syntax Conventions` to language-specific zone**

Move the `## TOML Syntax Conventions` section (currently at position 3, lines 110–176) to after `## Code Patterns`. This groups it as the language-specific zone.

- [ ] **Step 5: Verify the final section order**

The `##` headings must appear in this exact order:
1. File Header
2. Delimiter Hierarchy
3. Line Length
4. Comments
5. Code Patterns
6. TOML Syntax Conventions
7. Per-Tool Notes
8. Anti-Patterns
9. Security
10. References
11. Validation

Note: No Indentation section needed for TOML (not applicable — TOML structure is defined by table syntax, not whitespace).

- [ ] **Step 6: Commit**

```bash
git add .claude/rules/toml.md
git commit -m "docs(rules): refine toml.md — canonical section order

- Absorb Blank Line Rules into Blank Lines subsection
- Promote Comments from Code Patterns to top-level
- Move TOML Syntax Conventions to language-specific zone
- Strengthen Comments with when-to/when-not-to guidance

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 6: Refine yaml.md

**Files:**
- Modify: `.claude/rules/yaml.md`

The canonical section order for yaml.md is:

```
---
paths: [...]
---
# YAML Configuration Files
## File Header
## Delimiter Hierarchy
  ### Blank Lines                 ← absorb from Code Patterns > Blank Line Rules
  ### YAML Top-Level Keys vs. Delimiter Hierarchy
## Indentation                    ← already exists
## Line Length
## Comments                       ← NEW top-level (extracted from Code Patterns)
## YAML Syntax Conventions        ← MOVED to language-specific zone
  ### String Quoting
  ### Multiline Strings
  ### Null Values
  ### Anchors and Aliases
## Code Patterns
  ### Value Types
  ### Formatting
  ### Section Uniqueness
## Per-Tool Notes
## Anti-Patterns
## Security
## References
## Validation
```

- [ ] **Step 1: Absorb `### Blank Line Rules` from Code Patterns into `### Blank Lines` under Delimiter Hierarchy**

Currently there is no `### Blank Lines` subsection under Delimiter Hierarchy in yaml.md. The blank-line rules are under `### Blank Line Rules` inside Code Patterns (lines 254–277). Create `### Blank Lines` under Delimiter Hierarchy and move this content there. Delete the old location.

- [ ] **Step 2: Add `## Comments` as top-level section**

Extract `### Comments` from Code Patterns (lines 230–240) and promote to `## Comments` between `## Line Length` and `## YAML Syntax Conventions`. Strengthen:

```markdown
## Comments

Comments explain *why*, not *what*. Use separate comment lines above
the setting — never end-of-line.

**Comment syntax**: `#` followed by a single space.

**When to comment**:
- Non-obvious values, theme color codes, tool-specific quirks
- Why a specific configuration was chosen
- Dependencies between settings

**When NOT to comment**:
- Self-documenting key names (`quit: "q"` needs no comment)
- Default values matching the tool's documentation

**No end-of-line comments** — place comments on a separate line above
the value (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```yaml
# WRONG — end-of-line annotation
theme: catppuccin  # color scheme

# CORRECT — separate line explains reasoning
# Catppuccin Mocha Blue theme for visual consistency
theme: catppuccin
```
```

- [ ] **Step 3: Remove old `### Comments` from Code Patterns**

Delete the `### Comments` subsection from Code Patterns.

- [ ] **Step 4: Move `## YAML Syntax Conventions` to before Code Patterns (language-specific zone)**

Move `## YAML Syntax Conventions` (currently lines 129–227) to after `## Comments` and before `## Code Patterns`. This creates the language-specific zone.

- [ ] **Step 5: Verify the final section order**

The `##` headings must appear in this exact order:
1. File Header
2. Delimiter Hierarchy
3. Indentation
4. Line Length
5. Comments
6. YAML Syntax Conventions
7. Code Patterns
8. Per-Tool Notes
9. Anti-Patterns
10. Security
11. References
12. Validation

- [ ] **Step 6: Commit**

```bash
git add .claude/rules/yaml.md
git commit -m "docs(rules): refine yaml.md — canonical section order

- Create Blank Lines subsection under Delimiter Hierarchy
- Promote Comments from Code Patterns to top-level
- Move YAML Syntax Conventions to language-specific zone
- Strengthen Comments with when-to/when-not-to guidance

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 7: Refine vimrc.md

**Files:**
- Modify: `.claude/rules/vimrc.md`

The canonical section order for vimrc.md is:

```
---
paths: [...]
---
# Vim Script Configuration Files
## File Header
## Delimiter Hierarchy
  ### Blank Lines
## Indentation                    ← NEW
## Line Length
## Comments                       ← RENAMED from Comment Style, MOVED to position 5
## Code Patterns
  ### Plugin Management
  ### Options: set vs let
  ### Variables
  ### Key Mappings
  ### Auto Commands
  ### Functions
  ### Conditional Logic
  ### Strings
  ### Regular Expressions
  ### XDG Paths
  ### Section Uniqueness
  ### Validation at Boundaries
  ### Nesting Limit
  ### No Magic Numbers
  ### Highlight Definitions
## Anti-Patterns
## Security
## References
## Validation
```

- [ ] **Step 1: Add `## Indentation` section after Delimiter Hierarchy**

Insert between Delimiter Hierarchy and Line Length:

```markdown
## Indentation

2-space indent for Vim script block bodies (`if`, `for`, `while`,
`function`, `augroup`).  Continuation lines (`\`) align under the
opening statement.

```vim
if has('termguicolors')
  set termguicolors
endif

augroup vimrc
  autocmd!
  autocmd BufReadPost * if line("'\"") > 0 | execute 'normal! g`"' | endif
augroup END

let &runtimepath = s:cfg  . '/vim,'
               \ . s:cfg  . '/vim/after,'
               \ . $VIMRUNTIME
```
```

- [ ] **Step 2: Rename `## Comment Style` → `## Comments` and move from position 2 to position 5**

Cut the entire `## Comment Style` section (lines 48–65) and paste between `## Line Length` and `## Code Patterns`. Rename the heading and strengthen:

```markdown
## Comments

Vim script uses `"` for line comments (not `#`).

Comments explain *why*, not *what*. The code itself should be readable
enough to show what it does.

**When to comment**:
- Non-obvious option values (what does `timeoutlen=4200` mean?)
- Why a specific approach was chosen over alternatives
- Constraints from Vim's limited XDG support

**When NOT to comment**:
- Self-documenting options (`set number`, `set expandtab`)
- Obvious operations

**No end-of-line comments** — place comments on a separate `"` line above
the setting (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```vim
" WRONG — restates the obvious (what the code already says)
" Set expandtab to use spaces
set expandtab

" CORRECT — explains why (project convention)
" Use spaces instead of tabs (project convention)
set expandtab
```
```

- [ ] **Step 3: Verify the final section order**

The `##` headings must appear in this exact order:
1. File Header
2. Delimiter Hierarchy
3. Indentation
4. Line Length
5. Comments
6. Code Patterns
7. Anti-Patterns
8. Security
9. References
10. Validation

- [ ] **Step 4: Commit**

```bash
git add .claude/rules/vimrc.md
git commit -m "docs(rules): refine vimrc.md — canonical section order

- Add Indentation section (2-space block, continuation alignment)
- Rename Comment Style → Comments, move to position 5
- Strengthen Comments with when-to/when-not-to guidance

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 8: Fix CLAUDE.md

**Files:**
- Modify: `CLAUDE.md`

- [ ] **Step 1: Add vimrc.md to rules listing**

In CLAUDE.md around line 138, add `vimrc.md` to the rules list. Change:

```markdown
- `yaml.md` — YAML configuration files
```

to:

```markdown
- `yaml.md` — YAML configuration files
- `vimrc.md` — Vim script conventions
```

- [ ] **Step 2: Commit**

```bash
git add CLAUDE.md
git commit -m "docs: add vimrc.md to CLAUDE.md rules listing

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 9: Final Validation

**Files:**
- Read: all 7 rule files, CLAUDE.md

- [ ] **Step 1: Verify all 7 files have `## Comments` as top-level section**

```bash
grep -c '^## Comments' .claude/rules/*.md
```
Expected: each file shows `1`.

- [ ] **Step 2: Verify all 7 files have `## Indentation` or equivalent**

For bash, zsh, elisp, vimrc — new section. For config, yaml — already existed. For toml — not applicable (verify no Indentation section).

```bash
grep -c '^## Indentation' .claude/rules/bash.md .claude/rules/zsh.md .claude/rules/elisp.md .claude/rules/config.md .claude/rules/yaml.md .claude/rules/vimrc.md
```
Expected: `1` for bash, zsh, elisp, config, yaml, vimrc.

- [ ] **Step 3: Verify `### Blank Lines` is a subsection of Delimiter Hierarchy in all 7 files**

```bash
for f in .claude/rules/*.md; do
  printf '%s: ' "$f"
  grep -c '### Blank Lines' "$f"
done
```
Expected: each file shows `1`.

- [ ] **Step 4: Verify CLAUDE.md lists all 7 rule files**

```bash
grep -c 'vimrc.md' CLAUDE.md
```
Expected: `1`.

- [ ] **Step 5: Verify no file exceeds 800 lines**

```bash
wc -l .claude/rules/*.md
```
Expected: all files ≤ 800 lines.

- [ ] **Step 6: Run lint command (if available) for final check**

```bash
# Dry-run check that rules are internally consistent
bash -n setup.sh && echo "bash OK"
```
