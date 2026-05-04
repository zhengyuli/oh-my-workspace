# Rules Refinement Design

Refine the 7 existing rule files in `.claude/rules/` for cross-file
consistency, unified section ordering, and content strengthening.

## Problem

The 7 rule files (bash, zsh, elisp, config, toml, yaml, vimrc) are
individually well-written but inconsistent with each other:

- Section ordering varies across files (Comments is position 9 in bash,
  buried in Code Patterns in config)
- Some files lack sections that others have (no Indentation in bash/zsh/elisp)
- Content depth varies (Blank Lines: 8 lines in config vs 25 in bash)
- CLAUDE.md lists 6 rules but 7 exist (vimrc.md omitted)

## Approach

Template + Content Strengthening: define a canonical section order, reorder
all files to match, add missing sections, and strengthen weak content areas.
Each file remains self-contained (duplication OK, no shared preamble).
Target density: descriptive with examples, 500-800 lines per file.

## Canonical Section Template

Every rule file follows this exact section order. Sections marked (conditional)
are included only when relevant to that file type.

```
---
paths: [glob patterns]
---

# {Language} Conventions                       ← scope description

## File Header                                 ← ALL files
## File Tail                                   ← conditional: elisp only
## Module Loading                              ← conditional: elisp only
## Delimiter Hierarchy                         ← ALL files
  ### Blank Lines                              ← standardized subsection
  ### {Format} Headers vs. Delimiter Hierarchy ← conditional: INI/TOML/YAML
## Indentation                                 ← ALL files (new for bash/zsh/elisp)
## Line Length                                 ← ALL files
## Comments                                    ← ALL files (promoted to top-level)
## Error Handling                              ← conditional: executable files
  ### Strict Mode                              ← bash/zsh
  ### ERR Trap / EXIT Trap / Exit Codes        ← bash/zsh
  ### ignore-errors / condition-case            ← elisp
## Code Patterns                               ← ALL files (language-specific subs)
## {Language}-Specific Sections                ← grouped zone
  bash:  Bash-Specific Patterns (Arrays, Line Continuation, Dry-Run)
  zsh:   Zsh Features, Shell Options, conf.d/, Plugins, Completion
  elisp: use-package Declaration, LSP Configuration (eglot)
  toml:  TOML Syntax Conventions
  yaml:  YAML Syntax Conventions
  vim:   (none beyond Code Patterns)
  config: XDG Path Mapping (scope-setting)
## Functions                                   ← conditional: executable files
## Per-Tool Notes                              ← conditional: toml/yaml/config
## Anti-Patterns                               ← ALL files (BAD/GOOD pairs)
## Security                                    ← ALL files
## References                                  ← ALL files
## Validation                                  ← ALL files (copy-pasteable commands)
```

## Per-File Changes

### bash.md (572 → ~600 lines)

Section reordering:
- Add `## Indentation`: 2-space block indent, 4-space continuation
- Move `## Comments` from position 9 → 7 (before Error Handling)
- Move Arrays, Line Continuation, Dry-Run Pattern into language-specific zone
- Rename zone: `## Bash-Specific Patterns`

Content:
- Strengthen Blank Lines subsection to match canonical format
- Ensure Anti-Patterns use consistent BAD/GOOD example pairs

### zsh.md (771 → ~790 lines)

Section reordering:
- Add `## Indentation`: 2-space block indent
- Move `## Comments` from position 8 → 7
- Group Zsh Features, Shell Options, conf.d/, Plugins, Completion into
  language-specific zone (after Code Patterns, before Functions)

Content:
- Standardize Blank Lines subsection
- Ensure Anti-Patterns format matches template

### elisp.md (628 → ~650 lines)

Section reordering:
- Add `## Indentation`: 2-space standard Lisp indent
- Move `## Comments` from position 9 → 7
- Move `## Functions` from position 11 → after language-specific sections
- Group use-package and LSP Configuration as language-specific

Content:
- Strengthen Validation section with batch-byte-compile command
- Standardize Blank Lines and Anti-Patterns format

### config.md (314 → ~350 lines)

Section reordering:
- Keep `## XDG Path Mapping` after File Header (scope-setting)
- Add `## Comments` as top-level (extract from Code Patterns)
- Ensure Indentation stays in current position

Content:
- Add explicit comment rules (currently only in Code Patterns subsection)
- Strengthen Validation with concrete commands per tool

### toml.md (387 → ~410 lines)

Section reordering:
- Absorb standalone `## Blank Line Rules` into `### Blank Lines` under
  Delimiter Hierarchy
- Add `## Comments` as top-level (extract from Code Patterns)
- Move `## TOML Syntax Conventions` to language-specific zone

Content:
- Standardize Blank Lines explanation
- Add explicit comment section

### yaml.md (436 → ~450 lines)

Section reordering:
- Add `## Comments` as top-level (extract from Code Patterns)
- Move `## YAML Syntax Conventions` to language-specific zone
- Keep Indentation in current position

Content:
- Add explicit comment rules
- Strengthen Validation with yq/python command

### vimrc.md (482 → ~500 lines)

Section reordering:
- Rename `## Comment Style` → `## Comments`, move from position 2 → 7
- Add `## Indentation`: 2-space for vimscript

Content:
- Ensure Anti-Patterns use BAD/GOOD pairs
- Strengthen Validation section

### CLAUDE.md

- Add `vimrc.md` to rules listing (line 139)

## Content Strengthening Details

### 1. Standardized Blank Lines Subsection

Every file's `### Blank Lines` under Delimiter Hierarchy will cover:
- Rule for around delimiters (Level 1: blank before/after; Level 2: no blank after)
- Rule for between top-level statements
- Rule for inside function/block bodies (executable files only)
- Prohibition of 2+ consecutive blank lines
- One concrete example in the file's own language

### 2. Indentation Sections (New)

Added to bash, zsh, elisp (already exists in config, yaml):
- bash/zsh: 2 spaces for block indent; align continuation under opening
- elisp: 2 spaces; lisp-indent-function for special forms; use Emacs auto-indent

### 3. Comments Promoted to Top-Level

All 7 files get `## Comments` covering:
- Comment syntax for the language (#, ;;, ", etc.)
- When to comment: non-obvious logic, workarounds, why-not-what
- When not to comment: self-documenting names, obvious operations
- No end-of-line comments (reference to universal rule)
- Inline vs block comment styles

### 4. Validation Strengthened

Copy-pasteable commands for every file:
- bash: `bash -n file.sh` + `shellcheck file.sh`
- zsh: `zsh -n file.zsh`
- elisp: `emacs --batch -f batch-byte-compile file.el`
- config/git: `git config --list --show-origin`
- config/ghostty: `ghostty +validate-config`
- toml: `python3 -c "import tomllib; tomllib.load(open('file.toml','rb'))"`
- yaml: `python3 -c "import yaml; yaml.safe_load(open('file.yaml'))"`
- vimrc: `vim -u NONE -N -c 'source file | quit'`

### 5. Anti-Patterns Format

Every anti-pattern entry uses this structure:
```
### Anti-Pattern Name

BAD:
```lang
code example
```

GOOD:
```lang
code example
```

Optional one-line rationale.
```

### 6. Security Section Standardized

All files cover these three areas:
- Secrets management: never hardcode tokens, keys, passwords
- Split-file strategy: config.local pattern for machine-specific data
- Tool-specific notes: e.g., ripgrep doesn't expand $HOME

## Out of Scope

- No new rule files (Lua, Markdown, JSON deferred)
- No changes to lint.md command file
- No changes to actual config files (rules only)

## Implementation Order

1. Create the canonical template as a reference (not committed — working doc)
2. Refine bash.md (largest executable file, sets the pattern)
3. Refine zsh.md (most similar to bash, validate pattern)
4. Refine elisp.md (different language family, test template flexibility)
5. Refine config.md (shortest file, simplest changes)
6. Refine toml.md
7. Refine yaml.md
8. Refine vimrc.md
9. Fix CLAUDE.md
10. Validate all changes with lint command

## Success Criteria

- All 7 files follow the canonical section order
- `## Comments` exists as top-level in all 7 files
- `## Indentation` exists in all 7 files
- `### Blank Lines` is a subsection of Delimiter Hierarchy in all 7 files
- Every Anti-Pattern has a BAD/GOOD example pair
- Every Validation section has copy-pasteable commands
- CLAUDE.md lists all 7 rule files
- No rule file exceeds 800 lines
- Each file still follows its own conventions (self-adherence)
