---
date: 2026-04-09
status: approved
title: Revise lint.md for Full Rule Coverage
---

# Revise lint.md for Full Rule Coverage

## Goal

Revise `.claude/commands/lint.md` to check every enforceable rule from all 7
files in `.claude/rules/`. The current lint command covers ~30-40% of rules;
the revision brings coverage to 100%.

## Approach

Per-type sections (Approach B): reorganize Step 3 into dedicated subsections
that map 1:1 to each rule file. When a rule file changes, only the
corresponding lint section needs updating.

## Structure

The revised `lint.md` keeps the 4-step flow (Resolve -> Read Rules -> Check
-> Report) with Step 3 expanded into per-type subsections:

```
Step 1: Resolve Target          (unchanged)
Step 2: Read Rules              (expanded path patterns)
Step 3: Check Files
  3.0: Universal Checks         (all file types -- expanded)
  3.1: Bash Checks              (new dedicated section)
  3.2: Zsh Checks               (new dedicated section)
  3.3: Elisp Checks             (new dedicated section)
  3.4: Config Checks            (new dedicated section)
  3.5: TOML Checks              (new dedicated section)
  3.6: YAML Checks              (new dedicated section)
  3.7: Vim Checks               (new dedicated section)
Step 4: Output Report           (minor refinements)
```

Step 2 file-type mapping is updated with vim path patterns and corrected
glob patterns matching each rule file's frontmatter.

## Check Catalog

96 total checks across 7 types and universal rules.

### Universal Checks (11 checks, all file types)

1. **File header** [ERROR]: mode line, time-stamp, title, author, copyright,
   history with creation entry, commentary. Keywords required except
   config/toml/yaml. References required. Elisp/vim require location/XDG
   notes.

2. **Title Case** [ERROR]: capitalize first letter of every word in section
   and subsection titles. Abbreviations follow established convention.

3. **Delimiter hierarchy** [ERROR]: Level 0 (79 `=`), Level 1 (79 `-`),
   Level 2 (`# --- Title ---`). Elisp uses `;;`, vim uses `"`.

4. **Blank line rules** [ERROR]: one blank line before Level 1 opening and
   after closing. Level 2: no blank line after delimiter. Related statements
   not separated. Prohibited: 2+ consecutive blank lines.

5. **Line length** [WARN]: 79 characters max with per-type exceptions (URLs,
   help text, starship format strings, etc.).

6. **Section uniqueness** [ERROR]: no duplicate section titles at same
   delimiter level within a file.

7. **Comments explain why** [WARN]: flag comments restating code instead of
   explaining reasoning.

8. **End-of-line comments** [ERROR]: flag trailing comment annotations after
   code/settings. Ignore comment-only lines.

9. **Align values** [ERROR]: flag extra spacing to align `=`, `:`, or case
   formatting. Exception: vim highlight definitions.

10. **Security: secrets** [ERROR]: no hardcoded API keys, tokens, passwords,
    private keys, certificates.

11. **Nesting limit** [WARN]: max 3 levels of control structures.

### Bash Checks (16 checks)

1. **Shebang** [ERROR]: `#!/usr/bin/env bash` for executables, omitted for
   sourced files.

2. **Strict mode** [ERROR]: `set -euo pipefail` required for scripts, not
   `.bashrc`.

3. **Error handling** [WARN]: ERR trap, EXIT trap for cleanup, standard exit
   codes.

4. **Variable quoting** [ERROR]: all variable references quoted.

5. **Conditional style** [WARN]: `[[ ]]` for strings, `(( ))` for arithmetic.

6. **printf over echo** [WARN]: echo flagged except in help/usage functions.

7. **ANSI quoting** [ERROR]: `$'...'` for escape sequences, not `'\033'`.

8. **Naming conventions** [WARN]: UPPER_SNAKE constants, lower_snake locals,
   `_` prefix for private functions.

9. **Short-circuit control flow** [ERROR]: no standalone `&&`/`||` as control
   flow. Inside `if`/`while` is fine.

10. **eval usage** [ERROR]: flag `eval` on user input; use `case` dispatch.

11. **Local command substitution** [WARN]: separate `local` declaration and
    assignment when RHS is command substitution.

12. **Function patterns** [WARN]: `main()` wrapper for scripts >20 lines.
    Parameter validation at entry. Single responsibility [INFO].

13. **Arrays** [WARN]: explicit declaration, `"${arr[@]}"` quoting.

14. **Magic numbers** [WARN]: `readonly` named constants.

15. **Security: permissions** [INFO]: scripts 755, configs 644, secrets 600.

16. **Dry-run pattern** [INFO]: check flag at action point, not call site.

### Zsh Checks (17 checks)

1. **Shebang** [ERROR]: `#!/usr/bin/env zsh` for executables, omitted for
   sourced files.

2. **Strict mode** [ERROR]: `set -euo pipefail` for scripts, not `.zshrc`.

3. **Error handling** [WARN]: zsh-specific ERR trap (`${funcstack[2]}`,
   `${funcfiletrace[1]}`). EXIT trap for cleanup.

4. **print over echo** [WARN]: `print` for simple output. `printf` only for
   format specifiers.

5. **ANSI quoting** [ERROR]: `$'...'` for escape sequences.

6. **Glob qualifiers** [WARN]: flag `find`+`grep` where glob qualifiers work.

7. **Parameter expansion flags** [WARN]: flag `cut`/`tr`/`awk` where zsh
   parameter flags work.

8. **emulate for portable functions** [WARN]: autoloaded/shared functions need
   `emulate -L zsh`. Not needed for conf.d or zsh-shebang scripts.

9. **conf.d naming** [WARN]: `NN-name.zsh` pattern required.

10. **conf.d numbering ranges** [INFO]: flag files outside logical range.

11. **Shell options** [WARN]: one option per line with comment. All setopt in
    dedicated file.

12. **Zinit plugin declarations** [WARN]: ice+light pairs, modifiers on
    single line.

13. **Plugin comments** [WARN]: timing explanation, config vars before ice,
    non-obvious modifiers explained.

14. **compinit caching** [INFO]: cache at `$XDG_CACHE_HOME/zsh/zcompdump`,
    freshness via glob qualifiers.

15. **zstyle formatting** [WARN]: continuation lines for >79 chars, grouped
    by context.

16. **fzf-tab coexistence** [WARN]: flag `menu select` when fzf-tab loaded.

17. **Function patterns** [WARN]: `main()` wrapper, parameter validation,
    local command substitution (separate lines).

### Elisp Checks (22 checks)

1. **Lexical binding** [ERROR]: `-*- lexical-binding: t; -*-` in mode line.

2. **File tail: provide** [ERROR]: `(provide 'omw-module)` present.

3. **File tail: ends here** [ERROR]: `;;; filename.el ends here` present.

4. **Feature name** [ERROR]: matches file base name (hyphenated).

5. **Module loading** [ERROR]: `require` for local modules, `use-package` for
   third-party only.

6. **use-package keyword order** [WARN]: `:ensure` -> `:demand`/`:defer` ->
   `:when`/`:if` -> `:after` -> `:requires` -> `:mode` -> `:magic` ->
   `:hook` -> `:bind` -> `:bind-keymap` -> `:chords` -> `:custom-face` ->
   `:custom` -> `:init` -> `:config`.

7. **:after implies :defer** [WARN]: flag `:after X` + `:defer t` (redundant).

8. **:custom vs setq** [WARN]: flag `:custom` for ordinary assignments.

9. **Docstrings** [WARN]: required for public functions, first line a complete
   sentence.

10. **Naming** [WARN]: `omw/` prefix, kebab-case, `-p` for predicates.

11. **Key binding syntax** [ERROR]: `kbd` macro required, no raw escape
    strings.

12. **Sharp quoting** [WARN]: `#'` for function references in `add-hook`,
    `define-key`, etc.

13. **push over add-to-list** [ERROR]: flag `add-to-list` in init files.

14. **Hook functions** [ERROR]: named functions, no anonymous lambdas.

15. **Interactive declaration** [WARN]: `(interactive)` for M-x callable
    commands, not for internal helpers.

16. **Optional dependency guard** [WARN]: `featurep`/`fboundp` before calling
    optional features.

17. **Variable assignment** [WARN]: `setq` by default, `setopt` only for
    custom setter side-effects.

18. **Buffer-local variables** [WARN]: `defvar-local`, not
    `make-local-variable`.

19. **Validation at boundaries** [WARN]: function parameter validation.

20. **Error handling** [WARN]: `ignore-errors` only for side effects,
    `condition-case` never silently discards.

21. **Security: no .elc** [ERROR]: flag committed `.elc` files.

22. **LSP: eglot only** [ERROR]: flag any `lsp-mode` usage.

### Config Checks (6 checks)

1. **INI indentation** [ERROR]: 4 spaces within `[section]` (git), flat for
   ripgrep/ghostty.

2. **INI section headers** [ERROR]: one `[section]` per Level 1 block.

3. **Per-tool syntax** [WARN]: git key=value in section, ripgrep --flags,
   ghostty key=value.

4. **Local override patterns** [WARN]: git `[include]`, ghostty
   `config-file = ?`.

5. **Ripgrep caveat** [WARN]: flag `~`/`$HOME` in ripgrep rc.

6. **Value types** [WARN]: booleans unquoted, strings quoted when spaces,
   numbers bare.

### TOML Checks (7 checks)

1. **String quoting** [ERROR]: all string values quoted. Numbers/booleans bare.

2. **Trailing commas** [ERROR]: not allowed, flag `["a", "b",]`.

3. **Blank line rules** [ERROR]: one blank line between tables, none within a
   table, one between `[[array_of_tables]]` entries.

4. **Table styles** [WARN]: `[[table]]` for list entries, inline for 2 keys
   or fewer.

5. **Dotted keys** [WARN]: prefer `[section]` + `key` for multi-key tables.

6. **Multiline strings** [WARN]: `'''` for regex/format, `"""` for escapes.

7. **Formatting** [WARN]: one space around `=`.

### YAML Checks (7 checks)

1. **Indentation** [ERROR]: 2-space consistently.

2. **Null values** [ERROR]: explicit `null`, never `~` or empty.

3. **Boolean conversion** [ERROR]: flag `yes`/`no`/`on`/`off` (Norway
   problem).

4. **String quoting** [WARN]: plain for safe, single for special chars,
   double for escapes.

5. **Multiline strings** [WARN]: `|` for commands, `>` for prose.

6. **Anchors/aliases** [WARN]: flag in dotfiles configs.

7. **Blank line rules** [ERROR]: one between top-level keys, none within
   nested mappings, one after block scalars.

### Vim Checks (10 checks)

1. **Comment style** [ERROR]: `"` for comments, not `#`.

2. **augroup required** [ERROR]: all autocmds in `augroup` with `autocmd!`.

3. **function! + abort** [ERROR]: `function!` with `abort`, `s:` prefix.

4. **noremap variants** [WARN]: flag `map`/`nmap`/`vmap`, use
   `noremap`/`nnoremap`/`vnoremap`.

5. **Variable scope** [WARN]: `s:` for internal state, flag `g:` for
   internal.

6. **set vs let** [WARN]: `set` for options, `let &option` for computed,
   `setlocal` for buffer-local.

7. **Conditional logic** [WARN]: `has('feature')` not version numbers.

8. **Regular expressions** [WARN]: prefer `\v` very magic.

9. **Highlight definitions** [INFO]: group by category, alignment acceptable.

10. **XDG paths** [WARN]: script-local vars with fallbacks and mkdir guard.

## Output Report Changes

- **Severity assignments** confirmed: ERROR for violations, WARN for style,
  INFO for statistics.
- **Findings table**: unchanged format, descriptions include rule name.
- **Summary table**: unchanged.
- **Report header**: adds check count summary line.

## Scope

Single file change: `.claude/commands/lint.md`. No changes to rule files.
No new dependencies.
