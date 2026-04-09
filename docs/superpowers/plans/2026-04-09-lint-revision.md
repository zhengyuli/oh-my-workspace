# Lint Command Full Rule Coverage — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rewrite `.claude/commands/lint.md` to check every enforceable rule from all 7 rule files (96 checks total).

**Architecture:** Single-file rewrite. The command keeps the existing 4-step flow (Resolve → Read Rules → Check → Report). Step 3 expands from 3 flat sections to 8 per-type subsections mapping 1:1 to rule files.

**Tech Stack:** Markdown prompt file (Claude Code slash command). No code, no dependencies.

**Spec:** `docs/superpowers/specs/2026-04-09-lint-revision-design.md`

---

### Task 1: Write the complete revised lint.md

**Files:**
- Rewrite: `.claude/commands/lint.md` (entire file)

Read the current `.claude/commands/lint.md` first to preserve the frontmatter format and output report structure. Then rewrite the entire file with the content below.

The new file must follow this exact structure. Each section's content is specified below with the checks from the spec.

- [ ] **Step 1: Read existing lint.md and all 7 rule files**

Read these files to understand current format and rule details:
- `.claude/commands/lint.md` (current version)
- `.claude/rules/bash.md`
- `.claude/rules/zsh.md`
- `.claude/rules/elisp.md`
- `.claude/rules/config.md`
- `.claude/rules/toml.md`
- `.claude/rules/yaml.md`
- `.claude/rules/vimrc.md`

- [ ] **Step 2: Write the new frontmatter and Steps 1-2**

Replace the entire file with the new content. Frontmatter:

```markdown
---
description: "Check coding convention compliance against rules"
arguments:
  - name: "target"
    description: "File path, directory path, or 'all' to lint entire repo"
    required: true
---

You are a convention compliance checker for the oh-my-workspace dotfiles
repository. Your job is to read files, match them to the correct coding rules,
and report violations.

## Step 1: Resolve Target

Interpret `$ARGUMENTS` as the target to lint:

- If a **file path** → lint that single file
- If a **directory path** → find all config/script files recursively within it
- If `all` → scan the entire repo (exclude `.git/`, `node_modules/`, `elpa/`,
  `straight/`, `site-packages/`)

Use Glob to discover files. Only include files that match these path patterns:

```
bash:   setup.sh, claude/**/*.sh, platform/**/*.sh
zsh:    shell/zsh/**/*.zsh, shell/zsh/**/.zshenv
elisp:  editor/emacs/.config/emacs/**/*.el
vim:    editor/vim/**/vimrc, editor/vim/**/*.vim
config: **/git/config, **/ripgrep/rc, **/ghostty/config
toml:   shell/starship/**/*.toml, tool/yazi/**/*.toml, lang/**/*.toml
yaml:   tool/**/*.yml, tool/**/*.yaml
```

## Step 2: Read Rules

For each file type detected, read the corresponding rules file from
`.claude/rules/`:

| File type | Rules file |
|-----------|-----------|
| bash | `.claude/rules/bash.md` |
| zsh | `.claude/rules/zsh.md` |
| elisp | `.claude/rules/elisp.md` |
| vim | `.claude/rules/vimrc.md` |
| config | `.claude/rules/config.md` |
| toml | `.claude/rules/toml.md` |
| yaml | `.claude/rules/yaml.md` |

Read the rules files BEFORE reading target files, so you know what to check.
```

- [ ] **Step 3: Write Step 3.0 — Universal Checks**

Append this section (all checks that apply to every file type):

```markdown
## Step 3: Check Files

For each target file, read its full content and check against the applicable
rules.

### 3.0 Universal Checks (all file types)

**File header** [ERROR]:
Verify presence and format of:
- Mode line (`-*- mode: ...; -*-` or `-*- lexical-binding: t; -*-` for elisp)
- Time-stamp line
- Title line (inside Level 0 delimiter)
- Author line
- Keywords line (config, toml, yaml may omit)
- Copyright line
- History section with creation entry
- Commentary section
- References section (elisp/vim must also include Location/XDG notes)

**Title Case** [ERROR]:
Section and subsection titles must capitalize the first letter of every word.
Abbreviations follow their established convention: ALL CAPS for standard
abbreviations (e.g., `FZF Preview`, `PDF Tools`), lowercase for established
lowercase names (e.g., `cc Mode`, `sh Mode`, `xref`).

**Delimiter hierarchy** [ERROR]:
- Level 0 (file header): 79 `=` chars (`;;` for elisp, `"` for vim)
- Level 1 (primary sections): 79 `-` chars (`;;` for elisp, `"` for vim)
- Level 2 (subsections): `# --- Title ---` (`;; --- Title ---` for elisp,
  `" --- Title ---` for vim)

**Blank line rules** [ERROR]:
- One blank line before Level 1 opening, one after Level 1 closing
- Level 2: no blank line after the delimiter — code follows immediately
- Between top-level statements within same subsection: one blank line
- Related statements (consecutive exports, setqs, sets) not separated
- **Prohibited**: two or more consecutive blank lines anywhere

**Line length** [WARN]:
79 characters maximum. Per-type exceptions:
- bash/zsh: URLs, file paths, help text in printf/print/echo
- elisp: URLs, symbol names, docstrings that cannot split
- vim: URLs, option values that cannot split; line continuation with `\`
  aligned under opening statement
- config: URLs, tool-specific values that cannot split
- toml: format strings with `$variable` chains (starship), URLs
- yaml: URLs, long command strings in `run:` fields, block scalar content

**Section uniqueness** [ERROR]:
No duplicate section titles at the same delimiter level (Level 1 or Level 2)
within a single file.

**Comments explain why** [WARN]:
Flag comments that restate the code instead of explaining reasoning.

```bash
# WRONG — restates the code
(( count += 1 ))

# CORRECT — explains reasoning
# Retries capped at 3 to avoid hammering a down service
(( count += 1 ))
```

**End-of-line comments** [ERROR]:
Flag code lines with trailing comment annotations. Ignore comment-only lines.
Applies to all comment styles: `#` (shell, config, toml, yaml), `;;` (elisp),
`"` (vim).

```bash
# WRONG — flag
export PATH="$HOME/bin:$PATH"  # Add bin to PATH
# CORRECT — do NOT flag (comment-only line)
# Personal builds take precedence
export PATH="$HOME/bin:$PATH"
```

**Align values** [ERROR]:
Flag extra spacing used to align `=` assignments, `:` colons, or case branch
formatting. Exception: vim `hi` (highlight) definitions — column alignment
across `guifg`/`guibg`/`gui` fields is intentional.

```bash
# WRONG — flag
case "$mode" in
  install)    _install  ;;
  uninstall)  _remove   ;;
esac
```

**Security: secrets** [ERROR]:
No hardcoded API keys, tokens, passwords, private keys, certificates anywhere.
Flag suspicious patterns (hardcoded URLs with credentials, bare string secrets).
Each type has its own preferred strategy:
- bash/zsh: read from env vars (`${API_KEY:-}`)
- elisp: read from env vars (`(getenv "API_KEY")`)
- config: git uses `[include] path`, ghostty uses `config-file = ?`
- toml/yaml: prefer env vars; split-file only when tool documents merge
- vim: use `$VAR` environment variables

**Nesting limit** [WARN]:
Maximum 3 nesting levels of control structures. Use early-return guards to
flatten.
```

- [ ] **Step 4: Write Step 3.1 — Bash Checks**

Append the bash-specific section. Each check must include WRONG/CORRECT
examples drawn from `bash.md`:

```markdown
### 3.1 Bash Checks

Apply to files matching: `setup.sh`, `claude/**/*.sh`, `platform/**/*.sh`

**Shebang** [ERROR]:
`#!/usr/bin/env bash` required for directly executable scripts.
Sourced files (function libraries, env setup) must omit the shebang.

**Strict mode** [ERROR]:
`set -euo pipefail` required for executable scripts.
NOT required for interactive `.bashrc` files.

**Error handling** [WARN]:
- ERR trap present for scripts:
  ```bash
  _err_handler() {
    local -r code=$?
    printf '[error] %s() line %d: exit %d\n' \
      "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
  }
  trap '_err_handler' ERR
  ```
- EXIT trap for cleanup (temp files, lock files)
- Exit codes: 0 success, 1 error, 2 misuse, 126 not executable, 127 not found

**Variable quoting** [ERROR]:
All variable references must be quoted (`"$var"`). Flag unquoted variable
expansions.

**Conditional style** [WARN]:
`[[ ]]` for string tests, `(( ))` for arithmetic.
Flag `[ ]` (POSIX test) and `(( ))` for strings.

**printf over echo** [WARN]:
Flag `echo` usage except in help/usage functions. Use `printf` with
single-quoted format strings.

```bash
# WRONG
echo "error: $pkg not found"
# CORRECT
printf 'error: %s not found\n' "$pkg" >&2
```

**ANSI quoting** [ERROR]:
Flag `'\033[...]'` single-quoted escape sequences. Must use `$'...'`.

```bash
# WRONG — stores literal "\033" text
readonly _RED='\033[0;31m'
# CORRECT — $'...' converts to actual ESC byte
readonly _RED=$'\033[0;31m'
```

**Naming conventions** [WARN]:
- Constants: `UPPER_SNAKE_CASE` with `readonly`
- Function locals: `lower_snake_case` with `local`
- Private/internal functions: `_` prefix
- Public entry functions (e.g., `main`): no prefix

**Short-circuit control flow** [ERROR]:
Flag standalone statements using `&&` or `||` as control flow shorthand.
`&&`/`||` inside `if`, `while`, or `until` conditions are fine — do NOT flag.

```bash
# WRONG — flag
[[ -f "$file" ]] && cat "$file"
[[ ! -d "$dir" ]] || mkdir -p "$dir"
# CORRECT — do NOT flag
if [[ ! -r "$file" ]] || [[ ! -s "$file" ]]; then ...
while [[ -n "$line" ]] && [[ "$line" != "EOF" ]]; do ...
```

**eval usage** [ERROR]:
Flag `eval` on user input or variables. Use explicit `case` dispatch.

**Local command substitution** [WARN]:
Flag `local var="$(cmd)"` on one line — `local` always exits 0, masking the
command's exit code. Must use separate lines:
```bash
local dir
dir="$(dirname "$file")"
```

**Function patterns** [WARN]:
- Scripts longer than ~20 lines should wrap logic in `main()`
- Parameter validation at function entry for required params
- Single responsibility per function [INFO]

**Arrays** [WARN]:
- Explicit declaration: `local -a arr=(...)`, `declare -A map=(...)`
- Always quote `"${arr[@]}"` for word-splitting safety

**Magic numbers** [WARN]:
Flag bare numeric literals (not 0, 1, or values in obvious contexts like
`${arr[@]}`). Use `readonly` named constants.

**Security: permissions** [INFO]:
- Scripts: 755, Config files: 644, SSH keys/secrets: 600

**Dry-run pattern** [INFO]:
When script supports dry-run, check the flag at point of action, not call site.
```

- [ ] **Step 5: Write Step 3.2 — Zsh Checks**

```markdown
### 3.2 Zsh Checks

Apply to files matching: `shell/zsh/**/*.zsh`, `shell/zsh/**/.zshenv`

**Shebang** [ERROR]:
`#!/usr/bin/env zsh` for executable scripts, omitted for sourced files
(conf.d/*.zsh, .zshrc, completion scripts).

**Strict mode** [ERROR]:
`set -euo pipefail` for scripts. NOT for interactive `.zshrc` — causes
unexpected exits on failed commands.

**Error handling** [WARN]:
Zsh-specific ERR trap (not bash syntax):
```zsh
_err_handler() {
  print -u2 "[error] ${funcstack[2]:-main}: line ${funcfiletrace[1]##*:}: exit $?"
}
trap '_err_handler' ERR
```
Flag if ERR trap uses bash-specific `${FUNCNAME[1]}` or `${BASH_LINENO[0]}`.

**print over echo** [WARN]:
Flag `echo` for simple output — use `print`. Flag `printf` when no format
specifiers are needed — use `print`.
```zsh
# WRONG
echo "processing $file"
printf "error: %s not found\n" "$pkg"   # no format specifiers needed
# CORRECT
print "processing $file"
print -u2 "error: $pkg not found"
```

**ANSI quoting** [ERROR]:
Same as bash: `$'...'` for escape sequences.

**Glob qualifiers** [WARN]:
Flag `find` + `grep` pipelines where zsh glob qualifiers would work.
Common qualifiers: `.` (file), `/` (dir), `N` (null glob), `om` (mtime),
`[1]` (first match), `L+0` (non-empty).
```zsh
# WRONG — forks find
newest="$(find . -maxdepth 1 -type f -printf '%T@ %p\n' | sort -rn | head -1)"
# CORRECT — zsh-native
newest=( *(.Nom[1]) )
```

**Parameter expansion flags** [WARN]:
Flag `cut`, `tr`, `awk` calls where zsh parameter flags would work:
`${(@s.:.)var}` (split), `${(U)var}` (uppercase), `${(f)content}` (split
lines), `${(j., .)array}` (join).

**emulate for portable functions** [WARN]:
Autoloaded functions and shared library functions must use `emulate -L zsh`
at the top. NOT needed for conf.d files or scripts with `#!/usr/bin/env zsh`
shebang.

**conf.d naming** [WARN]:
Files must follow `NN-name.zsh` pattern: two-digit number with leading zero,
lowercase hyphen-separated name, `.zsh` extension (never `.sh`).

**conf.d numbering ranges** [INFO]:
| Range  | Purpose                    |
|--------|----------------------------|
| 00-09  | Environment & paths        |
| 10-19  | Shell behavior & options   |
| 20-29  | Aliases & abbreviations    |
| 30-39  | Completion system          |
| 40-49  | Plugin management          |
| 50-59  | Prompt theme               |
| 60-69  | Key bindings               |
| 70-89  | Tool integrations          |
| 90-99  | Local overrides            |

Flag files numbered outside their logical category.

**Shell options** [WARN]:
- One `setopt`/`unsetopt` per line, with comment explaining what it does
- All options should be in a dedicated conf.d file (e.g., `10-options.zsh`)
- Not scattered across multiple files (except completion-related options)

**Zinit plugin declarations** [WARN]:
- Must use `zinit ice` + `zinit light` pairs
- Ice modifiers on single line with space-separated key-value pairs

**Plugin comments** [WARN]:
Every plugin declaration must include:
1. Why it loads at this timing (sync vs turbo)
2. Config variables set BEFORE ice/light pair with comment
3. Non-obvious ice modifiers explained

**compinit caching** [INFO]:
Cache at `$XDG_CACHE_HOME/zsh/zcompdump` with freshness check using glob
qualifiers (not `find` or `stat`).

**zstyle formatting** [WARN]:
- Continuation lines (`\`) when value exceeds 79 chars
- Group zstyle by context (`:completion:*`, `:fzf-tab:*`)

**fzf-tab coexistence** [WARN]:
Flag `zstyle ':completion:*' menu select` when fzf-tab is loaded — it creates
dead config that fzf-tab overrides. Must use `menu no` instead.

**Function patterns** [WARN]:
- `main()` wrapper for scripts >20 lines
- Parameter validation at entry
- Local command substitution on separate lines (same as bash)
```

- [ ] **Step 6: Write Step 3.3 — Elisp Checks**

```markdown
### 3.3 Elisp Checks

Apply to files matching: `editor/emacs/.config/emacs/**/*.el`
(exclude `site-packages/` directory)

**Lexical binding** [ERROR]:
File header must include `-*- lexical-binding: t; -*-` in the mode line.

**File tail: provide** [ERROR]:
`(provide 'omw-module)` must be present before the ends-here line.

**File tail: ends here** [ERROR]:
`;;; filename.el ends here` must be the last line (without newline).

**Feature name** [ERROR]:
Feature name in `provide` must match file base name using hyphenated form:
file `omw-font.el` provides `'omw-font`. The `omw/` prefix is for code
symbols only — never for feature names or file names.

**Module loading** [ERROR]:
Local modules must use `require`, not `use-package`. `use-package` is for
third-party packages only.

**use-package keyword order** [WARN]:
Keywords must follow this order:
`:ensure` → `:demand`/`:defer` → `:when`/`:if` → `:after` → `:requires` →
`:mode`/`:interpreter` → `:magic`/`:magic-fallback` → `:hook` →
`:bind`/`:bind*` → `:bind-keymap`/`:bind-keymap*` → `:chords` →
`:custom-face` → `:custom` → `:init` → `:config`

**:after implies :defer** [WARN]:
Flag `:after X` combined with `:defer t` — the `:after` keyword already
defers loading.

**:custom vs setq** [WARN]:
Flag `:custom` for ordinary variable assignments — prefer `setq` in `:config`.
Only use `:custom` when a package's setter side-effect is explicitly needed.

**Docstrings** [WARN]:
Every public function (non-`s:`, non-helper) requires a docstring. First line
must be a complete sentence.

**Naming** [WARN]:
- All project symbols use `omw/` prefix: `omw/setup-fonts`, `omw/http-proxy`
- kebab-case only — flag camelCase
- Predicate names end with `-p`: `omw/buffer-empty-p`
- File names use `omw-` with hyphen: `omw-font.el`

**Key binding syntax** [ERROR]:
Must use `kbd` macro. Flag raw escape strings like `"\C-c\C-s"`.
```elisp
;; WRONG
(define-key map "\C-c\C-s" #'omw-shell-sync)
;; CORRECT
(define-key map (kbd "C-c C-s") #'omw-shell-sync)
```

**Sharp quoting** [WARN]:
Flag bare `'function-name` in `add-hook`, `define-key`, `push` onto hook
lists. Must use `#'function-name` for compile-time verification.

**push over add-to-list** [ERROR]:
Flag `add-to-list` in init/config files. Use `push` instead — O(1) prepend,
lexically safe.

**Hook functions** [ERROR]:
Flag anonymous lambdas in `add-hook`. Use named functions.
`use-package` `:hook` keyword is acceptable (generates named functions).

**Interactive declaration** [WARN]:
User-facing commands (key-bound, M-x callable) must have `(interactive)`.
Internal helpers (`s:`, non-interactive) must not.

**Optional dependency guard** [WARN]:
Check for `featurep`/`fboundp` before calling optional features.

**Variable assignment** [WARN]:
`setq` by default. `setopt` only when custom setter side-effect is needed.

**Buffer-local variables** [WARN]:
Must use `defvar-local`, not `make-local-variable`.

**Validation at boundaries** [WARN]:
Function parameters validated at entry for public functions.

**Error handling** [WARN]:
- `ignore-errors` only for side effects that must not interrupt
- `condition-case` must log or handle, never silently discard on critical paths

**Security: no .elc** [ERROR]:
Flag any committed `.elc` files in the elisp tree.

**Security: secrets** [ERROR]:
No hardcoded credentials. Use `(getenv "VAR")` for sensitive values.

**Package sources** [WARN]:
`:vc` installs (direct from Git) must include a comment explaining why the
package is not available on ELPA/MELPA.
```elisp
;; CORRECT — explains why :vc is needed
;; Not yet published to ELPA/MELPA; install directly from upstream.
(use-package some-package
  :vc (:url "https://github.com/author/some-package.el" :rev :newest)
  ...)
```

**LSP: eglot only** [ERROR]:
Flag any `lsp-mode` usage (imports, hooks, configuration). This project uses
`eglot`.
```

- [ ] **Step 7: Write Steps 3.4–3.7 (Config, TOML, YAML, Vim)**

```markdown
### 3.4 Config Checks

Apply to files matching: `**/git/config`, `**/ripgrep/rc`, `**/ghostty/config`

Detect tool by file path: git/config → git, ripgrep/rc → ripgrep,
ghostty/config → ghostty.

**INI indentation** [ERROR]:
- git config: 4 spaces for key-value pairs within `[section]` blocks
- ripgrep rc, ghostty config: no indentation (flat)

**INI section headers** [ERROR]:
For git config: one INI `[section]` per Level 1 block. Never place a Level 1
delimiter inside an INI section.

**Per-tool syntax** [WARN]:
- git: `key = value` within `[section]`; multi-value via repeated key;
  env var expansion (`$HOME`) and `~` expansion supported
- ripgrep: `--flag` or `--flag=value` syntax; no indentation
- ghostty: `key = value` syntax; multi-value comma-separated; no indentation

**Local override patterns** [WARN]:
- git: `[include] path = ~/.config/git/config.local` for secrets/identity
- ghostty: `config-file = ?config.local` (`?` suppresses errors if absent)
- ripgrep: N/A (no config merge)

**Ripgrep caveat** [WARN]:
Flag `~` or `$HOME` or any env var in ripgrep rc — ripgrep does not expand
them. `RIPGREP_CONFIG_PATH` must be an absolute path set in shell env.

**Value types** [WARN]:
- Booleans: `true`/`false` unquoted (not `"true"`)
- Strings: quoted when containing spaces
- Numbers: bare integer
- Paths: `~/.config/app/file` or `$XDG_CONFIG_HOME/...` (tool-dependent)

### 3.5 TOML Checks

Apply to files matching: `shell/starship/**/*.toml`, `tool/yazi/**/*.toml`,
`lang/**/*.toml`

**String quoting** [ERROR]:
All string values must be quoted (`"value"` or `'value'`). Numbers and
booleans must be bare. Flag unquoted string values.

**Trailing commas** [ERROR]:
TOML does not support trailing commas. Flag `["a", "b",]`.

**Blank line rules** [ERROR]:
- One blank line between top-level tables
- No blank lines within a table (between key-value pairs)
- One blank line between `[[array_of_tables]]` entries
- Prohibited: 2+ consecutive blank lines

**Table styles** [WARN]:
- `[[table]]` required for list entries — flag single-table workarounds
- Inline tables `{ k = "v" }` acceptable for 2 keys or fewer

**Dotted keys** [WARN]:
Prefer `[section]` + `key = "val"` over `section.key = "val"` for tables with
multiple keys.

**Multiline strings** [WARN]:
- `'''...'''` (literal) for regex patterns and format strings with `$` or `\`
- `"""..."""` (basic) when escape processing is needed

**Formatting** [WARN]:
One space around `=` in key-value pairs. Flag missing or extra spaces.

### 3.6 YAML Checks

Apply to files matching: `tool/**/*.yml`, `tool/**/*.yaml`

**Indentation** [ERROR]:
2-space indentation consistently. Flag tabs, 4-space, or mixed indentation.

**Null values** [ERROR]:
Must use explicit `null`. Flag `~` (tilde null) and bare empty values
(`key:` with nothing after the colon on same or next line).

**Boolean conversion** [ERROR]:
Flag `yes`, `no`, `on`, `off` as boolean values (YAML 1.1 Norway problem).
Must use `true`/`false`. Country codes or other string values that could be
misinterpreted must be quoted: `'NO'`.

**String quoting** [WARN]:
- Plain scalar (no quotes): safe values, no special characters
- Single quotes: value contains YAML special chars (`: # [ ] { } , & * ? | - < > = ! % @`)
- Double quotes: only when escape sequences needed (`\n`, `\t`)

**Multiline strings** [WARN]:
- `|` (literal): for scripts, shell commands — preserves newlines
- `>` (folded): for long prose — collapses newlines to spaces
- Flag wrong block scalar type for context

**Anchors/aliases** [WARN]:
Flag YAML anchors (`&`) and aliases (`*`) in dotfiles configs. They harm
grep-ability and confuse editors. Acceptable in CI/CD with comment.

**Blank line rules** [ERROR]:
- One blank line between top-level keys
- No blank lines within a nested mapping
- One blank line after a block scalar (`|` or `>`)
- Prohibited: 2+ consecutive blank lines

### 3.7 Vim Checks

Apply to files matching: `editor/vim/**/vimrc`, `editor/vim/**/*.vim`

**Comment style** [ERROR]:
Vim uses `"` for line comments. Flag `#` used as comment character in vim
files.

**augroup required** [ERROR]:
All autocmds must be wrapped in `augroup` with `autocmd!` to prevent
duplication on re-source.
```vim
" CORRECT
augroup vimrc
    autocmd!
    autocmd BufReadPost * ...
augroup END
```

**function! + abort** [ERROR]:
- `function!` with `abort` keyword required
- Script-local functions use `s:` prefix
- Flag `function` without `!` and functions missing `abort`

**noremap variants** [WARN]:
Flag recursive mappings: `map`, `nmap`, `vmap`, `imap`, `xmap`, `omap`,
`cmap`. Use `noremap` variants: `nnoremap`, `vnoremap`, `inoremap`, etc.
`<silent>` required for non-interactive mappings.

**Variable scope** [WARN]:
- `s:var` for script-local internal state (preferred)
- Flag `g:var` for internal state — should use `s:` instead
- `b:var`, `w:var` for buffer/window-local state
- `&option` for reading/writing Vim options

**set vs let** [WARN]:
- `set` for Vim options: `set number`, `set tabstop=4`
- `let &option` for computed values: `let &undodir = s:data . '/vim/undo'`
- `setlocal` for buffer-local settings in autocommands

**Conditional logic** [WARN]:
Use `has('feature')` for capability checks, not version number comparisons.

**Regular expressions** [WARN]:
Prefer `\v` (very magic) mode for readability.
```vim
if &filetype !~# '\v^(diff|xxd)$'
```

**Highlight definitions** [INFO]:
Group by semantic category. Column alignment across `guifg`/`guibg`/`gui`
fields is acceptable (exception to no-align rule).

**XDG paths** [WARN]:
Vim does not natively support XDG. Flag hardcoded paths — should use
script-local variables with fallbacks:
```vim
let s:cfg = ($XDG_CONFIG_HOME != '' ? $XDG_CONFIG_HOME : $HOME . '/.config')
```
```

- [ ] **Step 8: Write Step 4 — Output Report**

```markdown
## Step 4: Output Report

Severity levels:
- `ERROR` — clear violations (wrong syntax, security issues, anti-patterns,
  missing required elements)
- `WARN` — style suggestions (line length, naming, suboptimal patterns)
- `INFO` — statistics and passes (header OK, sections checked, no issues)

### Findings Table

If any ERROR or WARN findings exist, render them in a markdown table:

```markdown
| Severity | File | Line | Description |
|----------|------|------|-------------|
| ERROR    | path/to/file.el | 42 | description of the issue |
| WARN     | path/to/file.zsh | 15 | description of the issue |
```

If no findings exist, skip the findings table entirely.

### Summary Table

Always end with a per-file summary table and a totals row:

```markdown
| File | Type | Sections | Errors | Warnings | Status |
|------|------|----------|--------|----------|--------|
| path/to/file.el | elisp | 3 L1, 5 L2 | 0 | 0 | PASS |
| path/to/another.zsh | zsh | 2 L1, 4 L2 | 1 | 2 | FAIL |
| **Total** | | | **1** | **2** | |
```

- `Type` is the file type (bash, zsh, elisp, vim, config, toml, yaml)
- `Sections` shows the count of L1 and L2 sections checked
- `Status` is `PASS` (0 errors) or `FAIL` (any errors)

For skipped files (no matching rules), include a single row:
```markdown
| path/to/file | — | — | — | — | SKIP |
```

## Important Notes

- Read each file completely — do not skip sections
- Do NOT suggest fixes — this is a read-only check
- If a file matches no rules path pattern, mark it as SKIP in the summary
- For `all` target, process files in grouped order by category
  (shell → editor → tool → lang → platform)
- Report header must include check count: `Checked N files across T types`
```

- [ ] **Step 9: Write the complete file**

Using the Edit tool or Write tool, replace the entire content of
`.claude/commands/lint.md` with the assembled content from Steps 2-8.
The file must be a single coherent document with no gaps or placeholders.

- [ ] **Step 10: Verify file structure**

Read the completed file and verify:
1. Frontmatter is valid YAML with description and arguments
2. All 4 steps are present (Resolve, Read Rules, Check, Report)
3. Step 3 contains exactly 8 subsections (3.0-3.7)
4. Each per-type section lists all checks from the spec
5. No TBD, TODO, or placeholder text anywhere
6. All code examples use correct syntax for their language

---

### Task 2: Test the revised lint command

**Files:**
- Test: `.claude/commands/lint.md` (invoked via `/lint`)

- [ ] **Step 1: Run `/lint setup.sh`**

Run the lint command against the main setup script. This exercises bash checks.
Verify the output includes:
- File header checks
- Bash-specific checks (shebang, strict mode, naming, etc.)
- Findings table with severity levels
- Summary table with PASS/FAIL status

- [ ] **Step 2: Run `/lint shell/zsh/`**

Run against all zsh files. Verify zsh-specific checks appear:
- conf.d naming patterns
- print vs echo warnings
- Glob qualifier suggestions
- Plugin declaration checks

- [ ] **Step 3: Run `/lint editor/emacs/.config/emacs/lisp/`**

Run against elisp modules (exclude site-packages). Verify elisp-specific
checks appear:
- Lexical binding in mode line
- File tail (provide, ends here)
- use-package keyword order
- Sharp quoting warnings

- [ ] **Step 4: Run `/lint all`**

Full repo scan. Verify:
- All file types are detected and checked
- Files are processed in category order (shell → editor → tool → lang → platform)
- Skipped files (no matching rules) show SKIP status
- Report header includes check count

- [ ] **Step 5: Verify output completeness**

Check the `/lint all` output against the spec's 96-check catalog:
- Universal checks appear for every file
- Per-type checks appear for their respective file types
- No rule from the spec is missing

---

### Task 3: Commit

**Files:**
- Modified: `.claude/commands/lint.md`

- [ ] **Step 1: Stage and commit**

```bash
git add .claude/commands/lint.md
git commit -m "feat: expand lint command to full rule coverage (96 checks)

Reorganize Step 3 into per-type subsections mapping 1:1 to rule files.
Add bash (16), zsh (17), elisp (22), config (6), toml (7), yaml (7),
vim (10) type-specific checks plus 11 universal checks."
```

- [ ] **Step 2: Verify commit**

Run `git log -1 --stat` and confirm the commit shows
`.claude/commands/lint.md` as the only changed file.
