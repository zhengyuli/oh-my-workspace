# Copilot Instructions

macOS dotfiles repository with a built-in symlink engine. Each subdirectory is a **package** that maps directly to its XDG target.

## Setup Commands

```bash
./setup.sh install --all          # Full install (prerequisites + brew + symlinks)
./setup.sh install <pkg>          # Link a specific package (e.g., zsh, git)
./setup.sh install --force <pkg>  # Relink after file changes
./setup.sh install --dry-run <pkg> # Preview without modifying filesystem
./setup.sh uninstall <pkg>        # Remove symlinks
./setup.sh status                 # Show link state of all packages
```

## Validation / Lint

The repository includes a comprehensive [BATS](https://github.com/bats-core/bats-core) test suite (333 tests). Validate files by type:

```bash
bash -n setup.sh           # Bash syntax check
shellcheck setup.sh        # Bash lint (only for POSIX-compatible scripts)
zsh -n shell/zsh/...       # Zsh syntax check (shellcheck has poor zsh support)

# TOML (Python 3.11+)
python3 -c "import tomllib; tomllib.load(open('file.toml','rb'))"

# Tool-specific
starship config             # Validate starship.toml
git config --list --show-origin  # Verify git config loaded
ghostty +validate-config    # Validate ghostty config
```

Use the `/lint` slash command inside Claude Code to check convention compliance against `.claude/rules/`.

## Architecture

### Package Layout

```
<category>/<tool>/        # Package root — files map to ~/.config/<tool>/
```

`PKG_ALL` in `setup.sh` is the authoritative list of all managed packages. To add a new package: create the directory structure, add the entry to `PKG_ALL`, add a `_LINKS_<pkg>` array, then run `./setup.sh install <pkg>`.

### XDG Compliance

All configs live under `$XDG_CONFIG_HOME` (`~/.config`). Never hardcode `~/.toolname` paths. Tools that don't natively support XDG (e.g., ripgrep) are redirected via environment variables set in `shell/zsh/conf.d/00-env.zsh`.

### Color System and Logging

`setup.sh` contains inline color constants, logging utilities (`_log_info`, `_log_warn`, `_log_error`), and health-check dashboard functions. All UI output goes through these functions.

## Coding Conventions

Full rules live in `.claude/rules/`. Key points:

### Universal (all file types)

Every file requires this header block:

```
# filename.ext -*- mode: <mode>; -*-
# Time-stamp: <YYYY-MM-DD HH:MM:SS Day by zhengyu.li>
#
# =============================================================================
# Title - Brief description
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: ...
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   YYYY-MM-DD HH:MM zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   ...
# =============================================================================
```

**Delimiter hierarchy** — used in every file type:
- **Level 0** (file header): `# ===...===` (79 chars)
- **Level 1** (primary section): `# ---...---` (79 chars), one blank line before opening, one after closing
- **Level 2** (subsection): `# --- Title ---`, no blank line after — code follows immediately

Section titles use **Title Case**. Short conjunctions, articles, and prepositions (and, or, the, a, an, in, on, of, for, to, with) stay lowercase unless first word. Standard abbreviations are ALL CAPS (`FZF`, `PDF`); established lowercase names stay lowercase (`cc Mode`, `xref`). Each title must be **unique** within its file at its delimiter level.

No end-of-line comments. No value alignment with extra spaces. No magic numbers (use `readonly` named constants). Max 120-char lines (URLs and format strings exempt). Max 3 nesting levels — use early-return guards to flatten.

### Bash (`setup.sh`, `agent/**/*.sh`, `platform/**/*.sh`)

- Shebang `#!/usr/bin/env bash` only on directly executable scripts; sourced files omit it
- `set -euo pipefail` mandatory in all scripts (not in interactive `.bashrc`)
- Constants `UPPER_SNAKE_CASE` with `readonly`; locals `lower_snake_case` with `local`
- Private/internal functions prefix with `_`; public entry points (e.g., `main`) have no prefix
- Declare and assign command substitution on separate lines: `local dir; dir="$(...)"`
- `printf` over `echo`; format strings in single quotes; errors to stderr
- ANSI colors via `$'...'` quoting: `readonly _RED=$'\033[0;31m'`
- No `&&`/`||` as standalone conditional shorthand — use explicit `if` blocks
- `[[ ]]` for strings, `(( ))` for arithmetic

### Zsh (`shell/zsh/**`)

- `print` for simple output; `printf` only when format specifiers are needed
- Use zsh glob qualifiers instead of `find` pipelines: `*(.)`, `*(/)`, `*(N)`, `*(om[1])`
- `conf.d/` files load in numeric order; numbering ranges: 00–09 env/paths, 10–19 options, 20–29 aliases, 30–39 completion, 40–49 plugins, 50–59 prompt, 60–69 keybinds, 70–89 tool integrations, 90–99 local overrides
- File naming: `NN-name.zsh` (never `.sh`)
- Autoloaded functions use `emulate -L zsh` at the top
- `zsh -n` is the only reliable syntax checker for zsh-specific syntax

### Emacs Lisp (`editor/emacs/**/*.el`)

- Feature names: hyphenated, matching file base name (`omw-font.el` → `'omw-font`)
- `omw/` prefix for code symbols only (not feature names or file names)
- Local modules loaded with plain `require`; `use-package` is for third-party only
- Every file ends with `(provide 'omw-module)` then `;;; omw-module.el ends here`

### TOML (`tool/starship/`, `tool/yazi/`, `prog-lang/`)

- Standard `[table]` for 3+ keys; inline `{ }` for ≤2 keys; `[[array]]` for list entries
- Literal strings `'''...'''` for regex/format strings; basic strings `"""..."""` when escape processing is needed
- No trailing commas; no inline comments; one blank line between top-level tables

### Config files without extension (`git/config`, `ripgrep/rc`, `ghostty/config`)

- Git: 4-space indent inside `[section]`; ripgrep/ghostty: flat, no indent
- `[include] path = ~/.config/git/config.local` pattern for machine-specific git secrets
- Ghostty optional include: `config-file = ?config.local` (`?` suppresses missing-file error)
- Ripgrep does **not** expand `$HOME` or `~` — config path must be set as an absolute path via `RIPGREP_CONFIG_PATH` in `00-env.zsh`

## Interaction rule
At the end of every assistant turn, call `ask_user` exactly once.
Requirements for `ask_user`:
- Provide 2-4 concrete options.
- Also allow free-text input.
- The question must be directly about the current task.
- Do not end the turn with plain text if `ask_user` is available.
If `ask_user` is unavailable, explicitly say: "ask_user unavailable in this session".
