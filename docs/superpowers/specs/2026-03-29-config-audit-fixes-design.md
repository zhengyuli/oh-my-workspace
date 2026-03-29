# Config Audit Fixes Design

**Date**: 2026-03-29
**Status**: Approved
**Scope**: Fix 11 verified issues from comprehensive config code audit

## Background

A full audit of 92 configuration files across 6 dimensions (quality, correctness,
security, best practices, `.claude/rules` compliance) identified 11 verified issues.
No security vulnerabilities were found. This spec covers all fixes.

## Issues Summary

| ID   | Severity   | Description                              |
|------|------------|------------------------------------------|
| E1   | ERROR      | Missing git global ignore file           |
| C1   | CONVENTION | Git editor = vim conflicts with nvim     |
| C2   | CONVENTION | Git merge.tool = vimdiff should be nvim  |
| C3   | CONVENTION | Time-stamp author name inconsistency     |
| C4   | CONVENTION | Single-quoted strings in starship.toml   |
| C5   | CONVENTION | Lines exceeding 79-char limit            |
| C6   | CONVENTION | Magic numbers in defaults.sh             |
| C7   | CONVENTION | Template.sh header deviations            |
| C8   | CONVENTION | Unquoted string in lazygit config.yml    |
| Q1   | QUALITY    | WHAT comments in yazi theme.toml         |
| Q2   | QUALITY    | WHAT comment in lazygit config.yml       |

## Fix Design

### E1: Create Global Git Ignore File

Create `tool/git/.config/git/ignore` with standard cross-platform patterns. The
file `tool/git/.config/git/config` line 37 references `~/.config/git/ignore` via
`core.excludesfile`, but the file does not exist in the repository. After stowing,
git silently skips global ignore patterns.

The new file will include patterns for: macOS artifacts (.DS_Store), editor swap
files, IDE directories, Python bytecode, and Node modules. This follows the
`config.md` file header convention with 79-char line width.

No changes to `tool/git/.config/git/config` are needed.

### C1: Remove Hardcoded Git Editor

Remove `editor = vim` from `tool/git/.config/git/config` line 26. Git inherits
`$EDITOR`/`$VISUAL` from the shell environment, which `shell/zsh/.config/zsh/
conf.d/00-env.zsh` lines 28-29 set to `nvim`. Neovim is installed via Homebrew
and is the project's chosen editor.

### C2: Change Git Merge Tool

Change `merge.tool = vimdiff` to `merge.tool = nvimdiff` at
`tool/git/.config/git/config` line 191 for consistency with the nvim setup.

### C3: Fix Time-stamp Author Name

Nine `.el` files have `by zhengyuli` in their time-stamp while 23 others have
`by zhengyu.li`. The `time-stamp` package in `init.el` uses `%u` which expands
to `user-login-name`. Fix by changing the `time-stamp-format` in `init.el` to
use a hardcoded author string `zhengyu.li` instead of `%u`.

Files affected: `init.el`, `omw-appearance.el`, `omw-font.el`,
`omw-gitconfig.el`, `omw-json.el`, `omw-shell.el`, `omw-git.el`,
`omw-pdf.el`, `omw-term.el`.

### C4: Fix Single-quoted Strings in starship.toml

Per `toml.md`, strings must always use double quotes. Change two lines in
`shell/starship/.config/starship.toml`:

- Line 11: `"$schema" = 'https://...'` to `"$schema" = "https://..."`
- Line 192: `tag_symbol = '...'` to `tag_symbol = "..."`

### C5: Fix Lines Exceeding 79 Characters

Wrap approximately 15 notable code lines (excluding MIT license boilerplate
which is legal text and stays at 80 chars). Targets:

- `setup.sh`: error handler format, long comments
- `defaults.sh`: long comments
- `omw-json.el` line 41: split tool spec across lines
- `omw-dockerfile.el` line 41: split tool spec across lines
- `omw-appearance.el` lines 91-99, 127: split face specs
- `omw-explorer.el` line 72: split dired columns list
- `omw-proxy.el` line 93: split regex pattern
- `uv.toml` line 14: wrap comment

### C6: Extract Magic Numbers in defaults.sh

Add `readonly` named constants near `_general_ui()` in
`platform/darwin/defaults.sh`:

- `STANDBY_DELAY_24H=86400` for line 90
- `DISABLE_LINE_MARKS=0` for line 309

### C7: Fix template.sh Header

Fix `editor/emacs/.config/emacs/templates/template.sh`:

- Change delimiters from 88 chars to 79 chars
- Add mode line `# -*- mode: sh; -*-`
- Keep `#!/bin/bash` as-is (template output, env not needed for yasnippet)

### C8: Quote String in lazygit config.yml

Per `yaml.md`, strings with special characters must be quoted. Change
`tool/lazygit/.config/lazygit/config.yml` line 57:

`pager: delta --dark --paging=never` to `pager: "delta --dark --paging=never"`

### Q1: Improve yazi theme.toml Comments

Replace WHAT comments with WHY comments in `tool/yazi/.config/yazi/theme.toml`
lines 20, 24, 28:

- "Override mgr border style" to explain the border lacks contrast on dark themes
- "Override indicator..." to explain the preview pane indicator is redundant
- "Style status bar permissions..." to explain reducing visual weight

### Q2: Improve lazygit config.yml Comment

Change line 24 from `# Catppuccin Mocha Blue theme` to include a rationale
(e.g., "for visual consistency with terminal color scheme").

## Non-issues (Verified Correct)

These were flagged but verified as non-issues:

- `eval "$(gdircolors/starship/direnv/zoxide)"`: Standard trusted-tool init
- `config.local` with personal info: Gitignored, not tracked
- `bunfig.toml` hardcoded `~/.cache`: Bun TOML doesn't support variable expansion
- Git config hardcoded `~/.config/`: Git config can't expand `$XDG_CONFIG_HOME`
- `catppuccin-mocha flavor.toml` violations: Third-party managed file

## Out of Scope

- MIT license 80-char lines in .el files (legal boilerplate, leave unchanged)
- Third-party Yazi plugin files (managed by `ya pkg`)
- Structural changes (Stow, Brewfile, PKG_ALL all verified correct)
