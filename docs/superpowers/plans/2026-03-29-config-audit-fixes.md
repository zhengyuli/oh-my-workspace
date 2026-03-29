# Config Audit Fixes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix 12 verified issues from the comprehensive config code audit.

**Architecture:** Direct edits to config files across 7 categories. No new
dependencies. All fixes are independent except C1+C2 (same file) and
C8+C9+Q2 (same file).

**Tech Stack:** Config files only (gitconfig, TOML, YAML, Elisp, Bash)

**Spec:** `docs/superpowers/specs/2026-03-29-config-audit-fixes-design.md`

---

## File Map

| Action | File | Fix IDs |
|--------|------|---------|
| Create | `tool/git/.config/git/ignore` | E1 |
| Modify | `tool/git/.config/git/config` | C1, C2 |
| Modify | `editor/emacs/.config/emacs/init.el` | C3, C5 |
| Modify | `shell/starship/.config/starship.toml` | C4 |
| Modify | `setup.sh` | C5 |
| Modify | `platform/darwin/defaults.sh` | C5, C6 |
| Modify | `editor/emacs/.config/emacs/lisp/lang/omw-json.el` | C5 |
| Modify | `editor/emacs/.config/emacs/lisp/lang/omw-dockerfile.el` | C5 |
| Modify | `editor/emacs/.config/emacs/lisp/editor/omw-appearance.el` | C5 |
| Modify | `editor/emacs/.config/emacs/lisp/editor/omw-explorer.el` | C5 |
| Modify | `editor/emacs/.config/emacs/lisp/lib/omw-proxy.el` | C5 |
| Modify | `editor/emacs/.config/emacs/lisp/text/omw-markdown.el` | C5 |
| Modify | `editor/emacs/.config/emacs/templates/template.sh` | C7 |
| Modify | `tool/lazygit/.config/lazygit/config.yml` | C8, C9, Q2 |
| Modify | `tool/yazi/.config/yazi/theme.toml` | Q1 |
| Modify | `lang/python/uv/.config/uv/uv.toml` | C5 |

---

### Task 1: Create global git ignore file (E1)

**Files:**
- Create: `tool/git/.config/git/ignore`

- [ ] **Step 1: Create the ignore file**

Create `tool/git/.config/git/ignore` with config.md file header convention:

```
# ignore -*- mode: gitignore; -*-
# Time-stamp: <2026-03-29 00:00:00 Saturday by zhengyu.li>
# =============================================================================
# Global Git Ignore Patterns
#
# Cross-platform patterns applied to all repositories.
# Local repo ignores go in .gitignore per-repository.
#
# References:
#   1. gitignore docs: https://git-scm.com/docs/gitignore
# =============================================================================

# -----------------------------------------------------------------------------
# macOS
# -----------------------------------------------------------------------------
.DS_Store
.AppleDouble
.LSOverride

# -----------------------------------------------------------------------------
# Editor / IDE
# -----------------------------------------------------------------------------
*.swp
*~
.#*
\#*\#
.vscode/
.idea/

# -----------------------------------------------------------------------------
# Python
# -----------------------------------------------------------------------------
__pycache__/
*.pyc

# -----------------------------------------------------------------------------
# Node
# -----------------------------------------------------------------------------
node_modules/
```

All lines must be 79 chars or fewer. Delimiters follow Level 0/1 hierarchy.

- [ ] **Step 2: Verify file is tracked**

Run: `git status tool/git/.config/git/ignore`
Expected: File appears as untracked (will be committed with Task 2)

---

### Task 2: Fix git editor and merge tool (C1, C2)

**Files:**
- Modify: `tool/git/.config/git/config`

- [ ] **Step 1: Remove `editor = vim` (C1)**

In `tool/git/.config/git/config`, remove line 26 (`    editor = vim`) and
the preceding comment is not needed since removing the line is self-explanatory.
The `[core]` section already has good comments on remaining entries.

Change:
```
[core]
    editor = vim
    # Use delta for syntax-highlighted, line-numbered diff output
```
To:
```
[core]
    # Use delta for syntax-highlighted, line-numbered diff output
```

- [ ] **Step 2: Change merge tool to nvimdiff (C2)**

In `tool/git/.config/git/config`, change `tool = vimdiff` to `tool = nvimdiff`.

Change:
```
    tool = vimdiff
```
To:
```
    tool = nvimdiff
```

- [ ] **Step 3: Commit E1 + C1 + C2 together**

```bash
git add tool/git/.config/git/ignore tool/git/.config/git/config
git commit -m "fix(git): add global ignore file and use nvim as editor/merge tool

- Create tool/git/.config/git/ignore (E1: core.excludesfile referenced a
  non-existent file)
- Remove core.editor to inherit $EDITOR=nvim from shell (C1)
- Change merge.tool from vimdiff to nvimdiff (C2)"
```

---

### Task 3: Fix time-stamp author name (C3)

**Files:**
- Modify: `editor/emacs/.config/emacs/init.el`

- [ ] **Step 1: Change time-stamp-format in init.el**

In `editor/emacs/.config/emacs/init.el` line 235, change `%u` to hardcoded
`zhengyu.li`:

Change:
```elisp
  (setq time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S %:a by %u"
```
To:
```elisp
  (setq time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S %:a by zhengyu.li"
```

This makes future time-stamp updates use the consistent `zhengyu.li` author
name regardless of the system `user-login-name`.

- [ ] **Step 2: Verify the 6 affected files**

Run: `grep -r "by zhengyuli[^.]" editor/emacs/.config/emacs/ --include="*.el"`
Expected: 6 matches in init.el, omw-appearance.el, omw-gitconfig.el,
omw-git.el, omw-pdf.el, omw-term.el

These will self-correct on next save (time-stamp runs in before-save-hook).

- [ ] **Step 3: Commit**

```bash
git add editor/emacs/.config/emacs/init.el
git commit -m "fix(emacs): use consistent author name in time-stamp-format

Replace %u (user-login-name) with hardcoded zhengyu.li so all files
get consistent time-stamp author strings regardless of system login name."
```

---

### Task 4: Fix single-quoted strings in starship.toml (C4)

**Files:**
- Modify: `shell/starship/.config/starship.toml`

- [ ] **Step 1: Fix line 11 — $schema value**

Change:
```toml
"$schema" = 'https://starship.rs/config-schema.json'
```
To:
```toml
"$schema" = "https://starship.rs/config-schema.json"
```

- [ ] **Step 2: Fix line 192 — tag_symbol value**

Change:
```toml
tag_symbol = ' '
```
To:
```toml
tag_symbol = " "
```

- [ ] **Step 3: Commit**

```bash
git add shell/starship/.config/starship.toml
git commit -m "style(starship): use double-quoted strings per toml conventions"
```

---

### Task 5: Fix line length violations — shell scripts (C5, part 1)

**Files:**
- Modify: `setup.sh`
- Modify: `platform/darwin/defaults.sh`
- Modify: `lang/python/uv/.config/uv/uv.toml`

- [ ] **Step 1: Fix setup.sh line 34**

Change:
```
    • Security: bash 3.2 (macOS default) is from 2007 with known vulnerabilities
```
To:
```
    • Security: bash 3.2 (macOS default) has known vulnerabilities (2007)
```

- [ ] **Step 2: Fix setup.sh line 75**

Change:
```
# Google style: declaration and assignment must be separate when the value comes
# from a command substitution, so that a failure in $() is not masked by the
```
To:
```
# Declaration and assignment must be separate when the value comes from a
# command substitution, so that a failure in $() is not masked by the
```

- [ ] **Step 3: Fix setup.sh line 104**

Change:
```bash
  printf "  ${_RED}[error]${_RESET} unexpected failure in %s() at line %d (exit %d)\n" \
```
To:
```bash
  printf "  ${_RED}[error]${_RESET} failure in %s() line %d (exit %d)\n" \
```

- [ ] **Step 4: Fix setup.sh line 164**

Change:
```
# Returns the stow directory for a package ("shell/zsh" -> "$WORKSPACE_DIR/shell").
```
To:
```
# Returns the stow dir for a package ("shell/zsh" -> "$WORKSPACE_DIR/shell").
```

- [ ] **Step 5: Fix setup.sh lines 308, 310**

Change line 308:
```bash
        log_info "[dry-run] would remove foreign symlink: ${target} -> $(readlink "${target}")"
```
To:
```bash
        log_info "[dry-run] would remove foreign symlink:"
        log_info "  ${target} -> $(readlink "${target}")"
```

Change line 310:
```bash
        log_warn "Removing foreign symlink: ${target} -> $(readlink "${target}")"
```
To:
```bash
        log_warn "Removing foreign symlink:"
        log_warn "  ${target} -> $(readlink "${target}")"
```

- [ ] **Step 6: Fix setup.sh line 461**

Change:
```bash
  local -r url='https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh'
```
To:
```bash
  local -r url
  url='https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh'
```

- [ ] **Step 7: Fix setup.sh line 826**

Change:
```
  ./setup.sh install --all                    Bootstrap: prereqs + brew + stow all
```
To:
```
  ./setup.sh install --all                  Bootstrap: prereqs + brew + stow all
```

- [ ] **Step 8: Fix defaults.sh — wrap long comments**

Line 14: wrap the reference URL. Change:
```
#   1. mathiasbynens/dotfiles: https://github.com/mathiasbynens/dotfiles/blob/main/.macos
```
To:
```
#   1. mathiasbynens/dotfiles:
#      https://github.com/mathiasbynens/dotfiles/blob/main/.macos
```

Line 121: wrap the defaults write. Change:
```
  defaults write NSGlobalDomain NSWindowResizeTime -float "${WINDOW_RESIZE_FAST}"
```
To:
```
  defaults write NSGlobalDomain NSWindowResizeTime \
    -float "${WINDOW_RESIZE_FAST}"
```

Line 139: wrap the defaults write. Change:
```
  defaults write NSGlobalDomain InitialKeyRepeat -int "${INITIAL_KEY_REPEAT_FAST}"
```
To:
```
  defaults write NSGlobalDomain InitialKeyRepeat \
    -int "${INITIAL_KEY_REPEAT_FAST}"
```

Line 264: wrap the defaults write. Change:
```
  defaults write com.apple.dock autohide-delay -float "${DOCK_AUTOHIDE_NO_DELAY}"
```
To:
```
  defaults write com.apple.dock autohide-delay \
    -float "${DOCK_AUTOHIDE_NO_DELAY}"
```

Line 304: wrap the comment. Change:
```
  # Enable Secure Keyboard Entry - blocks other processes from reading keystrokes
```
To:
```
  # Enable Secure Keyboard Entry; blocks other processes from reading keystrokes
```

Line 372: wrap the defaults write. Change:
```
  defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add \
```
To:
```
  defaults write com.apple.messageshelper.MessageController \
    SOInputLineSettings -dict-add \
```

Line 422: wrap the printf. Change:
```
  printf 'Done. Note that some of these changes require a logout/restart to take effect.\n'
```
To:
```bash
  printf 'Done. Some changes require a logout/restart to take effect.\n'
```

- [ ] **Step 9: Fix uv.toml line 14**

Change:
```toml
#   3. PyTorch Integration: https://docs.astral.sh/uv/guides/integration/pytorch/
```
To:
```toml
#   3. PyTorch: https://docs.astral.sh/uv/guides/integration/pytorch/
```

- [ ] **Step 10: Commit shell + TOML line-length fixes**

```bash
git add setup.sh platform/darwin/defaults.sh lang/python/uv/.config/uv/uv.toml
git commit -m "style: wrap long lines to 79-char limit in shell and TOML files

Fix C5 line-length violations in setup.sh, defaults.sh, and uv.toml."
```

---

### Task 6: Fix line length violations — Emacs Lisp (C5, part 2)

**Files:**
- Modify: `editor/emacs/.config/emacs/lisp/lang/omw-json.el`
- Modify: `editor/emacs/.config/emacs/lisp/lang/omw-dockerfile.el`
- Modify: `editor/emacs/.config/emacs/lisp/editor/omw-appearance.el`
- Modify: `editor/emacs/.config/emacs/lisp/editor/omw-explorer.el`
- Modify: `editor/emacs/.config/emacs/lisp/lib/omw-proxy.el`
- Modify: `editor/emacs/.config/emacs/lisp/text/omw-markdown.el`
- Modify: `editor/emacs/.config/emacs/init.el`

- [ ] **Step 1: Fix omw-json.el line 41**

Change:
```elisp
  '(("vscode-json-languageserver" "bun install -g vscode-json-languageserver" "bun")
```
To:
```elisp
  '(("vscode-json-languageserver"
     "bun install -g vscode-json-languageserver" "bun")
```

- [ ] **Step 2: Fix omw-dockerfile.el line 41**

Change:
```elisp
  '(("docker-langserver" "bun install -g dockerfile-language-server-nodejs" "bun"))
```
To:
```elisp
  '(("docker-langserver"
     "bun install -g dockerfile-language-server-nodejs" "bun"))
```

- [ ] **Step 3: Fix omw-appearance.el lines 91-99**

Change:
```elisp
  (centaur-tabs-selected
   ((t (:inherit omw/centaur-tabs-base :bold t :foreground "#28cd41" :height 1.0))))
  (centaur-tabs-selected-modified
   ((t (:inherit omw/centaur-tabs-base :bold t :foreground "#ff9300" :height 1.0))))
  (centaur-tabs-unselected
   ((t (:inherit omw/centaur-tabs-base :bold t :foreground "grey" :height 1.0))))
  (centaur-tabs-unselected-modified
   ((t (:inherit omw/centaur-tabs-base :bold t :foreground "#ff9300" :height 1.0))))
  (centaur-tabs-display-line
   ((t (:inherit omw/centaur-tabs-base :box nil :overline nil :underline nil))))
```
To:
```elisp
  (centaur-tabs-selected
   ((t (:inherit omw/centaur-tabs-base :bold t
        :foreground "#28cd41" :height 1.0))))
  (centaur-tabs-selected-modified
   ((t (:inherit omw/centaur-tabs-base :bold t
        :foreground "#ff9300" :height 1.0))))
  (centaur-tabs-unselected
   ((t (:inherit omw/centaur-tabs-base :bold t
        :foreground "grey" :height 1.0))))
  (centaur-tabs-unselected-modified
   ((t (:inherit omw/centaur-tabs-base :bold t
        :foreground "#ff9300" :height 1.0))))
  (centaur-tabs-display-line
   ((t (:inherit omw/centaur-tabs-base
        :box nil :overline nil :underline nil))))
```

- [ ] **Step 4: Fix omw-appearance.el line 127**

Change:
```elisp
    (let* ((banners-dir (expand-file-name "banners" omw/emacs-config-root-path))
```
To:
```elisp
    (let* ((banners-dir
            (expand-file-name "banners" omw/emacs-config-root-path))
```

- [ ] **Step 5: Fix omw-explorer.el line 72**

Change:
```elisp
        '(vc-state subtree-state nerd-icons git-msg file-modes file-time file-size)
```
To:
```elisp
        '(vc-state subtree-state nerd-icons git-msg
          file-modes file-time file-size)
```

- [ ] **Step 6: Fix omw-proxy.el line 93**

Change:
```elisp
                 "^\\(127\\.0\\.0\\.1\\|localhost\\|10\\..*\\|192\\.168\\..*\\)")
```
To:
```elisp
                 (concat "^\\(127\\.0\\.0\\.1\\|localhost\\|"
                         "10\\..*\\|192\\.168\\..*\\)"))
```

- [ ] **Step 7: Fix omw-markdown.el line 74**

Change:
```elisp
    (face-remap-add-relative 'markdown-header-face-1
     `(:foreground ,header-color :weight bold :height ,omw/markdown-h1-height))
```
To:
```elisp
    (face-remap-add-relative 'markdown-header-face-1
      `(:foreground ,header-color
        :weight bold :height ,omw/markdown-h1-height))
```

- [ ] **Step 8: Fix init.el line 220**

Change:
```elisp
        (list (cons ".*" (expand-file-name "emacs/backup/" omw/xdg-state-home)))
```
To:
```elisp
        (list (cons ".*"
                    (expand-file-name "emacs/backup/" omw/xdg-state-home)))
```

- [ ] **Step 9: Commit Elisp line-length fixes**

```bash
git add editor/emacs/.config/emacs/lisp/lang/omw-json.el \
       editor/emacs/.config/emacs/lisp/lang/omw-dockerfile.el \
       editor/emacs/.config/emacs/lisp/editor/omw-appearance.el \
       editor/emacs/.config/emacs/lisp/editor/omw-explorer.el \
       editor/emacs/.config/emacs/lisp/lib/omw-proxy.el \
       editor/emacs/.config/emacs/lisp/text/omw-markdown.el \
       editor/emacs/.config/emacs/init.el
git commit -m "style(emacs): wrap long lines to 79-char limit in elisp files

Fix C5 line-length violations in 7 Emacs Lisp files."
```

---

### Task 7: Extract magic numbers in defaults.sh (C6)

**Files:**
- Modify: `platform/darwin/defaults.sh`

- [ ] **Step 1: Add constants before _general_ui()**

Before the `_general_ui()` function definition, add:

```bash
readonly STANDBY_DELAY_24H=86400
readonly DISABLE_LINE_MARKS=0
```

- [ ] **Step 2: Replace magic number at standbydelay**

Change:
```bash
  sudo pmset -c standbydelay 86400
```
To:
```bash
  sudo pmset -c standbydelay "${STANDBY_DELAY_24H}"
```

- [ ] **Step 3: Replace magic number at ShowLineMarks**

Change:
```bash
  defaults write com.apple.Terminal ShowLineMarks -int 0
```
To:
```bash
  defaults write com.apple.Terminal ShowLineMarks -int "${DISABLE_LINE_MARKS}"
```

- [ ] **Step 4: Commit**

```bash
git add platform/darwin/defaults.sh
git commit -m "refactor(darwin): extract magic numbers to named constants

Replace bare 86400 and 0 with STANDBY_DELAY_24H and DISABLE_LINE_MARKS."
```

---

### Task 8: Fix template.sh header (C7)

**Files:**
- Modify: `editor/emacs/.config/emacs/templates/template.sh`

- [ ] **Step 1: Add mode line and fix delimiters**

Change the entire file to fix delimiters (79 chars) and add mode line:

Line 1: keep `#!/bin/bash`
Line 2 (insert): `# -*- mode: sh; -*-`
Line 3: change 88-char dashes to 79-char dashes
Line 27: change 88-char equals to 79-char equals
Line 29: change 66-char equals to 79-char equals

Replace:
```
#!/bin/bash
# --------------------------------------------------------------------------------------
```
With:
```
#!/bin/bash
# -*- mode: sh; -*-
# -----------------------------------------------------------------------------
```

Replace (line 26):
```
# ======================================================================================
```
With:
```
# ===========================================================================
```

Replace (line 29):
```
# ================================================================
```
With:
```
# ===========================================================================
```

- [ ] **Step 2: Commit**

```bash
git add editor/emacs/.config/emacs/templates/template.sh
git commit -m "fix(template): add mode line and normalize delimiters to 79 chars"
```

---

### Task 9: Fix lazygit config (C8, C9, Q2)

**Files:**
- Modify: `tool/lazygit/.config/lazygit/config.yml`

- [ ] **Step 1: Quote pager string (C8)**

Line 57, change:
```yaml
      pager: delta --dark --paging=never
```
To:
```yaml
      pager: "delta --dark --paging=never"
```

- [ ] **Step 2: Change editPreset to nvim (C9)**

Line 64, change:
```yaml
  editPreset: vim
```
To:
```yaml
  editPreset: nvim
```

- [ ] **Step 3: Improve theme comment (Q2)**

Line 24, change:
```yaml
  # Catppuccin Mocha Blue theme
```
To:
```yaml
  # Catppuccin Mocha Blue theme for visual consistency with terminal
```

- [ ] **Step 4: Commit**

```bash
git add tool/lazygit/.config/lazygit/config.yml
git commit -m "fix(lazygit): quote pager string, use nvim, improve comment

- Quote pager value containing special characters (C8)
- Change editPreset from vim to nvim (C9)
- Add rationale to theme comment (Q2)"
```

---

### Task 10: Improve yazi theme.toml comments (Q1)

**Files:**
- Modify: `tool/yazi/.config/yazi/theme.toml`

- [ ] **Step 1: Replace WHAT comments with WHY comments**

Line 20, change:
```toml
# Override mgr border style
```
To:
```toml
# Border color lacks contrast on dark themes; use explicit color
```

Line 24, change:
```toml
# Override indicator to remove preview pane indicator
```
To:
```toml
# Preview pane indicator is redundant with custom status bar
```

Line 28, change:
```toml
# Style status bar permissions to be less prominent
```
To:
```toml
# File permissions are informational; reduce visual weight
```

- [ ] **Step 2: Commit**

```bash
git add tool/yazi/.config/yazi/theme.toml
git commit -m "docs(yazi): explain WHY for theme overrides in comments"
```

---

### Task 11: Final verification

- [ ] **Step 1: E1 — Verify git ignore file exists**

Run: `ls tool/git/.config/git/ignore`
Expected: File exists

- [ ] **Step 2: C1/C2 — Verify no vim references in git config**

Run: `grep -n "editor\|tool.*diff" tool/git/.config/git/config`
Expected: No `editor = vim` line, `tool = nvimdiff` present

- [ ] **Step 3: C3 — Verify time-stamp format uses hardcoded author**

Run: `grep 'time-stamp-format' editor/emacs/.config/emacs/init.el`
Expected: Contains `zhengyu.li` not `%u`

- [ ] **Step 4: C4 — Verify starship.toml has no single-quoted strings**

Run: `grep "'" shell/starship/.config/starship.toml | grep -v "^#"`
Expected: No matches

- [ ] **Step 5: C5 — Check no lines exceed 79 chars (excluding known exceptions)**

Run:
```bash
grep -rn '.\{80,\}' editor/emacs/.config/emacs/ --include="*.el" | \
  grep -v 'of this software and associated documentation' | \
  grep -v 'LIABILITY, WHETHER IN AN ACTION OF CONTRACT' | \
  wc -l
```
Expected: 0 (or very few remaining in MIT license lines)

- [ ] **Step 6: C6 — Verify magic numbers extracted in defaults.sh**

Run: `grep 'STANDBY_DELAY_24H\|DISABLE_LINE_MARKS' platform/darwin/defaults.sh`
Expected: Both constants defined and used

- [ ] **Step 7: C7 — Verify template.sh delimiters are 79 chars**

Run: `awk 'NR==3 || NR==27 || NR==30 { print NR, length($0) }' editor/emacs/.config/emacs/templates/template.sh`
Expected: All lines are 79 chars

- [ ] **Step 8: C8 — Verify lazygit pager string is quoted**

Run: `grep 'pager:' tool/lazygit/.config/lazygit/config.yml`
Expected: `pager: "delta --dark --paging=never"` (double-quoted)

- [ ] **Step 9: C9 — Verify lazygit editPreset is nvim**

Run: `grep 'editPreset' tool/lazygit/.config/lazygit/config.yml`
Expected: `editPreset: nvim`

- [ ] **Step 10: Q1 — Verify yazi theme.toml comments explain WHY**

Run: `grep -n '#' tool/yazi/.config/yazi/theme.toml | head -10`
Expected: Comments at lines 20, 24, 28 contain rationale

- [ ] **Step 11: Q2 — Verify lazygit theme comment has rationale**

Run: `grep 'Catppuccin' tool/lazygit/.config/lazygit/config.yml`
Expected: Comment includes "for visual consistency"
