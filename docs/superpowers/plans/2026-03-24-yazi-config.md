# Yazi Configuration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add yazi terminal file manager configuration to oh-my-workspace with Vim-style navigation and developer-focused settings.

**Architecture:** Create configuration package in `tool/yazi/` following GNU Stow conventions. Two TOML files (yazi.toml, keymap.toml) configure manager behavior, preview settings, file openers, and keybindings. Package registered in setup.sh PKG_ALL array.

**Tech Stack:** TOML configuration, GNU Stow, yazi terminal file manager

---

## File Structure

**New files to create:**
- `tool/yazi/.config/yazi/yazi.toml` - Main configuration (manager, preview, tasks, opener, open rules)
- `tool/yazi/.config/yazi/keymap.toml` - Keybindings (Vim navigation, tab management)

**Existing files to modify:**
- `setup.sh:52-63` - Add `"tool/yazi"` to PKG_ALL array

**Files after stow:**
- `~/.config/yazi/yazi.toml` (symlink)
- `~/.config/yazi/keymap.toml` (symlink)

---

## Task 1: Create Package Directory Structure

**Files:**
- Create: `tool/yazi/.config/yazi/` directory

- [ ] **Step 1: Create directory structure**

Run: `mkdir -p tool/yazi/.config/yazi`

Expected: Directory created successfully

Verify: `ls -la tool/yazi/.config/yazi/`

Expected output:
```
total 0
drwxr-xr-x  2 zhengyu.li  staff  64 Mar 24 17:00 .
drwxr-xr-x  3 zhengyu.li  staff  96 Mar 24 17:00 ..
```

---

## Task 2: Create Main Configuration File

**Files:**
- Create: `tool/yazi/.config/yazi/yazi.toml`

- [ ] **Step 1: Create yazi.toml with standard header**

Create file: `tool/yazi/.config/yazi/yazi.toml`

Content:
```toml
# yazi.toml -*- mode: toml; -*-
# Time-stamp: <2026-03-24 17:00:00 Mon by zhengyu.li>
# =============================================================================
# Yazi - Blazing fast terminal file manager
#
# Location: ~/.config/yazi/yazi.toml
# XDG: Follows XDG_CONFIG_HOME, uses ~/.config/yazi/
# References:
#   1. https://yazi-rs.github.io/docs/configuration/yazi/
# =============================================================================
```

- [ ] **Step 2: Add Manager section**

Append to file:
```toml

# -----------------------------------------------------------------------------
# Manager
# -----------------------------------------------------------------------------

[mgr]
ratio = [1, 4, 3]
sort_by = "natural"
sort_sensitive = false
sort_reverse = false
sort_dir_first = true
linemode = "size"
show_hidden = false
show_symlink = true
scrolloff = 5
```

- [ ] **Step 3: Add Preview section**

Append to file:
```toml

# -----------------------------------------------------------------------------
# Preview
# -----------------------------------------------------------------------------

[preview]
wrap = "no"
tab_size = 2
max_width = 600
max_height = 900
image_delay = 50
image_filter = "lanczos3"
image_quality = 75
```

- [ ] **Step 4: Add Tasks section**

Append to file:
```toml

# -----------------------------------------------------------------------------
# Tasks
# -----------------------------------------------------------------------------

[tasks]
micro_workers = 10
macro_workers = 25
image_alloc = 536870912
image_bound = [0, 0]
suppress_preload = false
```

- [ ] **Step 5: Add Opener section**

Append to file:
```toml

# -----------------------------------------------------------------------------
# Opener
# -----------------------------------------------------------------------------

[opener]
edit = [
  { run = '$EDITOR "$@"', block = true, desc = "Edit with $EDITOR" }
]

open = [
  { run = 'open "$@"', desc = "Open with system default", for = "macos" }
]

extract = [
  { run = 'ya pub extract --list "$@"', desc = "Extract with yazi" }
]
```

- [ ] **Step 6: Add Open Rules section**

Append to file:
```toml

# -----------------------------------------------------------------------------
# Open Rules
# -----------------------------------------------------------------------------

[open]
# Note: "enter" is a yazi built-in command for directory navigation
# "edit" and "open" are defined in the [opener] section above
prepend_rules = [
  { mime = "text/*", use = "edit" },
  { name = "*/", use = "enter" },
]

append_rules = [
  { name = "*", use = "open" }
]
```

- [ ] **Step 7: Verify file content**

Run: `cat tool/yazi/.config/yazi/yazi.toml`

Expected: Complete configuration file with all sections

- [ ] **Step 8: Commit yazi.toml**

Run:
```bash
git add tool/yazi/.config/yazi/yazi.toml
git commit -m "feat(yazi): add main configuration file"
```

Expected: Commit created successfully

---

## Task 3: Create Keybindings Configuration File

**Files:**
- Create: `tool/yazi/.config/yazi/keymap.toml`

- [ ] **Step 1: Create keymap.toml with standard header**

Create file: `tool/yazi/.config/yazi/keymap.toml`

Content:
```toml
# keymap.toml -*- mode: toml; -*-
# Time-stamp: <2026-03-24 17:00:00 Mon by zhengyu.li>
# =============================================================================
# Yazi Keybindings - Vim navigation with smart enter
#
# Location: ~/.config/yazi/keymap.toml
# References:
#   1. https://yazi-rs.github.io/docs/configuration/keymap/
# =============================================================================
```

- [ ] **Step 2: Add Navigation section**

Append to file:
```toml

# -----------------------------------------------------------------------------
# Manager - Navigation
# -----------------------------------------------------------------------------

[[manager.prepend_keymap]]
on = [ "h" ]
run = "escape"
desc = "Exit or go back to parent directory"

[[manager.prepend_keymap]]
on = [ "l" ]
run = "enter"
desc = "Enter directory or open file with default opener"

[[manager.prepend_keymap]]
on = [ "<Enter>" ]
run = "enter"
desc = "Enter directory or open file with default opener"
```

- [ ] **Step 3: Add Tab Management section**

Append to file:
```toml

# -----------------------------------------------------------------------------
# Manager - Tab Management
# -----------------------------------------------------------------------------

[[manager.prepend_keymap]]
on = [ "T" ]
run = "tab_create --current"
desc = "Create new tab at current path"

[[manager.prepend_keymap]]
on = [ "t" ]
run = "tab_create"
desc = "Create new tab"

[[manager.prepend_keymap]]
on = [ "1" ]
run = "tab_switch 0"
desc = "Switch to tab 1"

[[manager.prepend_keymap]]
on = [ "2" ]
run = "tab_switch 1"
desc = "Switch to tab 2"

[[manager.prepend_keymap]]
on = [ "3" ]
run = "tab_switch 2"
desc = "Switch to tab 3"

[[manager.prepend_keymap]]
on = [ "4" ]
run = "tab_switch 3"
desc = "Switch to tab 4"

[[manager.prepend_keymap]]
on = [ "5" ]
run = "tab_switch 4"
desc = "Switch to tab 5"
```

- [ ] **Step 4: Verify file content**

Run: `cat tool/yazi/.config/yazi/keymap.toml`

Expected: Complete keybindings file with all sections

- [ ] **Step 5: Commit keymap.toml**

Run:
```bash
git add tool/yazi/.config/yazi/keymap.toml
git commit -m "feat(yazi): add keybindings configuration"
```

Expected: Commit created successfully

---

## Task 4: Register Package in setup.sh

**Files:**
- Modify: `setup.sh:52-63`

- [ ] **Step 1: Read current PKG_ALL array**

Run: `sed -n '52,63p' setup.sh`

Expected output:
```bash
readonly -a PKG_ALL=(
  shell/zsh
  shell/starship
  editor/vim
  editor/emacs
  term/ghostty
  tool/git
  tool/lazygit
  tool/ripgrep
  lang/python/uv
  lang/typescript/bun
)
```

- [ ] **Step 2: Add tool/yazi to PKG_ALL array**

Edit `setup.sh` line 60 (after `tool/ripgrep`):

Add line:
```bash
  tool/yazi
```

The modified section should look like:
```bash
readonly -a PKG_ALL=(
  shell/zsh
  shell/starship
  editor/vim
  editor/emacs
  term/ghostty
  tool/git
  tool/lazygit
  tool/ripgrep
  tool/yazi
  lang/python/uv
  lang/typescript/bun
)
```

- [ ] **Step 3: Verify modification**

Run: `sed -n '52,64p' setup.sh`

Expected: Array includes `tool/yazi` entry

- [ ] **Step 4: Commit setup.sh modification**

Run:
```bash
git add setup.sh
git commit -m "feat(setup): register yazi package in PKG_ALL"
```

Expected: Commit created successfully

---

## Task 5: Test Stow Operation

**Files:**
- Test: Stow symlinks creation

- [ ] **Step 1: Run stow in dry-run mode**

Run: `./setup.sh install --dry-run yazi`

Expected output:
```
DRY RUN: No changes will be made
Loading Brewfile from: /Users/zhengyu.li/oh-my-workspace/pkg/homebrew/Brewfile
Prerequisites: ✓ Xcode CLI, ✓ Homebrew, ✓ GNU Stow
Installing packages: yazi

Would stow:
  tool/yazi -> ~/.config/yazi/
```

- [ ] **Step 2: Execute stow operation**

Run: `./setup.sh install yazi`

Expected output:
```
Loading Brewfile from: /Users/zhengyu.li/oh-my-workspace/pkg/homebrew/Brewfile
Prerequisites: ✓ Xcode CLI, ✓ Homebrew, ✓ GNU Stow
Installing packages: yazi
✓ Stowed tool/yazi
```

- [ ] **Step 3: Verify symlinks created**

Run: `ls -la ~/.config/yazi/`

Expected output:
```
lrwxr-xr-x  1 zhengyu.li  staff  45 Mar 24 17:00 yazi.toml -> ~/oh-my-workspace/tool/yazi/.config/yazi/yazi.toml
lrwxr-xr-x  1 zhengyu.li  staff  48 Mar 24 17:00 keymap.toml -> ~/oh-my-workspace/tool/yazi/.config/yazi/keymap.toml
```

- [ ] **Step 4: Verify symlink targets exist**

Run:
```bash
readlink ~/.config/yazi/yazi.toml
readlink ~/.config/yazi/keymap.toml
```

Expected: Symlink targets point to oh-my-workspace files

---

## Task 6: Verify Yazi Installation

**Files:**
- Test: Yazi binary and dependencies

- [ ] **Step 1: Check yazi is installed**

Run: `which yazi`

Expected output:
```
/opt/homebrew/bin/yazi
```

- [ ] **Step 2: Check yazi version**

Run: `yazi --version`

Expected: Version number displayed (e.g., `yazi 0.4.2`)

- [ ] **Step 3: Verify optional dependencies (optional)**

Run:
```bash
which ffmpegthumbnailer  # Video thumbnails
which 7z                 # Archive extraction
which pdftoppm          # PDF preview
```

Expected: Paths displayed if installed, or command not found if not installed (acceptable)

---

## Task 7: Manual Verification

**Files:**
- Test: Yazi configuration behavior

- [ ] **Step 1: Test directory navigation**

Run: `yazi` in a directory with subdirectories

Test:
1. Press `l` on a directory → Should enter the directory
2. Press `h` → Should go back to parent directory
3. Press `Enter` on a directory → Should enter the directory

Expected: Directory navigation works, does NOT open in vim

- [ ] **Step 2: Test file opening**

In yazi, navigate to a text file (e.g., `.zshenv`)

Test:
1. Press `l` or `Enter` on a text file → Should open in `$EDITOR`
2. Exit editor → Should return to yazi

Expected: Text files open in editor, returns to yazi after exit

- [ ] **Step 3: Test tab management**

In yazi:

Test:
1. Press `T` → Should create new tab at current path
2. Press `t` → Should create new tab
3. Press `1-5` → Should switch to corresponding tab

Expected: Tab creation and switching work correctly

- [ ] **Step 4: Test preview**

In yazi, navigate to different file types:

Test:
1. Code file → Should show syntax-highlighted preview
2. Image file → Should show inline preview
3. Directory → Should show directory contents in preview pane

Expected: Previews display correctly for different file types

- [ ] **Step 5: Test sorting**

In yazi, navigate to a directory with mixed files:

Test:
1. Verify directories appear at top
2. Verify natural sort order (1.md, 2.md, 10.md not 1.md, 10.md, 2.md)

Expected: Files sorted correctly with directories first

- [ ] **Step 6: Exit yazi**

Press `q` to quit yazi

Expected: Yazi exits cleanly

---

## Task 8: Final Commit and Push

**Files:**
- Commit: All changes

- [ ] **Step 1: Check git status**

Run: `git status`

Expected: All changes committed, working directory clean

- [ ] **Step 2: Verify commit history**

Run: `git log --oneline -5`

Expected: See commits for:
- `feat(yazi): add main configuration file`
- `feat(yazi): add keybindings configuration`
- `feat(setup): register yazi package in PKG_ALL`
- Design spec commit (if not already pushed)

- [ ] **Step 3: Push to remote**

Run: `git push origin master`

Expected: All commits pushed successfully

---

## Verification Checklist

After completing all tasks, verify:

- [ ] Directory structure: `tool/yazi/.config/yazi/` exists
- [ ] Configuration files: `yazi.toml` and `keymap.toml` created
- [ ] Package registered: `tool/yazi` in `setup.sh` PKG_ALL array
- [ ] Symlinks created: `~/.config/yazi/` contains symlinks
- [ ] Directory navigation: `h`/`l`/`Enter` work correctly
- [ ] File opening: Text files open in `$EDITOR`, returns to yazi
- [ ] Tab management: `T`/`t`/`1-5` work correctly
- [ ] Preview: Different file types show appropriate previews
- [ ] Sorting: Directories first, natural sort order
- [ ] All changes committed and pushed

---

## Rollback Plan

If issues arise:

1. **Remove symlinks:**
   ```bash
   ./setup.sh uninstall yazi
   ```

2. **Remove package from PKG_ALL:**
   Edit `setup.sh` and remove `tool/yazi` line

3. **Delete package directory:**
   ```bash
   rm -rf tool/yazi
   ```

4. **Revert commits:**
   ```bash
   git revert HEAD~3  # Adjust number based on commit count
   ```

---

## References

- [Yazi Configuration Documentation](https://yazi-rs.github.io/docs/configuration/yazi/)
- [Yazi Keymap Documentation](https://yazi-rs.github.io/docs/configuration/keymap/)
- [Design Specification](../specs/2026-03-24-yazi-config-design.md)
- [oh-my-workspace README](../../README.md)
- [GNU Stow Manual](https://www.gnu.org/software/stow/manual/)
