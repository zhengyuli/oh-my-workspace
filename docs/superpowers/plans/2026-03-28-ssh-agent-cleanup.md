# SSH Agent Cleanup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove redundant SSH agent initialization from `.zprofile` on macOS, where launchd already manages ssh-agent.

**Architecture:** Delete the SSH agent section (lines 29-57) from `.zprofile` and remove the SSH agent responsibility from the header. The `~/.ssh/config` change (adding `UseKeychain yes`) is a user action outside the repository.

**Tech Stack:** Zsh, macOS launchd, OpenSSH

---

### Task 1: Remove SSH agent block from .zprofile

**Files:**
- Modify: `shell/zsh/.config/zsh/.zprofile` (header line 11, SSH section lines 29-57)

- [ ] **Step 1: Verify current file state**

Run: `wc -l shell/zsh/.config/zsh/.zprofile`
Expected: 57 lines

Run: `cat -n shell/zsh/.config/zsh/.zprofile`
Verify: line 11 has SSH agent responsibility, lines 29-57 are the SSH agent block

- [ ] **Step 2: Update file header responsibility line**

On line 11, remove:
```
#   2. Initialize SSH agent with macOS Keychain (once per login session)
```

The "Responsibilities" section should only contain:
```
# Responsibilities:
#   1. Source conf.d fragments safe for non-interactive contexts
```

- [ ] **Step 3: Remove the entire SSH agent section (lines 29-57)**

Delete from line 29 (the Level 1 delimiter starting `# -----------------------------------------------------------------------------` with the "SSH Agent" comment) through line 57 (the closing `fi`). This removes:
- The SSH agent section header and comments (lines 29-43)
- The `if [[ "$OSTYPE" == darwin* ]]` block (lines 44-57)
- The trailing blank line before the section (line 28)

After removal, the file ends cleanly after the `source "$ZDOTDIR/conf.d/05-path.zsh"` line, with a trailing newline.

**IMPORTANT:** Do NOT delete lines 17-27 -- that is the "Core Configuration Fragments" section which must be preserved.

The final file should be:

```zsh
# .zprofile
# Time-stamp: <2026-03-20 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Login Shell Initialization
#
# Loaded by: Login shells only (first terminal open, SSH login)
# Load order: Second (after .zshenv, before .zshrc)
#
# Responsibilities:
#   1. Source conf.d fragments safe for non-interactive contexts
#
# Do NOT add: plugins, completion, prompt, aliases, keybindings
#             → Put these in .zshrc (interactive only)
# =============================================================================

# -----------------------------------------------------------------------------
# Core Configuration Fragments
# -----------------------------------------------------------------------------
# These modules are safe for non-interactive contexts and should be available
# to all login shells (including scripts run via 'ssh host command').

# Environment variables (EDITOR, PAGER, XDG paths, Homebrew settings, etc.)
source "$ZDOTDIR/conf.d/00-env.zsh"

# PATH / FPATH / manpath management
source "$ZDOTDIR/conf.d/05-path.zsh"
```

- [ ] **Step 4: Syntax check the modified file**

Run: `zsh -n shell/zsh/.config/zsh/.zprofile`
Expected: No output (clean syntax)

- [ ] **Step 5: Verify the diff**

Run: `git diff shell/zsh/.config/zsh/.zprofile`
Expected: Deletion of line 11 (SSH agent responsibility) and lines 28-57 (SSH agent section)

- [ ] **Step 6: Commit**

```bash
git add shell/zsh/.config/zsh/.zprofile
git commit -m "$(cat <<'EOF'
fix(zsh): remove redundant SSH agent initialization from .zprofile

macOS launchd manages ssh-agent via socket activation. The manual
eval $(ssh-agent -s) created competing agent processes. SSH key
management should use ~/.ssh/config with UseKeychain yes instead.
EOF
)"
```

---

### Task 2: User action -- Update ~/.ssh/config (manual, not in repo)

This task is for the user to perform manually. It is documented here for completeness.

- [ ] **Step 1: Edit ~/.ssh/config**

Replace:
```
Host github.com
  AddKeysToAgent yes
  IgnoreUnknown UseKeychain
  IdentityFile ~/.ssh/id_ed25519
```

With:
```
Host github.com
  AddKeysToAgent yes
  UseKeychain yes
  IdentityFile ~/.ssh/id_ed25519
```

For multiple keys, use a `Host *` block:
```
Host *
  AddKeysToAgent yes
  UseKeychain yes
```

- [ ] **Step 2: Store key passphrase in macOS Keychain (one-time, per key)**

```bash
ssh-add --apple-use-keychain ~/.ssh/id_ed25519
# Repeat for each additional key:
# ssh-add --apple-use-keychain ~/.ssh/id_rsa
```

- [ ] **Step 3: Verify SSH works**

Run: `ssh -T git@github.com`
Expected: "Hi <username>! You've successfully authenticated..."
