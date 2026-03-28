# SSH Agent Cleanup Design

## Problem

`.zprofile` contains SSH agent initialization code (lines 29-57) that is
redundant on macOS:

1. `eval "$(ssh-agent -s)"` starts an extra ssh-agent process, competing
   with the launchd-managed agent (`com.openssh.ssh-agent`). Verified:
   the user's system runs 3 ssh-agent processes (1 launchd + 2 from shell
   startup).

2. The `ssh-add --apple-use-keychain` loop is redundant when `~/.ssh/config`
   has `AddKeysToAgent yes` and `UseKeychain yes` -- the first `ssh`
   connection automatically loads keys from Keychain.

**Root cause:** Apple's recommended approach (TN2449) is `~/.ssh/config`
with `UseKeychain yes` + `AddKeysToAgent yes`, not shell-level agent
management.

## Verification Sources

- Apple Technical Note TN2449: https://developer.apple.com/library/archive/technotes/tn2449/_index.html
- macOS launchd plist `/System/Library/LaunchAgents/com.openssh.ssh-agent.plist`
  uses on-demand socket activation via `SSH_AUTH_SOCK` (no `RunAtLoad`)

## Changes

### 1. Remove SSH agent block from `.zprofile`

**File:** `shell/zsh/.config/zsh/.zprofile`

Remove lines 29 through end of file (the entire "SSH Agent -- macOS
Keychain Integration" section). After removal, the file ends cleanly
after line 27 (`source "$ZDOTDIR/conf.d/05-path.zsh"`).

Also update the file header (line 11): remove the responsibility line
`2. Initialize SSH agent with macOS Keychain (once per login session)`.

### 2. Add `UseKeychain yes` to `~/.ssh/config` (user action)

**File:** `~/.ssh/config` (not in repository -- user must apply manually)

Current config:
```
Host github.com
  AddKeysToAgent yes
  IgnoreUnknown UseKeychain
  IdentityFile ~/.ssh/id_ed25519
```

Change to:
```
Host github.com
  AddKeysToAgent yes
  UseKeychain yes
  IdentityFile ~/.ssh/id_ed25519
```

`IgnoreUnknown UseKeychain` is replaced with `UseKeychain yes`. The
`IgnoreUnknown` workaround was for older OpenSSH versions; macOS Monterey
12.0+ (OpenSSH 8.4+) supports `UseKeychain` natively.

For users with multiple keys (e.g., `id_rsa`, `id_ecdsa`), apply the
same config to a `Host *` block or run `ssh-add --apple-use-keychain`
for each key.

### 3. One-time keychain storage (user action)

After applying the config change, run once per key:

```bash
ssh-add --apple-use-keychain ~/.ssh/id_ed25519
```

This stores the passphrase in macOS Keychain. Subsequent `ssh` connections
retrieve it automatically.

## What Stays Unchanged

- `.zshenv` -- no changes
- `.zshrc` -- no changes
- All `conf.d/` modules -- no changes

## Rollback

If the launchd-managed agent fails, the user can:

1. **Restore from git history** (preferred -- preserves login shell
   semantics):
   ```bash
   git checkout HEAD -- shell/zsh/.config/zsh/.zprofile
   ```

2. Or add to `99-local.zsh` (note: this runs in interactive shells only,
   not non-interactive login shells like `ssh host command`):
   ```zsh
   eval "$(ssh-agent -s)"
   ssh-add --apple-use-keychain ~/.ssh/id_ed25519 2>/dev/null
   ```
