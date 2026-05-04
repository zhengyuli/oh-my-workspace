# Security & Robustness Improvements — Design Spec

## Problem

The oh-my-workspace repository has several security and robustness gaps
identified during deep analysis:

1. No explicit `umask` in zsh startup — relies on system default
2. Homebrew installer download has no retry logic — single network failure
   aborts setup
3. Xcode CLI installation polling is silent and suppresses errors
4. `package.toml` uses `--skip-worktree` hack that doesn't persist across
   clones

## Approach

Four targeted fixes, ordered by risk. Each is independent and can be
verified in isolation.

---

## Task 1: Add Umask to Zsh Environment

**File**: `shell/zsh/.config/zsh/conf.d/00-env.zsh`

**Change**: Add `umask 022` in the environment setup section. This ensures
files are created with `rw-r--r--` and directories with `rwxr-xr-x`
regardless of system default.

**Placement**: After the locale settings block (around line 33), before
the editor/pager section. Add a Level 2 delimiter (`# --- File Permissions ---`)
to group it.

**Validation**: `zsh -n` syntax check on the file.

---

## Task 2: Homebrew Download Retry Logic

**File**: `setup.sh` — `_install_homebrew()` function (lines 324–351)

**Change**: Wrap the `curl` download in a retry loop with exponential
backoff.

**Design**:
- Named constant: `readonly DOWNLOAD_MAX_RETRIES=3`
- Named constant: `readonly DOWNLOAD_RETRY_DELAY=5` (initial delay in seconds)
- Loop structure:
  ```
  attempt 1: try curl, on fail → log_warn, sleep 5
  attempt 2: try curl, on fail → log_warn, sleep 10
  attempt 3: try curl, on fail → log_err, return 1
  ```
- Exponential backoff: delay doubles each attempt (`DELAY * 2^(attempt-1)`)
- Place constants near existing `NETWORK_TIMEOUT` (line 39)
- The `mktemp` + `trap` cleanup stays outside the loop (create once, reuse)
- On success at any attempt, break and continue with installation

**Coding conventions**:
- Use `(( ))` for arithmetic
- Use `log_warn` for retries, `log_err` for final failure
- No `&&`/`||` as standalone conditional — use explicit `if`

**Validation**: `bash -n setup.sh` and `shellcheck setup.sh`.

---

## Task 3: Xcode CLI Polling Improvements

**File**: `setup.sh` — `_install_xcode_cli()` function (lines 300–322)

**Changes**:

### 3a. Progress feedback during polling

Add a dot-printing mechanism inside the polling loop so users see activity
instead of silence.

Design:
- Print a `.` (no newline) every `XCODE_POLL_INTERVAL` seconds
- Print a newline after polling completes (success or timeout)
- Use `printf '.'` (not `echo`) for inline dots

### 3b. Post-install clang verification

After the polling loop succeeds (Xcode CLI path exists), verify that
`clang` is actually functional:

```bash
if ! clang --version >/dev/null 2>&1; then
  log_err "Xcode CLI path exists but clang is not functional"
  return 1
fi
```

**Placement**: Between the `log_ok` line and the end of the function.

**Validation**: `bash -n setup.sh` and `shellcheck setup.sh`.

---

## Task 4: Git Clean/Smudge Filter for package.toml

**Goal**: Replace `--skip-worktree` hack with a proper git filter driver
that normalizes volatile `rev` and `hash` fields.

### 4a. Create `.gitattributes`

**File**: `.gitattributes` (new, repo root)

```
tool/yazi/.config/yazi/package.toml filter=yazi-package
```

This tells git to run the `yazi-package` filter when staging/checking out
this file.

### 4b. Configure filter in setup.sh

Add a helper function `_setup_git_filters()` that configures the clean
filter:

```bash
_setup_git_filters() {
  git config --local filter.yazi-package.clean \
    "sed -e 's/^rev = .*/rev = \"pinned\"/' -e 's/^hash = .*/hash = \"0\"/'"
  git config --local filter.yazi-package.smudge cat
  git config --local filter.yazi-package.required true
}
```

- **clean**: Replaces `rev = "..."` with `rev = "pinned"` and
  `hash = "..."` with `hash = "0"` when staging
- **smudge**: Identity (`cat`) — working copy keeps real values from
  `ya pack`
- **required**: true — fail if filter is not configured (safety net)

Call `_setup_git_filters()` from `ensure_prerequisites()` or as a
standalone phase in the install flow.

### 4c. Normalize the committed file

After filter is configured:
1. Remove `--skip-worktree` flag:
   `git update-index --no-skip-worktree tool/yazi/.config/yazi/package.toml`
2. Add the standard header to `package.toml`
3. Stage the file (clean filter normalizes rev/hash automatically)
4. Commit

The committed version will have:
```toml
# package.toml -*- mode: toml; -*-
# Time-stamp: <...>
#
# =============================================================================
# Yazi Plugin Dependencies
# ...
# =============================================================================

[[plugin.deps]]
use = "yazi-rs/plugins:smart-enter"
rev = "pinned"
hash = "0"
...
```

### 4d. Document the pattern

Add a comment in `setup.sh` near the filter setup explaining the pattern
for future maintainers.

**Validation**:
- `git diff --cached` after staging shows normalized values
- After `ya pack`, working copy has real hashes but `git diff` is clean
- New clone: `./setup.sh install` configures filter; `ya pack` populates
  real values

---

## Out of Scope

- Git GPG signing (user declined)
- `flavor.toml` header (third-party theme content)
- Stow retry logic (rare edge case, low risk)
- Signal trapping beyond existing ERR trap

## Success Criteria

1. `umask` explicitly set; verified by `zsh -c 'umask'` returning `022`
2. `setup.sh` retries Homebrew download up to 3 times with backoff
3. Xcode polling shows progress dots; post-install verifies `clang`
4. `package.toml` tracked with header; hash churn filtered by clean driver
5. `--skip-worktree` flag removed
6. `bash -n setup.sh` and `shellcheck setup.sh` pass
7. `zsh -n` passes on modified zsh files
