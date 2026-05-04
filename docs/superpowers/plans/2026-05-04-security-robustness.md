# Security & Robustness Improvements — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix 4 security and robustness gaps: missing umask, no download retry, silent Xcode polling, and fragile package.toml tracking.

**Architecture:** Four independent fixes targeting `00-env.zsh`, `setup.sh`, `.gitattributes`, and `package.toml`. Each task is self-contained with its own commit and validation.

**Tech Stack:** Bash (setup.sh), Zsh (conf.d), Git filter drivers, TOML

---

### Task 1: Add Umask to Zsh Environment

**Files:**
- Modify: `shell/zsh/.config/zsh/conf.d/00-env.zsh:33-34`

- [ ] **Step 1: Add umask setting after Locale section**

Insert a new Level 1 section between the Locale block (line 33) and the Zsh History block (line 35). The new section goes after `export LANG=en_US.UTF-8` (line 33) and before the blank line + `# --- Zsh History ---` delimiter (line 35).

Add these lines after line 33:

```zsh
# -----------------------------------------------------------------------------
# File Permissions
# -----------------------------------------------------------------------------

# Ensure predictable permissions regardless of system default.
# Files: rw-r--r-- (644)  Directories: rwxr-xr-x (755)
umask 022
```

- [ ] **Step 2: Validate syntax**

Run: `zsh -n shell/zsh/.config/zsh/conf.d/00-env.zsh`
Expected: No output (clean parse)

- [ ] **Step 3: Commit**

```bash
git add shell/zsh/.config/zsh/conf.d/00-env.zsh
git commit -m "feat(zsh): add explicit umask 022 for predictable file permissions

Ensures files are created rw-r--r-- and directories rwxr-xr-x
regardless of system default.

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 2: Homebrew Download Retry Logic

**Files:**
- Modify: `setup.sh:39` (add constants)
- Modify: `setup.sh:324-351` (`_install_homebrew` function)

- [ ] **Step 1: Add retry constants**

After line 39 (`readonly NETWORK_TIMEOUT=60`), add:

```bash
# --- Download Retry ---
readonly DOWNLOAD_MAX_RETRIES=3
readonly DOWNLOAD_RETRY_DELAY=5
```

- [ ] **Step 2: Rewrite `_install_homebrew` with retry loop**

Replace the entire `_install_homebrew` function (lines 324–351) with:

```bash
_install_homebrew() {
  log_info "Installing Homebrew..."
  local -r url='https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh'
  local installer
  installer="$(mktemp)"
  trap 'rm -f "${installer:-}"' RETURN

  local attempt=1
  local delay="${DOWNLOAD_RETRY_DELAY}"
  while (( attempt <= DOWNLOAD_MAX_RETRIES )); do
    if curl --fail --silent --show-error \
      --connect-timeout "${NETWORK_TIMEOUT}" \
      --output "$installer" "${url}"; then
      break
    fi

    if (( attempt >= DOWNLOAD_MAX_RETRIES )); then
      log_err "Homebrew download failed after ${DOWNLOAD_MAX_RETRIES} attempts"
      return 1
    fi

    log_warn "Download attempt ${attempt} failed, retrying in ${delay}s..."
    sleep "${delay}"
    attempt=$(( attempt + 1 ))
    delay=$(( delay * 2 ))
  done

  if ! /bin/bash "$installer"; then
    log_err "Homebrew installation failed"
    return 1
  fi

  rm -f "$installer"
  trap - RETURN

  if ! _bootstrap_homebrew_env; then
    log_err "Homebrew not found after installation"
    return 1
  fi
  log_ok "Homebrew: installed"
}
```

Key changes:
- `attempt` tracks current attempt (1-indexed)
- `delay` doubles each retry: 5s → 10s → (fail)
- `log_warn` on retries, `log_err` only on final failure
- All existing cleanup (`mktemp`, `trap`, `rm`) preserved
- `curl` flags unchanged
- Post-install steps (`/bin/bash`, `_bootstrap_homebrew_env`) unchanged

- [ ] **Step 3: Validate syntax**

Run: `bash -n setup.sh`
Expected: No output (clean parse)

- [ ] **Step 4: Run shellcheck**

Run: `shellcheck setup.sh`
Expected: No new warnings from the modified function. Pre-existing warnings (if any) are acceptable.

- [ ] **Step 5: Commit**

```bash
git add setup.sh
git commit -m "feat(setup): add retry with exponential backoff for Homebrew download

3 attempts with 5s/10s delays. Improves reliability on flaky
networks.

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 3: Xcode CLI Polling Improvements

**Files:**
- Modify: `setup.sh:300-322` (`_install_xcode_cli` function)

- [ ] **Step 1: Add progress dots and clang verification**

Replace the `_install_xcode_cli` function (lines 300–322) with:

```bash
_install_xcode_cli() {
  if _has_xcode_cli; then
    log_info "Xcode CLI already installed"
    return 0
  fi

  log_info "Installing Xcode Command Line Tools..."
  log_warn "Complete the dialog that appears."

  xcode-select --install 2>/dev/null || true

  printf '%s' "${_LOG_INDENT}"
  local waited=0
  until _has_xcode_cli; do
    if (( waited >= XCODE_POLL_MAX )); then
      printf '\n'
      log_err "Xcode CLI install timed out after ${XCODE_POLL_MAX}s"
      return 1
    fi
    printf '.'
    sleep "${XCODE_POLL_INTERVAL}"
    waited=$(( waited + XCODE_POLL_INTERVAL ))
  done
  printf '\n'

  if ! clang --version >/dev/null 2>&1; then
    log_err "Xcode CLI path exists but clang is not functional"
    return 1
  fi

  log_ok "Xcode CLI: installed"
}
```

Key changes:
- `printf '%s' "${_LOG_INDENT}"` starts the dot line at the correct indent
- `printf '.'` each poll interval (no newline) for visual progress
- `printf '\n'` after loop completes (success or timeout)
- `clang --version` check after polling succeeds — catches edge case where
  Xcode path exists but tools are broken
- All existing logic preserved (early return, timeout, polling interval)

- [ ] **Step 2: Validate syntax**

Run: `bash -n setup.sh`
Expected: No output (clean parse)

- [ ] **Step 3: Commit**

```bash
git add setup.sh
git commit -m "feat(setup): add progress dots and clang verification for Xcode CLI

Shows visual progress during polling. Verifies clang works after
installation to catch broken Xcode paths.

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 4: Git Clean/Smudge Filter for package.toml

**Files:**
- Create: `.gitattributes`
- Modify: `setup.sh:437-470` (`ensure_prerequisites` function)
- Modify: `tool/yazi/.config/yazi/package.toml`

This task has multiple sub-steps that must be executed in order.

- [ ] **Step 1: Create `.gitattributes`**

Create `.gitattributes` in the repo root with:

```
tool/yazi/.config/yazi/package.toml filter=yazi-package
```

- [ ] **Step 2: Add `_setup_git_filters` function to setup.sh**

Add a new function after `_install_stow()` and before `ensure_prerequisites()`.
Find the exact insertion point by locating `ensure_prerequisites()` (currently
around line 437) and insert the new function before it:

```bash
_setup_git_filters() {
  git config --local filter.yazi-package.clean \
    "sed -e 's/^rev = .*/rev = \"pinned\"/' -e 's/^hash = .*/hash = \"0\"/'"
  git config --local filter.yazi-package.smudge cat
  git config --local filter.yazi-package.required true
  log_ok "Git filter: yazi-package configured"
}
```

- [ ] **Step 3: Call `_setup_git_filters` from `ensure_prerequisites`**

Add a git filters section at the end of `ensure_prerequisites()`, after the
GNU Stow block (after the closing `fi` of the stow check). Add:

```bash
  # --- Git Filters ---
  _setup_git_filters
```

- [ ] **Step 4: Add standard header to package.toml**

Replace the entire contents of `tool/yazi/.config/yazi/package.toml` with:

```toml
# package.toml -*- mode: toml; -*-
# Time-stamp: <2026-05-04 00:00:00 Sun by zhengyu.li>
#
# =============================================================================
# Yazi Plugin Dependencies
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: yazi, file-manager, plugins, dependencies
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   2026-05-04 00:00 zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   Defines Yazi plugin and flavor dependencies managed by `ya pack`.
#   Plugin hashes are normalized by the yazi-package git clean filter
#   so that `ya pack` updates do not create noisy diffs.
# =============================================================================

[[plugin.deps]]
use = "yazi-rs/plugins:smart-enter"
rev = "1962818"
hash = "187cc58ba7ac3befd49c342129e6f1b6"

[[plugin.deps]]
use = "yazi-rs/plugins:smart-filter"
rev = "1962818"
hash = "c887903a63a2ff520081b6d90a4b3392"

[[plugin.deps]]
use = "yazi-rs/plugins:full-border"
rev = "1962818"
hash = "6fa6a05a81c98dd000fbca3cca6e9682"

[[plugin.deps]]
use = "yazi-rs/plugins:toggle-pane"
rev = "1962818"
hash = "4c0260feb50ea2437380690ffc241da7"

[[plugin.deps]]
use = "yazi-rs/plugins:zoom"
rev = "1962818"
hash = "fa268cb59989a87780605fdcfb8d99a9"

[[plugin.deps]]
use = "yazi-rs/plugins:git"
rev = "1962818"
hash = "26db011a778f261d730d4f5f8bf24b3f"

[[flavor.deps]]
use = "yazi-rs/flavors:catppuccin-mocha"
rev = "9511cb0"
hash = "7dfe41c9c3d9be76da8a844263d0bbed"
```

- [ ] **Step 5: Remove skip-worktree flag**

Run:
```bash
git update-index --no-skip-worktree tool/yazi/.config/yazi/package.toml
```

Verify flag is removed:
```bash
git ls-files -v -- tool/yazi/.config/yazi/package.toml
```
Expected: Line starts with `H` (not `S`)

- [ ] **Step 6: Configure the filter locally before staging**

Run:
```bash
git config --local filter.yazi-package.clean \
  "sed -e 's/^rev = .*/rev = \"pinned\"/' -e 's/^hash = .*/hash = \"0\"/'"
git config --local filter.yazi-package.smudge cat
git config --local filter.yazi-package.required true
```

- [ ] **Step 7: Validate filter works**

Stage the file and check what git sees:
```bash
git add tool/yazi/.config/yazi/package.toml
git diff --cached -- tool/yazi/.config/yazi/package.toml
```

Expected: The staged diff shows `rev = "pinned"` and `hash = "0"` for all
entries (not the real hash values). The working copy still has real values.

- [ ] **Step 8: Validate setup.sh syntax**

Run: `bash -n setup.sh`
Expected: No output (clean parse)

- [ ] **Step 9: Commit all Task 4 changes**

```bash
git add .gitattributes setup.sh tool/yazi/.config/yazi/package.toml
git commit -m "feat(yazi): add git clean filter for package.toml hash normalization

Replace --skip-worktree hack with a proper git filter driver that
normalizes volatile rev/hash fields. Add standard header to
package.toml for consistency.

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 5: Final Validation

**Files:** None (validation only)

- [ ] **Step 1: Run full syntax checks**

```bash
bash -n setup.sh
zsh -n shell/zsh/.config/zsh/conf.d/00-env.zsh
```

Expected: Both pass with no output.

- [ ] **Step 2: Verify git filter on package.toml**

```bash
git show HEAD:tool/yazi/.config/yazi/package.toml | grep -c 'rev = "pinned"'
```

Expected: 7 (all entries normalized)

```bash
grep -c 'rev = "1962818\|rev = "9511cb0"' tool/yazi/.config/yazi/package.toml
```

Expected: 7 (working copy has real values)

- [ ] **Step 3: Verify no skip-worktree flags remain**

```bash
git ls-files -v | grep '^S'
```

Expected: No output (no files with skip-worktree flag)

- [ ] **Step 4: Review commit log**

```bash
git --no-pager log --oneline -5
```

Expected: 4 new commits (one per task).
