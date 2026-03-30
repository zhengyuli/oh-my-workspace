# Implementation Plan: setup.sh Refactoring

**Date:** 2026-03-30
**Spec:** docs/superpowers/specs/2026-03-30-setup-refactor-design.md
**Estimated time:** 30-45 minutes

## Overview

Refactor setup.sh to eliminate code duplication and remove unnecessary PATH persistence. The refactoring consolidates bash upgrade logic into a single focused function and removes config file modifications since users switch to zsh after setup.

## Prerequisites

- [ ] Read the design spec: `docs/superpowers/specs/2026-03-30-setup-refactor-design.md`
- [ ] Read bash.md conventions: `.claude/rules/lang/bash.md`
- [ ] Verify current setup.sh works: `./setup.sh status`

## Implementation Tasks

### Task 1: Move platform check before bash version check

**File:** `setup.sh`
**Lines:** 28-115 (current), 28-70 (after)

**Current structure:**
```bash
# Lines 28-109: Bash version check + inline upgrade logic
# Lines 111-115: Platform check
```

**Target structure:**
```bash
# Lines 28-32: Platform check
# Lines 34-70: Bash version check + function call
```

**Changes:**
1. Move lines 111-115 (platform check) to before bash version check
2. Update line numbers in comments if needed

**Why:** Platform check should fail fast on unsupported systems before attempting bash upgrade.

**Testing:**
```bash
# On macOS (should pass platform check)
./setup.sh status

# On Linux (should fail with "macOS required")
# Can't test locally, but verify logic is correct
```

**Commit:** `refactor(setup): move platform check before bash version check`

---

### Task 2: Create `_upgrade_bash()` function

**File:** `setup.sh`
**Location:** Add after `_has_stow()` function (around line 533)

**Code to add (~50 lines):**
```bash
_upgrade_bash() {
  log_info "bash 4.3+ required for nameref and associative arrays"
  log_info "Current version: ${BASH_VERSION}"
  log_info ""
  log_info "This script will:"
  log_info "  1. Install Homebrew (skipped if already installed)"
  log_info "  2. Install the latest bash via Homebrew"
  log_info "  3. Re-run this script with the new bash automatically"

  if ! confirm "Continue with bash upgrade?" n; then
    log_info "Aborted"
    exit 1
  fi

  # Install Homebrew if needed
  if ! _install_homebrew; then
    exit 1
  fi

  # Install latest bash
  log_info "Installing latest bash via Homebrew..."
  if ! brew install bash; then
    log_err "Failed to install bash via Homebrew"
    exit 1
  fi

  # Locate new bash
  local brew_prefix
  brew_prefix=$(brew --prefix)
  local new_bash="${brew_prefix}/bin/bash"
  if [[ ! -x "${new_bash}" ]]; then
    log_err "New bash not found at ${new_bash}"
    exit 1
  fi

  # Re-exec with new bash
  log_ok "Re-running with ${new_bash}..."
  exec "${new_bash}" "$0" "$@"
}
```

**Key points:**
- Uses existing logging functions (log_info, log_ok, log_err)
- Uses existing `confirm()` function for user prompt
- Delegates Homebrew installation to `_install_homebrew()`
- No PATH persistence (session-only via `eval`)
- Standard error handling with exit 1

**Why:** Consolidates all bash upgrade logic in one place, eliminating duplication.

**Testing:**
```bash
# Test function is defined (syntax check)
bash -n setup.sh

# Test function can be called (manual test)
# Add temporary test code, remove after verification
```

**Commit:** `feat(setup): add _upgrade_bash() function for bash upgrades`

---

### Task 3: Simplify bootstrap section

**File:** `setup.sh`
**Lines:** 28-109 (current), 34-65 (after)

**Current code (lines 31-109):**
- Lines 31-56: User prompt
- Lines 58-67: Inline Homebrew installation
- Lines 69-83: Inline PATH setup
- Lines 85-92: Inline PATH persistence
- Lines 94-99: Inline bash installation
- Lines 101-108: Inline re-exec

**Target code (lines 37-65):**
```bash
# Require bash 4.3+ for local -n (nameref) and associative arrays.
# If the running bash is too old, offer to upgrade via _upgrade_bash().
if (( BASH_VERSINFO[0] < 4 ||
      ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] < 3 ) )); then
  sed "s/BASH_VERSION_PLACEHOLDER/${BASH_VERSION}/" >&2 <<'EOF'
warn: bash 4.3 or later required (current: BASH_VERSION_PLACEHOLDER)

  Why upgrade?
    • Security: bash 3.2 (macOS default) has known vulnerabilities (2007)
    • Features: nameref, associative arrays, better glob patterns
    • Compatibility: bash 5.x runs virtually all bash 3.2 scripts

  This script will:
    1. Install Homebrew (skipped if already installed)
    2. Install the latest bash via Homebrew
    3. Re-run this script with the new bash automatically

EOF
  if ! confirm "Continue with bash upgrade?" n; then
    printf '  Aborted.\n' >&2
    exit 1
  fi
  _upgrade_bash
  # _upgrade_bash() calls exec, so this line is only reached on error
  exit 1
fi
```

**Key changes:**
- Keep user prompt (lines 31-56 → 37-62)
- Replace 58 lines of inline logic with single function call (line 63)
- Add fallback exit in case `_upgrade_bash()` returns without exec (line 65)

**Why:** Reduces bootstrap from 86 to 30 lines while maintaining same user experience.

**Testing:**
```bash
# Verify bootstrap still works with old bash
/bin/bash ./setup.sh status
# Should prompt for upgrade

# Verify bootstrap skips with new bash
/opt/homebrew/bin/bash ./setup.sh status 2>/dev/null || /usr/local/bin/bash ./setup.sh status 2>/dev/null || echo "New bash not available, manual test only"
```

**Commit:** `refactor(setup): simplify bootstrap by calling _upgrade_bash()`

---

### Task 4: Remove PATH persistence from `_install_homebrew()`

**File:** `setup.sh`
**Lines:** 582-593 (to remove)

**Current code (lines 582-593):**
```bash
  # Persist Homebrew PATH in ~/.bash_profile for future shell sessions.
  local brew_prefix
  brew_prefix=$(brew --prefix)
  local -r profile="${HOME}/.bash_profile"
  local -r shellenv_line="eval \"\$(${brew_prefix}/bin/brew shellenv)\""
  if ! grep -qF "${shellenv_line}" "${profile}" 2>/dev/null; then
    {
      printf '\n# Homebrew\n'
      printf '%s\n' "${shellenv_line}"
    } >> "${profile}"
    log_ok "Homebrew PATH configured in ${profile}"
  fi
```

**Action:** Delete these 12 lines

**Why:** PATH persistence is unnecessary since:
- Users switch to zsh after setup (zsh handles its own Homebrew config)
- Only session-scoped env vars needed for subsequent script tasks
- Reduces code duplication (bootstrap also removed PATH persistence)

**Testing:**
```bash
# Verify Homebrew still works after installation
brew --version

# Verify no PATH persistence in config files
grep -i homebrew ~/.bash_profile 2>/dev/null || echo "No Homebrew in bash_profile (expected)"
```

**Commit:** `refactor(setup): remove PATH persistence from _install_homebrew()`

---

## Testing Strategy

### Manual Test 1: Fresh bash upgrade flow

**Setup:**
```bash
# Backup current setup
cp setup.sh setup.sh.backup

# Verify Homebrew is installed
brew --version
```

**Test:**
```bash
# Run with bash 3.2 (macOS default)
/bin/bash ./setup.sh status
```

**Expected behavior:**
1. Platform check passes (macOS)
2. Bash version check triggers upgrade prompt
3. User confirms upgrade
4. `_upgrade_bash()` installs Homebrew (skips if present)
5. `_upgrade_bash()` installs bash
6. `_upgrade_bash()` re-execs with new bash
7. New bash continues setup

**Success criteria:**
- ✅ Same user prompts as before
- ✅ No errors
- ✅ Re-exec succeeds
- ✅ No modifications to `~/.bash_profile`

---

### Manual Test 2: Modern bash (skip upgrade)

**Test:**
```bash
# Run with bash 4.3+ (if available)
if [[ -x /opt/homebrew/bin/bash ]]; then
  /opt/homebrew/bin/bash ./setup.sh status
elif [[ -x /usr/local/bin/bash ]]; then
  /usr/local/bin/bash ./setup.sh status
else
  echo "Modern bash not available, skipping test"
fi
```

**Expected behavior:**
1. Platform check passes (macOS)
2. Bash version check passes (>= 4.3)
3. No upgrade prompt
4. Setup continues normally

**Success criteria:**
- ✅ No upgrade prompt
- ✅ Setup runs normally
- ✅ Status command works

---

### Manual Test 3: Verify no PATH persistence

**Test:**
```bash
# Run full setup (if not already done)
./setup.sh install --all

# Check for Homebrew PATH in config files
grep -i homebrew ~/.bash_profile 2>/dev/null && echo "FAIL: Found Homebrew in bash_profile" || echo "PASS: No Homebrew in bash_profile"
grep -i homebrew ~/.zshrc 2>/dev/null && echo "INFO: Found Homebrew in zshrc (expected)" || echo "INFO: No Homebrew in zshrc"
```

**Expected behavior:**
- No Homebrew configuration added to `~/.bash_profile`
- Homebrew works in current session (env vars populated)
- After switching to zsh, zsh handles its own Homebrew config

**Success criteria:**
- ✅ `~/.bash_profile` has no Homebrew PATH
- ✅ Homebrew commands work in current session
- ✅ Setup completes successfully

---

### Edge Case Test: User declines upgrade

**Test:**
```bash
# Run with bash 3.2 and decline upgrade
/bin/bash ./setup.sh status
# When prompted: "Continue with bash upgrade? [y/N]:"
# Press Enter or type 'n'
```

**Expected behavior:**
1. Upgrade prompt appears
2. User declines
3. Script exits with message: "Aborted"
4. Exit code: 1

**Success criteria:**
- ✅ Same behavior as before refactoring
- ✅ Clear error message

---

## Verification Checklist

After implementation, verify:

- [ ] Platform check runs before bash version check
- [ ] `_upgrade_bash()` function exists and is syntactically correct
- [ ] Bootstrap section simplified (86 → 30 lines)
- [ ] `_install_homebrew()` no longer persists PATH to config files
- [ ] All existing functionality preserved
- [ ] Error handling works correctly
- [ ] User prompts are identical to before
- [ ] Code follows bash.md conventions
- [ ] No `~/.bash_profile` modifications
- [ ] Homebrew works in current session
- [ ] Re-exec with new bash succeeds

## Rollback Plan

If issues arise:

```bash
# Restore backup
cp setup.sh.backup setup.sh

# Or revert commits
git revert <commit-hash>
```

## Success Metrics

1. ✅ **Code reduction:** -18 lines total (bootstrap -56, function -12, new function +50)
2. ✅ **Duplication eliminated:** Single source of truth for Homebrew installation
3. ✅ **No config pollution:** No modifications to `~/.bash_profile`
4. ✅ **Maintainability:** Clearer separation of concerns
5. ✅ **User experience:** Identical prompts and flow

## Next Steps

After completing this plan:

1. Run all manual tests
2. Verify edge cases
3. Check code follows bash.md conventions
4. Create PR with these commits:
   - `refactor(setup): move platform check before bash version check`
   - `feat(setup): add _upgrade_bash() function for bash upgrades`
   - `refactor(setup): simplify bootstrap by calling _upgrade_bash()`
   - `refactor(setup): remove PATH persistence from _install_homebrew()`

## References

- Design spec: `docs/superpowers/specs/2026-03-30-setup-refactor-design.md`
- Bash conventions: `.claude/rules/lang/bash.md`
- Current setup.sh: Lines 28-109 (bootstrap), 554-595 (`_install_homebrew`)
