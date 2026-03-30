# setup.sh Refactoring Design

**Date:** 2026-03-30
**Status:** Approved
**Scope:** Eliminate code duplication and remove unnecessary PATH persistence

## Problem Statement

The recent commit (742fd97) introduced code duplication in setup.sh:

1. **Code duplication:** Homebrew installation logic exists in two places:
   - Bootstrap section (lines 58-92): 86 lines of inline installation logic
   - `_install_homebrew()` function (lines 554-595): 42 lines of duplicate logic

2. **Unnecessary PATH persistence:** Both locations persist Homebrew PATH to `~/.bash_profile` (lines 85-92, 582-593), which is not needed because:
   - After setup completes, user switches to zsh
   - zsh handles Homebrew PATH via zsh config
   - Only session-scoped environment variables are needed for subsequent script tasks

3. **Bootstrap bloat:** Simple bash version check expanded from ~30 lines to 86 lines of inline installation logic

## Goals

1. Eliminate code duplication between bootstrap and `_install_homebrew()`
2. Remove PATH persistence to config files (session-only env vars)
3. Simplify bootstrap logic (86 → ~30 lines)
4. Improve maintainability and code clarity
5. Keep all bash-related logic in one focused function

## Design

### Architecture

**Current state:**
- Bootstrap section (lines 28-109): Inline bash version check + Homebrew installation + bash install + re-exec
- `_install_homebrew()` function (lines 554-595): Duplicates installation + adds PATH persistence
- Both persist Homebrew PATH to `~/.bash_profile`

**Proposed architecture:**
```
Platform check (macOS only)
  ↓
Bash version check
  ↓ (if < 4.3)
_upgrade_bash() function
  ├─ Prompt user
  ├─ Install Homebrew (delegates to _install_homebrew())
  ├─ Install bash via Homebrew
  ├─ Populate env vars (session-only)
  └─ Re-exec with new bash
  ↓
Continue setup with bash 4.3+
```

### Component Responsibilities

**`_upgrade_bash()` function (new, ~50 lines):**
- Check bash version
- Prompt user for confirmation
- Install Homebrew (delegates to `_install_homebrew()`)
- Install bash via Homebrew
- Populate environment variables (session-only via `eval`)
- Re-exec script with new bash

**`_install_homebrew()` function (simplified):**
- Check if Homebrew is installed
- Install Homebrew if needed
- Populate environment variables (session-only)
- ~~Persist PATH to config files~~ (REMOVED)

**Bootstrap section (simplified):**
- Platform check (macOS)
- Bash version check
- Call `_upgrade_bash()` if needed
- Exit if upgraded (re-exec handles continuation)

### Environment Variable Handling

**Current behavior:**
- Both locations persist PATH to `~/.bash_profile`
- Bootstrap uses inline logic
- Function uses duplicate logic

**Proposed behavior:**
- Session-only: `eval "$(brew shellenv)"` populates PATH in current process only
- No file modifications to `~/.bash_profile`
- Environment variables inherited by re-exec'd bash process (standard Unix behavior)
- After setup completes, user switches to zsh (zsh handles its own Homebrew config)

**Why this works:**
- Homebrew's `shellenv` command sets up all necessary environment variables
- `eval` executes the output in current shell process
- Environment variables are automatically inherited when using `exec` to replace process
- No persistent configuration needed since zsh will be the login shell

### Data Flow

**Proposed flow (when bash < 4.3):**
1. Script starts with bash 3.2 (macOS default)
2. Bootstrap checks platform → macOS required
3. Bootstrap checks bash version → too old
4. Calls `_upgrade_bash()`
5. `_upgrade_bash()` calls `_install_homebrew()` (installs if needed)
6. `_upgrade_bash()` installs bash via Homebrew
7. `_upgrade_bash()` populates env vars (session-only)
8. `_upgrade_bash()` re-execs with new bash
9. New bash continues setup

**User experience:**
- Same prompts and messages (familiar UX)
- Faster execution (less code duplication)
- No config file pollution (no `~/.bash_profile` modifications)
- Cleaner separation (easier to understand and maintain)

### Error Handling

**Bootstrap errors:**
- User declines upgrade → exit 1 (current behavior ✓)
- Homebrew installation fails → exit 1 (handled by `_install_homebrew`)
- Bash installation fails → exit 1
- Re-exec fails → exit 1

**New error handling in `_upgrade_bash()`:**
- Check if brew is available after installation
- Check if new bash is executable before re-exec
- Clear error messages for each failure point

## Implementation

### Files to Modify

**`setup.sh`** - Single file modification

### Changes Summary

1. Move platform check before bash version check
2. Create new `_upgrade_bash()` function
3. Simplify `_install_homebrew()` (remove PATH persistence)
4. Simplify bootstrap section (86 → 30 lines)

### Detailed Changes

#### Change 1: Reorder checks

**Before:**
```bash
# Bootstrap checks (bash version) - lines 28-109
# Platform check - lines 111-115
```

**After:**
```bash
# Platform check - lines 28-32
# Bootstrap checks (bash version) - lines 34-65
```

#### Change 2: Create `_upgrade_bash()` function (~50 lines)

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

#### Change 3: Simplify `_install_homebrew()` (remove lines 582-593)

**Remove PATH persistence logic:**
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

**Keep only session-scoped env vars:**
```bash
  # Note: eval is required per Homebrew's official post-install instructions.
  # Security: paths are hardcoded to official Homebrew locations.
  local b
  for b in /opt/homebrew/bin/brew /usr/local/bin/brew; do
    if [[ -x "${b}" ]]; then
      eval "$("${b}" shellenv)"
      break
    fi
  done
```

#### Change 4: Simplify bootstrap section (86 → 30 lines)

**Replace inline logic with function call:**
```bash
# Require bash 4.3+ for local -n (nameref) and associative arrays.
if (( BASH_VERSINFO[0] < 4 ||
      ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] < 3 ) )); then
  _upgrade_bash
  # Re-exec is handled within _upgrade_bash()
fi
```

### Net Change

- Bootstrap section: -56 lines (86 → 30)
- `_install_homebrew()`: -12 lines (remove PATH persistence)
- New `_upgrade_bash()` function: +50 lines
- **Total: -18 lines** (more maintainable, cleaner code)

## Testing Strategy

### Manual Testing

1. **Test bash upgrade flow:**
   ```bash
   # Run with bash 3.2 (macOS default)
   /bin/bash setup.sh install --all
   # Should prompt for upgrade, install Homebrew/bash, re-exec
   ```

2. **Test with bash 4.3+:**
   ```bash
   # Run with modern bash (after upgrade)
   /usr/local/bin/bash setup.sh install --all
   # Should skip upgrade, continue normally
   ```

3. **Test Homebrew already installed:**
   ```bash
   # Ensure Homebrew is installed
   brew --version
   # Run setup
   ./setup.sh install --all
   # Should skip Homebrew installation
   ```

4. **Verify no PATH persistence:**
   ```bash
   # Check ~/.bash_profile does NOT contain Homebrew PATH
   grep -i homebrew ~/.bash_profile
   # Should return nothing
   ```

### Edge Cases

1. **User declines upgrade:** Should exit immediately (current behavior ✓)
2. **Homebrew installation fails:** Should exit with clear error message
3. **Bash installation fails:** Should exit with clear error message
4. **Re-exec fails:** Should exit with clear error message
5. **Non-macOS platform:** Should exit early with error message

## Migration Path

No migration needed - this is a refactoring of internal implementation only. External behavior remains identical:

- Same user prompts
- Same error messages
- Same functionality
- No breaking changes

## Future Considerations

1. **Alternative shells:** If zsh becomes a requirement, bash upgrade logic can be removed entirely
2. **Linux support:** Platform check can be extended to support Linux distributions
3. **Version pinning:** If specific bash versions are needed, can add version constraints to `_upgrade_bash()`

## Success Criteria

1. ✅ Code duplication eliminated (single source of truth for Homebrew installation)
2. ✅ PATH persistence removed (no config file modifications)
3. ✅ Bootstrap section simplified (86 → 30 lines)
4. ✅ All existing functionality preserved
5. ✅ Error handling maintained or improved
6. ✅ Code follows bash.md conventions
7. ✅ User experience unchanged (same prompts, same flow)

## References

- Commit 742fd97: Recent changes that introduced duplication
- bash.md: Bash scripting conventions for this project
- GNU Stow Manual: https://www.gnu.org/software/stow/manual/
- Homebrew Bundle: https://github.com/Homebrew/homebrew-bundle
