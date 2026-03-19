# oh-my-dotfiles Project Audit Report
# Time-stamp: <2026-03-19 by Claude>
# =============================================================================
# Comprehensive audit of project code quality, style, and bug analysis.
# Focus: setup.sh and files changed in the last 20 commits.
# =============================================================================

## Critical Bugs

### Bug 1: `(( n++ ))` triggers `set -e` exit in `backup_file()` / `restore_file()`

**Location:** `setup.sh:204`, `setup.sh:227`

In bash, `(( expr ))` returns exit code 1 when the expression evaluates to 0.
`(( n++ ))` uses post-increment: when `n=0`, the expression value is 0 (old value),
so exit code is 1. Under `set -euo pipefail`, this aborts the script mid-operation.

**Trigger condition:** Any package that already has `.bak.0` in `.backups/<pkg>/`.

**Evidence of awareness:** `stowed_count()` (line 160) already guards this correctly:
```bash
# Protected in stowed_count:
is_stowed "$p" && (( n++ )) || true

# NOT protected in backup_file/restore_file:
while [...]; do
    (( n++ ))   # BUG: exits when n=0 under set -e
done
```

**Fix:** Use `(( ++n ))` (pre-increment, returns new value ≥ 1) or `(( n++ )) || true`.

---

### Bug 2: `do_update_pkgs()` uses `&&/||` anti-pattern — errors silently swallowed

**Location:** `setup.sh:388-392`

```bash
is_stowed "$pkg" \
    && restow_package "$pkg" \
    || log_warn "${pkg}: not stowed, skipping"  # BUG
```

When `is_stowed` returns true (pkg IS stowed) but `restow_package` fails:
1. `is_stowed && restow_package` evaluates to failure
2. `|| log_warn "...not stowed..."` fires with a **misleading message**
3. The actual restow error is silently swallowed; overall exit code is 0

This violates the project's own CLAUDE.md coding standard (use `if` statements,
avoid `[[ ]] && cmd || fallback` pattern). Same issue exists in `cmd_restore()` (line 567).

---

### Bug 3: `stow_targets()` does not respect `.stow-local-ignore` rules

**Location:** `setup.sh:126-139`

The function uses `find` to enumerate all files, but GNU Stow skips files matching
`.stow-local-ignore` patterns. Confirmed divergences for the `zsh` package:

| `stow_targets` returns | Stow actually ignores |
|---|---|
| `~/CLAUDE.md` | Ignored by `zsh/.stow-local-ignore` (`^/CLAUDE\.md`) |
| `~/.stow-local-ignore` | Ignored by Stow default (`^\\.stow.*`) |
| `~/.config/ghostty/config.local` | Ignored by ghostty ignore pattern |
| `~/completions/.gitkeep` | Ignored by Stow default (`.gitkeep`) |

**Risk:** If a user happens to have `~/CLAUDE.md` or `~/.stow-local-ignore` in `$HOME`,
`backup_file` will move them away even though Stow would never touch those paths.

---

## High Severity Bugs

### Bug 4: Backup rotation logic can exceed `MAX_BACKUPS`

**Location:** `setup.sh:207-214`

Scenario with `MAX_BACKUPS=5`, existing `.bak.0` through `.bak.4` (5 total):

1. 6th call: while-loop scans 0→4, n=5. Purge deletes `.bak.0`. Creates `.bak.5`.
   State: `.bak.1`–`.bak.5` (5 backups ✓)

2. 7th call: while-loop exits immediately (`.bak.0` missing), n=0.
   `n(0) < MAX_BACKUPS(5)`: no purge. Creates `.bak.0`.
   State: `.bak.0`–`.bak.5` **(6 backups — exceeds MAX_BACKUPS ✗)**

Root cause: After deletion creates a gap at `.bak.0`, the next call refills it
without triggering the MAX_BACKUPS guard.

---

## Medium Severity Issues

### Issue 5: `VIMINIT` set in `00-env.zsh` (interactive-only) instead of `.zshenv`

**Location:** `zsh/.config/zsh/conf.d/00-env.zsh:79`

```bash
export VIMINIT="set nocp | source $XDG_CONFIG_HOME/vim/vimrc"
```

`00-env.zsh` is sourced by `.zshrc` (interactive shells only). Non-interactive zsh
invocations (cron jobs, `zsh -c '...'`, SSH non-interactive sessions) never load
`.zshrc`, so `VIMINIT` is unset and Vim falls back to `~/.vim`, breaking XDG compliance.

**Fix:** Move `VIMINIT` to `.zshenv` alongside the XDG variables.

---

### Issue 6: `&&/||` anti-pattern also in `validate_pkgs()` and `cmd_restore()`

**Location:** `setup.sh:174`, `setup.sh:567-569`

`cmd_restore`: if `do_restore_pkg` fails, `|| log_warn "Unknown package"` fires
with a misleading message, masking the real error.

---

## Low Severity Issues

### Issue 7: `arg` not declared `local` in `cmd_install/uninstall/update`

**Location:** `setup.sh:456`, `485`, `517`

The loop variable `arg` in all three `cmd_*` functions is not declared `local`,
leaking into the global scope after function return.

---

### Issue 8: `size` not declared `local` in `targz()`

**Location:** `zsh/.config/zsh/functions/targz:22`

`size=$(stat ...)` without `local size` leaks into global scope.

---

### Issue 9: `stow_targets()` returns duplicate paths for folded directories

**Location:** `setup.sh:126-139`

When Stow would "fold" a leaf directory (creating a single directory symlink),
`stow_targets` returns both the directory path AND all individual file paths
within it. This causes `backup_file` to attempt redundant operations.

---

### Issue 10: `is_stowed()` grep pattern could produce false negatives

**Location:** `setup.sh:148`

```bash
! grep -q "conflicts\|cannot stow" <<< "$output"
```

If stow output contains "conflicts" in a filename context (e.g., a file named
`myconflicts.txt`), this would incorrectly report the package as not-stowed.
Low probability in practice.

---

## Shell Function Minor Issues

| File | Issue |
|---|---|
| `targz` | `"${@%/}.tar"` concatenation breaks with multiple args (undocumented single-arg limitation) |
| `fs` | Bare glob `.[^.]* ./*` in else branch relies on zsh `NULL_GLOB`; fails in bash |
| `mkd` | `cd "${_}"` with multiple args jumps to last arg only (undocumented behavior) |

---

## Recent Commits Quality Assessment

| Commit | Files | Assessment |
|---|---|---|
| 841f690 | `setup.sh` (major rewrite) | Introduced Bugs 1-4 |
| b749c11, 7e570e2 | `ghostty/config`, zsh functions | Fixed `echo -e`, quoting — good |
| d657475 | `git-contributors` | Fixed default HEAD arg — good |
| a67e447 | `40-plugins.zsh` | Fixed zcompdump XDG path — good |
| 51521c7 | `emacs/init.el` | Login shell exec-path fix — good |
| 186c6ad | Emacs configs | Fixed `:defer t` + `:after` conflicts — good |

---

## Summary

| Severity | Count | Priority |
|---|---|---|
| Critical | 3 | Fix immediately — data loss / silent crash risk |
| High | 1 | Fix before next release |
| Medium | 2 | Fix in next maintenance pass |
| Low | 4 | Fix when convenient |

The project overall has excellent code quality and documentation. The critical issues
are concentrated in `setup.sh` from the major rewrite in commit `841f690`. The most
urgent fix is Bug 1 (`(( n++ ))` under `set -e`) which will crash `install/update`
whenever any package already has backup files.
