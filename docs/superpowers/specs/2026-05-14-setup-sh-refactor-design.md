# setup.sh Refactor Design Spec

## Problem

The current `setup.sh` (1214 lines) works correctly but suffers from:

- Verbose comments that narrate obvious code instead of explaining "why"
- Inconsistent function grouping (helpers scattered between callers)
- Phase counter ceremony adds complexity for a simple progress indicator
- Terse variable names (`C_R`, `C_G`) sacrifice readability
- Overall structure is hard to follow for a new reader

## Approach

**Single-file functional pipeline refactor.** Rewrite from scratch, preserving
every capability and external behavior, with:

- Clean top-to-bottom execution flow
- Self-documenting names (no abbreviations beyond `pkg`, `dir`, `fn`)
- Comments only where non-obvious logic requires explanation
- Strict adherence to `.claude/rules/bash.md` conventions

## File Layout

```
File header (bash.md format)
set -euo pipefail

# --- Constants ---
# --- Color System ---
# --- Logging ---
# --- Signal Handling ---
# --- Package Model ---
# --- Prerequisites ---
# --- Stow Engine ---
# --- Post-Install Hooks ---
# --- Health Check ---
# --- Commands ---
# --- Entry Point ---
```

Each Level 1 section uses `# ---...---` (79 chars). Level 2 subsections use
`# --- Title ---` inline format. No other section markers.

## Naming Conventions

| Current | Refactored | Rationale |
|---------|-----------|-----------|
| `C_R`, `C_G`, `C_Y`, `C_B` | `_RED`, `_GREEN`, `_YELLOW`, `_BLUE` | Matches bash.md examples (line 263) |
| `C_BOLD`, `C_DIM`, `C_RESET` | `_BOLD`, `_DIM`, `_RESET` | Consistent with above |
| `show_help` | `cmd_help` | Consistent with `cmd_install`, etc. |
| `_resolve_stow_conflict` | `_resolve_conflict` | Stow context is already obvious |
| `_LOG_INDENT` | `_LOG_INDENT` | Already readable, keep |
| `_PHASE_TOTAL/_PHASE_INDEX` | `_phase_total/_phase_index` | Mutable state, not constants |

### Variable Categories

| Category | Convention | Example |
|----------|-----------|---------|
| Readonly constants | `UPPER_SNAKE` with `readonly` | `readonly NETWORK_TIMEOUT=60` |
| Private readonly constants | `_UPPER_SNAKE` with `readonly` | `readonly _RED=$'\033[0;31m'` |
| Mutable script state | `lower_snake` (no `local`, no `readonly`) | `dry_run=false` |
| Private mutable state | `_lower_snake` | `_phase_total=1` |
| Array "return values" | `_UPPER_SNAKE` (global, set by callee) | `_VALIDATED_PKGS=()` |
| Function locals | `lower_snake` with `local` | `local pkg_base` |

## Phase Counter

Keep the `[N/M] Section Name` UX. Simplify implementation:

```bash
_phase_total=1
_phase_index=0

_phase() {
  (( _phase_index += 1 ))
  printf '\n%b[%d/%d]%b %b%s%b\n' \
    "${_DIM}" "${_phase_index}" "${_phase_total}" "${_RESET}" \
    "${_BOLD}" "$*" "${_RESET}"
}
```

Each `cmd_*` function sets `_phase_total` at the top based on its own logic
(already the pattern, just cleaner).

## Dry-Run Approach

Use a mutable global `dry_run=false`, checked at the point of action (per
bash.md §Dry-Run Pattern). Each function that performs side effects checks
`"${dry_run}"` internally and prints `[dry-run] would ...` messages. No
centralized `_run()` wrapper — the script's operations are too varied (stow,
brew, curl, chsh, mv) for a single wrapper to add clarity.

## Entry Point Guard

For testability, guard the `main` call so sourcing the file loads functions
without executing:

```bash
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  main "$@"
fi
```

This follows bash.md's prohibition on `&&`/`||` as standalone conditionals.

## Capabilities Preserved (Complete List)

1. **Commands**: install, uninstall, status, help
2. **Flags**: --all, --force, --dry-run
3. **Package validation**: Full path (`shell/zsh`) or short name (`zsh`)
4. **Duplicate detection**: Same package specified twice is deduplicated
5. **Conflict resolution**: Back up files to `*.pre-stow-backup` (with `.1`, `.2` suffixes)
6. **Xcode CLI**: Polling install with timeout
7. **Homebrew**: Version check, reinstall if too old, retry download
8. **Download**: Exponential-backoff retry (configurable retries/delay)
9. **Post-install hooks**: zsh shell switch, yazi plugin install
10. **Health check**: Binary resolution with fallback paths
11. **NO_COLOR / non-TTY**: Color suppression
12. **SIGINT handler**: Kill background children, exit 130
13. **EXIT cleanup**: Remove temp files matching `/tmp/omw-setup-*.$$`
14. **ERR trap**: Print function name, line number, exit code
15. **Exit code propagation**: Non-zero if any stow operation failed
16. **Confirmation prompt**: `uninstall --all` requires y/N confirmation
17. **Git filter**: yazi-package clean/smudge filter setup
18. **Dry-run**: Complete simulation for all operations
19. **brew bundle**: Run with --file flag, continue on partial failure
20. **Platform check**: macOS only (uname -s == Darwin)

## Comment Philosophy

- **DO comment**: Non-obvious "why" decisions, external tool quirks, gotchas
- **DO NOT comment**: What the code literally does, function signatures, obvious flow
- **Section headers**: Level 1 delimiters only (no Level 0 mid-file)
- **No boilerplate**: No "Flow:", "Return codes:", "Usage:" blocks on internal functions

## Test Strategy

### Framework

BATS (Bash Automated Testing System). Install via Homebrew: `brew install bats-core`.

### File Location

`tests/setup.bats`

### Isolation

- Fake `$HOME` in per-test tmpdir
- Mock external tools (`stow`, `brew`, `xcode-select`, `curl`) via PATH prepend
- Source functions without running main: guard `main` call with
  `if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then main "$@"; fi`
- Fresh tmpdir per test via BATS `setup()` / `teardown()`

### Test Categories

| Category | What to Test |
|----------|-------------|
| Package model | validate_pkgs resolves short names, rejects unknown, deduplicates |
| Prerequisite probes | _has_xcode_cli, _has_homebrew, _has_stow detect presence/absence |
| Download | _download succeeds, retries on failure, fails after max attempts |
| Color system | NO_COLOR disables all colors, non-TTY disables all colors |
| Logging | die exits 1, _misuse exits 2, output format matches expected |
| Stow engine | is_stowed detects state, conflict backs up, refuses outside HOME, refuses non-empty dirs |
| Commands | install --all iterates PKG_ALL, --force restows, --dry-run no-ops, uninstall --all prompts |
| Entry point | No args → help, unknown cmd → exit 2, non-Darwin → exit 1 |
| Signal handling | EXIT cleanup removes matching temp files |
| Phase counter | Displays correct [N/M] format |

### Mock Strategy

Create stub scripts in `$BATS_TEST_TMPDIR/bin/`:

```bash
# stow stub
#!/usr/bin/env bash
echo "LINK: .config/zsh => ../../shell/zsh/.config/zsh"
```

Prepend to PATH so real tools are never called.

## Success Criteria

1. All BATS tests pass against the refactored script
2. `bash -n setup.sh` passes
3. `shellcheck -S warning setup.sh` passes (zero warnings)
4. `./setup.sh status` produces identical output to current version
5. `./setup.sh install --dry-run --all` produces equivalent output
6. Net line reduction of 10-20% (target: ~1000-1100 lines)
