# Comprehensive Test Suite Design

## Problem

The oh-my-workspace dotfiles repo has 78 BATS tests covering `setup.sh` and
`claude/pre-setup.sh`, but 18 other sourceable scripts (~2,800 lines) have zero
test coverage. These include zsh configuration modules with complex caching
logic, path manipulation, and tool integrations that can silently break.

## Approach

Extend the existing BATS infrastructure to cover all sourceable scripts. Zsh
modules are tested via `zsh -c` subprocesses within BATS — BATS orchestrates
environment setup and assertions while zsh executes natively.

## Architecture

```
tests/
├── setup.bats              # (existing, 36 tests)
├── pre-setup.bats          # (existing, 42 tests)
├── darwin-defaults.bats    # platform/darwin/defaults.sh
├── zsh-env.bats            # shell/zsh/.config/zsh/.zshenv (XDG bootstrap)
├── zsh-00-env.bats         # conf.d/00-env.zsh
├── zsh-05-path.bats        # conf.d/05-path.zsh
├── zsh-10-options.bats     # conf.d/10-options.zsh
├── zsh-15-history.bats     # conf.d/15-history.zsh
├── zsh-20-aliases.bats     # conf.d/20-aliases.zsh
├── zsh-30-completion.bats  # conf.d/30-completion.zsh
├── zsh-40-plugins.bats     # conf.d/40-plugins.zsh
├── zsh-50-prompt.bats      # conf.d/50-prompt.zsh
├── zsh-60-keybinds.bats    # conf.d/60-keybinds.zsh
├── zsh-70-tools.bats       # conf.d/70-tools.zsh
├── zsh-functions.bats      # functions/brew-upgrade, jsonpp
├── zsh_helper.bash         # Shared zsh test utilities
├── bin/                    # (existing mock scripts for setup.sh)
├── pre-setup-bin/          # (existing mock scripts for pre-setup.sh)
└── zsh-bin/                # Mock scripts for zsh tests
```

## Shared Helper: `tests/zsh_helper.bash`

Provides utilities for all zsh test files:

```bash
# run_zsh_isolated <script> [<zsh-expression>]
# Sources a zsh module in a sandboxed environment and optionally
# evaluates an expression after sourcing. Returns zsh exit code.
# Environment:
#   - HOME = BATS_TEST_TMPDIR/home
#   - XDG_CONFIG_HOME = HOME/.config
#   - ZDOTDIR = HOME/.config/zsh
#   - PATH = tests/zsh-bin:tests/bin:/usr/bin:/bin
#   - NO_COLOR=1 (suppress ANSI)
```

## Test Specifications Per Module

### `.zshenv` (tests/zsh-env.bats)

Target: `shell/zsh/.config/zsh/.zshenv` (33 lines)

Tests:
- XDG_CONFIG_HOME set to `$HOME/.config` when unset
- XDG_DATA_HOME set to `$HOME/.local/share` when unset
- XDG_CACHE_HOME set to `$HOME/.cache` when unset
- XDG_STATE_HOME set to `$HOME/.local/state` when unset
- Pre-existing XDG vars are NOT overwritten
- ZDOTDIR set to `$XDG_CONFIG_HOME/zsh`

### `00-env.zsh` (tests/zsh-00-env.bats)

Target: `shell/zsh/.config/zsh/conf.d/00-env.zsh` (172 lines)

Tests:
- EDITOR set to nvim/vim fallback
- PAGER set to less
- LANG and LC_ALL set to en_US.UTF-8
- Homebrew prefix detection: arm64 → /opt/homebrew, x86 → /usr/local
- HOMEBREW_NO_AUTO_UPDATE=1 set
- gdircolors cache: generates on first run
- gdircolors cache: reuses existing cache file
- RIPGREP_CONFIG_PATH points to XDG location
- FZF_DEFAULT_OPTS contains expected color scheme
- BAT_THEME set

### `05-path.zsh` (tests/zsh-05-path.bats)

Target: `shell/zsh/.config/zsh/conf.d/05-path.zsh` (109 lines)

Tests:
- PATH contains expected directories in priority order
- Duplicate entries removed (typeset -U)
- Non-existent directories NOT added to PATH
- FPATH includes custom functions directory
- MANPATH and INFOPATH populated when dirs exist

### `10-options.zsh` (tests/zsh-10-options.bats)

Target: `shell/zsh/.config/zsh/conf.d/10-options.zsh` (68 lines)

Tests:
- AUTO_CD enabled
- EXTENDED_GLOB enabled
- NO_BEEP set (BEEP disabled)
- CORRECT enabled
- NO_FLOW_CONTROL set
- INTERACTIVE_COMMENTS enabled

### `15-history.zsh` (tests/zsh-15-history.bats)

Target: `shell/zsh/.config/zsh/conf.d/15-history.zsh` (69 lines)

Tests:
- HISTSIZE = 60000
- SAVEHIST = 50000
- HISTFILE points to XDG state dir
- SHARE_HISTORY enabled
- HIST_IGNORE_DUPS enabled
- HIST_IGNORE_SPACE enabled
- HIST_EXPIRE_DUPS_FIRST enabled

### `20-aliases.zsh` (tests/zsh-20-aliases.bats)

Target: `shell/zsh/.config/zsh/conf.d/20-aliases.zsh` (92 lines)

Tests:
- Core aliases defined (ll, la, ..., grep)
- `ls` alias uses eza when available
- `ls` alias falls back to system ls with color when eza absent
- Editor aliases set (e → $EDITOR)
- Safety aliases present (rm -i, cp -i, mv -i)

### `30-completion.zsh` (tests/zsh-30-completion.bats)

Target: `shell/zsh/.config/zsh/conf.d/30-completion.zsh` (129 lines)

Tests:
- compinit runs without errors
- zcompdump file created in XDG cache
- Stale zcompdump triggers recompile
- Fresh zcompdump skips recompile
- Completion styles set (case-insensitive matching)
- Menu selection enabled

### `40-plugins.zsh` (tests/zsh-40-plugins.bats)

Target: `shell/zsh/.config/zsh/conf.d/40-plugins.zsh` (193 lines)

Tests:
- ZINIT_INITIALIZED guard prevents double-load
- Zinit home directory created if missing
- Cache files generated when tools available (uv, direnv)
- Cache files reused when fresh
- Plugin load commands are syntactically valid

### `50-prompt.zsh` (tests/zsh-50-prompt.bats)

Target: `shell/zsh/.config/zsh/conf.d/50-prompt.zsh` (59 lines)

Tests:
- Starship init cached and sourced when starship available
- Starship cache regenerated when binary changes
- Fallback to vcs_info when starship unavailable
- Terminal title set via precmd hook

### `60-keybinds.zsh` (tests/zsh-60-keybinds.bats)

Target: `shell/zsh/.config/zsh/conf.d/60-keybinds.zsh` (114 lines)

Tests:
- Emacs keymap selected (bindkey -e output)
- sudo-command-line function: adds sudo prefix
- sudo-command-line function: removes sudo prefix if present
- History search bindings declared

### `70-tools.zsh` (tests/zsh-70-tools.bats)

Target: `shell/zsh/.config/zsh/conf.d/70-tools.zsh` (181 lines)

Tests:
- git wrapper: plain `git` commands pass through
- git wrapper: `git config --global` redirects to config.local
- git wrapper: `git config --local` passes through unchanged
- zoxide init cached and sourced
- direnv hook sourced when available
- Completion caching for bun, uv, carapace

### `functions/` (tests/zsh-functions.bats)

Targets: `functions/brew-upgrade` (17 lines), `functions/jsonpp` (21 lines)

Tests:
- brew-upgrade: calls update, upgrade, cleanup in order
- brew-upgrade: propagates brew failure
- jsonpp: formats valid JSON from stdin
- jsonpp: formats valid JSON from file argument
- jsonpp: exits non-zero on invalid JSON

### `defaults.sh` (tests/darwin-defaults.bats)

Target: `platform/darwin/defaults.sh` (452 lines)

Tests:
- Source guard: sourcing doesn't run main
- Each config function calls `defaults write` (mock verification)
- _general_ui sets expected domains/keys
- _keyboard repeat rate values correct
- _finder shows extensions, search defaults to current folder
- _dock autohide enabled
- _restart_apps: calls osascript quit for expected apps
- Error handler: captures function name and line

## Mock Scripts (tests/zsh-bin/)

New mocks needed:
- `brew` — configurable output for prefix, upgrade, cleanup
- `eza` — simulates eza availability
- `starship` — returns cached init script
- `zoxide` — returns cached init script
- `direnv` — returns hook script
- `nvim` — simulates editor availability
- `defaults` — logs all `defaults write` calls (for darwin tests)
- `osascript` — logs all quit commands (for darwin tests)

## Test Patterns

### Pattern: Zsh option verification
```bash
@test "AUTO_CD enabled" {
  run zsh -c '
    source "$MODULE"
    [[ -o AUTO_CD ]]
  '
  (( status == 0 ))
}
```

### Pattern: Environment variable check
```bash
@test "EDITOR set to nvim when available" {
  run_zsh_isolated "$MODULE" 'print $EDITOR'
  [[ "$output" == *"nvim"* ]]
}
```

### Pattern: Cache file generation
```bash
@test "starship cache created on first source" {
  [[ ! -f "$cache_file" ]]
  run_zsh_isolated "$MODULE" 'true'
  [[ -f "$cache_file" ]]
}
```

### Pattern: Mock command logging (darwin)
```bash
@test "_keyboard sets repeat rate" {
  run _keyboard
  grep -q 'NSGlobalDomain KeyRepeat -int 2' "$MOCK_DEFAULTS_LOG"
}
```

## Implementation Order

1. `tests/zsh_helper.bash` — shared utilities
2. `tests/zsh-env.bats` — simplest, proves the pattern works
3. `tests/zsh-00-env.bats` — most complex env setup
4. `tests/zsh-05-path.bats` — path logic
5. `tests/zsh-10-options.bats` — declarative verification
6. `tests/zsh-15-history.bats` — declarative verification
7. `tests/zsh-20-aliases.bats` — conditional logic
8. `tests/zsh-30-completion.bats` — caching logic
9. `tests/zsh-40-plugins.bats` — plugin guard and caching
10. `tests/zsh-50-prompt.bats` — starship caching
11. `tests/zsh-60-keybinds.bats` — function logic
12. `tests/zsh-70-tools.bats` — git wrapper, tool caching
13. `tests/zsh-functions.bats` — utility functions
14. `tests/darwin-defaults.bats` — macOS defaults
15. `tests/zsh-bin/*` — mock scripts (created as needed per module)

## Success Criteria

- All existing 78 tests continue to pass
- Every sourceable script has at least one test file
- Every conditional branch (if/else, command -v checks) tested both paths
- Cache logic tested: cold start (generate) + warm start (reuse)
- `bats tests/` runs the full suite in under 30 seconds
- No real system side effects (all writes mocked or in BATS_TEST_TMPDIR)

## Constraints

- No real network calls in tests
- No real `defaults write` or `osascript` calls
- No dependency on machine-specific state (installed tools, PATH)
- Tests must pass on any macOS machine with bash 3.2+ and zsh 5.8+
- Interactive features (keybinds, completion widgets) get declaration-level
  tests only — no TTY simulation
