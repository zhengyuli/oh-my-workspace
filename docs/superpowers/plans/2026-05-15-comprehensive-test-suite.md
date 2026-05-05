# Comprehensive Test Suite Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Full BATS test coverage for all 18 sourceable scripts in the oh-my-workspace dotfiles repo.

**Architecture:** BATS orchestrates tests; zsh modules run in `zsh -c` subprocesses for native behavior. One `.bats` file per module. Shared `zsh_helper.bash` provides isolated environment setup. Mock scripts in `tests/zsh-bin/` prevent real tool execution.

**Tech Stack:** BATS (bash), zsh subprocesses, mock scripts

---

### Task 1: Shared Zsh Test Helper

**Files:**
- Create: `tests/zsh_helper.bash`
- Create: `tests/zsh-bin/brew`
- Create: `tests/zsh-bin/eza`
- Create: `tests/zsh-bin/starship`
- Create: `tests/zsh-bin/zoxide`
- Create: `tests/zsh-bin/direnv`
- Create: `tests/zsh-bin/nvim`
- Create: `tests/zsh-bin/gdircolors`
- Create: `tests/zsh-bin/fd`
- Create: `tests/zsh-bin/bat`
- Create: `tests/zsh-bin/carapace`
- Create: `tests/zsh-bin/fzf`
- Create: `tests/zsh-bin/uv`
- Create: `tests/zsh-bin/defaults`
- Create: `tests/zsh-bin/osascript`
- Create: `tests/zsh-bin/python3`

- [ ] **Step 1: Create `tests/zsh_helper.bash`**

```bash
# zsh_helper.bash — shared utilities for zsh BATS tests

# Root of the dotfiles repository
ZSH_CONF_DIR="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh"

# Source a zsh module in an isolated environment and evaluate an expression.
# Usage: run_zsh <module_path> [zsh_expression]
# Returns: zsh exit code; output in $output (via BATS run)
run_zsh() {
  local module="$1"
  local expr="${2:-true}"

  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export NO_COLOR=1
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:${BATS_TEST_DIRNAME}/bin:/usr/bin:/bin\"
    export HOMEBREW_PREFIX=\"/opt/homebrew\"

    mkdir -p \"\$XDG_CACHE_HOME/zsh\" \"\$XDG_STATE_HOME/zsh\"

    source \"${module}\"
    ${expr}
  "
}

setup_zsh_env() {
  export ORIG_HOME="${HOME}"
  export HOME="${BATS_TEST_TMPDIR}/home"
  mkdir -p "${HOME}/.config/zsh/conf.d"
  mkdir -p "${HOME}/.config/zsh/functions"
  mkdir -p "${HOME}/.config/zsh/completions"
  mkdir -p "${HOME}/.cache/zsh"
  mkdir -p "${HOME}/.local/share"
  mkdir -p "${HOME}/.local/state/zsh"

  export ORIG_PATH="${PATH}"
  export PATH="${BATS_TEST_DIRNAME}/zsh-bin:${BATS_TEST_DIRNAME}/bin:/usr/bin:/bin"

  export MOCK_BREW_LOG="${BATS_TEST_TMPDIR}/brew.log"
  export MOCK_DEFAULTS_LOG="${BATS_TEST_TMPDIR}/defaults.log"
  export MOCK_OSASCRIPT_LOG="${BATS_TEST_TMPDIR}/osascript.log"
}

teardown_zsh_env() {
  export HOME="${ORIG_HOME}"
  export PATH="${ORIG_PATH}"
}
```

- [ ] **Step 2: Create mock scripts in `tests/zsh-bin/`**

All mocks must be executable (`chmod +x`). Each mock logs its invocation and returns configurable output.

`tests/zsh-bin/brew`:
```bash
#!/usr/bin/env bash
echo "$*" >> "${MOCK_BREW_LOG:-/dev/null}"
case "$1" in
  --prefix) echo "${MOCK_BREW_PREFIX:-/opt/homebrew}" ;;
  update|upgrade|cleanup) exit "${MOCK_BREW_RC:-0}" ;;
  *) exit 0 ;;
esac
```

`tests/zsh-bin/eza`:
```bash
#!/usr/bin/env bash
exit 0
```

`tests/zsh-bin/starship`:
```bash
#!/usr/bin/env bash
if [[ "$1" == "init" && "$2" == "zsh" ]]; then
  echo '# starship init stub'
fi
exit 0
```

`tests/zsh-bin/zoxide`:
```bash
#!/usr/bin/env bash
if [[ "$1" == "init" ]]; then
  echo '# zoxide init stub'
fi
exit 0
```

`tests/zsh-bin/direnv`:
```bash
#!/usr/bin/env bash
if [[ "$1" == "hook" ]]; then
  echo '# direnv hook stub'
fi
exit 0
```

`tests/zsh-bin/nvim`:
```bash
#!/usr/bin/env bash
if [[ "$1" == "--version" ]]; then
  echo "NVIM v0.10.0"
fi
exit 0
```

`tests/zsh-bin/gdircolors`:
```bash
#!/usr/bin/env bash
echo "export LS_COLORS='di=1;34:ln=1;36:ex=1;32'"
exit 0
```

`tests/zsh-bin/fd`:
```bash
#!/usr/bin/env bash
exit 0
```

`tests/zsh-bin/bat`:
```bash
#!/usr/bin/env bash
cat
exit 0
```

`tests/zsh-bin/carapace`:
```bash
#!/usr/bin/env bash
if [[ "$1" == "_carapace" ]]; then
  echo '# carapace stub'
fi
exit 0
```

`tests/zsh-bin/fzf`:
```bash
#!/usr/bin/env bash
exit 0
```

`tests/zsh-bin/uv`:
```bash
#!/usr/bin/env bash
if [[ "$1" == "generate-shell-completion" ]]; then
  echo '# uv completion stub'
fi
exit 0
```

`tests/zsh-bin/defaults`:
```bash
#!/usr/bin/env bash
echo "$*" >> "${MOCK_DEFAULTS_LOG:-/dev/null}"
exit 0
```

`tests/zsh-bin/osascript`:
```bash
#!/usr/bin/env bash
echo "$*" >> "${MOCK_OSASCRIPT_LOG:-/dev/null}"
exit 0
```

`tests/zsh-bin/python3`:
```bash
#!/usr/bin/env bash
if [[ "$1" == "-c" ]]; then
  # Simulate packaging module availability
  exit "${MOCK_PYTHON3_RC:-0}"
fi
if [[ "$1" == "-m" && "$2" == "json.tool" ]]; then
  # Actually run python3 for json formatting
  /usr/bin/python3 "$@"
  exit $?
fi
exit 0
```

- [ ] **Step 3: Make all mocks executable**

```bash
chmod +x tests/zsh-bin/*
```

- [ ] **Step 4: Run existing tests to verify no regressions**

Run: `bats tests/setup.bats tests/pre-setup.bats`
Expected: 78 tests, 0 failures

- [ ] **Step 5: Commit**

```bash
git add tests/zsh_helper.bash tests/zsh-bin/
git commit -m 'test: add shared zsh test helper and mock scripts

zsh_helper.bash provides run_zsh() for isolated zsh subprocess tests.
16 mock scripts in tests/zsh-bin/ for brew, eza, starship, zoxide,
direnv, nvim, gdircolors, fd, bat, carapace, fzf, uv, defaults,
osascript, python3.'
```

---

### Task 2: `.zshenv` Tests

**Files:**
- Create: `tests/zsh-env.bats`
- Test: `shell/zsh/.zshenv`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-env.bats — tests for shell/zsh/.zshenv

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.zshenv"

@test "XDG_CONFIG_HOME defaults to HOME/.config" {
  run_zsh "$MODULE" 'print $XDG_CONFIG_HOME'
  [[ "$output" == "${HOME}/.config" ]]
}

@test "XDG_CACHE_HOME defaults to HOME/.cache" {
  run_zsh "$MODULE" 'print $XDG_CACHE_HOME'
  [[ "$output" == "${HOME}/.cache" ]]
}

@test "XDG_DATA_HOME defaults to HOME/.local/share" {
  run_zsh "$MODULE" 'print $XDG_DATA_HOME'
  [[ "$output" == "${HOME}/.local/share" ]]
}

@test "XDG_STATE_HOME defaults to HOME/.local/state" {
  run_zsh "$MODULE" 'print $XDG_STATE_HOME'
  [[ "$output" == "${HOME}/.local/state" ]]
}

@test "pre-existing XDG_CONFIG_HOME is not overwritten" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"/custom/config\"
    source \"${MODULE}\"
    print \$XDG_CONFIG_HOME
  "
  [[ "$output" == "/custom/config" ]]
}

@test "ZDOTDIR set to XDG_CONFIG_HOME/zsh" {
  run_zsh "$MODULE" 'print $ZDOTDIR'
  [[ "$output" == "${HOME}/.config/zsh" ]]
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-env.bats`
Expected: 6 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-env.bats
git commit -m 'test(zsh): add .zshenv XDG bootstrap tests (6 tests)'
```

---

### Task 3: `00-env.zsh` Tests

**Files:**
- Create: `tests/zsh-00-env.bats`
- Test: `shell/zsh/.config/zsh/conf.d/00-env.zsh`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-00-env.bats — tests for conf.d/00-env.zsh

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/00-env.zsh"

@test "EDITOR set to nvim" {
  run_zsh "$MODULE" 'print $EDITOR'
  [[ "$output" == "nvim" ]]
}

@test "PAGER set to less" {
  run_zsh "$MODULE" 'print $PAGER'
  [[ "$output" == "less" ]]
}

@test "LANG set to en_US.UTF-8" {
  run_zsh "$MODULE" 'print $LANG'
  [[ "$output" == "en_US.UTF-8" ]]
}

@test "HISTFILE points to XDG_STATE_HOME" {
  run_zsh "$MODULE" 'print $HISTFILE'
  [[ "$output" == *"/.local/state/zsh/history" ]]
}

@test "state and cache dirs created" {
  run_zsh "$MODULE" '
    [[ -d "$XDG_STATE_HOME/zsh" ]] && [[ -d "$XDG_CACHE_HOME/zsh" ]]
  '
  (( status == 0 ))
}

@test "HOMEBREW_NO_AUTO_UPDATE=1" {
  run_zsh "$MODULE" 'print $HOMEBREW_NO_AUTO_UPDATE'
  [[ "$output" == "1" ]]
}

@test "HOMEBREW_NO_ANALYTICS=1" {
  run_zsh "$MODULE" 'print $HOMEBREW_NO_ANALYTICS'
  [[ "$output" == "1" ]]
}

@test "RIPGREP_CONFIG_PATH points to XDG" {
  run_zsh "$MODULE" 'print $RIPGREP_CONFIG_PATH'
  [[ "$output" == *"/.config/ripgrep/rc" ]]
}

@test "STARSHIP_CONFIG points to XDG" {
  run_zsh "$MODULE" 'print $STARSHIP_CONFIG'
  [[ "$output" == *"/.config/starship.toml" ]]
}

@test "FZF_DEFAULT_OPTS contains layout=reverse" {
  run_zsh "$MODULE" 'print $FZF_DEFAULT_OPTS'
  [[ "$output" == *"--layout=reverse"* ]]
}

@test "gdircolors cache created when gdircolors available" {
  run_zsh "$MODULE" '[[ -f "$XDG_CACHE_HOME/zsh/gdircolors.zsh" ]]'
  (( status == 0 ))
}

@test "LS_COLORS fallback when gdircolors unavailable" {
  # Remove gdircolors from PATH
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export PATH=\"/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\" \"\$XDG_STATE_HOME/zsh\"
    source \"${MODULE}\"
    print \$LS_COLORS
  "
  [[ "$output" == *"di=1;34"* ]]
}

@test "GOPATH defaults to XDG_DATA_HOME/go" {
  run_zsh "$MODULE" 'print $GOPATH'
  [[ "$output" == *"/.local/share/go" ]]
}

@test "BUN_INSTALL defaults to XDG_DATA_HOME/bun" {
  run_zsh "$MODULE" 'print $BUN_INSTALL'
  [[ "$output" == *"/.local/share/bun" ]]
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-00-env.bats`
Expected: 14 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-00-env.bats
git commit -m 'test(zsh): add 00-env.zsh tests (14 tests)

Covers: EDITOR, PAGER, LANG, HISTFILE, dir creation, Homebrew vars,
tool paths, gdircolors caching (both hit and miss), FZF opts.'
```

---

### Task 4: `05-path.zsh` Tests

**Files:**
- Create: `tests/zsh-05-path.bats`
- Test: `shell/zsh/.config/zsh/conf.d/05-path.zsh`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-05-path.bats — tests for conf.d/05-path.zsh

load zsh_helper

setup() {
  setup_zsh_env
  # Create directories that 05-path.zsh expects
  mkdir -p "${HOME}/.local/bin"
  mkdir -p "${HOME}/.config/zsh/functions"
  mkdir -p "${HOME}/.config/zsh/completions"
}
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/05-path.zsh"

@test "HOME/.local/bin is in PATH" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export CARGO_HOME=\"${HOME}/.local/share/cargo\"
    export GOPATH=\"${HOME}/.local/share/go\"
    export BUN_INSTALL=\"${HOME}/.local/share/bun\"
    export HOMEBREW_PREFIX=\"/opt/homebrew\"
    source \"${MODULE}\"
    print -l \"\${path[@]}\"
  "
  [[ "$output" == *"${HOME}/.local/bin"* ]]
}

@test "PATH is deduplicated" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export CARGO_HOME=\"${HOME}/.local/share/cargo\"
    export GOPATH=\"${HOME}/.local/share/go\"
    export BUN_INSTALL=\"${HOME}/.local/share/bun\"
    export HOMEBREW_PREFIX=\"/opt/homebrew\"
    export PATH=\"/usr/bin:/usr/bin:/bin\"
    source \"${MODULE}\"
    # Count /usr/bin occurrences
    print -l \"\${path[@]}\" | grep -c '^/usr/bin$'
  "
  [[ "$output" == "1" ]]
}

@test "non-existent dirs excluded from PATH" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export CARGO_HOME=\"${HOME}/.local/share/cargo\"
    export GOPATH=\"${HOME}/.local/share/go\"
    export BUN_INSTALL=\"${HOME}/.local/share/bun\"
    export HOMEBREW_PREFIX=\"/nonexistent_homebrew\"
    source \"${MODULE}\"
    print -l \"\${path[@]}\"
  "
  [[ "$output" != *"/nonexistent_homebrew/bin"* ]]
}

@test "FPATH includes custom functions dir" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export CARGO_HOME=\"${HOME}/.local/share/cargo\"
    export GOPATH=\"${HOME}/.local/share/go\"
    export BUN_INSTALL=\"${HOME}/.local/share/bun\"
    export HOMEBREW_PREFIX=\"/opt/homebrew\"
    source \"${MODULE}\"
    print -l \"\${fpath[@]}\"
  "
  [[ "$output" == *"/.config/zsh/functions"* ]]
}

@test "autoload registers functions from functions dir" {
  # Create a dummy function file
  echo 'echo hello' > "${HOME}/.config/zsh/functions/testfunc"
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export CARGO_HOME=\"${HOME}/.local/share/cargo\"
    export GOPATH=\"${HOME}/.local/share/go\"
    export BUN_INSTALL=\"${HOME}/.local/share/bun\"
    export HOMEBREW_PREFIX=\"/opt/homebrew\"
    source \"${MODULE}\"
    whence -w testfunc
  "
  [[ "$output" == *"function"* ]]
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-05-path.bats`
Expected: 5 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-05-path.bats
git commit -m 'test(zsh): add 05-path.zsh tests (5 tests)

Covers: PATH inclusion, deduplication, non-existent dir exclusion,
FPATH custom functions, autoload registration.'
```

---

### Task 5: `10-options.zsh` Tests

**Files:**
- Create: `tests/zsh-10-options.bats`
- Test: `shell/zsh/.config/zsh/conf.d/10-options.zsh`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-10-options.bats — tests for conf.d/10-options.zsh

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/10-options.zsh"

@test "AUTO_CD enabled" {
  run_zsh "$MODULE" '[[ -o AUTO_CD ]]'
  (( status == 0 ))
}

@test "EXTENDED_GLOB enabled" {
  run_zsh "$MODULE" '[[ -o EXTENDED_GLOB ]]'
  (( status == 0 ))
}

@test "BEEP disabled" {
  run_zsh "$MODULE" '[[ ! -o BEEP ]]'
  (( status == 0 ))
}

@test "INTERACTIVE_COMMENTS enabled" {
  run_zsh "$MODULE" '[[ -o INTERACTIVE_COMMENTS ]]'
  (( status == 0 ))
}

@test "AUTO_PUSHD enabled" {
  run_zsh "$MODULE" '[[ -o AUTO_PUSHD ]]'
  (( status == 0 ))
}

@test "FLOW_CONTROL disabled" {
  run_zsh "$MODULE" '[[ ! -o FLOW_CONTROL ]]'
  (( status == 0 ))
}

@test "NULL_GLOB enabled" {
  run_zsh "$MODULE" '[[ -o NULL_GLOB ]]'
  (( status == 0 ))
}

@test "CLOBBER disabled" {
  run_zsh "$MODULE" '[[ ! -o CLOBBER ]]'
  (( status == 0 ))
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-10-options.bats`
Expected: 8 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-10-options.bats
git commit -m 'test(zsh): add 10-options.zsh tests (8 tests)

Verifies: AUTO_CD, EXTENDED_GLOB, BEEP off, INTERACTIVE_COMMENTS,
AUTO_PUSHD, FLOW_CONTROL off, NULL_GLOB, CLOBBER off.'
```

---

### Task 6: `15-history.zsh` Tests

**Files:**
- Create: `tests/zsh-15-history.bats`
- Test: `shell/zsh/.config/zsh/conf.d/15-history.zsh`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-15-history.bats — tests for conf.d/15-history.zsh

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/15-history.zsh"

@test "HISTSIZE equals 60000" {
  run_zsh "$MODULE" 'print $HISTSIZE'
  [[ "$output" == "60000" ]]
}

@test "SAVEHIST equals 50000" {
  run_zsh "$MODULE" 'print $SAVEHIST'
  [[ "$output" == "50000" ]]
}

@test "SHARE_HISTORY enabled" {
  run_zsh "$MODULE" '[[ -o SHARE_HISTORY ]]'
  (( status == 0 ))
}

@test "HIST_IGNORE_SPACE enabled" {
  run_zsh "$MODULE" '[[ -o HIST_IGNORE_SPACE ]]'
  (( status == 0 ))
}

@test "HIST_EXPIRE_DUPS_FIRST enabled" {
  run_zsh "$MODULE" '[[ -o HIST_EXPIRE_DUPS_FIRST ]]'
  (( status == 0 ))
}

@test "HIST_IGNORE_DUPS enabled" {
  run_zsh "$MODULE" '[[ -o HIST_IGNORE_DUPS ]]'
  (( status == 0 ))
}

@test "EXTENDED_HISTORY enabled" {
  run_zsh "$MODULE" '[[ -o EXTENDED_HISTORY ]]'
  (( status == 0 ))
}

@test "HIST_VERIFY enabled" {
  run_zsh "$MODULE" '[[ -o HIST_VERIFY ]]'
  (( status == 0 ))
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-15-history.bats`
Expected: 8 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-15-history.bats
git commit -m 'test(zsh): add 15-history.zsh tests (8 tests)

Verifies: HISTSIZE/SAVEHIST values, SHARE_HISTORY, HIST_IGNORE_SPACE,
HIST_EXPIRE_DUPS_FIRST, HIST_IGNORE_DUPS, EXTENDED_HISTORY, HIST_VERIFY.'
```

---

### Task 7: `20-aliases.zsh` Tests

**Files:**
- Create: `tests/zsh-20-aliases.bats`
- Test: `shell/zsh/.config/zsh/conf.d/20-aliases.zsh`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-20-aliases.bats — tests for conf.d/20-aliases.zsh

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/20-aliases.zsh"

@test "ls uses eza when available" {
  run_zsh "$MODULE" 'alias ls'
  [[ "$output" == *"eza"* ]]
}

@test "ls falls back to system ls when eza unavailable" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export OSTYPE=\"darwin22.0\"
    export PATH=\"/usr/bin:/bin\"
    source \"${MODULE}\"
    alias ls
  "
  [[ "$output" == *"-G"* ]]
}

@test "safety aliases defined (cp -iv)" {
  run_zsh "$MODULE" 'alias cp'
  [[ "$output" == *"-iv"* ]]
}

@test "safety aliases defined (rm -iv)" {
  run_zsh "$MODULE" 'alias rm'
  [[ "$output" == *"-iv"* ]]
}

@test "editor alias v points to nvim" {
  run_zsh "$MODULE" 'alias v'
  [[ "$output" == *"nvim"* ]]
}

@test "grep has --color=auto" {
  run_zsh "$MODULE" 'alias grep'
  [[ "$output" == *"--color=auto"* ]]
}

@test "navigation aliases defined" {
  run_zsh "$MODULE" 'alias ..'
  [[ "$output" == *"cd .."* ]]
}

@test "reload-zsh alias defined" {
  run_zsh "$MODULE" 'alias reload-zsh'
  [[ "$output" == *"exec zsh"* ]]
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-20-aliases.bats`
Expected: 8 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-20-aliases.bats
git commit -m 'test(zsh): add 20-aliases.zsh tests (8 tests)

Covers: eza detection, BSD ls fallback, safety aliases, editor
aliases, grep color, navigation, reload.'
```

---

### Task 8: `30-completion.zsh` Tests

**Files:**
- Create: `tests/zsh-30-completion.bats`
- Test: `shell/zsh/.config/zsh/conf.d/30-completion.zsh`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-30-completion.bats — tests for conf.d/30-completion.zsh

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/30-completion.zsh"

@test "compinit runs without error" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
  "
  (( status == 0 ))
}

@test "zcompdump created in XDG cache" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
    [[ -f \"\$XDG_CACHE_HOME/zsh/zcompdump\" ]]
  "
  (( status == 0 ))
}

@test "COMPDUMP_MAX_AGE_HOURS defaults to 20" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
    print \$COMPDUMP_MAX_AGE_HOURS
  "
  [[ "$output" == "20" ]]
}

@test "completion cache path uses XDG" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
    zstyle -L ':completion:*' cache-path
  "
  [[ "$output" == *"/.cache/zsh/completion-cache"* ]]
}

@test "matcher-list includes smart case" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
    zstyle -L ':completion:*' matcher-list
  "
  [[ "$output" == *"m:{a-z}={A-Za-z}"* ]]
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-30-completion.bats`
Expected: 5 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-30-completion.bats
git commit -m 'test(zsh): add 30-completion.zsh tests (5 tests)

Covers: compinit execution, zcompdump location, age threshold,
cache path, matcher-list smart case.'
```

---

### Task 9: `40-plugins.zsh` Tests

**Files:**
- Create: `tests/zsh-40-plugins.bats`
- Test: `shell/zsh/.config/zsh/conf.d/40-plugins.zsh`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-40-plugins.bats — tests for conf.d/40-plugins.zsh

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/40-plugins.zsh"

@test "ZINIT_INITIALIZED guard prevents double load" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    ZINIT_INITIALIZED=1
    source \"${MODULE}\"
    # If guard works, ZINIT_HOME won't be set
    [[ -z \"\${ZINIT_HOME:-}\" ]]
  "
  (( status == 0 ))
}

@test "ZINIT_HOME points to XDG data dir" {
  # Note: this test only verifies the variable assignment, not actual git clone
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    # Simulate zinit already installed
    mkdir -p \"\$XDG_DATA_HOME/zinit/zinit.git\"
    echo '# stub' > \"\$XDG_DATA_HOME/zinit/zinit.git/zinit.zsh\"
    source \"${MODULE}\"
    print \$ZINIT_HOME
  "
  [[ "$output" == *"/.local/share/zinit/zinit.git" ]]
}

@test "returns 1 when zinit.zsh missing after clone attempt" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export PATH=\"/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    mkdir -p \"\$XDG_DATA_HOME/zinit/zinit.git\"
    # No zinit.zsh file → should return 1
    source \"${MODULE}\"
  "
  (( status == 1 ))
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-40-plugins.bats`
Expected: 3 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-40-plugins.bats
git commit -m 'test(zsh): add 40-plugins.zsh tests (3 tests)

Covers: ZINIT_INITIALIZED idempotency guard, ZINIT_HOME XDG location,
failure when zinit.zsh missing.'
```

---

### Task 10: `50-prompt.zsh` Tests

**Files:**
- Create: `tests/zsh-50-prompt.bats`
- Test: `shell/zsh/.config/zsh/conf.d/50-prompt.zsh`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-50-prompt.bats — tests for conf.d/50-prompt.zsh

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/50-prompt.zsh"

@test "starship cache file created" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
    [[ -f \"\$XDG_CACHE_HOME/zsh/starship-init.zsh\" ]]
  "
  (( status == 0 ))
}

@test "starship cache reused when fresh" {
  # Pre-create cache
  mkdir -p "${HOME}/.cache/zsh"
  echo '# cached' > "${HOME}/.cache/zsh/starship-init.zsh"
  # Make cache newer than the starship binary (touch it)
  sleep 0.1
  touch "${HOME}/.cache/zsh/starship-init.zsh"

  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin:/bin\"
    source \"${MODULE}\"
    cat \"\$XDG_CACHE_HOME/zsh/starship-init.zsh\"
  "
  # Should still contain original content (not regenerated)
  [[ "$output" == *"# cached"* ]]
}

@test "vcs_info fallback when starship unavailable" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export PATH=\"/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
    print \$PROMPT
  "
  [[ "$output" == *"%~"* ]]
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-50-prompt.bats`
Expected: 3 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-50-prompt.bats
git commit -m 'test(zsh): add 50-prompt.zsh tests (3 tests)

Covers: starship cache creation, cache reuse, vcs_info fallback.'
```

---

### Task 11: `60-keybinds.zsh` Tests

**Files:**
- Create: `tests/zsh-60-keybinds.bats`
- Test: `shell/zsh/.config/zsh/conf.d/60-keybinds.zsh`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-60-keybinds.bats — tests for conf.d/60-keybinds.zsh

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/60-keybinds.zsh"

@test "emacs keymap selected" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    source \"${MODULE}\"
    bindkey -lL main
  "
  [[ "$output" == *"emacs"* ]]
}

@test "KEYTIMEOUT set to 10" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    source \"${MODULE}\"
    print \$KEYTIMEOUT
  "
  [[ "$output" == "10" ]]
}

@test "sudo-command-line widget registered" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    source \"${MODULE}\"
    zle -lL | grep sudo-command-line
  "
  (( status == 0 ))
  [[ "$output" == *"sudo-command-line"* ]]
}

@test "Ctrl+P bound to history-search-backward" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    source \"${MODULE}\"
    bindkey '^P'
  "
  [[ "$output" == *"history-search-backward"* ]]
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-60-keybinds.bats`
Expected: 4 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-60-keybinds.bats
git commit -m 'test(zsh): add 60-keybinds.zsh tests (4 tests)

Covers: emacs keymap, KEYTIMEOUT, sudo-command-line widget, history
search binding.'
```

---

### Task 12: `70-tools.zsh` Tests

**Files:**
- Create: `tests/zsh-70-tools.bats`
- Test: `shell/zsh/.config/zsh/conf.d/70-tools.zsh`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-70-tools.bats — tests for conf.d/70-tools.zsh

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/70-tools.zsh"

# git wrapper needs compinit loaded first
_source_with_compinit() {
  local expr="$1"
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export BUN_INSTALL=\"${HOME}/.local/share/bun\"
    export HOMEBREW_PREFIX=\"/opt/homebrew\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    autoload -Uz compinit && compinit -C -d \"\$XDG_CACHE_HOME/zsh/zcompdump\"
    source \"${MODULE}\"
    ${expr}
  "
}

@test "git wrapper: non-config commands pass through" {
  _source_with_compinit 'whence -w git'
  [[ "$output" == *"function"* ]]
}

@test "_git_check_flag: --get returns r" {
  _source_with_compinit 'print $(_git_check_flag --get)'
  [[ "$output" == "r" ]]
}

@test "_git_check_flag: --add returns w" {
  _source_with_compinit 'print $(_git_check_flag --add)'
  [[ "$output" == "w" ]]
}

@test "_git_check_flag: unknown flag returns empty" {
  _source_with_compinit 'print $(_git_check_flag --unknown)'
  [[ "$output" == "" ]]
}

@test "_git_classify_config_op: two positional args = write" {
  _source_with_compinit 'print $(_git_classify_config_op user.name "John")'
  [[ "$output" == "w" ]]
}

@test "_git_classify_config_op: one positional arg = read" {
  _source_with_compinit 'print $(_git_classify_config_op user.name)'
  [[ "$output" == "r" ]]
}

@test "_git_classify_config_op: --list flag = read" {
  _source_with_compinit 'print $(_git_classify_config_op --list)'
  [[ "$output" == "r" ]]
}

@test "uv completion cache created" {
  _source_with_compinit '[[ -f "$XDG_CACHE_HOME/zsh/uv-completion.zsh" ]]'
  (( status == 0 ))
}

@test "zoxide cache created" {
  _source_with_compinit '[[ -f "$XDG_CACHE_HOME/zsh/zoxide-init.zsh" ]]'
  (( status == 0 ))
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-70-tools.bats`
Expected: 9 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-70-tools.bats
git commit -m 'test(zsh): add 70-tools.zsh tests (9 tests)

Covers: git wrapper function type, _git_check_flag read/write/unknown,
_git_classify_config_op positional/flag, uv and zoxide cache creation.'
```

---

### Task 13: `functions/` Tests

**Files:**
- Create: `tests/zsh-functions.bats`
- Test: `shell/zsh/.config/zsh/functions/brew-upgrade`, `functions/jsonpp`

- [ ] **Step 1: Write tests**

```bash
#!/usr/bin/env bats
# zsh-functions.bats — tests for zsh autoloaded functions

load zsh_helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

FUNC_DIR="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/functions"

@test "brew-upgrade: calls update, upgrade, cleanup in order" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin:/bin\"
    export MOCK_BREW_LOG=\"${BATS_TEST_TMPDIR}/brew.log\"
    source \"${FUNC_DIR}/brew-upgrade\"
  "
  (( status == 0 ))
  grep -q 'update' "${BATS_TEST_TMPDIR}/brew.log"
  grep -q 'upgrade' "${BATS_TEST_TMPDIR}/brew.log"
  grep -q 'cleanup' "${BATS_TEST_TMPDIR}/brew.log"
  # Verify order: update before upgrade before cleanup
  local update_line upgrade_line cleanup_line
  update_line=$(grep -n 'update' "${BATS_TEST_TMPDIR}/brew.log" | head -1 | cut -d: -f1)
  upgrade_line=$(grep -n 'upgrade' "${BATS_TEST_TMPDIR}/brew.log" | head -1 | cut -d: -f1)
  cleanup_line=$(grep -n 'cleanup' "${BATS_TEST_TMPDIR}/brew.log" | head -1 | cut -d: -f1)
  (( update_line < upgrade_line ))
  (( upgrade_line < cleanup_line ))
}

@test "brew-upgrade: propagates brew failure" {
  export MOCK_BREW_RC=1
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin:/bin\"
    export MOCK_BREW_LOG=\"${BATS_TEST_TMPDIR}/brew.log\"
    export MOCK_BREW_RC=1
    source \"${FUNC_DIR}/brew-upgrade\"
  "
  (( status != 0 ))
}

@test "jsonpp: formats valid JSON from stdin" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin:/bin\"
    echo '{\"a\":1}' | source \"${FUNC_DIR}/jsonpp\"
  "
  (( status == 0 ))
  [[ "$output" == *'"a"'* ]]
}

@test "jsonpp: exits non-zero on invalid JSON" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin:/bin\"
    echo 'not json' | source \"${FUNC_DIR}/jsonpp\"
  "
  (( status != 0 ))
}
```

- [ ] **Step 2: Run and verify**

Run: `bats tests/zsh-functions.bats`
Expected: 4 tests, 0 failures

- [ ] **Step 3: Commit**

```bash
git add tests/zsh-functions.bats
git commit -m 'test(zsh): add functions/ tests (4 tests)

Covers: brew-upgrade order and failure propagation, jsonpp valid/invalid.'
```

---

### Task 14: `darwin/defaults.sh` Tests

**Files:**
- Create: `tests/darwin-defaults.bats`
- Modify: `platform/darwin/defaults.sh` (add source guard)
- Test: `platform/darwin/defaults.sh`

- [ ] **Step 1: Add source guard to `defaults.sh`**

Replace the last line `main "$@"` with:

```bash
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  main "$@"
fi
```

- [ ] **Step 2: Write tests**

```bash
#!/usr/bin/env bats
# darwin-defaults.bats — tests for platform/darwin/defaults.sh

setup() {
  export ORIG_HOME="${HOME}"
  export HOME="${BATS_TEST_TMPDIR}/home"
  mkdir -p "${HOME}"

  export ORIG_PATH="${PATH}"
  export PATH="${BATS_TEST_DIRNAME}/zsh-bin:${BATS_TEST_DIRNAME}/bin:/usr/bin:/bin"

  export MOCK_DEFAULTS_LOG="${BATS_TEST_TMPDIR}/defaults.log"
  export MOCK_OSASCRIPT_LOG="${BATS_TEST_TMPDIR}/osascript.log"
  touch "${MOCK_DEFAULTS_LOG}" "${MOCK_OSASCRIPT_LOG}"

  source "${BATS_TEST_DIRNAME}/../platform/darwin/defaults.sh"
}

teardown() {
  export HOME="${ORIG_HOME}"
  export PATH="${ORIG_PATH}"
}

@test "sourcing does not run main" {
  # _err_handler would have printed errors if main ran with mock defaults
  [[ ! -s "${MOCK_DEFAULTS_LOG}" ]]
}

@test "_general_ui calls defaults write NSGlobalDomain" {
  run _general_ui
  (( status == 0 ))
  grep -q 'NSGlobalDomain' "${MOCK_DEFAULTS_LOG}"
}

@test "_keyboard sets KeyRepeat" {
  run _keyboard
  (( status == 0 ))
  grep -q 'KeyRepeat' "${MOCK_DEFAULTS_LOG}"
}

@test "_trackpad_mouse enables tap to click" {
  run _trackpad_mouse
  (( status == 0 ))
  grep -q 'Clicking' "${MOCK_DEFAULTS_LOG}"
}

@test "_finder shows all extensions" {
  run _finder
  (( status == 0 ))
  grep -q 'AppleShowAllExtensions' "${MOCK_DEFAULTS_LOG}"
}

@test "_dock enables autohide" {
  run _dock
  (( status == 0 ))
  grep -q 'autohide' "${MOCK_DEFAULTS_LOG}"
}

@test "_time_machine disables new disk prompt" {
  run _time_machine
  (( status == 0 ))
  grep -q 'DoNotOfferNewDisksForBackup' "${MOCK_DEFAULTS_LOG}"
}

@test "_restart_apps calls osascript" {
  run _restart_apps
  grep -q 'quit' "${MOCK_OSASCRIPT_LOG}" || grep -q 'Quit' "${MOCK_OSASCRIPT_LOG}"
}
```

- [ ] **Step 3: Run and verify**

Run: `bats tests/darwin-defaults.bats`
Expected: 8 tests, 0 failures

- [ ] **Step 4: Commit**

```bash
git add platform/darwin/defaults.sh tests/darwin-defaults.bats
git commit -m 'test(darwin): add defaults.sh tests (8 tests)

Add source guard to defaults.sh. Tests verify each config function
calls correct defaults domains. Mocked defaults and osascript.'
```

---

### Task 15: Final Verification

- [ ] **Step 1: Run full test suite**

```bash
bats tests/
```

Expected: All tests pass (78 existing + ~73 new ≈ 151 total)

- [ ] **Step 2: Verify no regressions**

```bash
bash -n setup.sh && bash -n claude/pre-setup.sh && bash -n platform/darwin/defaults.sh
shellcheck setup.sh claude/pre-setup.sh platform/darwin/defaults.sh
```

Expected: Clean output, no errors

- [ ] **Step 3: Commit summary (if needed)**

No commit needed unless fixes were required.

---

## Summary

| Task | Module | Tests |
|------|--------|-------|
| 1 | Shared helper + mocks | 0 (infrastructure) |
| 2 | .zshenv | 6 |
| 3 | 00-env.zsh | 14 |
| 4 | 05-path.zsh | 5 |
| 5 | 10-options.zsh | 8 |
| 6 | 15-history.zsh | 8 |
| 7 | 20-aliases.zsh | 8 |
| 8 | 30-completion.zsh | 5 |
| 9 | 40-plugins.zsh | 3 |
| 10 | 50-prompt.zsh | 3 |
| 11 | 60-keybinds.zsh | 4 |
| 12 | 70-tools.zsh | 9 |
| 13 | functions/ | 4 |
| 14 | darwin/defaults.sh | 8 |
| **Total new** | | **85** |
| **Grand total** | (+ 78 existing) | **163** |
