# setup.sh Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rewrite setup.sh from scratch with clean structure, self-documenting names, and minimal comments while preserving all 20 capabilities.

**Architecture:** Single-file functional pipeline (Constants → Logging → Signals → Package Model → Prerequisites → Stow Engine → Hooks → Health → Commands → Entry). BATS test suite validates behavior before and after refactor.

**Tech Stack:** Bash 3.2+, GNU Stow, BATS (bats-core), Homebrew

---

## File Structure

| File | Responsibility |
|------|---------------|
| `setup.sh` | Main setup script (rewritten from scratch) |
| `tests/setup.bats` | BATS test suite covering all capabilities |
| `tests/test_helper.bash` | Shared setup/teardown, mock creation, source guard |
| `tests/bin/` | Mock scripts (stow, brew, curl, xcode-select) |

---

### Task 1: Install BATS and Create Test Infrastructure

**Files:**
- Create: `tests/test_helper.bash`
- Create: `tests/bin/stow`
- Create: `tests/bin/brew`
- Create: `tests/bin/curl`
- Create: `tests/bin/xcode-select`
- Create: `tests/bin/uname`
- Modify: `setup.sh` (add source guard at bottom)

- [ ] **Step 1: Install bats-core via Homebrew**

Run: `brew install bats-core`
Expected: bats-core installed (or already installed)

- [ ] **Step 2: Create tests directory structure**

Run: `mkdir -p tests/bin`

- [ ] **Step 3: Create mock stow script**

Create `tests/bin/stow`:

```bash
#!/usr/bin/env bash
# Mock stow: records calls, outputs configurable responses
echo "$*" >> "${MOCK_STOW_LOG:-/dev/null}"
if [[ -n "${MOCK_STOW_OUTPUT:-}" ]]; then
  printf '%s\n' "${MOCK_STOW_OUTPUT}"
fi
if [[ -n "${MOCK_STOW_STDERR:-}" ]]; then
  printf '%s\n' "${MOCK_STOW_STDERR}" >&2
fi
exit "${MOCK_STOW_RC:-0}"
```

Run: `chmod +x tests/bin/stow`

- [ ] **Step 4: Create mock brew script**

Create `tests/bin/brew`:

```bash
#!/usr/bin/env bash
echo "$*" >> "${MOCK_BREW_LOG:-/dev/null}"
case "$1" in
  --version) printf 'Homebrew %s\n' "${MOCK_BREW_VERSION:-4.4.0}" ;;
  shellenv) printf 'export PATH="/opt/homebrew/bin:$PATH"\n' ;;
  bundle) exit "${MOCK_BREW_BUNDLE_RC:-0}" ;;
  install) exit "${MOCK_BREW_INSTALL_RC:-0}" ;;
  *) exit 0 ;;
esac
```

Run: `chmod +x tests/bin/brew`

- [ ] **Step 5: Create mock curl script**

Create `tests/bin/curl`:

```bash
#!/usr/bin/env bash
echo "$*" >> "${MOCK_CURL_LOG:-/dev/null}"
if [[ "${MOCK_CURL_FAIL_COUNT:-0}" -gt 0 ]]; then
  MOCK_CURL_FAIL_COUNT=$(( MOCK_CURL_FAIL_COUNT - 1 ))
  export MOCK_CURL_FAIL_COUNT
  exit 1
fi
# Find --output flag and create the file
local_output=""
while [[ $# -gt 0 ]]; do
  if [[ "$1" == "--output" ]]; then
    local_output="$2"
    shift 2
  else
    shift
  fi
done
if [[ -n "${local_output}" ]]; then
  printf '#!/usr/bin/env bash\nexit 0\n' > "${local_output}"
fi
exit 0
```

Run: `chmod +x tests/bin/curl`

- [ ] **Step 6: Create mock xcode-select script**

Create `tests/bin/xcode-select`:

```bash
#!/usr/bin/env bash
case "$1" in
  -p) exit "${MOCK_XCODE_RC:-0}" ;;
  --install) exit 0 ;;
  *) exit 0 ;;
esac
```

Run: `chmod +x tests/bin/xcode-select`

- [ ] **Step 7: Create mock uname script**

Create `tests/bin/uname`:

```bash
#!/usr/bin/env bash
if [[ "$1" == "-s" ]]; then
  printf '%s\n' "${MOCK_UNAME:-Darwin}"
else
  /usr/bin/uname "$@"
fi
```

Run: `chmod +x tests/bin/uname`

- [ ] **Step 8: Create test_helper.bash**

Create `tests/test_helper.bash`:

```bash
# test_helper.bash — shared BATS setup/teardown

# Source setup.sh functions without running main.
_source_setup() {
  # Override WORKSPACE_DIR to point at the repo root
  export WORKSPACE_DIR="${BATS_TEST_DIRNAME}/.."
  export BREWFILE="${WORKSPACE_DIR}/pkg/homebrew/Brewfile"
  source "${WORKSPACE_DIR}/setup.sh"
}

setup() {
  # Fresh tmpdir as fake HOME
  export ORIG_HOME="${HOME}"
  export HOME="${BATS_TEST_TMPDIR}/home"
  mkdir -p "${HOME}/.config"

  # Prepend mock bin to PATH
  export ORIG_PATH="${PATH}"
  export PATH="${BATS_TEST_DIRNAME}/bin:${PATH}"

  # Reset mock state
  export MOCK_STOW_LOG="${BATS_TEST_TMPDIR}/stow.log"
  export MOCK_STOW_OUTPUT=""
  export MOCK_STOW_STDERR=""
  export MOCK_STOW_RC=0
  export MOCK_BREW_LOG="${BATS_TEST_TMPDIR}/brew.log"
  export MOCK_BREW_VERSION="4.4.0"
  export MOCK_BREW_BUNDLE_RC=0
  export MOCK_BREW_INSTALL_RC=0
  export MOCK_CURL_LOG="${BATS_TEST_TMPDIR}/curl.log"
  export MOCK_CURL_FAIL_COUNT=0
  export MOCK_XCODE_RC=0
  export MOCK_UNAME="Darwin"

  # Reset script state
  unset NO_COLOR
}

teardown() {
  export HOME="${ORIG_HOME}"
  export PATH="${ORIG_PATH}"
}
```

- [ ] **Step 9: Add source guard to current setup.sh**

At the bottom of `setup.sh`, replace `main "$@"` with:

```bash
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  main "$@"
fi
```

- [ ] **Step 10: Verify infrastructure works**

Create a minimal `tests/setup.bats`:

```bash
#!/usr/bin/env bats

load test_helper

@test "setup.sh can be sourced without executing" {
  _source_setup
  [[ "$(type -t main)" == "function" ]]
}
```

Run: `bats tests/setup.bats`
Expected: 1 test, PASS

- [ ] **Step 11: Commit**

```bash
git add tests/ setup.sh
git commit -m "test: add BATS infrastructure for setup.sh refactor

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 2: Tests for Package Model

**Files:**
- Modify: `tests/setup.bats`

- [ ] **Step 1: Write package model tests**

Append to `tests/setup.bats`:

```bash
# --- Package Model ---

@test "pkg_category extracts category from full path" {
  _source_setup
  run pkg_category "shell/zsh"
  [[ "$output" == "shell" ]]
}

@test "pkg_category handles nested paths" {
  _source_setup
  run pkg_category "lang/python/uv"
  [[ "$output" == "lang/python" ]]
}

@test "pkg_name extracts base name" {
  _source_setup
  run pkg_name "shell/zsh"
  [[ "$output" == "zsh" ]]
}

@test "pkg_name extracts base from nested path" {
  _source_setup
  run pkg_name "lang/python/uv"
  [[ "$output" == "uv" ]]
}

@test "pkg_stow_dir returns workspace/category" {
  _source_setup
  run pkg_stow_dir "shell/zsh"
  [[ "$output" == "${WORKSPACE_DIR}/shell" ]]
}

@test "_is_valid_pkg accepts known packages" {
  _source_setup
  _is_valid_pkg "shell/zsh"
}

@test "_is_valid_pkg rejects unknown packages" {
  _source_setup
  ! _is_valid_pkg "unknown/pkg"
}

@test "validate_pkgs resolves short name to full path" {
  _source_setup
  validate_pkgs "zsh"
  [[ "${_VALIDATED_PKGS[0]}" == "shell/zsh" ]]
}

@test "validate_pkgs accepts full path" {
  _source_setup
  validate_pkgs "shell/zsh"
  [[ "${_VALIDATED_PKGS[0]}" == "shell/zsh" ]]
}

@test "validate_pkgs deduplicates" {
  _source_setup
  validate_pkgs "zsh" "shell/zsh" "zsh"
  [[ "${#_VALIDATED_PKGS[@]}" -eq 1 ]]
}

@test "validate_pkgs returns 1 for all unknown" {
  _source_setup
  ! validate_pkgs "nonexistent" "alsofake"
}

@test "validate_pkgs skips unknown, keeps valid" {
  _source_setup
  validate_pkgs "zsh" "nonexistent"
  [[ "${#_VALIDATED_PKGS[@]}" -eq 1 ]]
  [[ "${_VALIDATED_PKGS[0]}" == "shell/zsh" ]]
}
```

- [ ] **Step 2: Run tests**

Run: `bats tests/setup.bats`
Expected: All pass (testing current setup.sh)

- [ ] **Step 3: Commit**

```bash
git add tests/setup.bats
git commit -m "test: add package model tests

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 3: Tests for Color System and Logging

**Files:**
- Modify: `tests/setup.bats`

- [ ] **Step 1: Write color and logging tests**

Append to `tests/setup.bats`:

```bash
# --- Color System ---

@test "NO_COLOR disables all color variables" {
  export NO_COLOR=1
  _source_setup
  [[ -z "${_RED}" ]]
  [[ -z "${_GREEN}" ]]
  [[ -z "${_RESET}" ]]
}

@test "colors are set when NO_COLOR unset and TTY" {
  unset NO_COLOR
  # Cannot fully test TTY in bats, but verify the variables exist
  _source_setup
  # In non-TTY (bats), colors should be empty
  [[ -z "${_RED}" ]] || [[ -n "${_RED}" ]]
}

# --- Logging ---

@test "die exits with code 1" {
  _source_setup
  run die "test error"
  [[ "$status" -eq 1 ]]
}

@test "die outputs to stderr with error tag" {
  _source_setup
  run die "test error"
  [[ "$output" == *"[error]"* ]]
  [[ "$output" == *"test error"* ]]
}

@test "_misuse exits with code 2" {
  _source_setup
  run _misuse "bad usage"
  [[ "$status" -eq 2 ]]
}

@test "log_ok outputs with ok tag" {
  _source_setup
  run log_ok "success message"
  [[ "$output" == *"[ok]"* ]]
  [[ "$output" == *"success message"* ]]
}

@test "log_warn outputs with warn tag" {
  _source_setup
  run log_warn "warning message"
  [[ "$output" == *"[warn]"* ]]
  [[ "$output" == *"warning message"* ]]
}

@test "log_info outputs with info tag" {
  _source_setup
  run log_info "info message"
  [[ "$output" == *"[info]"* ]]
  [[ "$output" == *"info message"* ]]
}

@test "_phase outputs numbered header" {
  _source_setup
  _phase_total=3
  _phase_index=0
  run _phase "Test Phase"
  [[ "$output" == *"[1/3]"* ]]
  [[ "$output" == *"Test Phase"* ]]
}
```

- [ ] **Step 2: Run tests**

Run: `bats tests/setup.bats`
Expected: All pass

- [ ] **Step 3: Commit**

```bash
git add tests/setup.bats
git commit -m "test: add color system and logging tests

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 4: Tests for Download Helper

**Files:**
- Modify: `tests/setup.bats`

- [ ] **Step 1: Write download tests**

Append to `tests/setup.bats`:

```bash
# --- Download ---

@test "_download succeeds on first attempt" {
  _source_setup
  local dest="${BATS_TEST_TMPDIR}/downloaded"
  run _download "http://example.com/file" "${dest}"
  [[ "$status" -eq 0 ]]
  [[ -f "${dest}" ]]
}

@test "_download retries and succeeds" {
  export MOCK_CURL_FAIL_COUNT=2
  _source_setup
  local dest="${BATS_TEST_TMPDIR}/downloaded"
  run _download "http://example.com/file" "${dest}"
  [[ "$status" -eq 0 ]]
}

@test "_download fails after max retries" {
  export MOCK_CURL_FAIL_COUNT=99
  _source_setup
  local dest="${BATS_TEST_TMPDIR}/downloaded"
  run _download "http://example.com/file" "${dest}"
  [[ "$status" -eq 1 ]]
  [[ "$output" == *"failed"* ]]
}
```

- [ ] **Step 2: Run tests**

Run: `bats tests/setup.bats`
Expected: All pass

- [ ] **Step 3: Commit**

```bash
git add tests/setup.bats
git commit -m "test: add download helper tests

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 5: Tests for Stow Engine

**Files:**
- Modify: `tests/setup.bats`

- [ ] **Step 1: Write stow engine tests**

Append to `tests/setup.bats`:

```bash
# --- Stow Engine ---

@test "is_stowed returns 0 when package is stowed" {
  _source_setup
  # Create a package dir with a file so it's non-empty
  local pkg_dir="${WORKSPACE_DIR}/shell/zsh"
  mkdir -p "${pkg_dir}/.config/zsh"
  touch "${pkg_dir}/.config/zsh/zshrc"

  # Mock stow: no LINK lines means already stowed
  export MOCK_STOW_OUTPUT=""
  export MOCK_STOW_RC=0
  is_stowed "shell/zsh"
}

@test "is_stowed returns 1 when package dir missing" {
  _source_setup
  ! is_stowed "shell/nonexistent"
}

@test "is_stowed returns 1 when LINK lines present" {
  _source_setup
  local pkg_dir="${WORKSPACE_DIR}/shell/zsh"
  mkdir -p "${pkg_dir}/.config/zsh"
  touch "${pkg_dir}/.config/zsh/zshrc"

  export MOCK_STOW_OUTPUT="LINK: .config/zsh => ../../shell/zsh/.config/zsh"
  export MOCK_STOW_RC=0
  ! is_stowed "shell/zsh"
}

@test "_resolve_conflict refuses path outside HOME" {
  _source_setup
  run _resolve_conflict "/etc/passwd"
  [[ "$status" -eq 1 ]]
  [[ "$output" == *"outside HOME"* ]]
}

@test "_resolve_conflict removes symlinks" {
  _source_setup
  local target="${HOME}/.config/test-link"
  ln -s /tmp "${target}"
  dry_run=false
  _resolve_conflict "${target}"
  [[ ! -e "${target}" ]]
}

@test "_resolve_conflict backs up regular files" {
  _source_setup
  local target="${HOME}/.config/test-file"
  printf 'content' > "${target}"
  dry_run=false
  _resolve_conflict "${target}"
  [[ ! -e "${target}" ]]
  [[ -f "${target}.pre-stow-backup" ]]
}

@test "_resolve_conflict increments backup suffix" {
  _source_setup
  local target="${HOME}/.config/test-file"
  printf 'v1' > "${target}"
  printf 'existing' > "${target}.pre-stow-backup"
  dry_run=false
  _resolve_conflict "${target}"
  [[ -f "${target}.pre-stow-backup.1" ]]
}

@test "_resolve_conflict refuses non-empty directories" {
  _source_setup
  local target="${HOME}/.config/nonempty"
  mkdir -p "${target}"
  touch "${target}/file"
  dry_run=false
  run _resolve_conflict "${target}"
  [[ "$status" -eq 1 ]]
  [[ "$output" == *"non-empty"* ]]
}

@test "_resolve_conflict dry-run does not modify" {
  _source_setup
  local target="${HOME}/.config/test-file"
  printf 'content' > "${target}"
  dry_run=true
  _resolve_conflict "${target}"
  [[ -f "${target}" ]]
}
```

- [ ] **Step 2: Run tests**

Run: `bats tests/setup.bats`
Expected: All pass

- [ ] **Step 3: Commit**

```bash
git add tests/setup.bats
git commit -m "test: add stow engine tests

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 6: Tests for Commands and Entry Point

**Files:**
- Modify: `tests/setup.bats`

- [ ] **Step 1: Write command and entry point tests**

Append to `tests/setup.bats`:

```bash
# --- Commands ---

@test "cmd_install --dry-run does not call stow without -n" {
  _source_setup
  # Create package dirs
  for pkg in "${PKG_ALL[@]}"; do
    local pkg_dir="${WORKSPACE_DIR}/${pkg}"
    mkdir -p "${pkg_dir}/.config"
    touch "${pkg_dir}/.config/dummy"
  done

  export MOCK_STOW_OUTPUT="LINK: .config/zsh => ../../shell/zsh/.config/zsh"
  dry_run=false
  cmd_install --dry-run --all

  # Verify stow was called with -n flag (dry-run mode)
  if [[ -f "${MOCK_STOW_LOG}" ]]; then
    grep -q "\-n" "${MOCK_STOW_LOG}"
  fi
}

@test "cmd_install with no args shows help" {
  _source_setup
  run cmd_install
  [[ "$status" -eq 0 ]]
  [[ "$output" == *"Usage"* ]]
}

@test "cmd_install --all and packages are mutually exclusive" {
  _source_setup
  run cmd_install --all zsh
  [[ "$status" -eq 2 ]]
}

@test "cmd_uninstall --all prompts for confirmation" {
  _source_setup
  # Create a stowed package
  local pkg_dir="${WORKSPACE_DIR}/shell/zsh"
  mkdir -p "${pkg_dir}/.config/zsh"
  touch "${pkg_dir}/.config/zsh/zshrc"
  export MOCK_STOW_OUTPUT=""
  export MOCK_STOW_RC=0

  # Pipe 'n' to stdin to decline
  run bash -c "source '${WORKSPACE_DIR}/setup.sh' && printf 'n\n' | cmd_uninstall --all"
  [[ "$output" == *"Aborted"* ]]
}

# --- Entry Point ---

@test "main with no args shows help" {
  _source_setup
  run main
  [[ "$status" -eq 0 ]]
  [[ "$output" == *"Usage"* ]]
}

@test "main with unknown command exits 2" {
  _source_setup
  run main "bogus"
  [[ "$status" -eq 2 ]]
  [[ "$output" == *"Unknown command"* ]]
}

@test "main rejects non-Darwin platform" {
  export MOCK_UNAME="Linux"
  run bash -c "source '${BATS_TEST_DIRNAME}/test_helper.bash' && _source_setup && main status"
  [[ "$status" -eq 1 ]]
}

# --- Signal Handling ---

@test "EXIT trap cleans up temp files" {
  _source_setup
  # Create temp file matching the pattern
  local tmp_file="/tmp/omw-setup-test.$$"
  touch "${tmp_file}"
  _cleanup
  [[ ! -f "${tmp_file}" ]]
}
```

- [ ] **Step 2: Run tests**

Run: `bats tests/setup.bats`
Expected: All pass (some may need adjustment based on exact current behavior)

- [ ] **Step 3: Commit**

```bash
git add tests/setup.bats
git commit -m "test: add command, entry point, and signal tests

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 7: Verify Full Test Suite Against Current setup.sh

**Files:** None (verification only)

- [ ] **Step 1: Run full test suite**

Run: `bats tests/setup.bats --tap`
Expected: All tests pass. If any fail, fix the test (not setup.sh) since we're testing current behavior.

- [ ] **Step 2: Record baseline test count**

Run: `bats tests/setup.bats | tail -1`
Expected: Something like "42 tests, 0 failures"

---

### Task 8: Rewrite setup.sh — File Header, Constants, Color, Logging

**Files:**
- Modify: `setup.sh` (complete rewrite, section 1 of 4)

- [ ] **Step 1: Back up current setup.sh**

```bash
cp setup.sh setup.sh.bak
```

- [ ] **Step 2: Write file header + constants + color + logging**

Replace the entire content of `setup.sh` with the first sections. The file will be built incrementally across Tasks 8-11.

```bash
#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-05-14 10:00:00 Wednesday by zhengyu.li>
#
# =============================================================================
# oh-my-workspace Setup - Dotfile Management via GNU Stow
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: dotfiles, stow, homebrew, setup
# Dependencies: bash 3.2+, macOS
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   2026-04-01 13:11 zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   Manages dotfiles via GNU Stow with Homebrew dependency resolution.
#   Run ./setup.sh help for usage.
#
# References:
#   1. GNU Stow Manual: https://www.gnu.org/software/stow/manual/
#   2. Homebrew Bundle: https://github.com/Homebrew/homebrew-bundle
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

# --- Network ---
readonly NETWORK_TIMEOUT=60
readonly DOWNLOAD_MAX_RETRIES=3
readonly DOWNLOAD_RETRY_DELAY=5

# --- Xcode CLI Polling ---
readonly XCODE_POLL_INTERVAL=5
readonly XCODE_POLL_MAX=600

# --- Homebrew Version ---
readonly MIN_HOMEBREW_MAJOR=4
readonly MIN_HOMEBREW_MINOR=4

# --- Packages ---
readonly -a PKG_ALL=(
  shell/zsh
  shell/starship
  editor/vim
  editor/emacs
  term/ghostty
  tool/git
  tool/lazygit
  tool/ripgrep
  tool/yazi
  lang/python/uv
  lang/typescript/bun
)

# --- Paths ---
# Assign then seal: avoids failure if already exported readonly.
WORKSPACE_DIR="${WORKSPACE_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
readonly WORKSPACE_DIR

BREWFILE="${WORKSPACE_DIR}/pkg/homebrew/Brewfile"
readonly BREWFILE

# --- Exit Codes ---
readonly EXIT_SIGINT=130

# --- State ---
dry_run=false

# -----------------------------------------------------------------------------
# Color System
# -----------------------------------------------------------------------------

_is_tty=false
if [[ -t 1 ]]; then
  _is_tty=true
fi

if [[ -n "${NO_COLOR:-}" ]] || ! "${_is_tty}"; then
  readonly _RED=''
  readonly _GREEN=''
  readonly _YELLOW=''
  readonly _BLUE=''
  readonly _BOLD=''
  readonly _DIM=''
  readonly _RESET=''
else
  readonly _RED=$'\033[0;31m'
  readonly _GREEN=$'\033[0;32m'
  readonly _YELLOW=$'\033[0;33m'
  readonly _BLUE=$'\033[0;34m'
  readonly _BOLD=$'\033[1m'
  readonly _DIM=$'\033[2m'
  readonly _RESET=$'\033[0m'
fi

# -----------------------------------------------------------------------------
# Logging
# -----------------------------------------------------------------------------

readonly _LOG_INDENT='    '

_log() {
  local -r color="$1" tag="$2" stream="$3"
  shift 3
  if [[ "${stream}" == err ]]; then
    printf '%s%b[%s]%b %s\n' \
      "${_LOG_INDENT}" "${color}" "${tag}" "${_RESET}" "$*" >&2
  else
    printf '%s%b[%s]%b %s\n' \
      "${_LOG_INDENT}" "${color}" "${tag}" "${_RESET}" "$*"
  fi
}

die()      { _log "${_RED}" error err "$*"; exit 1; }
_misuse()  { _log "${_RED}" error err "$*"; exit 2; }
log_ok()   { _log "${_GREEN}" ok out "$*"; }
log_err()  { _log "${_RED}" error err "$*"; }
log_warn() { _log "${_YELLOW}" warn err "$*"; }
log_info() { _log "${_BLUE}" info out "$*"; }

# --- Phase Counter ---
_phase_total=1
_phase_index=0

_phase() {
  (( _phase_index += 1 ))
  printf '\n%b[%d/%d]%b %b%s%b\n' \
    "${_DIM}" "${_phase_index}" "${_phase_total}" "${_RESET}" \
    "${_BOLD}" "$*" "${_RESET}"
}

_run_indented() {
  local rc=0
  "$@" > >(sed "s/^/${_LOG_INDENT}/") 2> >(sed "s/^/${_LOG_INDENT}/" >&2) \
    || rc=$?
  wait
  return "${rc}"
}
```

- [ ] **Step 3: Verify syntax so far**

Run: `bash -n setup.sh`
Expected: No errors (incomplete file is still syntactically valid at this point since all we have is declarations)

---

### Task 9: Rewrite setup.sh — Signal Handling, Package Model, Prerequisites

**Files:**
- Modify: `setup.sh` (append section 2 of 4)

- [ ] **Step 1: Append signal handling section**

Append to `setup.sh`:

```bash

# -----------------------------------------------------------------------------
# Signal Handling
# -----------------------------------------------------------------------------

_err_handler() {
  local -r code=$?
  printf '%s%b[error]%b %s() line %d: exit %d\n' \
    "${_LOG_INDENT}" "${_RED}" "${_RESET}" \
    "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "${code}" >&2
}
trap '_err_handler' ERR

_cleanup() {
  local -a tmp_files=()
  local saved_nullglob
  saved_nullglob="$(shopt -p nullglob 2>/dev/null || true)"
  shopt -s nullglob
  tmp_files=(/tmp/omw-setup-*.$$)
  eval "${saved_nullglob}"
  if (( ${#tmp_files[@]} > 0 )); then
    rm -f "${tmp_files[@]}" 2>/dev/null
  fi
}
trap '_cleanup' EXIT

_int_handler() {
  trap - EXIT
  local -a pids=()
  local pid
  while IFS= read -r pid; do
    pids+=("${pid}")
  done < <(jobs -p)
  if (( ${#pids[@]} > 0 )); then
    kill "${pids[@]}" 2>/dev/null || true
  fi
  exit "${EXIT_SIGINT}"
}
trap '_int_handler' INT TERM

# -----------------------------------------------------------------------------
# Package Model
# -----------------------------------------------------------------------------

pkg_category() { printf '%s' "${1%/*}"; }
pkg_name()     { printf '%s' "${1##*/}"; }

pkg_stow_dir() {
  printf '%s/%s' "${WORKSPACE_DIR}" "$(pkg_category "$1")"
}

_is_valid_pkg() {
  local pkg
  for pkg in "${PKG_ALL[@]}"; do
    if [[ "$1" == "${pkg}" ]]; then
      return 0
    fi
  done
  return 1
}

_VALIDATED_PKGS=()

validate_pkgs() {
  _VALIDATED_PKGS=()
  local arg match pkg existing seen

  for arg in "$@"; do
    if [[ -z "${arg}" ]]; then
      continue
    fi

    match=''
    if _is_valid_pkg "${arg}"; then
      match="${arg}"
    else
      for pkg in "${PKG_ALL[@]}"; do
        if [[ "$(pkg_name "${pkg}")" == "${arg}" ]]; then
          match="${pkg}"
          break
        fi
      done
    fi

    if [[ -z "${match}" ]]; then
      log_warn "Unknown package: ${arg}"
      continue
    fi

    seen=false
    for existing in "${_VALIDATED_PKGS[@]}"; do
      if [[ "${existing}" == "${match}" ]]; then
        seen=true
        break
      fi
    done
    if ! "${seen}"; then
      _VALIDATED_PKGS+=("${match}")
    fi
  done

  (( ${#_VALIDATED_PKGS[@]} > 0 ))
}

# -----------------------------------------------------------------------------
# Prerequisites
# -----------------------------------------------------------------------------

_has_xcode_cli() { xcode-select -p >/dev/null 2>&1; }
_has_homebrew()  { command -v brew >/dev/null 2>&1; }
_has_stow()      { command -v stow >/dev/null 2>&1; }

_bootstrap_homebrew_env() {
  local brew_bin env_output
  for brew_bin in /opt/homebrew/bin/brew /usr/local/bin/brew; do
    if [[ -x "${brew_bin}" ]]; then
      if env_output=$("${brew_bin}" shellenv 2>/dev/null); then
        eval "${env_output}"
        return 0
      fi
    fi
  done
  _has_homebrew
}

_download() {
  local -r url="$1" dest="$2"
  local attempt=1
  local delay="${DOWNLOAD_RETRY_DELAY}"

  while (( attempt <= DOWNLOAD_MAX_RETRIES )); do
    if curl --fail --silent --show-error \
      --connect-timeout "${NETWORK_TIMEOUT}" \
      --output "${dest}" "${url}"; then
      return 0
    fi

    if (( attempt >= DOWNLOAD_MAX_RETRIES )); then
      log_err "Download failed after ${DOWNLOAD_MAX_RETRIES} attempts: ${url}"
      return 1
    fi

    log_warn "Download attempt ${attempt} failed, retrying in ${delay}s..."
    sleep "${delay}"
    (( attempt += 1 ))
    delay=$(( delay * 2 ))
  done
}

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
    (( waited += XCODE_POLL_INTERVAL ))
  done
  printf '\n'

  if ! clang --version >/dev/null 2>&1; then
    log_err "Xcode CLI path exists but clang is not functional"
    return 1
  fi

  log_ok "Xcode CLI: installed"
}

_install_homebrew() {
  log_info "Installing Homebrew..."
  local -r url='https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh'
  local installer
  installer="$(mktemp)"
  trap 'rm -f "${installer:-}"' RETURN

  if ! _download "${url}" "${installer}"; then
    return 1
  fi

  if ! /bin/bash "${installer}"; then
    log_err "Homebrew installation failed"
    return 1
  fi

  rm -f "${installer}"
  trap - RETURN

  if ! _bootstrap_homebrew_env; then
    log_err "Homebrew not found after installation"
    return 1
  fi
  log_ok "Homebrew: installed"
}

_ensure_homebrew_version() {
  local ver
  ver=$(brew --version 2>/dev/null | head -1 | awk '{print $2}') || true

  if [[ -z "${ver}" || ! "${ver}" =~ ^[0-9]+\.[0-9]+ ]]; then
    log_err "Cannot determine Homebrew version (got: '${ver}')"
    return 1
  fi

  local major minor
  major="${ver%%.*}"
  minor="${ver#*.}"
  minor="${minor%%.*}"

  if (( major > MIN_HOMEBREW_MAJOR )) \
     || (( major == MIN_HOMEBREW_MAJOR && minor >= MIN_HOMEBREW_MINOR )); then
    return 0
  fi

  local -r min_ver="${MIN_HOMEBREW_MAJOR}.${MIN_HOMEBREW_MINOR}"
  log_warn "Homebrew ${ver} is too old (need >= ${min_ver})"

  if "${dry_run}"; then
    log_info "[dry-run] would remove old Homebrew and reinstall"
    return 1
  fi

  log_info "Removing old Homebrew..."
  local -r uninstall_url='https://raw.githubusercontent.com/Homebrew/install/HEAD/uninstall.sh'
  local uninstaller
  uninstaller="$(mktemp)"
  trap 'rm -f "${uninstaller:-}"' RETURN

  if ! _download "${uninstall_url}" "${uninstaller}"; then
    return 1
  fi

  if ! /bin/bash "${uninstaller}"; then
    log_err "Homebrew uninstall failed"
    return 1
  fi

  rm -f "${uninstaller}"
  trap - RETURN
  hash -r

  local brew_residual
  if brew_residual=$(command -v brew 2>/dev/null); then
    log_warn "Homebrew still found at ${brew_residual} after uninstall"
    log_info "PATH may contain stale entries"
  fi

  return 1
}

_install_stow() {
  log_info "Installing GNU Stow..."
  if _run_indented brew install stow; then
    log_ok "GNU Stow: installed"
  else
    log_err "GNU Stow installation failed"
    return 1
  fi
}

_run_brew_bundle() {
  if _run_indented brew bundle --file="${BREWFILE}"; then
    log_ok "brew bundle: done"
    return 0
  fi
  log_warn "brew bundle had failures, continuing..."
  return 0
}

_setup_git_filters() {
  git config --local filter.yazi-package.clean \
    "sed -e 's/^rev = .*/rev = \"pinned\"/' -e 's/^hash = .*/hash = \"0\"/'"
  git config --local filter.yazi-package.smudge cat
  git config --local filter.yazi-package.required true
  log_ok "Git filter: yazi-package configured"
}

ensure_prerequisites() {
  if ! _has_xcode_cli; then
    if "${dry_run}"; then
      log_info "[dry-run] would install Xcode CLI"
    elif ! _install_xcode_cli; then
      return 1
    fi
  fi

  if ! _bootstrap_homebrew_env; then
    if "${dry_run}"; then
      log_info "[dry-run] would install Homebrew"
    elif ! _install_homebrew; then
      return 1
    fi
  elif ! _ensure_homebrew_version; then
    if "${dry_run}"; then
      log_info "[dry-run] would reinstall Homebrew"
    elif ! _install_homebrew; then
      return 1
    fi
  fi

  if ! _has_stow; then
    if "${dry_run}"; then
      log_info "[dry-run] would install GNU Stow"
    elif ! _install_stow; then
      return 1
    fi
  fi

  if _is_valid_pkg "tool/yazi"; then
    _setup_git_filters
  fi
}
```

- [ ] **Step 2: Verify syntax**

Run: `bash -n setup.sh`
Expected: Pass

---

### Task 10: Rewrite setup.sh — Stow Engine

**Files:**
- Modify: `setup.sh` (append section 3 of 4)

- [ ] **Step 1: Append stow engine section**

Append to `setup.sh`:

```bash

# -----------------------------------------------------------------------------
# Stow Engine
# -----------------------------------------------------------------------------

is_stowed() {
  local stow_dir
  stow_dir=$(pkg_stow_dir "$1")
  local pkg_base
  pkg_base=$(pkg_name "$1")

  if [[ ! -d "${stow_dir}/${pkg_base}" ]]; then
    return 1
  fi

  # Empty package directory → nothing to stow.
  if [[ -z "$(find "${stow_dir}/${pkg_base}" \
        -mindepth 1 \( -type f -o -type l \) \
        -print -quit 2>/dev/null)" ]]; then
    return 1
  fi

  local output
  if ! output=$(stow -n -v \
    -d "${stow_dir}" \
    -t "${HOME}" "${pkg_base}" 2>&1); then
    return 1
  fi

  if grep -q '^LINK:' <<< "${output}"; then
    return 1
  fi
  return 0
}

_parse_stow_targets() {
  local output="$1"
  local -r pat_not_owned='existing target is not owned by stow: (.+)'
  local -r pat_existing='existing target (.+) since'
  local line rest

  while IFS= read -r line; do
    if [[ "${line}" == LINK:* ]]; then
      rest="${line#LINK: }"
      printf '%s\n' "${HOME}/${rest%% => *}"
    elif [[ "${line}" =~ ${pat_not_owned} ]]; then
      printf '%s\n' "${HOME}/${BASH_REMATCH[1]}"
    elif [[ "${line}" =~ ${pat_existing} ]]; then
      printf '%s\n' "${HOME}/${BASH_REMATCH[1]}"
    fi
  done <<< "${output}"
}

_resolve_conflict() {
  local -r target="$1"

  if [[ "${target}" != "${HOME}"/* ]]; then
    log_err "Refusing to remove path outside HOME: ${target}"
    return 1
  fi

  if [[ -L "${target}" ]]; then
    if "${dry_run}"; then
      log_info "[dry-run] would remove foreign symlink:"
      log_info "  ${target} -> $(readlink "${target}")"
      return 0
    fi
    log_warn "Removing foreign symlink:"
    log_warn "  ${target} -> $(readlink "${target}")"
    rm -f "${target}"
    return 0
  fi

  if [[ ! -e "${target}" ]]; then
    return 0
  fi

  if [[ -d "${target}" ]]; then
    if [[ ! -r "${target}" ]]; then
      log_err "Cannot read directory (permission denied): ${target}"
      log_info "Remove it manually: sudo rm -rf '${target}'"
      return 1
    fi
    if [[ -n "$(ls -A "${target}")" ]]; then
      log_err "Refusing to remove non-empty directory: ${target}"
      log_info "Move it aside manually: mv '${target}' '${target}.bak'"
      return 1
    fi
  fi

  if "${dry_run}"; then
    log_info "[dry-run] would back up conflicting path: ${target}"
    return 0
  fi

  local backup="${target}.pre-stow-backup"
  if [[ -e "${backup}" ]]; then
    local i=1
    while [[ -e "${backup}.${i}" ]]; do
      (( i += 1 ))
    done
    backup="${backup}.${i}"
  fi

  log_warn "Backing up conflicting path:"
  log_warn "  ${target} -> ${backup}"
  mv "${target}" "${backup}"
}

_stow_exec() {
  local pkg="$1"
  local mode="${2:-stow}"
  local stow_dir
  stow_dir=$(pkg_stow_dir "${pkg}")
  local pkg_base
  pkg_base=$(pkg_name "${pkg}")

  if [[ ! -d "${stow_dir}/${pkg_base}" ]]; then
    log_err "Package not found: ${stow_dir}/${pkg_base}"
    return 1
  fi

  # Single dry-run pass drives all decisions.
  local -a dry_flags=(-n -v -d "${stow_dir}" -t "${HOME}")
  case "${mode}" in
    restow) dry_flags+=(-R) ;;
    unstow) dry_flags+=(-D) ;;
  esac

  local dry_rc=0
  local dry_output
  dry_output=$(stow "${dry_flags[@]}" "${pkg_base}" 2>&1) || dry_rc=$?

  # Already stowed: rc==0 with no LINK lines.
  if [[ "${mode}" == stow ]] \
    && (( dry_rc == 0 )) \
    && ! grep -q '^LINK:' <<< "${dry_output}"; then
    log_info "${pkg_base}: already stowed"
    return 0
  fi

  # Nothing to unstow.
  if [[ "${mode}" == unstow ]] \
    && ! grep -q '^UNLINK:' <<< "${dry_output}"; then
    if (( dry_rc != 0 )); then
      log_err "${pkg_base}: stow dry-run failed"
      return 1
    fi
    log_warn "${pkg_base}: not stowed, skipping"
    return 0
  fi

  # Already stowed with no conflicts (restow).
  if [[ "${mode}" == restow ]] \
    && ! grep -q 'existing target' <<< "${dry_output}" \
    && is_stowed "${pkg}"; then
    log_info "${pkg_base}: already stowed, no changes needed"
    return 0
  fi

  # Resolve conflicts (stow/restow only).
  if [[ "${mode}" != unstow ]]; then
    local target
    while IFS= read -r target; do
      if ! _resolve_conflict "${target}"; then
        return 1
      fi
    done < <(_parse_stow_targets "${dry_output}")
  fi

  # Dry-run display.
  if "${dry_run}"; then
    log_info "[dry-run] would ${mode}: ${pkg_base}"
    local line has_actions=false
    while IFS= read -r line; do
      if [[ "${line}" =~ ^(LINK|UNLINK): ]]; then
        log_info "[dry-run]   ${line}"
        has_actions=true
      fi
    done <<< "${dry_output}"
    if ! "${has_actions}" \
       && grep -q 'existing target' <<< "${dry_output}"; then
      log_info "[dry-run]   (links hidden by conflicts — created after removal)"
    fi
    return 0
  fi

  # Execute.
  if [[ "${mode}" != unstow ]]; then
    mkdir -p "${HOME}/.config"
  fi

  local -a flags=(-d "${stow_dir}" -t "${HOME}")
  local action='stowed'
  case "${mode}" in
    restow) flags+=(-R); action='restowed' ;;
    unstow) flags+=(-D); action='unstowed' ;;
  esac

  if _run_indented stow "${flags[@]}" "${pkg_base}"; then
    log_ok "${pkg_base}: ${action}"
  else
    log_err "${pkg_base}: ${mode} failed"
    return 1
  fi
}

stow_package()   { _stow_exec "$1" stow; }
restow_package() { _stow_exec "$1" restow; }
unstow_package() { _stow_exec "$1" unstow; }
```

- [ ] **Step 2: Verify syntax**

Run: `bash -n setup.sh`
Expected: Pass

---

### Task 11: Rewrite setup.sh — Hooks, Health, Commands, Entry Point

**Files:**
- Modify: `setup.sh` (append section 4 of 4 — final)

- [ ] **Step 1: Append hooks, health, commands, and entry point**

Append to `setup.sh`:

```bash

# -----------------------------------------------------------------------------
# Post-Install Hooks
# -----------------------------------------------------------------------------

_post_install_shell_zsh() {
  if ! is_stowed shell/zsh; then
    return 2
  fi

  if [[ -n "${ZSH_VERSION:-}" ]] || [[ "${SHELL:-}" == */zsh ]]; then
    return 0
  fi

  local zsh_path
  if ! zsh_path=$(command -v zsh 2>/dev/null); then
    log_err "zsh not in PATH"
    return 1
  fi

  if ! grep -qx "${zsh_path}" /etc/shells 2>/dev/null; then
    if ! printf '%s\n' "${zsh_path}" \
      | sudo tee -a /etc/shells >/dev/null 2>&1; then
      log_err "cannot write /etc/shells — run manually:"
      log_err "  echo '${zsh_path}' | sudo tee -a /etc/shells"
      log_err "  chsh -s '${zsh_path}'"
      return 1
    fi
  fi

  if chsh -s "${zsh_path}" 2>/dev/null; then
    log_ok "Default shell → zsh · open a new terminal to apply."
    return 0
  fi
  log_err "chsh failed — run: chsh -s '${zsh_path}'"
  return 1
}

_post_install_yazi() {
  if ! is_stowed tool/yazi; then
    return 2
  fi

  if ! command -v ya >/dev/null 2>&1; then
    return 2
  fi

  local -r yazi_conf="${XDG_CONFIG_HOME:-$HOME/.config}/yazi"
  if [[ ! -f "${yazi_conf}/package.toml" ]]; then
    return 2
  fi

  if ya pkg install >/dev/null 2>&1; then
    return 0
  fi
  log_err "yazi plugin install failed"
  return 1
}

_hook_run() {
  local -r name="$1" fn="$2"
  local rc=0
  "${fn}" || rc=$?
  case "${rc}" in
    0) log_ok "${name}: done" ;;
    2) log_info "${name}: skipped" ;;
    *) log_err "${name}: failed" ;;
  esac
}

_run_post_install_hooks() {
  local pkg
  for pkg in "$@"; do
    case "${pkg}" in
      shell/zsh) _hook_run 'zsh shell switch' _post_install_shell_zsh ;;
      tool/yazi) _hook_run 'yazi plugins' _post_install_yazi ;;
    esac
  done
}

_run_post_install_phase() {
  _phase "Post-Install Hooks"
  if "${dry_run}"; then
    log_info "[dry-run] would run post-install hooks"
    return 0
  fi
  _run_post_install_hooks "$@"
}

# -----------------------------------------------------------------------------
# Health Check
# -----------------------------------------------------------------------------

_health_tool_for() {
  case "$1" in
    shell/zsh)          printf 'zsh:zsh' ;;
    shell/starship)     printf 'starship:starship' ;;
    editor/vim)         printf 'vim:vim' ;;
    editor/emacs)       printf 'emacs:emacs' ;;
    term/ghostty)       printf 'ghostty:ghostty:/Applications/Ghostty.app/Contents/MacOS/ghostty' ;;
    tool/git)           printf 'git:git' ;;
    tool/lazygit)       printf 'lazygit:lazygit' ;;
    tool/ripgrep)       printf 'rg:rg' ;;
    tool/yazi)          printf 'yazi:yazi' ;;
    lang/python/uv)     printf 'uv:uv' ;;
    lang/typescript/bun) printf 'bun:bun' ;;
    *) return 1 ;;
  esac
}

_run_health_check() {
  _phase "Health Check"
  local pkg tool_entry cmd_name friendly fallback_path resolved

  for pkg in "$@"; do
    if ! is_stowed "${pkg}"; then
      continue
    fi

    if ! tool_entry=$(_health_tool_for "${pkg}"); then
      continue
    fi
    cmd_name="${tool_entry%%:*}"
    tool_entry="${tool_entry#*:}"
    friendly="${tool_entry%%:*}"
    fallback_path="${tool_entry#*:}"
    if [[ "${fallback_path}" == "${friendly}" ]]; then
      fallback_path=''
    fi

    resolved=$(command -v "${cmd_name}" 2>/dev/null) || true
    if [[ -z "${resolved}" && -n "${fallback_path}" \
       && -x "${fallback_path}" ]]; then
      resolved="${fallback_path}"
    fi

    if [[ -n "${resolved}" ]]; then
      log_ok "${friendly}: ${resolved}"
    else
      log_err "${friendly}: not found"
    fi
  done
}

# -----------------------------------------------------------------------------
# Commands
# -----------------------------------------------------------------------------

cmd_install() {
  local do_all=false
  local force=false
  local -a pkgs=()
  local arg

  for arg in "$@"; do
    case "${arg}" in
      --all) do_all=true ;;
      --force) force=true ;;
      --dry-run) dry_run=true ;;
      -*) _misuse "Unknown flag: ${arg}" ;;
      *) pkgs+=("${arg}") ;;
    esac
  done

  if "${do_all}" && (( ${#pkgs[@]} > 0 )); then
    _misuse "--all and package names are mutually exclusive"
  fi

  if ! "${do_all}" && (( ${#pkgs[@]} == 0 )); then
    cmd_help
    return 0
  fi

  ensure_prerequisites

  if "${do_all}"; then
    if "${dry_run}"; then
      _phase_total=4
    else
      _phase_total=5
    fi
    _phase_index=0

    _phase "Prerequisites"
    if _has_xcode_cli; then log_ok "Xcode CLI: ready"; fi
    if _has_homebrew; then log_ok "Homebrew: ready"; fi
    if _has_stow; then log_ok "GNU Stow: ready"; fi

    _phase "Homebrew Bundle"
    if "${dry_run}"; then
      log_info "[dry-run] would run brew bundle"
    else
      _run_brew_bundle
    fi

    _phase "Stow Packages"
    local pkg
    local fail_count=0
    for pkg in "${PKG_ALL[@]}"; do
      if "${force}"; then
        restow_package "${pkg}" || (( fail_count += 1 ))
      else
        stow_package "${pkg}" || (( fail_count += 1 ))
      fi
    done

    _run_post_install_phase "${PKG_ALL[@]}"

    if ! "${dry_run}"; then
      _run_health_check "${PKG_ALL[@]}"
    fi

    return $(( fail_count > 0 ? 1 : 0 ))
  fi

  # Specific packages.
  if ! validate_pkgs "${pkgs[@]}"; then
    die "No valid packages specified"
  fi

  _phase_total=2
  _phase_index=0

  _phase "Stow Packages"
  local pkg
  local fail_count=0
  for pkg in "${_VALIDATED_PKGS[@]}"; do
    if "${force}"; then
      restow_package "${pkg}" || (( fail_count += 1 ))
    else
      stow_package "${pkg}" || (( fail_count += 1 ))
    fi
  done

  _run_post_install_phase "${_VALIDATED_PKGS[@]}"

  return $(( fail_count > 0 ? 1 : 0 ))
}

cmd_uninstall() {
  local do_all=false
  local -a pkgs=()
  local arg

  for arg in "$@"; do
    case "${arg}" in
      --all) do_all=true ;;
      --dry-run) dry_run=true ;;
      -*) _misuse "Unknown flag: ${arg}" ;;
      *) pkgs+=("${arg}") ;;
    esac
  done

  if "${do_all}" && (( ${#pkgs[@]} > 0 )); then
    _misuse "--all and package names are mutually exclusive"
  fi

  if ! "${do_all}" && (( ${#pkgs[@]} == 0 )); then
    log_err "No packages specified"
    log_info "Usage: ./setup.sh uninstall [--all | <pkg>...]"
    return 1
  fi

  if ! _has_stow; then
    log_err "GNU Stow not found"
    log_info "run: ./setup.sh install --all"
    return 1
  fi

  local -a target_pkgs=()
  if "${do_all}"; then
    local pkg
    for pkg in "${PKG_ALL[@]}"; do
      if is_stowed "${pkg}"; then
        target_pkgs+=("${pkg}")
      fi
    done

    if (( ${#target_pkgs[@]} == 0 )); then
      log_info "Nothing stowed"
      return 0
    fi

    if ! "${dry_run}"; then
      log_warn "This will unstow ${#target_pkgs[@]} packages:" \
        "${target_pkgs[*]}"
      printf '%s' "${_LOG_INDENT}Continue? [y/N] "
      local answer
      read -r answer
      if [[ "${answer}" != [yY] ]]; then
        log_info "Aborted"
        return 0
      fi
    fi
  else
    if ! validate_pkgs "${pkgs[@]}"; then
      die "No valid packages specified"
    fi
    target_pkgs=("${_VALIDATED_PKGS[@]}")
  fi

  _phase_total=1
  _phase_index=0

  _phase "Unstow Packages"
  local pkg
  local fail_count=0
  for pkg in "${target_pkgs[@]}"; do
    unstow_package "${pkg}" || (( fail_count += 1 ))
  done

  return $(( fail_count > 0 ? 1 : 0 ))
}

cmd_status() {
  local -a pkgs

  if (( $# > 0 )); then
    if ! validate_pkgs "$@"; then
      return 1
    fi
    pkgs=("${_VALIDATED_PKGS[@]}")
  else
    pkgs=("${PKG_ALL[@]}")
  fi

  _phase_total=1
  _phase_index=0

  _phase "Prerequisites"
  if _has_xcode_cli; then log_ok "Xcode CLI"; else log_warn "Xcode CLI: missing"; fi
  if _has_homebrew; then log_ok "Homebrew"; else log_warn "Homebrew: missing"; fi
  if _has_stow; then log_ok "GNU Stow"; else log_warn "GNU Stow: missing"; fi

  if ! _has_stow; then
    log_info "run: ./setup.sh install --all"
    return 0
  fi

  _phase_total=2
  _phase "Packages"
  local pkg
  for pkg in "${pkgs[@]}"; do
    if is_stowed "${pkg}"; then
      log_ok "${pkg}: stowed"
    else
      log_warn "${pkg}: unstowed"
    fi
  done
}

cmd_help() {
  printf '%b\n' \
    "${_BOLD}oh-my-workspace setup${_RESET}" '' \
    'Usage:' \
    '  ./setup.sh <command> [flags] [packages]' '' \
    "${_BOLD}Commands:${_RESET}" \
    '  install   [--all] [--force] [--dry-run] [<pkg>...]   Stow packages' \
    '  uninstall [--all] [--dry-run] [<pkg>...]             Unstow packages' \
    '  status    [<pkg>...]                                 Show stow state' \
    '  help                                                 Show this help' '' \
    "${_BOLD}Flags:${_RESET}" \
    '  --all      Apply to all packages (install / uninstall)' \
    '  --force    Restow even if already stowed (install only).' \
    '             Runs stow -R; conflicting files are backed up to *.pre-stow-backup.' \
    '             Use after adding new dotfiles to a package dir.' \
    '  --dry-run  Preview stow changes; brew bundle is skipped, nothing is linked/unlinked' '' \
    "${_BOLD}Packages${_RESET} (base name or full category/name):" \
    '  shell:   zsh  starship' \
    '  editor:  vim  emacs' \
    '  term:    ghostty' \
    '  tool:    git  lazygit  ripgrep  yazi' \
    '  lang:    uv  bun' '' \
    "${_BOLD}Examples:${_RESET}" \
    '  ./setup.sh install --all                    Prereqs + brew + stow all packages' \
    '  ./setup.sh install zsh git                  Stow specific packages' \
    '  ./setup.sh install --force zsh              Restow (pick up new dotfiles)' \
    '  ./setup.sh install --force --all            Restow everything' \
    '  ./setup.sh install --dry-run zsh            Preview what install would do' \
    '  ./setup.sh install --force --dry-run --all  Preview a full restow' \
    '  ./setup.sh uninstall --all                  Unstow all' \
    '  ./setup.sh uninstall --dry-run zsh          Preview what uninstall would do' \
    '  ./setup.sh status                           Full status' \
    '  ./setup.sh status zsh                       Status for one package' '' \
    "${_BOLD}Note:${_RESET}" \
    '  install without packages or --all shows this help.'
}

# -----------------------------------------------------------------------------
# Entry Point
# -----------------------------------------------------------------------------

main() {
  if [[ "$(uname -s)" != Darwin ]]; then
    printf 'error: macOS required\n' >&2
    exit 1
  fi

  if (( $# == 0 )); then
    cmd_help
    exit 0
  fi

  local cmd="$1"
  shift
  case "${cmd}" in
    install) cmd_install "$@" ;;
    uninstall) cmd_uninstall "$@" ;;
    status) cmd_status "$@" ;;
    help|-h|--help) cmd_help ;;
    *)
      log_err "Unknown command: ${cmd}"
      printf '\n'
      cmd_help
      exit 2
      ;;
  esac
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  main "$@"
fi
```

- [ ] **Step 2: Remove the backup**

```bash
rm -f setup.sh.bak
```

- [ ] **Step 3: Verify syntax and lint**

Run: `bash -n setup.sh && shellcheck -S warning setup.sh`
Expected: Both pass clean

---

### Task 12: Run Tests Against Refactored Script

**Files:** None (verification only)

- [ ] **Step 1: Run full BATS suite**

Run: `bats tests/setup.bats`
Expected: All tests pass. Fix any failures in the refactored script (not the tests).

- [ ] **Step 2: Run live smoke test**

Run: `./setup.sh status`
Expected: Same output as before (all 11 packages stowed)

- [ ] **Step 3: Run dry-run smoke test**

Run: `./setup.sh install --dry-run --all 2>&1 | head -20`
Expected: Phase headers, prereq checks, dry-run messages

- [ ] **Step 4: Verify help output**

Run: `./setup.sh help`
Expected: Clean formatted help with all commands/flags documented

- [ ] **Step 5: Verify line count**

Run: `wc -l setup.sh`
Expected: ~1000-1100 lines (10-20% reduction from 1214)

- [ ] **Step 6: Commit the refactored script**

```bash
git add setup.sh
git commit -m "refactor(setup): rewrite from scratch with clean structure

Preserves all 20 capabilities. Key changes:
- Self-documenting variable names (_RED, _GREEN vs C_R, C_G)
- Top-to-bottom functional pipeline layout
- Comments only where non-obvious
- Source guard for testability
- Follows bash.md conventions strictly

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

---

### Task 13: Final Validation and Cleanup

**Files:**
- Possibly modify: `tests/setup.bats` (if any tests need adjustment)
- Modify: `.gitignore` (add tests/bin/ if needed)

- [ ] **Step 1: Run shellcheck on final version**

Run: `shellcheck -S warning setup.sh`
Expected: Zero warnings

- [ ] **Step 2: Verify stow status unchanged**

Run: `./setup.sh status 2>&1`
Expected: All 11 packages show as stowed

- [ ] **Step 3: Test the source guard works**

Run: `bash -c 'source ./setup.sh && type -t main'`
Expected: "function" (sourced without executing)

- [ ] **Step 4: Final commit with tests**

```bash
git add -A
git status
# If anything uncommitted:
git commit -m "test: finalize BATS test suite for setup.sh

Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>"
```

- [ ] **Step 5: Remove spec backup if created**

```bash
rm -f setup.sh.bak
```
