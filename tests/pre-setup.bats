#!/usr/bin/env bats
# pre-setup.bats — tests for claude/pre-setup.sh

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_source_pre_setup() {
  export NO_COLOR=1
  export PATH="${BATS_TEST_DIRNAME}/pre-setup-bin:${BATS_TEST_DIRNAME}/bin:${PATH}"
  source "${BATS_TEST_DIRNAME}/../claude/pre-setup.sh"
}

setup() {
  export ORIG_HOME="${HOME}"
  export HOME="${BATS_TEST_TMPDIR}/home"
  mkdir -p "${HOME}/.claude"

  export ORIG_PATH="${PATH}"

  # Mock defaults
  export MOCK_UNAME="Darwin"
  export MOCK_MACOS_VERSION="14.0"
  export MOCK_JQ_LOG="${BATS_TEST_TMPDIR}/jq.log"
  export MOCK_JQ_RC=0
  export MOCK_JQ_KEY_EXISTS=0
  export MOCK_JQ_VALUE="null"
  export MOCK_BUNX_LOG="${BATS_TEST_TMPDIR}/bunx.log"
  export MOCK_BUNX_RC=0
  export MOCK_CLAUDE_RC=0
  export MOCK_CURL_LOG="${BATS_TEST_TMPDIR}/curl.log"
  export MOCK_CURL_FAIL_FILE="${BATS_TEST_TMPDIR}/curl_fail_count"
  echo "0" > "${MOCK_CURL_FAIL_FILE}"
}

teardown() {
  export HOME="${ORIG_HOME}"
  export PATH="${ORIG_PATH}"
}

# ---------------------------------------------------------------------------
# Color System
# ---------------------------------------------------------------------------

@test "NO_COLOR disables ANSI escapes" {
  _source_pre_setup
  [[ -z "$C_R" ]]
  [[ -z "$C_G" ]]
  [[ -z "$C_RESET" ]]
}

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------

@test "log_ok prints [ok] tag" {
  _source_pre_setup
  run log_ok "test message"
  [[ "$output" == *"[ok]"* ]]
  [[ "$output" == *"test message"* ]]
}

@test "log_err prints [error] tag to stderr" {
  _source_pre_setup
  run log_err "fail message"
  [[ "$output" == *"[error]"* ]]
  [[ "$output" == *"fail message"* ]]
}

@test "log_warn prints [warn] tag" {
  _source_pre_setup
  run log_warn "warning message"
  [[ "$output" == *"[warn]"* ]]
}

@test "log_info prints [info] tag" {
  _source_pre_setup
  run log_info "info message"
  [[ "$output" == *"[info]"* ]]
}

@test "_phase increments counter" {
  _source_pre_setup
  _PHASE_TOTAL=3
  _PHASE_INDEX=0
  _phase "Test Phase" >/dev/null
  (( _PHASE_INDEX == 1 ))
  _phase "Phase 2" >/dev/null
  (( _PHASE_INDEX == 2 ))
}

# ---------------------------------------------------------------------------
# _abort
# ---------------------------------------------------------------------------

@test "_abort prints warning and exits 1" {
  _source_pre_setup
  run _abort "something failed"
  (( status == 1 ))
  [[ "$output" == *"something failed"* ]]
}

# ---------------------------------------------------------------------------
# _check_cmd
# ---------------------------------------------------------------------------

@test "_check_cmd finds existing command" {
  _source_pre_setup
  run _check_cmd "bash"
  (( status == 0 ))
  [[ "$output" == *"[ok]"* ]]
  [[ "$output" == *"bash"* ]]
}

@test "_check_cmd fails for missing command" {
  _source_pre_setup
  run _check_cmd "__nonexistent_command_xyz__"
  (( status == 1 ))
  [[ "$output" == *"NOT INSTALLED"* ]]
}

@test "_check_cmd fails for empty input" {
  _source_pre_setup
  run _check_cmd ""
  (( status == 1 ))
}

# ---------------------------------------------------------------------------
# _check_macos_version
# ---------------------------------------------------------------------------

@test "_check_macos_version: 14.0 >= 13.0" {
  _source_pre_setup
  run _check_macos_version "14.0" "13.0"
  (( status == 0 ))
}

@test "_check_macos_version: 13.0 >= 13.0" {
  _source_pre_setup
  run _check_macos_version "13.0" "13.0"
  (( status == 0 ))
}

@test "_check_macos_version: 12.0 < 13.0" {
  _source_pre_setup
  run _check_macos_version "12.0" "13.0"
  (( status == 1 ))
}

@test "_check_macos_version: empty string fails" {
  _source_pre_setup
  run _check_macos_version "" "13.0"
  (( status == 1 ))
}

@test "_check_macos_version: non-numeric fails" {
  _source_pre_setup
  run _check_macos_version "abc" "13.0"
  (( status == 1 ))
}

# ---------------------------------------------------------------------------
# _install_claude
# ---------------------------------------------------------------------------

@test "_install_claude skips when already installed" {
  _source_pre_setup
  run _install_claude
  (( status == 0 ))
  [[ "$output" == *"already installed"* ]]
}

# ---------------------------------------------------------------------------
# _verify
# ---------------------------------------------------------------------------

@test "_verify fails when settings.json missing" {
  _source_pre_setup
  rm -f "${HOME}/.claude/settings.json"
  run _verify
  (( status == 1 ))
  [[ "$output" == *"not found"* ]]
}

@test "_verify fails when settings.json is invalid JSON" {
  _source_pre_setup
  echo "not json" > "${HOME}/.claude/settings.json"
  export MOCK_JQ_RC=1
  run _verify
  (( status == 1 ))
  [[ "$output" == *"not valid JSON"* ]]
}

@test "_verify succeeds with all required vars present" {
  _source_pre_setup
  echo '{}' > "${HOME}/.claude/settings.json"
  export MOCK_JQ_RC=0
  # Mock returns this for all jq -r calls: env vars need non-null,
  # model needs "sonnet[1m]" — use the model value since it satisfies both.
  export MOCK_JQ_VALUE="sonnet[1m]"
  run _verify
  (( status == 0 ))
}

# ---------------------------------------------------------------------------
# _apply_post_fixes
# ---------------------------------------------------------------------------

@test "_apply_post_fixes fails when settings.json missing" {
  _source_pre_setup
  rm -f "${HOME}/.claude/settings.json"
  run _apply_post_fixes
  (( status == 1 ))
  [[ "$output" == *"not found"* ]]
}

@test "_apply_post_fixes succeeds with valid settings" {
  _source_pre_setup
  echo '{"model":"old"}' > "${HOME}/.claude/settings.json"
  run _apply_post_fixes
  (( status == 0 ))
  [[ "$output" == *"[ok]"* ]]
}

# ---------------------------------------------------------------------------
# Source Guard
# ---------------------------------------------------------------------------

@test "sourcing does not run main" {
  _source_pre_setup
  # If main ran, _PHASE_INDEX would be > 0
  (( _PHASE_INDEX == 0 ))
}
