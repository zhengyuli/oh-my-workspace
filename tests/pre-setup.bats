#!/usr/bin/env bats
# pre-setup.bats — tests for ai-agent/claude/pre-setup.sh

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_source_pre_setup() {
  export NO_COLOR=1
  export PATH="${BATS_TEST_DIRNAME}/mocks/pre-setup:${BATS_TEST_DIRNAME}/mocks/setup:${PATH}"
  source "${BATS_TEST_DIRNAME}/../ai-agent/claude/pre-setup.sh"
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

@test "NO_COLOR also clears bold and dim" {
  _source_pre_setup
  [[ -z "$C_BOLD" ]]
  [[ -z "$C_DIM" ]]
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

@test "_phase prints step indicator" {
  _source_pre_setup
  _PHASE_TOTAL=5
  _PHASE_INDEX=0
  run _phase "Prerequisites"
  [[ "$output" == *"[1/5]"* ]]
  [[ "$output" == *"Prerequisites"* ]]
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

# ---------------------------------------------------------------------------
# _check_macos_version Edge Cases
# ---------------------------------------------------------------------------

@test "_check_macos_version: 3-segment 14.2.1 >= 13.0" {
  _source_pre_setup
  run _check_macos_version "14.2.1" "13.0"
  (( status == 0 ))
}

@test "_check_macos_version: 13.0.1 >= 13.0" {
  _source_pre_setup
  run _check_macos_version "13.0.1" "13.0"
  (( status == 0 ))
}

@test "_check_macos_version: 12.7.3 < 13.0" {
  _source_pre_setup
  run _check_macos_version "12.7.3" "13.0"
  (( status == 1 ))
}

@test "_check_macos_version: exact match 15.0 >= 15.0" {
  _source_pre_setup
  run _check_macos_version "15.0" "15.0"
  (( status == 0 ))
}

@test "_check_macos_version: empty min_version fails" {
  _source_pre_setup
  run _check_macos_version "14.0" ""
  (( status == 1 ))
}

@test "_check_macos_version: both empty fails" {
  _source_pre_setup
  run _check_macos_version "" ""
  (( status == 1 ))
}

# ---------------------------------------------------------------------------
# _check_prerequisites Edge Cases
# ---------------------------------------------------------------------------

@test "_check_prerequisites: non-Darwin platform fails" {
  _source_pre_setup
  export MOCK_UNAME="Linux"
  run _check_prerequisites
  (( status == 1 ))
  [[ "$output" == *"Not macOS"* ]]
}

@test "_check_prerequisites: old macOS fails" {
  _source_pre_setup
  export MOCK_MACOS_VERSION="11.0"
  run _check_prerequisites
  (( status == 1 ))
  [[ "$output" == *"need ${MIN_MACOS_VERSION}+"* ]]
}

@test "_check_prerequisites: network failure" {
  _source_pre_setup
  # Set high fail count: _check_cmd "curl" consumes one call
  # (via --version), then the actual network check gets another.
  echo "99" > "${MOCK_CURL_FAIL_FILE}"
  run _check_prerequisites
  (( status == 1 ))
  [[ "$output" == *"Cannot reach GitHub API"* ]]
}

# ---------------------------------------------------------------------------
# _install_claude Edge Cases
# ---------------------------------------------------------------------------

@test "_install_claude: installs when not present" {
  _source_pre_setup
  # Hide the mock claude from PATH so it looks "not installed"
  # Create a temp bin dir without claude
  local tmpbin="${BATS_TEST_TMPDIR}/nobin"
  mkdir -p "$tmpbin"
  # Only keep mocks that _install_claude needs (curl)
  cp "${BATS_TEST_DIRNAME}/mocks/setup/curl" "$tmpbin/"
  export PATH="$tmpbin:/usr/bin:/bin"
  run _install_claude
  # Should attempt install (curl piped to bash)
  [[ "$output" == *"Installing claude CLI"* ]]
}

# ---------------------------------------------------------------------------
# _configure_glm Edge Cases
# ---------------------------------------------------------------------------

@test "_configure_glm: skips when already configured" {
  _source_pre_setup
  echo '{"env":{"ANTHROPIC_BASE_URL":"x","ANTHROPIC_AUTH_TOKEN":"y"}}' \
    > "${HOME}/.claude/settings.json"
  export MOCK_JQ_KEY_EXISTS=0
  run _configure_glm
  (( status == 0 ))
  [[ "$output" == *"already configured"* ]]
}

@test "_configure_glm: bunx failure logs warning" {
  _source_pre_setup
  # No settings.json means credentials not present — ZAI will run
  rm -f "${HOME}/.claude/settings.json"
  export MOCK_JQ_KEY_EXISTS=1
  export MOCK_BUNX_RC=42
  run _configure_glm
  [[ "$output" == *"exited with code 42"* ]]
}

# ---------------------------------------------------------------------------
# _verify Edge Cases
# ---------------------------------------------------------------------------

@test "_verify: wrong model fails" {
  _source_pre_setup
  echo '{}' > "${HOME}/.claude/settings.json"
  export MOCK_JQ_RC=0
  export MOCK_JQ_VALUE="opus"
  run _verify
  (( status == 1 ))
  [[ "$output" == *"expected sonnet"* ]]
}

@test "_verify: null env var fails" {
  _source_pre_setup
  echo '{}' > "${HOME}/.claude/settings.json"
  export MOCK_JQ_RC=0
  export MOCK_JQ_VALUE="null"
  run _verify
  (( status == 1 ))
  [[ "$output" == *"missing or empty"* ]]
}

@test "_verify: empty env var fails" {
  _source_pre_setup
  echo '{}' > "${HOME}/.claude/settings.json"
  export MOCK_JQ_RC=0
  export MOCK_JQ_VALUE=""
  run _verify
  (( status == 1 ))
  [[ "$output" == *"missing or empty"* ]]
}

# ---------------------------------------------------------------------------
# _apply_post_fixes Edge Cases
# ---------------------------------------------------------------------------

@test "_apply_post_fixes: jq failure reports error" {
  _source_pre_setup
  echo '{}' > "${HOME}/.claude/settings.json"
  export MOCK_JQ_RC=1
  run _apply_post_fixes
  (( status == 1 ))
}

# ---------------------------------------------------------------------------
# _check_cmd Edge Cases
# ---------------------------------------------------------------------------

@test "_check_cmd: shows version when available" {
  _source_pre_setup
  run _check_cmd "claude"
  (( status == 0 ))
  [[ "$output" == *"claude"* ]]
}

# ---------------------------------------------------------------------------
# _err_handler
# ---------------------------------------------------------------------------

@test "_err_handler prints function and line info" {
  _source_pre_setup
  run _err_handler
  [[ "$output" == *"[error]"* ]]
  [[ "$output" == *"line"* ]]
  [[ "$output" == *"exit"* ]]
}
