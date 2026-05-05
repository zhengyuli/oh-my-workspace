#!/usr/bin/env bash
# setup.bats — BATS test suite for setup.sh

load test-helper

# =============================================================================
# Package Model Tests
# =============================================================================

@test "pkg_category: shell/zsh → shell" {
  _source_setup
  run pkg_category "shell/zsh"
  [[ "$output" == "shell" ]]
}

@test "pkg_category: lang/python/uv → lang/python" {
  _source_setup
  run pkg_category "lang/python/uv"
  [[ "$output" == "lang/python" ]]
}

@test "pkg_name: shell/zsh → zsh" {
  _source_setup
  run pkg_name "shell/zsh"
  [[ "$output" == "zsh" ]]
}

@test "pkg_name: lang/python/uv → uv" {
  _source_setup
  run pkg_name "lang/python/uv"
  [[ "$output" == "uv" ]]
}

@test "pkg_stow_dir: shell/zsh → WORKSPACE_DIR/shell" {
  _source_setup
  run pkg_stow_dir "shell/zsh"
  [[ "$output" == "${WORKSPACE_DIR}/shell" ]]
}

@test "_is_valid_pkg: shell/zsh returns 0" {
  _source_setup
  run _is_valid_pkg "shell/zsh"
  [[ "$status" -eq 0 ]]
}

@test "_is_valid_pkg: unknown/pkg returns 1" {
  _source_setup
  run _is_valid_pkg "unknown/pkg"
  [[ "$status" -eq 1 ]]
}

@test "validate_pkgs: short name zsh resolves to shell/zsh" {
  _source_setup
  validate_pkgs "zsh"
  [[ "${_VALIDATED_PKGS[0]}" == "shell/zsh" ]]
}

@test "validate_pkgs: full path shell/zsh resolves correctly" {
  _source_setup
  validate_pkgs "shell/zsh"
  [[ "${_VALIDATED_PKGS[0]}" == "shell/zsh" ]]
}

@test "validate_pkgs: deduplicates zsh shell/zsh zsh → 1 entry" {
  _source_setup
  validate_pkgs "zsh" "shell/zsh" "zsh"
  [[ "${#_VALIDATED_PKGS[@]}" -eq 1 ]]
}

@test "validate_pkgs: nonexistent returns 1" {
  _source_setup
  run validate_pkgs "nonexistent"
  [[ "$status" -eq 1 ]]
}

@test "validate_pkgs: mixed valid and invalid keeps valid, skips unknown" {
  _source_setup
  validate_pkgs "zsh" "nonexistent"
  [[ "${#_VALIDATED_PKGS[@]}" -eq 1 ]]
  [[ "${_VALIDATED_PKGS[0]}" == "shell/zsh" ]]
}

# =============================================================================
# Color/Logging Tests
# =============================================================================

@test "NO_COLOR=1 makes color vars empty" {
  export NO_COLOR=1
  _source_setup
  [[ -z "${_RED}" ]]
  [[ -z "${_GREEN}" ]]
  [[ -z "${_YELLOW}" ]]
  [[ -z "${_BLUE}" ]]
  [[ -z "${_BOLD}" ]]
  [[ -z "${_DIM}" ]]
  [[ -z "${_RESET}" ]]
}

@test "die exits 1 with error tag and message" {
  _source_setup
  run die "something broke"
  [[ "$status" -eq 1 ]]
  [[ "$output" == *"[error]"* ]]
  [[ "$output" == *"something broke"* ]]
}

@test "_misuse exits 2 with error tag" {
  _source_setup
  run _misuse "bad usage"
  [[ "$status" -eq 2 ]]
  [[ "$output" == *"[error]"* ]]
}

@test "log_ok outputs ok tag and message" {
  _source_setup
  run log_ok "all good"
  [[ "$output" == *"[ok]"* ]]
  [[ "$output" == *"all good"* ]]
}

@test "log_warn outputs warn tag" {
  _source_setup
  run log_warn "careful"
  [[ "$output" == *"[warn]"* ]]
}

@test "log_info outputs info tag" {
  _source_setup
  run log_info "fyi"
  [[ "$output" == *"[info]"* ]]
}

@test "_phase outputs phase counter and title" {
  _source_setup
  _phase_total=3
  _phase_index=0
  run _phase "Test Phase"
  [[ "$output" == *"[1/3]"* ]]
  [[ "$output" == *"Test Phase"* ]]
}

# =============================================================================
# Download Tests
# =============================================================================

@test "_download succeeds and creates file" {
  _source_setup
  local dest="${BATS_TEST_TMPDIR}/downloaded_file"
  echo "0" > "${MOCK_CURL_FAIL_FILE}"
  run _download "http://example.com/file" "${dest}"
  [[ "$status" -eq 0 ]]
  [[ -f "${dest}" ]]
}

@test "_download retries on failure and succeeds" {
  _source_setup
  local dest="${BATS_TEST_TMPDIR}/downloaded_file"
  # Fail 2 times then succeed
  echo "2" > "${MOCK_CURL_FAIL_FILE}"
  run _download "http://example.com/file" "${dest}"
  [[ "$status" -eq 0 ]]
}

@test "_download fails after max retries" {
  _source_setup
  local dest="${BATS_TEST_TMPDIR}/downloaded_file"
  # Fail more times than max retries (3)
  echo "10" > "${MOCK_CURL_FAIL_FILE}"
  run _download "http://example.com/file" "${dest}"
  [[ "$status" -eq 1 ]]
  [[ "$output" == *"failed"* ]] || [[ "$output" == *"Download failed"* ]]
}

# =============================================================================
# Stow Engine Tests
# =============================================================================

@test "is_stowed: returns 0 when pkg is fully stowed" {
  _source_setup
  # Create the package directory with a file
  local stow_dir="${WORKSPACE_DIR}/shell"
  mkdir -p "${stow_dir}/zsh/.config/zsh"
  touch "${stow_dir}/zsh/.config/zsh/conf"
  # Mock stow outputs no LINK lines (package already stowed)
  export MOCK_STOW_OUTPUT=""
  run is_stowed "shell/zsh"
  [[ "$status" -eq 0 ]]
}

@test "is_stowed: returns 1 for nonexistent package dir" {
  _source_setup
  run is_stowed "shell/nonexistent"
  [[ "$status" -eq 1 ]]
}

@test "is_stowed: returns 1 when stow outputs LINK lines" {
  _source_setup
  local stow_dir="${WORKSPACE_DIR}/shell"
  mkdir -p "${stow_dir}/zsh/.config/zsh"
  touch "${stow_dir}/zsh/.config/zsh/conf"
  export MOCK_STOW_OUTPUT="LINK: .config/zsh => ../../shell/zsh/.config/zsh"
  run is_stowed "shell/zsh"
  [[ "$status" -eq 1 ]]
}

@test "_resolve_conflict: refuses path outside HOME" {
  _source_setup
  dry_run=false
  run _resolve_conflict "/etc/passwd"
  [[ "$status" -eq 1 ]]
  [[ "$output" == *"outside HOME"* ]] || [[ "$output" == *"Refusing"* ]]
}

@test "_resolve_conflict: removes symlink" {
  _source_setup
  dry_run=false
  local target="${HOME}/testlink"
  ln -s "/some/target" "${target}"
  run _resolve_conflict "${target}"
  [[ "$status" -eq 0 ]]
  [[ ! -e "${target}" ]]
}

@test "_resolve_conflict: backs up regular file" {
  _source_setup
  dry_run=false
  local target="${HOME}/testfile"
  echo "content" > "${target}"
  run _resolve_conflict "${target}"
  [[ "$status" -eq 0 ]]
  [[ -f "${target}.pre-stow-backup" ]]
}

@test "_resolve_conflict: increments backup suffix" {
  _source_setup
  dry_run=false
  local target="${HOME}/testfile"
  echo "content" > "${target}"
  touch "${target}.pre-stow-backup"
  run _resolve_conflict "${target}"
  [[ "$status" -eq 0 ]]
  [[ -f "${target}.pre-stow-backup.1" ]]
}

@test "_resolve_conflict: refuses non-empty directory" {
  _source_setup
  dry_run=false
  local target="${HOME}/testdir"
  mkdir -p "${target}"
  touch "${target}/somefile"
  run _resolve_conflict "${target}"
  [[ "$status" -eq 1 ]]
  [[ "$output" == *"non-empty"* ]]
}

@test "_resolve_conflict: dry-run does not modify filesystem" {
  _source_setup
  dry_run=true
  local target="${HOME}/testfile"
  echo "content" > "${target}"
  run _resolve_conflict "${target}"
  [[ "$status" -eq 0 ]]
  [[ -f "${target}" ]]
  [[ ! -f "${target}.pre-stow-backup" ]]
}

# =============================================================================
# Command/Entry Tests
# =============================================================================

@test "cmd_install with no args shows help" {
  _source_setup
  run cmd_install
  [[ "$status" -eq 0 ]]
  [[ "$output" == *"Usage"* ]]
}

@test "cmd_install --all with pkg name exits 2" {
  _source_setup
  run cmd_install --all zsh
  [[ "$status" -eq 2 ]]
}

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

@test "_cleanup removes temp files" {
  _source_setup
  local tmpfile="/tmp/omw-setup-test.$$"
  touch "${tmpfile}"
  _cleanup
  [[ ! -f "${tmpfile}" ]]
}
