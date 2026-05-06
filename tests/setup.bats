#!/usr/bin/env bash
# setup.bats — BATS test suite for setup.sh

load setup-helper

# =============================================================================
# Package Model Tests
# =============================================================================

@test "pkg_category: shell/zsh → shell" {
  _source_setup
  run pkg_category "shell/zsh"
  [[ "$output" == "shell" ]]
}

@test "pkg_category: prog-lang/python/uv → prog-lang/python" {
  _source_setup
  run pkg_category "prog-lang/python/uv"
  [[ "$output" == "prog-lang/python" ]]
}

@test "pkg_name: shell/zsh → zsh" {
  _source_setup
  run pkg_name "shell/zsh"
  [[ "$output" == "zsh" ]]
}

@test "pkg_name: prog-lang/python/uv → uv" {
  _source_setup
  run pkg_name "prog-lang/python/uv"
  [[ "$output" == "uv" ]]
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
# Symlink Engine Tests
# =============================================================================

@test "is_linked: returns 0 when directory symlink exists" {
  _source_setup
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local pkg_dir="${WORKSPACE_DIR}/tool/git"
  mkdir -p "${pkg_dir}"
  echo "test" > "${pkg_dir}/config"
  ln -sf "${pkg_dir}" "${HOME}/.config/git"
  run is_linked "tool/git"
  [[ "$status" -eq 0 ]]
}

@test "is_linked: returns 1 for nonexistent package dir" {
  _source_setup
  run is_linked "shell/nonexistent"
  [[ "$status" -eq 1 ]]
}

@test "is_linked: returns 1 when symlink is missing" {
  _source_setup
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local pkg_dir="${WORKSPACE_DIR}/tool/git"
  mkdir -p "${pkg_dir}"
  echo "test" > "${pkg_dir}/config"
  run is_linked "tool/git"
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
  [[ -f "${target}.pre-link-backup" ]]
}

@test "_resolve_conflict: increments backup suffix" {
  _source_setup
  dry_run=false
  local target="${HOME}/testfile"
  echo "content" > "${target}"
  touch "${target}.pre-link-backup"
  run _resolve_conflict "${target}"
  [[ "$status" -eq 0 ]]
  [[ -f "${target}.pre-link-backup.1" ]]
}

@test "_resolve_conflict: backs up non-workspace directory" {
  _source_setup
  dry_run=false
  local target="${HOME}/testdir"
  mkdir -p "${target}"
  touch "${target}/somefile"
  run _resolve_conflict "${target}"
  [[ "$status" -eq 0 ]]
  [[ -d "${target}.pre-link-backup" ]]
  [[ -f "${target}.pre-link-backup/somefile" ]]
}

@test "_resolve_conflict: dry-run does not modify filesystem" {
  _source_setup
  dry_run=true
  local target="${HOME}/testfile"
  echo "content" > "${target}"
  run _resolve_conflict "${target}"
  [[ "$status" -eq 0 ]]
  [[ -f "${target}" ]]
  [[ ! -f "${target}.pre-link-backup" ]]
}

@test "_resolve_conflict: refuses path traversal via .." {
  _source_setup
  dry_run=false
  # Create a path that starts with HOME but resolves outside via ..
  mkdir -p "${HOME}/subdir"
  local target="${HOME}/subdir/../../etc/passwd"
  run _resolve_conflict "${target}"
  [[ "$status" -eq 1 ]]
  [[ "$output" == *"resolves outside HOME"* ]] || [[ "$output" == *"Refusing"* ]]
}

@test "_resolve_conflict: removes workspace-owned directory" {
  _source_setup
  dry_run=false
  local target="${HOME}/testdir"
  mkdir -p "${target}"
  ln -sf "${WORKSPACE_DIR}/some/file" "${target}/link1"
  ln -sf "${WORKSPACE_DIR}/other/file" "${target}/link2"
  run _resolve_conflict "${target}"
  [[ "$status" -eq 0 ]]
  [[ ! -e "${target}" ]]
}

@test "_collect_links: resolves per-package nameref" {
  _source_setup
  local -a _LINK_SRCS _LINK_DESTS
  run _collect_links "tool/git"
  [[ "$status" -eq 0 ]]
}

@test "_collect_links: returns 1 for unknown package" {
  _source_setup
  local -a _LINK_SRCS _LINK_DESTS
  run _collect_links "nonexistent/pkg"
  [[ "$status" -eq 1 ]]
}

@test "is_linked: file-level mapping (starship)" {
  _source_setup
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local src="${WORKSPACE_DIR}/tool/starship/starship.toml"
  local target="${HOME}/.config/starship.toml"
  mkdir -p "$(dirname "${src}")" "$(dirname "${target}")"
  echo "test" > "${src}"
  ln -sf "${src}" "${target}"
  run is_linked "tool/starship"
  [[ "$status" -eq 0 ]]
}

# =============================================================================
# _create_link Tests
# =============================================================================

@test "_create_link: creates symlink" {
  _source_setup
  dry_run=false
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local src="${WORKSPACE_DIR}/tool/git/config"
  local dest="${HOME}/.config/git/config"
  mkdir -p "$(dirname "${src}")"
  echo "content" > "${src}"
  _create_link "${src}" "${dest}" false
  [[ -L "${dest}" ]]
  [[ "$(readlink "${dest}")" == "${src}" ]]
}

@test "_create_link: skips when symlink already correct" {
  _source_setup
  dry_run=false
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local src="${WORKSPACE_DIR}/tool/git/config"
  local dest="${HOME}/.config/git/config"
  mkdir -p "$(dirname "${src}")" "$(dirname "${dest}")"
  echo "content" > "${src}"
  ln -sf "${src}" "${dest}"
  run _create_link "${src}" "${dest}" false
  [[ "$status" -eq 0 ]]
  [[ -L "${dest}" ]]
}

@test "_create_link: force replaces existing symlink" {
  _source_setup
  dry_run=false
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local src="${WORKSPACE_DIR}/tool/git/config"
  local dest="${HOME}/.config/git/config"
  mkdir -p "$(dirname "${src}")" "$(dirname "${dest}")"
  echo "content" > "${src}"
  ln -sf "/some/old/path" "${dest}"
  _create_link "${src}" "${dest}" true
  [[ -L "${dest}" ]]
  [[ "$(readlink "${dest}")" == "${src}" ]]
}

@test "_create_link: dry-run does not create symlink" {
  _source_setup
  dry_run=true
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local src="${WORKSPACE_DIR}/tool/git/config"
  local dest="${HOME}/.config/git/config"
  mkdir -p "$(dirname "${src}")"
  echo "content" > "${src}"
  run _create_link "${src}" "${dest}" false
  [[ "$status" -eq 0 ]]
  [[ ! -L "${dest}" ]]
  [[ "$output" == *"dry-run"* ]]
}

@test "_create_link: creates parent directories" {
  _source_setup
  dry_run=false
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local src="${WORKSPACE_DIR}/tool/git/config"
  local dest="${HOME}/.config/deep/nested/dir/config"
  mkdir -p "$(dirname "${src}")"
  echo "content" > "${src}"
  _create_link "${src}" "${dest}" false
  [[ -L "${dest}" ]]
  [[ -d "${HOME}/.config/deep/nested/dir" ]]
}

# =============================================================================
# link_package / relink_package / unlink_package Tests
# =============================================================================

@test "link_package: creates directory symlink" {
  _source_setup
  dry_run=false
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local pkg_dir="${WORKSPACE_DIR}/tool/git"
  mkdir -p "${pkg_dir}"
  echo "content" > "${pkg_dir}/config"
  link_package "tool/git"
  [[ -L "${HOME}/.config/git" ]]
  [[ "$(readlink "${HOME}/.config/git")" == "${pkg_dir}" ]]
}

@test "link_package: idempotent when already linked" {
  _source_setup
  dry_run=false
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local pkg_dir="${WORKSPACE_DIR}/tool/git"
  mkdir -p "${pkg_dir}"
  echo "content" > "${pkg_dir}/config"
  ln -sf "${pkg_dir}" "${HOME}/.config/git"
  run link_package "tool/git"
  [[ "$status" -eq 0 ]]
  [[ "$output" == *"already linked"* ]]
}

@test "relink_package: replaces existing symlink" {
  _source_setup
  dry_run=false
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local pkg_dir="${WORKSPACE_DIR}/tool/git"
  mkdir -p "${pkg_dir}" "${HOME}/.config"
  echo "content" > "${pkg_dir}/config"
  ln -sf "/old/path" "${HOME}/.config/git"
  relink_package "tool/git"
  [[ -L "${HOME}/.config/git" ]]
  [[ "$(readlink "${HOME}/.config/git")" == "${pkg_dir}" ]]
}

@test "unlink_package: removes directory symlink" {
  _source_setup
  dry_run=false
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local pkg_dir="${WORKSPACE_DIR}/tool/git"
  mkdir -p "${pkg_dir}"
  echo "content" > "${pkg_dir}/config"
  ln -sf "${pkg_dir}" "${HOME}/.config/git"
  unlink_package "tool/git"
  [[ ! -L "${HOME}/.config/git" ]]
}

@test "unlink_package: cleans empty parent directories" {
  _source_setup
  dry_run=false
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local src="${WORKSPACE_DIR}/tool/starship/starship.toml"
  local dest="${HOME}/.config/starship.toml"
  mkdir -p "$(dirname "${src}")" "$(dirname "${dest}")"
  echo "content" > "${src}"
  ln -sf "${src}" "${dest}"
  unlink_package "tool/starship"
  [[ ! -L "${dest}" ]]
}

@test "unlink_package: skips symlinks owned by others" {
  _source_setup
  dry_run=false
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  local pkg_dir="${WORKSPACE_DIR}/tool/git"
  mkdir -p "${pkg_dir}" "${HOME}/.config"
  echo "content" > "${pkg_dir}/config"
  # Symlink points elsewhere, not to our workspace
  ln -sf "/other/repo/git" "${HOME}/.config/git"
  run unlink_package "tool/git"
  [[ "$status" -eq 0 ]]
  [[ "$output" == *"not linked"* ]]
  # Foreign symlink should still exist
  [[ -L "${HOME}/.config/git" ]]
}

# =============================================================================
# Homebrew Version Tests
# =============================================================================

@test "_ensure_homebrew_version: accepts valid version above minimum" {
  _source_setup
  export MOCK_BREW_VERSION="4.4.0"
  run _ensure_homebrew_version
  [[ "$status" -eq 0 ]]
}

@test "_ensure_homebrew_version: rejects version below minimum" {
  _source_setup
  export MOCK_BREW_VERSION="3.9.0"
  dry_run=false
  run _ensure_homebrew_version
  [[ "$status" -ne 0 ]]
}

@test "_ensure_homebrew_version: rejects non-numeric minor version" {
  _source_setup
  export MOCK_BREW_VERSION="4.4beta"
  run _ensure_homebrew_version
  [[ "$status" -eq 1 ]]
  [[ "$output" == *"Cannot parse"* ]]
}

@test "_ensure_homebrew_version: rejects unparseable version" {
  _source_setup
  export MOCK_BREW_VERSION="invalid"
  run _ensure_homebrew_version
  [[ "$status" -eq 1 ]]
  [[ "$output" == *"Cannot determine"* ]]
}

# =============================================================================
# Hook Run Tests
# =============================================================================

@test "_hook_run: reports done on success (rc=0)" {
  _source_setup
  _test_hook_ok() { return 0; }
  run _hook_run "test hook" _test_hook_ok
  [[ "$status" -eq 0 ]]
  [[ "$output" == *"done"* ]]
}

@test "_hook_run: reports failed on rc=1" {
  _source_setup
  _test_hook_fail() { return 1; }
  run _hook_run "test hook" _test_hook_fail
  [[ "$output" == *"failed"* ]]
}

@test "_hook_run: reports skipped on rc=2" {
  _source_setup
  _test_hook_skip() { return 2; }
  run _hook_run "test hook" _test_hook_skip
  [[ "$output" == *"skipped"* ]]
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

# =============================================================================
# cmd_status Tests
# =============================================================================

@test "cmd_status: lists all packages" {
  _source_setup
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  run cmd_status
  [[ "$status" -eq 0 ]]
  [[ "$output" == *"Prerequisites"* ]]
  [[ "$output" == *"Packages"* ]]
}

@test "cmd_status: filters to specific package" {
  _source_setup
  WORKSPACE_DIR="${BATS_TEST_TMPDIR}/workspace"
  run cmd_status "zsh"
  [[ "$status" -eq 0 ]]
  [[ "$output" == *"shell/zsh"* ]]
}

@test "cmd_status: rejects invalid package" {
  _source_setup
  run cmd_status "nonexistent"
  [[ "$status" -eq 1 ]]
}

# =============================================================================
# cmd_uninstall Tests
# =============================================================================

@test "cmd_uninstall: no args shows error" {
  _source_setup
  run cmd_uninstall
  [[ "$status" -eq 1 ]]
  [[ "$output" == *"No packages specified"* ]]
}

@test "cmd_uninstall: --all and pkg name exits 2" {
  _source_setup
  run cmd_uninstall --all zsh
  [[ "$status" -eq 2 ]]
}

# =============================================================================
# _health_tool_for Tests
# =============================================================================

@test "_health_tool_for: known package returns descriptor" {
  _source_setup
  run _health_tool_for "tool/git"
  [[ "$status" -eq 0 ]]
  [[ "$output" == "git:git" ]]
}

@test "_health_tool_for: unknown package returns 1" {
  _source_setup
  run _health_tool_for "unknown/pkg"
  [[ "$status" -eq 1 ]]
}

@test "_health_tool_for: ghostty includes fallback path" {
  _source_setup
  run _health_tool_for "terminal/ghostty"
  [[ "$status" -eq 0 ]]
  [[ "$output" == *"/Applications/Ghostty.app"* ]]
}

# =============================================================================
# cmd_help Tests
# =============================================================================

@test "cmd_help: shows usage information" {
  _source_setup
  run cmd_help
  [[ "$status" -eq 0 ]]
  [[ "$output" == *"Usage"* ]]
  [[ "$output" == *"install"* ]]
  [[ "$output" == *"uninstall"* ]]
  [[ "$output" == *"status"* ]]
}
