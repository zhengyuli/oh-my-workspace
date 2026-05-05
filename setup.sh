#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-05-05 12:37:36 Tuesday by zhengyu.li>
#
# =============================================================================
# oh-my-workspace Setup — Dotfile Management via GNU Stow
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
#   Manages dotfiles via GNU Stow with prerequisite bootstrapping
#   (Xcode CLI, Homebrew, Stow), Homebrew bundle, and per-package
#   post-install hooks. Uses stow -n -v to probe state, delegating
#   ignore rules and tree-folding to stow itself.
#
# References:
#   1. GNU Stow Manual:
#      https://www.gnu.org/software/stow/manual/
#   2. Homebrew Bundle:
#      https://github.com/Homebrew/homebrew-bundle
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

readonly NETWORK_TIMEOUT=60
readonly DOWNLOAD_MAX_RETRIES=3
readonly DOWNLOAD_RETRY_DELAY=5
readonly XCODE_POLL_INTERVAL=5
readonly XCODE_POLL_MAX=600
readonly MIN_HOMEBREW_MAJOR=4
readonly MIN_HOMEBREW_MINOR=4

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

# Two-step assign-then-seal avoids failure when the variable is
# already exported as readonly from the environment.
WORKSPACE_DIR="${WORKSPACE_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
readonly WORKSPACE_DIR

BREWFILE="${WORKSPACE_DIR}/pkg/homebrew/Brewfile"
readonly BREWFILE

readonly EXIT_SIGINT=130

dry_run=false

# -----------------------------------------------------------------------------
# Color System
# -----------------------------------------------------------------------------

# Empty when NO_COLOR is set or stdout is not a TTY, so piped output
# never contains escape codes.
_IS_TTY=false
if [[ -t 1 ]]; then
  _IS_TTY=true
fi
readonly _IS_TTY

if [[ -n "${NO_COLOR:-}" ]] || ! "${_IS_TTY}"; then
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

# Log a colored, tagged message to stdout or stderr.
# Arguments:
#   color  - ANSI escape sequence.
#   tag    - Label shown in brackets.
#   stream - "out" for stdout, "err" for stderr.
#   ...    - Message text.
_log() {
  local -r color="$1" tag="$2" stream="$3"
  shift 3
  if [[ "${stream}" == err ]]; then
    printf '%s%b[%s]%b %s\n' "${_LOG_INDENT}" "${color}" "${tag}" "${_RESET}" "$*" >&2
  else
    printf '%s%b[%s]%b %s\n' "${_LOG_INDENT}" "${color}" "${tag}" "${_RESET}" "$*"
  fi
}

# Log error and exit 1 (runtime failure).
die() {
  _log "${_RED}" error err "$*"
  exit 1
}

# Log error and exit 2 (usage/argument error).
_misuse() {
  _log "${_RED}" error err "$*"
  exit 2
}

# Convenience log wrappers: colored output at various severity levels.
log_ok() { _log "${_GREEN}" ok out "$*"; }
log_err() { _log "${_RED}" error err "$*"; }
log_warn() { _log "${_YELLOW}" warn err "$*"; }
log_info() { _log "${_BLUE}" info out "$*"; }

_phase_total=1
_phase_index=0

# Print a phase progress header: [N/Total] Title.
_phase() {
  _phase_index=$(( _phase_index + 1 ))
  printf '\n%b[%d/%d]%b %b%s%b\n' "${_DIM}" "${_phase_index}" "${_phase_total}" "${_RESET}" "${_BOLD}" "$*" "${_RESET}"
}

# Run a command with its output indented by _LOG_INDENT.
# Uses temp files instead of process substitution because >()
# is not tracked by wait in bash < 4.4.
# Arguments:
#   ... - Command and arguments to execute.
# Returns:
#   The exit code of the wrapped command.
_run_indented() {
  local rc=0
  local tmp_out
  local tmp_err
  tmp_out=$(mktemp)
  tmp_err=$(mktemp)

  if [[ -z "${tmp_out}" || -z "${tmp_err}" ]]; then
    log_err "mktemp failed in _run_indented"
    return 1
  fi

  "$@" >"${tmp_out}" 2>"${tmp_err}" || rc=$?
  sed "s/^/${_LOG_INDENT}/" "${tmp_out}"
  sed "s/^/${_LOG_INDENT}/" "${tmp_err}" >&2
  rm -f "${tmp_out}" "${tmp_err}"
  return "${rc}"
}

# -----------------------------------------------------------------------------
# Signal Handling
# -----------------------------------------------------------------------------

# ERR trap: log failing function, line number, and exit code.
_err_handler() {
  local -r code=$?
  printf '%s%b[error]%b %s() line %d: exit %d\n' \
    "${_LOG_INDENT}" "${_RED}" "${_RESET}" "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "${code}" >&2
}
trap '_err_handler' ERR

# EXIT trap: remove temp files matching /tmp/omw-setup-*.$$.
_cleanup() {
  local -a tmpfiles=()
  shopt -s nullglob
  tmpfiles=(/tmp/omw-setup-*.$$)
  shopt -u nullglob
  if (( ${#tmpfiles[@]} > 0 )); then
    rm -f "${tmpfiles[@]}" 2>/dev/null
  fi
}
trap '_cleanup' EXIT

# INT/TERM trap: kill background children then exit 130.
_int_handler() {
  trap - EXIT
  local -a pids=()
  local _pid
  while IFS= read -r _pid; do
    pids+=("${_pid}")
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

# Extract category from a package path (e.g., "shell" from "shell/zsh").
pkg_category() {
  printf '%s' "${1%/*}"
}

# Extract base name from a package path (e.g., "zsh" from "shell/zsh").
pkg_name() {
  printf '%s' "${1##*/}"
}

# Return the stow source directory for a given package.
pkg_stow_dir() {
  printf '%s/%s' "${WORKSPACE_DIR}" "$(pkg_category "$1")"
}

# Check if a package path is in PKG_ALL.
_is_valid_pkg() {
  local p
  for p in "${PKG_ALL[@]}"; do
    if [[ "$1" == "${p}" ]]; then
      return 0
    fi
  done
  return 1
}

_VALIDATED_PKGS=()

# Resolve user-provided package names to full paths.
# Accepts both short names ("zsh") and full paths ("shell/zsh").
# Deduplicates results into the global _VALIDATED_PKGS array.
# Globals:
#   PKG_ALL (read)
#   _VALIDATED_PKGS (write)
# Arguments:
#   ... - One or more package names or paths.
# Returns:
#   0 if at least one valid package resolved, 1 otherwise.
validate_pkgs() {
  _VALIDATED_PKGS=()
  local p pkg match existing seen

  for p in "$@"; do
    if [[ -z "${p}" ]]; then
      continue
    fi
    match=''

    if _is_valid_pkg "${p}"; then
      match="${p}"
    else
      for pkg in "${PKG_ALL[@]}"; do
        if [[ "$(pkg_name "${pkg}")" == "${p}" ]]; then
          match="${pkg}"
          break
        fi
      done
    fi

    if [[ -z "${match}" ]]; then
      log_warn "Unknown package: ${p}"
      continue
    fi

    seen=false
    if (( ${#_VALIDATED_PKGS[@]} > 0 )); then
      for existing in "${_VALIDATED_PKGS[@]}"; do
        if [[ "${existing}" == "${match}" ]]; then
          seen=true
          break
        fi
      done
    fi
    if ! "${seen}"; then
      _VALIDATED_PKGS+=("${match}")
    fi
  done

  (( ${#_VALIDATED_PKGS[@]} > 0 ))
}

# -----------------------------------------------------------------------------
# Prerequisites
# -----------------------------------------------------------------------------

# True if Xcode Command Line Tools are installed.
_has_xcode_cli() {
  xcode-select -p >/dev/null 2>&1
}

# True if Homebrew is on PATH.
_has_homebrew() {
  command -v brew >/dev/null 2>&1
}

# True if GNU Stow is on PATH.
_has_stow() {
  command -v stow >/dev/null 2>&1
}

# Source brew shellenv from well-known Homebrew prefixes.
# Ensures PATH is fully configured even when brew isn't
# on the default PATH.
# Returns:
#   0 if brew is now available, 1 otherwise.
_bootstrap_homebrew_env() {
  local b env_output
  for b in /opt/homebrew/bin/brew /usr/local/bin/brew; do
    if [[ -x "${b}" ]]; then
      if env_output=$("${b}" shellenv 2>/dev/null); then
        eval "${env_output}"
        return 0
      fi
    fi
  done
  _has_homebrew
}

# Download a file via curl with exponential-backoff retry.
# Arguments:
#   url  - Remote URL to fetch.
#   dest - Local file path to save to.
# Returns:
#   0 on success, 1 after all retries exhausted.
_download() {
  local -r url="$1" dest="$2"
  local attempt=1
  local delay="${DOWNLOAD_RETRY_DELAY}"

  while (( attempt <= DOWNLOAD_MAX_RETRIES )); do
    if curl --fail --silent --show-error --connect-timeout "${NETWORK_TIMEOUT}" --output "${dest}" "${url}"; then
      return 0
    fi

    if (( attempt >= DOWNLOAD_MAX_RETRIES )); then
      log_err "Download failed after ${DOWNLOAD_MAX_RETRIES} attempts: ${url}"
      return 1
    fi

    log_warn "Download attempt ${attempt} failed, retrying in ${delay}s..."
    sleep "${delay}"
    attempt=$(( attempt + 1 ))
    delay=$(( delay * 2 ))
  done
}

# Spawn the Xcode CLI installer dialog and poll until ready.
# xcode-select --install only opens a GUI dialog; we must
# poll until the tools are present or the timeout is hit.
# Returns:
#   0 on success, 1 on timeout or verification failure.
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
    waited=$(( waited + XCODE_POLL_INTERVAL ))
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

  if [[ -z "${installer}" ]]; then
    log_err "mktemp failed"
    return 1
  fi

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

# Verify Homebrew version meets minimum requirements.
# If too old, uninstall it and signal a fresh install is needed.
# Globals:
#   MIN_HOMEBREW_MAJOR, MIN_HOMEBREW_MINOR (read)
#   dry_run (read)
# Returns:
#   0 if version is acceptable, 1 if reinstall needed.
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

  if [[ ! "${major}" =~ ^[0-9]+$ ]] || [[ ! "${minor}" =~ ^[0-9]+$ ]]; then
    log_err "Cannot parse Homebrew version components: '${ver}'"
    return 1
  fi

  if (( major > MIN_HOMEBREW_MAJOR || (major == MIN_HOMEBREW_MAJOR && minor >= MIN_HOMEBREW_MINOR) )); then
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

  if [[ -z "${uninstaller}" ]]; then
    log_err "mktemp failed"
    return 1
  fi

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

  # Flush the command hash so `command -v brew` reflects reality.
  hash -r

  local brew_residual
  if brew_residual=$(command -v brew 2>/dev/null); then
    log_warn "Homebrew still found at ${brew_residual} after uninstall"
    log_info "PATH may contain stale entries"
  fi

  return 1
}

# Install GNU Stow via Homebrew.
_install_stow() {
  log_info "Installing GNU Stow..."

  if _run_indented brew install stow; then
    log_ok "GNU Stow: installed"
  else
    log_err "GNU Stow installation failed"
    return 1
  fi
}

# Run brew bundle. Always returns 0; partial failures are warnings.
_run_brew_bundle() {
  if _run_indented brew bundle --file="${BREWFILE}"; then
    log_ok "brew bundle: done"
    return 0
  fi
  log_warn "brew bundle had failures, continuing..."
  return 0
}

# Configure git clean/smudge filters for yazi package.toml.
_setup_git_filters() {
  git config --local filter.yazi-package.clean "sed -e 's/^rev = .*/rev = \"pinned\"/' -e 's/^hash = .*/hash = \"0\"/'"
  git config --local filter.yazi-package.smudge cat
  git config --local filter.yazi-package.required true
  log_ok "Git filter: yazi-package configured"
}

# Install prerequisites: Xcode CLI → Homebrew → Stow → git filters.
# Globals:
#   dry_run (read)
ensure_prerequisites() {
  # --- Xcode CLI ---
  if ! _has_xcode_cli; then
    if "${dry_run}"; then
      log_info "[dry-run] would install Xcode CLI"
    elif ! _install_xcode_cli; then
      return 1
    fi
  fi

  # --- Homebrew ---
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

  # --- GNU Stow ---
  if ! _has_stow; then
    if "${dry_run}"; then
      log_info "[dry-run] would install GNU Stow"
    elif ! _install_stow; then
      return 1
    fi
  fi

  # --- Git Filters ---
  if _is_valid_pkg "tool/yazi"; then
    _setup_git_filters
  fi
}

# -----------------------------------------------------------------------------
# Stow Engine
# -----------------------------------------------------------------------------

# Test whether a package is currently stowed.
# Uses stow's dry-run to check if any LINK actions remain.
# Arguments:
#   $1 - Full package path (e.g., "shell/zsh").
# Returns:
#   0 if fully stowed, 1 otherwise.
is_stowed() {
  local stow_dir
  stow_dir=$(pkg_stow_dir "$1")
  local pkg_base
  pkg_base=$(pkg_name "$1")

  if [[ ! -d "${stow_dir}/${pkg_base}" ]]; then
    return 1
  fi

  # Empty package dir → nothing to stow; treat as not-stowed to avoid
  # false positives in status / uninstall / hook gating.
  if [[ -z "$(find "${stow_dir}/${pkg_base}" -mindepth 1 \( -type f -o -type l \) -print -quit 2>/dev/null)" ]]; then
    return 1
  fi

  local output
  if ! output=$(stow -n -v -d "${stow_dir}" -t "${HOME}" "${pkg_base}" 2>&1); then
    return 1
  fi

  if grep -q '^LINK:' <<< "${output}"; then
    return 1
  fi

  return 0
}

# Parse stow dry-run output to extract target paths.
# Emits one absolute path per line for links and conflicts.
# Arguments:
#   output - Captured text from `stow -n -v`.
# Outputs:
#   Writes absolute file paths to stdout.
_parse_stow_targets() {
  local output="$1"
  local -r conflict_owned_pat='existing target is not owned by stow: (.+)'
  local -r conflict_file_pat='existing target (.+) since'
  local line rest

  while IFS= read -r line; do
    if [[ "${line}" == LINK:* ]]; then
      rest="${line#LINK: }"
      printf '%s\n' "${HOME}/${rest%% => *}"
    elif [[ "${line}" =~ ${conflict_owned_pat} ]]; then
      printf '%s\n' "${HOME}/${BASH_REMATCH[1]}"
    elif [[ "${line}" =~ ${conflict_file_pat} ]]; then
      printf '%s\n' "${HOME}/${BASH_REMATCH[1]}"
    fi
  done <<< "${output}"
}

# Resolve a conflicting file before stow can proceed.
# Handles foreign symlinks (remove), missing files (no-op),
# non-empty dirs (refuse), and regular files (back up to
# *.pre-stow-backup with incrementing suffix).
# Arguments:
#   target - Absolute path of the conflicting file.
# Returns:
#   0 if resolved, 1 if manual action needed.
_resolve_conflict() {
  local -r target="$1"

  if [[ "${target}" != "${HOME}"/* ]]; then
    log_err "Refusing to remove path outside HOME: ${target}"
    return 1
  fi

  local canonical
  canonical="$(cd "$(dirname "${target}")" 2>/dev/null && pwd -P)/$(basename "${target}")" || true
  if [[ -z "${canonical}" || "${canonical}" != "${HOME}"/* ]]; then
    log_err "Refusing to remove path that resolves outside HOME: ${target}"
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
    log_info "[dry-run] would back up and remove conflicting path: ${target}"
    return 0
  fi

  local backup="${target}.pre-stow-backup"
  if [[ -e "${backup}" ]]; then
    local i=1
    while [[ -e "${backup}.${i}" ]]; do
      i=$(( i + 1 ))
    done
    backup="${backup}.${i}"
  fi

  log_warn "Backing up conflicting path:"
  log_warn "  ${target} -> ${backup}"
  mv "${target}" "${backup}"
}

# Core stow execution engine.
# Probes state with dry-run, resolves conflicts, displays
# dry-run previews, then executes stow/restow/unstow.
# Arguments:
#   pkg  - Full package path (e.g., "shell/zsh").
#   mode - One of: "stow", "restow", "unstow".
# Returns:
#   0 on success or already-stowed skip, 1 on failure.
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

  # Single dry-run drives all skip/conflict/display decisions.
  local -a dry_flags=(-n -v -d "${stow_dir}" -t "${HOME}")
  case "${mode}" in
    restow) dry_flags+=(-R) ;;
    unstow) dry_flags+=(-D) ;;
  esac

  local dry_rc=0
  local dry_output
  dry_output=$(stow "${dry_flags[@]}" "${pkg_base}" 2>&1) || dry_rc=$?

  # --- State Check ---
  if [[ "${mode}" == stow ]] && (( dry_rc == 0 )) && ! grep -q '^LINK:' <<< "${dry_output}"; then
    log_info "${pkg_base}: already stowed"
    return 0
  fi

  # Distinguish "nothing to unstow" (rc==0) from a genuine error.
  if [[ "${mode}" == unstow ]] && ! grep -q '^UNLINK:' <<< "${dry_output}"; then
    if (( dry_rc != 0 )); then
      log_err "${pkg_base}: stow dry-run failed"
      return 1
    fi
    log_warn "${pkg_base}: not stowed, skipping"
    return 0
  fi

  if [[ "${mode}" == restow ]] && ! grep -q 'existing target' <<< "${dry_output}" && is_stowed "${pkg}"; then
    log_info "${pkg_base}: already stowed, no changes needed"
    return 0
  fi

  # --- Conflict Resolution ---
  if [[ "${mode}" != unstow ]]; then
    local target
    while IFS= read -r target; do
      if ! _resolve_conflict "${target}"; then
        return 1
      fi
    done < <(_parse_stow_targets "${dry_output}")
  fi

  # --- Dry-Run Display ---
  if "${dry_run}"; then
    log_info "[dry-run] would ${mode}: ${pkg_base}"
    local line has_actions=false

    while IFS= read -r line; do
      if [[ "${line}" =~ ^(LINK|UNLINK): ]]; then
        log_info "[dry-run]   ${line}"
        has_actions=true
      fi
    done <<< "${dry_output}"

    if ! "${has_actions}" && grep -q 'existing target' <<< "${dry_output}"; then
      log_info "[dry-run]   (exact links hidden by conflicts — will be created after removal)"
    fi
    return 0
  fi

  # --- Execute ---
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

# Public stow wrappers.
stow_package() { _stow_exec "$1" stow; }
restow_package() { _stow_exec "$1" restow; }
unstow_package() { _stow_exec "$1" unstow; }

# -----------------------------------------------------------------------------
# Post-Install Hooks
# -----------------------------------------------------------------------------

# --- Shell (zsh) ---

# Set default shell to zsh if not already active.
# Returns:
#   0 on success, 1 on failure, 2 if skipped.
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
    if ! printf '%s\n' "${zsh_path}" | sudo tee -a /etc/shells >/dev/null 2>&1; then
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

# --- Yazi ---

# Install yazi plugins from package.toml.
# Returns:
#   0 on success, 1 on failure, 2 if skipped.
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

# Execute a hook function and log its result.
# Hook convention: 0=success, 1=failure, 2=skipped.
# Arguments:
#   name - Human-readable label for logging.
#   fn   - Function name to call.
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

# Dispatch post-install hooks for packages that have one.
_run_post_install_hooks() {
  local pkg
  for pkg in "$@"; do
    case "${pkg}" in
      shell/zsh) _hook_run 'zsh shell switch' _post_install_shell_zsh ;;
      tool/yazi) _hook_run 'yazi plugins' _post_install_yazi ;;
    esac
  done
}

# Phase wrapper: print header, skip in dry-run, else run hooks.
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

# Map a package path to its health-check descriptor.
# Outputs:
#   Writes "cmd:friendly[:fallback_path]" to stdout.
# Returns:
#   1 if the package has no known binary.
_health_tool_for() {
  case "$1" in
    shell/zsh) printf 'zsh:zsh' ;;
    shell/starship) printf 'starship:starship' ;;
    editor/vim) printf 'vim:vim' ;;
    editor/emacs) printf 'emacs:emacs' ;;
    term/ghostty)
      printf 'ghostty:ghostty:/Applications/Ghostty.app/Contents/MacOS/ghostty'
      ;;
    tool/git) printf 'git:git' ;;
    tool/lazygit) printf 'lazygit:lazygit' ;;
    tool/ripgrep) printf 'rg:rg' ;;
    tool/yazi) printf 'yazi:yazi' ;;
    lang/python/uv) printf 'uv:uv' ;;
    lang/typescript/bun) printf 'bun:bun' ;;
    *) return 1 ;;
  esac
}

# Verify each stowed package's binary is reachable.
_run_health_check() {
  _phase "Health Check"
  local pkg cmd_name friendly fallback_path resolved

  for pkg in "$@"; do
    if ! is_stowed "${pkg}"; then
      continue
    fi

    local tool_entry
    if ! tool_entry=$(_health_tool_for "${pkg}"); then
      continue
    fi

    IFS=: read -r cmd_name friendly fallback_path <<< "${tool_entry}"

    resolved=$(command -v "${cmd_name}" 2>/dev/null) || true
    if [[ -z "${resolved}" && -n "${fallback_path}" && -x "${fallback_path}" ]]; then
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

# Stow or restow a single package based on the force flag.
_stow_one() {
  local -r pkg="$1" force="$2"
  if "${force}"; then
    restow_package "${pkg}"
  else
    stow_package "${pkg}"
  fi
}

# Command: install packages.
# Handles prerequisites, brew bundle, stowing, post-install
# hooks, and health check. With --all installs everything;
# otherwise accepts specific package names.
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

  # --- All Packages ---
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
      if ! _stow_one "${pkg}" "${force}"; then
        (( fail_count += 1 ))
      fi
    done

    _run_post_install_phase "${PKG_ALL[@]}"

    if ! "${dry_run}"; then
      _run_health_check "${PKG_ALL[@]}"
    fi

    return $(( fail_count > 0 ? 1 : 0 ))
  fi

  # --- Specific Packages ---
  if ! validate_pkgs "${pkgs[@]}"; then
    die "No valid packages specified"
  fi

  _phase_total=2
  _phase_index=0

  _phase "Stow Packages"
  local pkg
  local fail_count=0
  for pkg in "${_VALIDATED_PKGS[@]}"; do
    if ! _stow_one "${pkg}" "${force}"; then
      (( fail_count += 1 ))
    fi
  done

  _run_post_install_phase "${_VALIDATED_PKGS[@]}"

  return $(( fail_count > 0 ? 1 : 0 ))
}

# Command: remove symlinks for specified packages.
# With --all, prompts for confirmation (unless dry-run).
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
    local p
    for p in "${PKG_ALL[@]}"; do
      if is_stowed "${p}"; then
        target_pkgs+=("${p}")
      fi
    done

    if (( ${#target_pkgs[@]} == 0 )); then
      log_info "Nothing stowed"
      return 0
    fi

    if ! "${dry_run}"; then
      log_warn "This will unstow ${#target_pkgs[@]} packages: ${target_pkgs[*]}"
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
    if ! unstow_package "${pkg}"; then
      (( fail_count += 1 ))
    fi
  done

  return $(( fail_count > 0 ? 1 : 0 ))
}

# Command: display prerequisite health and stow status.
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
  cat <<EOF
${_BOLD}oh-my-workspace setup${_RESET}

Usage:
  ./setup.sh <command> [flags] [packages]

${_BOLD}Commands:${_RESET}
  install   [--all] [--force] [--dry-run] [<pkg>...]   Stow packages
  uninstall [--all] [--dry-run] [<pkg>...]             Unstow packages
  status    [<pkg>...]                                 Show status and symlinks
  help                                                 Show this help

${_BOLD}Flags:${_RESET}
  --all      Apply to all packages (install / uninstall)
  --force    Restow even if already stowed (install only).
             Runs stow -R; conflicting files are backed up to *.pre-stow-backup.
             Use after adding new dotfiles to a package dir.
  --dry-run  Preview stow changes; brew bundle is skipped, nothing is linked/unlinked

${_BOLD}Packages${_RESET} (base name or full category/name):
  shell:   zsh  starship
  editor:  vim  emacs
  term:    ghostty
  tool:    git  lazygit  ripgrep  yazi
  lang:    uv  bun

${_BOLD}Examples:${_RESET}
  ./setup.sh install --all                    Prereqs + brew + stow all packages
  ./setup.sh install zsh git                  Stow specific packages
  ./setup.sh install --force zsh              Restow (pick up new dotfiles)
  ./setup.sh install --force --all            Restow everything
  ./setup.sh install --dry-run zsh            Preview what install would do
  ./setup.sh install --force --dry-run --all  Preview a full restow
  ./setup.sh uninstall --all                  Unstow all
  ./setup.sh uninstall --dry-run zsh          Preview what uninstall would do
  ./setup.sh status                           Full status with symlinks
  ./setup.sh status zsh                       Status for one package

${_BOLD}Note:${_RESET}
  install without packages or --all shows this help.
EOF
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
