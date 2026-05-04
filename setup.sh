#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-04-24 16:48:16 Friday by zhengyu.li>
# =============================================================================
# oh-my-workspace Setup Script
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
#   Main setup script for managing dotfiles via GNU Stow.
#   Handles prerequisites, Homebrew bundle, and package stowing.
#   Uses stow -n -v to simulate operations, delegating all ignore
#   rules, tree-folding logic, and edge cases to stow itself.
#   Output uses simple colored log lines with passthrough tool output.
#
# Usage:    ./setup.sh help
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

# --- Network ---
readonly NETWORK_TIMEOUT=60

# --- Download Retry ---
readonly DOWNLOAD_MAX_RETRIES=3
readonly DOWNLOAD_RETRY_DELAY=5

# --- Xcode CLI Install Polling ---
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
# Two-step: assign with default first, then seal as readonly.
# A single "readonly VAR=${VAR:-default}" would fail if VAR is
# already exported as readonly from the environment.
WORKSPACE_DIR="${WORKSPACE_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
readonly WORKSPACE_DIR

BREWFILE="${WORKSPACE_DIR}/pkg/homebrew/Brewfile"
readonly BREWFILE

# --- State ---
dry_run=false

# Standard exit code for SIGINT (128 + signal 2).
readonly EXIT_SIGINT=130

# -----------------------------------------------------------------------------
# Color System
# -----------------------------------------------------------------------------

# Standard ANSI colors. All constants are empty when NO_COLOR is set
# or stdout is not a TTY, ensuring no escape codes in piped output.

_IS_TTY=false
if [[ -t 1 ]]; then
  _IS_TTY=true
fi
readonly _IS_TTY

if [[ -n "${NO_COLOR:-}" ]] || ! "${_IS_TTY}"; then
  readonly C_R=''
  readonly C_G=''
  readonly C_Y=''
  readonly C_B=''
  readonly C_BOLD=''
  readonly C_DIM=''
  readonly C_RESET=''
else
  readonly C_R=$'\033[0;31m'
  readonly C_G=$'\033[0;32m'
  readonly C_Y=$'\033[0;33m'
  readonly C_B=$'\033[0;34m'
  readonly C_BOLD=$'\033[1m'
  readonly C_DIM=$'\033[2m'
  readonly C_RESET=$'\033[0m'
fi

# -----------------------------------------------------------------------------
# Logging
# -----------------------------------------------------------------------------

# Content indent: 4 spaces for items nested under a phase header.
readonly _LOG_INDENT='    '

_log() {
  local -r color="$1" tag="$2" stream="$3"
  shift 3
  if [[ "${stream}" == err ]]; then
    printf '%s%b[%s]%b %s\n' "${_LOG_INDENT}" "${color}" "${tag}" "${C_RESET}" "$*" >&2
  else
    printf '%s%b[%s]%b %s\n' "${_LOG_INDENT}" "${color}" "${tag}" "${C_RESET}" "$*"
  fi
}

die()      { _log "${C_R}" error err "$*"; exit 1; }
_misuse()  { _log "${C_R}" error err "$*"; exit 2; }
log_ok()   { _log "${C_G}" ok    out "$*"; }
log_err()  { _log "${C_R}" error err "$*"; }
log_warn() { _log "${C_Y}" warn  err "$*"; }
log_info() { _log "${C_B}" info  out "$*"; }

_PHASE_TOTAL=1
_PHASE_INDEX=0

_phase() {
  _PHASE_INDEX=$(( _PHASE_INDEX + 1 ))
  printf '\n%b[%d/%d]%b %b%s%b\n' \
    "${C_DIM}" "${_PHASE_INDEX}" "${_PHASE_TOTAL}" "${C_RESET}" \
    "${C_BOLD}" "$*" "${C_RESET}"
}

_run_indented() {
  "$@" > >( sed "s/^/${_LOG_INDENT}/" ) 2> >( sed "s/^/${_LOG_INDENT}/" >&2 )
}

# -----------------------------------------------------------------------------
# Error Handling
# -----------------------------------------------------------------------------

_err_handler() {
  local -r code=$?
  printf '%s%b[error]%b %s() line %d: exit %d\n' \
    "${_LOG_INDENT}" "${C_R}" "${C_RESET}" \
    "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
}
trap '_err_handler' ERR

_cleanup() {
  :
}
trap '_cleanup' EXIT

# Forward Ctrl-C cleanly: kill any background children (brew bundle,
# ya pkg install) so they don't outlive the script.
_int_handler() {
  trap - EXIT
  local -a pids=()
  local _pid
  while IFS= read -r _pid; do
    pids+=("$_pid")
  done < <(jobs -p)
  if (( ${#pids[@]} > 0 )); then
    kill "${pids[@]}" 2>/dev/null || true
  fi
  exit "${EXIT_SIGINT}"
}
trap '_int_handler' INT TERM

# -----------------------------------------------------------------------------
# Package Path Helpers
# -----------------------------------------------------------------------------

pkg_category() {
  printf '%s' "${1%/*}"
}

pkg_name() {
  printf '%s' "${1##*/}"
}

pkg_stow_dir() {
  printf '%s/%s' "${WORKSPACE_DIR}" "$(pkg_category "$1")"
}

_is_valid_pkg() {
  local p

  for p in "${PKG_ALL[@]}"; do
    if [[ "$1" == "${p}" ]]; then
      return 0
    fi
  done
  return 1
}

# Populates global _VALIDATED_PKGS with resolved "category/name" identifiers.
_VALIDATED_PKGS=()

validate_pkgs() {
  _VALIDATED_PKGS=()
  local p pkg match existing _seen

  for p in "$@"; do
    if [[ -z "${p}" ]]; then
      continue
    fi
    match=''

    # Exact "category/name" match.
    if _is_valid_pkg "${p}"; then
      match="${p}"
    else
      # Fallback: match by short name (e.g. "zsh" → "shell/zsh").
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

    _seen=false
    for existing in "${_VALIDATED_PKGS[@]}"; do
      if [[ "${existing}" == "${match}" ]]; then
        _seen=true
        break
      fi
    done
    if ! "${_seen}"; then
      _VALIDATED_PKGS+=("${match}")
    fi
  done

  (( ${#_VALIDATED_PKGS[@]} > 0 ))
}

# -----------------------------------------------------------------------------
# Prerequisite Probes
# -----------------------------------------------------------------------------

_has_xcode_cli() {
  xcode-select -p >/dev/null 2>&1
}

_has_homebrew() {
  command -v brew >/dev/null 2>&1
}

_has_stow() {
  command -v stow >/dev/null 2>&1
}

# -----------------------------------------------------------------------------
# Bootstrap
# -----------------------------------------------------------------------------

_bootstrap_homebrew_env() {
  # Always source brew shellenv from known Homebrew locations so the
  # full PATH (bin, sbin) is available even when brew is reachable
  # via a partial environment.
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

# -----------------------------------------------------------------------------
# Prerequisite Installation
# -----------------------------------------------------------------------------

# Install Xcode CLI (blocks until xcode-select -p succeeds or timeout).
# `xcode-select --install` only spawns the GUI installer and returns
# immediately; we must poll _has_xcode_cli to know when the user has
# completed the dialog. Required because Homebrew install needs CLT.
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
  trap 'rm -f "${installer:-}"' RETURN

  local attempt=1
  local delay="${DOWNLOAD_RETRY_DELAY}"
  while (( attempt <= DOWNLOAD_MAX_RETRIES )); do
    if curl --fail --silent --show-error \
      --connect-timeout "${NETWORK_TIMEOUT}" \
      --output "$installer" "${url}"; then
      break
    fi

    if (( attempt >= DOWNLOAD_MAX_RETRIES )); then
      log_err "Homebrew download failed after ${DOWNLOAD_MAX_RETRIES} attempts"
      return 1
    fi

    log_warn "Download attempt ${attempt} failed, retrying in ${delay}s..."
    sleep "${delay}"
    attempt=$(( attempt + 1 ))
    delay=$(( delay * 2 ))
  done

  if ! /bin/bash "$installer"; then
    log_err "Homebrew installation failed"
    return 1
  fi

  rm -f "$installer"
  trap - RETURN

  if ! _bootstrap_homebrew_env; then
    log_err "Homebrew not found after installation"
    return 1
  fi
  log_ok "Homebrew: installed"
}

# Returns 1 so the caller can reinstall when Homebrew is too old.
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

  if ! curl --fail --silent --show-error \
    --connect-timeout "${NETWORK_TIMEOUT}" \
    --output "$uninstaller" "${uninstall_url}"; then
    log_err "Homebrew uninstall script download failed"
    return 1
  fi

  if ! /bin/bash "$uninstaller"; then
    log_err "Homebrew uninstall failed"
    return 1
  fi

  rm -f "$uninstaller"
  trap - RETURN

  # Clear the command hash table so command -v brew reflects the current
  # PATH state rather than a stale cached entry from before uninstall.
  hash -r

  # Verify brew is truly gone from PATH after uninstall.
  local _brew_residual
  if _brew_residual=$(command -v brew 2>/dev/null); then
    log_warn "Homebrew still found at ${_brew_residual} after uninstall"
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
}

# -----------------------------------------------------------------------------
# Stow State Queries
# -----------------------------------------------------------------------------

# Returns 0 when stow -n -v produces no LINK: lines (already stowed).
is_stowed() {
  local stow_dir
  stow_dir=$(pkg_stow_dir "$1")
  local pkg_base
  pkg_base=$(pkg_name "$1")

  if [[ ! -d "${stow_dir}/${pkg_base}" ]]; then
    return 1
  fi

  # Empty package dir → nothing to stow; report as not-stowed to avoid
  # false positives in cmd_status / cmd_uninstall / hook gating.
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

# -----------------------------------------------------------------------------
# Stow Operations
# -----------------------------------------------------------------------------

# Extract absolute target paths from stow dry-run output (LINK/conflict lines).
_parse_stow_targets() {
  local output="$1"
  local -r conflict_owned_pat='existing target is not owned by stow: (.+)'
  local -r conflict_file_pat='existing target (.+) since'
  local line _rest

  while IFS= read -r line; do
    if [[ "${line}" == LINK:* ]]; then
      # Split on first " => " to extract the link target path.
      _rest="${line#LINK: }"
      printf '%s\n' "${HOME}/${_rest%% => *}"
    elif [[ "${line}" =~ ${conflict_owned_pat} ]]; then
      printf '%s\n' "${HOME}/${BASH_REMATCH[1]}"
    elif [[ "${line}" =~ ${conflict_file_pat} ]]; then
      printf '%s\n' "${HOME}/${BASH_REMATCH[1]}"
    fi
  done <<< "${output}"
}

# Refuses to operate outside $HOME or on non-empty directories for safety.
_resolve_stow_conflict() {
  local -r target="$1"

  # Safety: refuse to operate outside $HOME (check before any other logic).
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

  # Safety: refuse to remove non-empty directories (e.g. ~/.config).
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
    log_info "[dry-run] would remove conflicting path: ${target}"
    return 0
  fi

  log_warn "Removing conflicting path: ${target}"
  rm -rf "${target}"
}

# Flow: dry-run → state check → conflict resolution → execute.
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

  # --- Single Stow Dry Run For All Decisions ---
  local -a dry_flags=(-n -v -d "${stow_dir}" -t "${HOME}")
  case "${mode}" in
    restow) dry_flags+=(-R) ;;
    unstow) dry_flags+=(-D) ;;
  esac

  local dry_rc=0
  local dry_output
  dry_output=$(stow "${dry_flags[@]}" "${pkg_base}" 2>&1) || dry_rc=$?

  # --- State Check ---
  # stow: skip if already fully stowed (rc==0, no LINK: lines).
  if [[ "${mode}" == stow ]] \
    && (( dry_rc == 0 )) \
    && ! grep -q '^LINK:' <<< "${dry_output}"; then
    log_info "${pkg_base}: already stowed"
    return 0
  fi

  # unstow: skip if nothing is stowed (no UNLINK: lines).
  # Distinguish between "nothing to unstow" (rc==0) and a genuine stow
  # error (rc!=0) — the latter should not be silently swallowed.
  if [[ "${mode}" == unstow ]] \
    && ! grep -q '^UNLINK:' <<< "${dry_output}"; then
    if (( dry_rc != 0 )); then
      log_err "${pkg_base}: stow dry-run failed"
      return 1
    fi
    log_warn "${pkg_base}: not stowed, skipping"
    return 0
  fi

  # restow: skip if already fully stowed with no conflicts.
  if [[ "${mode}" == restow ]] \
    && ! grep -q 'existing target' <<< "${dry_output}" \
    && is_stowed "${pkg}"; then
    log_info "${pkg_base}: already stowed, no changes needed"
    return 0
  fi

  # --- Conflict Resolution (Stow/Restow Only) ---
  if [[ "${mode}" != unstow ]]; then
    local target
    while IFS= read -r target; do
      if ! _resolve_stow_conflict "${target}"; then
        return 1
      fi
    done < <(_parse_stow_targets "${dry_output}")
  fi

  # --- Dry Run Display ---
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
      log_info "[dry-run]   (exact links hidden by conflicts" \
        "— will be created after removal)"
    fi
    return 0
  fi

  # --- Actual Execution ---
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

stow_package() {
  _stow_exec "$1" stow
}

restow_package() {
  _stow_exec "$1" restow
}

unstow_package() {
  _stow_exec "$1" unstow
}

# -----------------------------------------------------------------------------
# Internal Helpers
# -----------------------------------------------------------------------------

# --- Shell (zsh) ---
# Change default login shell to zsh if not already.
# Return codes: 0=ok, 1=fail, 2=skip (not applicable).
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

# --- Yazi ---
# Install yazi plugins via ya pkg (requires package.toml to be stowed).
# Return codes: 0=ok, 1=fail, 2=skip.
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
  local _rc=0
  "${fn}" || _rc=$?
  case "${_rc}" in
    0) log_ok "${name}: done" ;;
    2) log_info "${name}: skipped" ;;
    *) log_err "${name}: failed" ;;
  esac
}

_run_post_install_hooks() {
  local _pkg
  for _pkg in "$@"; do
    case "${_pkg}" in
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

# Map a package identifier to its "cmd_name:friendly_name" health entry.
# For cask apps whose binary is not on PATH, append a third field with
# the known binary path: "cmd:friendly:/path/to/binary".
# Returns 1 when the package has no health check.
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
    # When no fallback field exists, friendly == fallback_path.
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
    show_help
    return 0
  fi

  ensure_prerequisites

  # --- All Packages ---
  if "${do_all}"; then
    if "${dry_run}"; then
      _PHASE_TOTAL=4
    else
      _PHASE_TOTAL=5
    fi
    _PHASE_INDEX=0

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
    for pkg in "${PKG_ALL[@]}"; do
      if "${force}"; then
        restow_package "${pkg}" || true
      else
        stow_package "${pkg}" || true
      fi
    done

    _run_post_install_phase "${PKG_ALL[@]}"

    if ! "${dry_run}"; then
      _run_health_check "${PKG_ALL[@]}"
    fi

    return 0
  fi

  # --- Specific Packages ---
  if ! validate_pkgs "${pkgs[@]}"; then
    die "No valid packages specified"
  fi

  _PHASE_TOTAL=2
  _PHASE_INDEX=0

  _phase "Stow Packages"
  local pkg
  for pkg in "${_VALIDATED_PKGS[@]}"; do
    if "${force}"; then
      restow_package "${pkg}" || true
    else
      stow_package "${pkg}" || true
    fi
  done

  _run_post_install_phase "${_VALIDATED_PKGS[@]}"
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
  else
    if ! validate_pkgs "${pkgs[@]}"; then
      die "No valid packages specified"
    fi
    target_pkgs=("${_VALIDATED_PKGS[@]}")
  fi

  _PHASE_TOTAL=1
  _PHASE_INDEX=0

  _phase "Unstow Packages"
  local pkg
  for pkg in "${target_pkgs[@]}"; do
    unstow_package "${pkg}" || true
  done
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

  _PHASE_TOTAL=1
  _PHASE_INDEX=0

  _phase "Prerequisites"
  if _has_xcode_cli; then log_ok "Xcode CLI"; else log_warn "Xcode CLI: missing"; fi
  if _has_homebrew; then log_ok "Homebrew"; else log_warn "Homebrew: missing"; fi
  if _has_stow; then log_ok "GNU Stow"; else log_warn "GNU Stow: missing"; fi

  if ! _has_stow; then
    log_info "run: ./setup.sh install --all"
    return 0
  fi

  _PHASE_TOTAL=2
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

# -----------------------------------------------------------------------------
# Help
# -----------------------------------------------------------------------------

show_help() {
  printf '%b\n' \
    "${C_BOLD}oh-my-workspace setup${C_RESET}" '' \
    'Usage:' \
    '  ./setup.sh <command> [flags] [packages]' '' \
    "${C_BOLD}Commands:${C_RESET}" \
    '  install   [--all] [--force] [--dry-run] [<pkg>...]   Stow packages' \
    '  uninstall [--all] [--dry-run] [<pkg>...]             Unstow packages' \
    '  status    [<pkg>...]                                 Show status and symlinks' \
    '  help                                                 Show this help' '' \
    "${C_BOLD}Flags:${C_RESET}" \
    '  --all      Apply to all packages (install / uninstall)' \
    '  --force    Restow even if already stowed (install only).' \
    '             Runs stow -R; conflicts at target paths are deleted.' \
    '             Use after adding new dotfiles to a package dir.' \
    '  --dry-run  Preview stow changes; brew bundle is skipped, nothing is linked/unlinked' '' \
    "${C_BOLD}Packages${C_RESET} (base name or full category/name):" \
    '  shell:   zsh  starship' \
    '  editor:  vim  emacs' \
    '  term:    ghostty' \
    '  tool:    git  lazygit  ripgrep  yazi' \
    '  lang:    uv  bun' '' \
    "${C_BOLD}Examples:${C_RESET}" \
    '  ./setup.sh install --all                    Prereqs + brew + stow all packages' \
    '  ./setup.sh install zsh git                  Stow specific packages' \
    '  ./setup.sh install --force zsh              Restow (pick up new dotfiles)' \
    '  ./setup.sh install --force --all            Restow everything' \
    '  ./setup.sh install --dry-run zsh            Preview what install would do' \
    '  ./setup.sh install --force --dry-run --all  Preview a full restow' \
    '  ./setup.sh uninstall --all                  Unstow all' \
    '  ./setup.sh uninstall --dry-run zsh          Preview what uninstall would do' \
    '  ./setup.sh status                           Full status with symlinks' \
    '  ./setup.sh status zsh                       Status for one package' '' \
    "${C_BOLD}Note:${C_RESET}" \
    '  install without packages or --all shows this help.'
}

# -----------------------------------------------------------------------------
# Entry Point
# -----------------------------------------------------------------------------

main() {
  # Platform check (fast, no side effects)
  if [[ "$(uname -s)" != Darwin ]]; then
    printf 'error: macOS required\n' >&2
    exit 1
  fi

  # Parse args before any dependency installation
  if (( $# == 0 )); then
    show_help
    exit 0
  fi
  local cmd="$1"
  shift
  case "${cmd}" in
    install) cmd_install "$@" ;;
    uninstall) cmd_uninstall "$@" ;;
    status) cmd_status "$@" ;;
    help|-h|--help) show_help ;;
    *)
      log_err "Unknown command: ${cmd}"
      printf '\n'
      show_help
      exit 2
      ;;
  esac
}

main "$@"
