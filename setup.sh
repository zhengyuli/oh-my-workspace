#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-04-07 19:58:35 Tuesday by zhengyu.li>
# =============================================================================
# oh-my-workspace Setup Script
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: dotfiles, stow, homebrew, setup
# Dependencies: bash 4.3+, macOS
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

# Source TUI library (color system, logging, dashboard, health check).
# shellcheck source=lib/tui.sh
source "${WORKSPACE_DIR}/lib/tui.sh"

# -----------------------------------------------------------------------------
# Error Handling
# -----------------------------------------------------------------------------

_err_handler() {
  local -r code=$?
  printf '  %b[error]%b %s() line %d: exit %d\n' \
    "${C_R}" "${C_RESET}" "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
}
trap '_err_handler' ERR

_cleanup() {
  _tui_cursor_show 2>/dev/null || true
  _tui_result_cleanup
}
trap '_cleanup' EXIT

# Forward Ctrl-C cleanly: kill any background children (brew bundle,
# ya pkg install) so they don't outlive the script. Clear EXIT trap
# first so _cleanup runs exactly once.
_int_handler() {
  trap - EXIT
  _tui_cursor_show 2>/dev/null || true
  local -a pids=()
  mapfile -t pids < <(jobs -p)
  if (( ${#pids[@]} > 0 )); then
    kill "${pids[@]}" 2>/dev/null || true
  fi
  _tui_result_cleanup
  exit 130
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

# Resolve package identifiers (short name or category/name) to canonical
# "category/name" form.  Out-parameter via nameref ($1) to avoid subshell.
# Returns 0 if at least one package resolved, 1 otherwise.
validate_pkgs() {
  local -n _out="$1"
  shift
  _out=()
  local p pkg match
  declare -A _seen=()

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
    if [[ -z "${_seen[${match}]:-}" ]]; then
      _out+=("${match}")
      _seen[${match}]=1
    fi
  done

  (( ${#_out[@]} > 0 ))
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
  if _has_homebrew; then
    return 0
  fi

  local b env_output
  for b in /opt/homebrew/bin/brew /usr/local/bin/brew; do
    if [[ -x "${b}" ]]; then
      if ! env_output=$("${b}" shellenv 2>/dev/null); then
        continue
      fi
      # shellcheck disable=SC1090
      source <(printf '%s\n' "${env_output}")
      if _has_homebrew; then
        return 0
      fi
    fi
  done

  return 1
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

  local waited=0
  until _has_xcode_cli; do
    if (( waited >= XCODE_POLL_MAX )); then
      log_err "Xcode CLI install timed out after ${XCODE_POLL_MAX}s"
      return 1
    fi
    sleep "${XCODE_POLL_INTERVAL}"
    waited=$(( waited + XCODE_POLL_INTERVAL ))
  done

  log_ok "Xcode CLI: installed"
}

_install_homebrew() {
  log_info "Installing Homebrew..."
  local -r url='https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh'
  local installer
  installer="$(mktemp)"

  if ! curl --fail --silent --show-error \
    --connect-timeout "${NETWORK_TIMEOUT}" \
    --output "$installer" "${url}"; then
    rm -f "$installer"
    log_err "Homebrew download failed"
    return 1
  fi

  if ! /bin/bash "$installer"; then
    rm -f "$installer"
    log_err "Homebrew installation failed"
    return 1
  fi

  rm -f "$installer"
  if ! _bootstrap_homebrew_env; then
    log_err "Homebrew not found after installation"
    return 1
  fi
  log_ok "Homebrew: installed"
}

# Verify Homebrew meets the minimum version requirement.
# If too old: offer to uninstall, then return 1 so the caller reinstalls.
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

  if ! confirm "Remove old Homebrew and reinstall?" y; then
    log_info "Skipped — upgrade Homebrew manually: brew update"
    return 0
  fi

  log_info "Removing old Homebrew..."

  local -r uninstall_url='https://raw.githubusercontent.com/Homebrew/install/HEAD/uninstall.sh'
  local uninstaller
  uninstaller="$(mktemp)"

  if ! curl --fail --silent --show-error \
    --connect-timeout "${NETWORK_TIMEOUT}" \
    --output "$uninstaller" "${uninstall_url}"; then
    rm -f "$uninstaller"
    log_err "Homebrew uninstall script download failed"
    return 1
  fi

  if ! /bin/bash "$uninstaller"; then
    rm -f "$uninstaller"
    log_err "Homebrew uninstall failed"
    return 1
  fi
  rm -f "$uninstaller"

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

  if brew install stow; then
    log_ok "GNU Stow: installed"
  else
    log_err "GNU Stow installation failed"
    return 1
  fi
}

_run_brew_bundle() {
  if ! _tui_brew_run "${BREWFILE}"; then
    return 1
  fi
}

_run_brew_bundle_checked() {
  if _run_brew_bundle; then
    return 0
  fi
  # confirm() emits 2 newlines on a TTY: one from the printf '\n' and one
  # from the terminal echoing the user's Enter. Cursor advances 2 lines.
  if ! confirm "brew bundle had failures. Continue with stow?" n; then
    _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 2 ))
    log_info "Cancelled"
    _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 1 ))
    return 1
  fi
  _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 2 ))
  return 2
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

# Check if a package is fully stowed by running stow in dry-run mode.
# Returns 0 when no LINK: lines appear (nothing to link → already stowed).
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
        -mindepth 1 \( -type f -o -type l \) -print -quit 2>/dev/null)" ]]; then
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

# Parse stow dry-run output for planned links and conflicts.
# Returns absolute target paths, one per line.
_parse_stow_targets() {
  local output="$1"
  local -r conflict_owned_pat='existing target is not owned by stow: (.+)'
  local -r conflict_file_pat='existing target ([^ ]+) since'
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

# Remove a conflicting file/symlink at the stow target path.
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

# Core stow dispatcher for stow_package / restow_package / unstow_package.
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
    && is_stowed "$1"; then
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
  case "${mode}" in
    restow) flags+=(-R) ;;
    unstow) flags+=(-D) ;;
  esac

  local action
  case "${mode}" in
    stow) action='stowed' ;;
    restow) action='restowed' ;;
    unstow) action='unstowed' ;;
  esac

  if stow "${flags[@]}" "${pkg_base}"; then
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
# Offer to change default login shell to zsh.
# Return codes (consumed by _run_post_install_hooks):
#   0  success or no-op (already zsh)
#   1  attempted but failed (chsh failed, /etc/shells not writable, etc.)
#   2  user declined or zsh not stowed (mapped to 'skip' in the table)
# Append a hook error message to the buffered error file (no-op if unset).
# Stdin → file. Use heredoc / printf | _hook_err_log.
_hook_err_log() {
  if [[ -n "${_TUI_HOOK_ERR_FILE:-}" ]]; then
    cat >> "${_TUI_HOOK_ERR_FILE}"
  else
    cat >/dev/null
  fi
}

_post_install_shell_zsh() {
  if ! is_stowed shell/zsh; then
    return 2
  fi

  # Already on zsh — nothing to do, stay silent.
  if [[ -n "${ZSH_VERSION:-}" || "${SHELL##*/}" == zsh ]]; then
    return 0
  fi

  # Interactive: ask the user, then attempt chsh.
  if ! confirm "Switch default shell to zsh?" y; then
    return 2
  fi

  local zsh_path
  if ! zsh_path=$(command -v zsh 2>/dev/null); then
    printf 'zsh not in PATH\n' | _hook_err_log
    return 1
  fi

  if ! grep -qx "${zsh_path}" /etc/shells 2>/dev/null; then
    if ! printf '%s\n' "${zsh_path}" \
      | sudo tee -a /etc/shells >/dev/null 2>&1; then
      {
        printf 'cannot write /etc/shells — run manually:\n'
        printf "  echo '%s' | sudo tee -a /etc/shells\n" "${zsh_path}"
        printf "  chsh -s '%s'\n" "${zsh_path}"
      } | _hook_err_log
      return 1
    fi
  fi

  if chsh -s "${zsh_path}" 2>/dev/null; then
    if [[ -n "${_TUI_HOOK_OK_FILE:-}" ]]; then
      printf 'Default shell → zsh · open a new terminal to apply.\n' \
        >> "${_TUI_HOOK_OK_FILE}"
    fi
    return 0
  fi
  printf "chsh failed — run: chsh -s '%s'\n" "${zsh_path}" | _hook_err_log
  return 1
}

# --- Yazi ---
# Install yazi plugins via ya pkg (requires package.toml to be stowed).
# Return codes match _post_install_shell_zsh: 0=ok, 1=fail, 2=skip.
# Failure output is buffered into _TUI_HOOK_ERR_FILE so the post-install
# table renders cleanly; the dispatcher prints the buffer afterwards.
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

  local tmp
  tmp=$(mktemp)
  local ya_rc=0
  ya pkg install >"${tmp}" 2>&1 || ya_rc=$?

  if (( ya_rc != 0 )); then
    {
      printf 'yazi plugin install failed:\n'
      tail -n "${_TUI_BREW_ERROR_TAIL}" "${tmp}"
    } | _hook_err_log
  fi
  rm -f "${tmp}"
  if (( ya_rc != 0 )); then
    return 1
  fi
  return 0
}

# Dispatch post-install hooks for the given packages.
# Updates the named status variables for the post-install table.
# Hook return codes: 0=ok, 1=fail, 2=skip (declined / not applicable).
# Failure output is buffered to _TUI_HOOK_ERR_FILE so the table renders
# cleanly; the buffer is printed by the caller after the table closes.
# Usage: _run_post_install_hooks zsh_var yazi_var pkg1 pkg2 ...
_run_post_install_hooks() {
  local -n _zsh_st_ref="$1"
  local -n _yazi_st_ref="$2"
  shift 2
  local _pkg
  for _pkg in "$@"; do
    case "${_pkg}" in
      shell/zsh) _hook_run _post_install_shell_zsh _zsh_st_ref ;;
      tool/yazi) _hook_run _post_install_yazi _yazi_st_ref ;;
    esac
  done
}

# Run a single hook and map its return code to a status string via nameref.
_hook_run() {
  local -r fn="$1"
  local -n _st_ref="$2"
  local _rc=0
  "${fn}" || _rc=$?
  case "${_rc}" in
    0) _st_ref='ok' ;;
    2) _st_ref='skip' ;;
    *) _st_ref='fail' ;;
  esac
}

# Render the "Post-Install Hooks" phase: spinner → hook table → flush.
# Args: phase_num pkg1 pkg2 ...
_run_post_install_phase() {
  local -r num="$1"
  shift
  _tui_phase_start "${num}" 'Post-Install'
  if "${dry_run}"; then
    _tui_hooks_table 'shell/zsh:dry-run' 'tool/yazi:dry-run'
    _tui_phase_done "${num}" 'Post-Install' 'dry-run'
    return 0
  fi
  local _zsh_hook_st='skip' _yazi_hook_st='skip'
  _run_post_install_hooks _zsh_hook_st _yazi_hook_st "$@"
  _tui_hooks_table \
    "shell/zsh:${_zsh_hook_st}" \
    "tool/yazi:${_yazi_hook_st}"
  _tui_phase_done "${num}" 'Post-Install'
  _tui_hook_msg_flush
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

  # Initialize result file for tracking per-package outcomes.
  _tui_result_init

  ensure_prerequisites

  # --- All Packages ---
  if "${do_all}"; then
    _TUI_PHASE_TOTAL=5
    _tui_banner

    # Phase 1: Prerequisites — check current state and render table.
    _tui_phase_start 1 'Prerequisites'
    local xcode_st brew_st stow_st
    if _has_xcode_cli; then xcode_st='ok'; else xcode_st='dry-run'; fi
    if _has_homebrew; then brew_st='ok'; else brew_st='dry-run'; fi
    if _has_stow; then stow_st='ok'; else stow_st='dry-run'; fi
    _tui_prereq_table \
      "Xcode CLI:${xcode_st}" \
      "Homebrew:${brew_st}" \
      "GNU Stow:${stow_st}"
    _tui_phase_done 1 'Prerequisites' 'ready'

    # Phase 2: Homebrew Bundle.
    _tui_phase_start 2 'Homebrew Bundle'
    local _brew_f _brew_c _brew_t
    _tui_parse_brewfile "${BREWFILE}" _brew_f _brew_c _brew_t
    if "${dry_run}"; then
      _tui_brew_table "${_brew_f}" "${_brew_c}" "${_brew_t}"
      _tui_phase_done 2 'Homebrew Bundle' 'dry-run'
    else
      local _brew_rc=0
      _run_brew_bundle_checked || _brew_rc=$?
      case "${_brew_rc}" in
        0)
          _tui_brew_table "${_brew_f}" "${_brew_c}" "${_brew_t}"
          _tui_phase_done 2 'Homebrew Bundle' "${_TUI_BREW_DETAIL}"
          ;;
        2)
          # User opted to continue past brew failures — render as failed
          # phase but proceed to the stow phases below.
          _tui_phase_fail 2 'Homebrew Bundle' \
            "${_TUI_BREW_DETAIL} · continued"
          ;;
        *)
          _tui_phase_fail 2 'Homebrew Bundle' "${_TUI_BREW_DETAIL}"
          return 1
          ;;
      esac
    fi

    # Phase 3: Stow Packages (live dashboard).
    _tui_phase_start 3 'Stow Packages'

    # shellcheck disable=SC2034  # mutated via nameref in _tui_dash_render
    declare -A dash_statuses=()
    local pkg stow_output stow_rc
    local ok_count=0 fail_count=0
    for pkg in "${PKG_ALL[@]}"; do
      dash_statuses[${pkg}]=pending
    done
    _tui_dash_render dash_statuses "${PKG_ALL[@]}"

    for pkg in "${PKG_ALL[@]}"; do
      dash_statuses[${pkg}]=run
      _tui_dash_up
      _tui_dash_render dash_statuses "${PKG_ALL[@]}"

      stow_rc=0
      if "${force}"; then
        stow_output=$(restow_package "${pkg}" 2>&1) || stow_rc=$?
      else
        stow_output=$(stow_package "${pkg}" 2>&1) || stow_rc=$?
      fi

      if "${dry_run}" && (( stow_rc == 0 )); then
        dash_statuses[${pkg}]='dry-run'
        _tui_result_write "${pkg}" 'dry-run'
        ok_count=$(( ok_count + 1 ))
      elif is_stowed "${pkg}"; then
        dash_statuses[${pkg}]=ok
        _tui_result_write "${pkg}" ok
        ok_count=$(( ok_count + 1 ))
      else
        dash_statuses[${pkg}]=fail
        _tui_result_write "${pkg}" fail
        fail_count=$(( fail_count + 1 ))
        # Buffer stow error to err file; the dashboard renders in-place
        # via cursor-up math, so writing to stderr here would be
        # overwritten on the next render. Flushed after phase_done.
        if [[ -n "${stow_output}" ]] && (( stow_rc != 0 )); then
          {
            printf 'stow %s failed:\n' "${pkg}"
            printf '%s\n' "${stow_output}"
          } | _hook_err_log
        fi
      fi

      _tui_dash_up
      _tui_dash_render dash_statuses "${PKG_ALL[@]}"
    done

    local stow_detail="${ok_count}/${#PKG_ALL[@]} packages"
    if (( fail_count > 0 )); then
      stow_detail+=" · ${fail_count} failed"
    fi
    _tui_phase_done 3 'Stow Packages' "${stow_detail}"
    _tui_hook_msg_flush

    # Phase 4: Post-Install Hooks — table shows hook results.
    _run_post_install_phase 4 "${PKG_ALL[@]}"

    # Phase 5: Health Check.
    if ! "${dry_run}"; then
      _tui_health_check "$(_tui_result_path)"
    else
      _tui_phase_start 5 'Health Check'
      _tui_phase_done 5 'Health Check' 'dry-run'
    fi

    return 0
  fi

  # --- Specific Packages ---
  local -a resolved=()
  if ! validate_pkgs resolved "${pkgs[@]}"; then
    die "No valid packages specified"
  fi

  _TUI_PHASE_TOTAL=2
  _tui_banner

  # Phase 1: Stow packages — mini table.
  _tui_phase_start 1 'Stow Packages'
  _tui_result_init

  _tui_tbl_header 'PACKAGE' 'STATUS'
  local pkg stow_rc stow_output
  local ok_count=0 fail_count=0
  for pkg in "${resolved[@]}"; do
    stow_rc=0
    if "${force}"; then
      stow_output=$(restow_package "${pkg}" 2>&1) || stow_rc=$?
    else
      stow_output=$(stow_package "${pkg}" 2>&1) || stow_rc=$?
    fi

    local pkg_base="${pkg##*/}"
    if "${dry_run}" && (( stow_rc == 0 )); then
      _tui_tbl_row "${_ICON_SKIP}" "${pkg_base}" 'dry-run' "${C_DIM}"
      _tui_result_write "${pkg}" 'dry-run'
      ok_count=$(( ok_count + 1 ))
    elif is_stowed "${pkg}"; then
      _tui_tbl_row "${_ICON_OK}" "${pkg_base}" 'ok' "${C_G}"
      _tui_result_write "${pkg}" ok
      ok_count=$(( ok_count + 1 ))
    else
      _tui_tbl_row "${_ICON_FAIL}" "${pkg_base}" 'fail' "${C_R}"
      _tui_result_write "${pkg}" fail
      fail_count=$(( fail_count + 1 ))
      if [[ -n "${stow_output}" ]] && (( stow_rc != 0 )); then
        printf '  %s\n' "${stow_output}" >&2
      fi
    fi
  done
  _tui_tbl_footer

  local stow_detail="${ok_count}/${#resolved[@]} packages"
  if "${dry_run}"; then
    stow_detail="dry-run · ${stow_detail}"
  fi
  if (( fail_count > 0 )); then
    stow_detail+=" · ${fail_count} failed"
  fi
  if (( fail_count > 0 )); then
    _tui_phase_fail 1 'Stow Packages' "${stow_detail}"
  else
    _tui_phase_done 1 'Stow Packages' "${stow_detail}"
  fi

  # Phase 2: Post-Install Hooks.
  _run_post_install_phase 2 "${resolved[@]}"
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

  _TUI_PHASE_TOTAL=2
  _tui_banner

  # Phase 1: Verify GNU Stow is available.
  _tui_phase_start 1 'Prerequisites'
  if ! _has_stow; then
    _tui_prereq_table 'Xcode CLI:skip' 'Homebrew:skip' 'GNU Stow:fail'
    _tui_phase_fail 1 'Prerequisites' 'GNU Stow not found'
    printf '\n  %s→%s run: %s./setup.sh install --all%s\n\n' \
      "${C_Y}" "${C_RESET}" "${C_BOLD}" "${C_RESET}"
    return 1
  fi
  _tui_prereq_table 'Xcode CLI:skip' 'Homebrew:skip' 'GNU Stow:ok'
  _tui_phase_done 1 'Prerequisites' 'GNU Stow ready'

  # Phase 2: Unstow packages.
  _tui_phase_start 2 'Unstow Packages'

  # Resolve the target package list.
  local -a target_pkgs=()
  if "${do_all}"; then
    local p
    for p in "${PKG_ALL[@]}"; do
      if is_stowed "${p}"; then
        target_pkgs+=("${p}")
      fi
    done

    if (( ${#target_pkgs[@]} == 0 )); then
      _tui_tbl_header 'PACKAGE' 'STATUS'
      _tui_tbl_footer
      _tui_phase_done 2 'Unstow Packages' 'nothing stowed'
      printf '\n'
      return 0
    fi

    if ! "${dry_run}"; then
      if ! confirm "Uninstall all ${#target_pkgs[@]} stowed packages?" n; then
        # confirm() emits 2 newlines on TTY (printf \n + Enter echo).
        _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 2 ))
        _tui_phase_done 2 'Unstow Packages' 'cancelled'
        printf '\n'
        return 0
      fi
      _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 2 ))
    fi
  else
    local -a resolved=()
    if ! validate_pkgs resolved "${pkgs[@]}"; then
      _tui_phase_fail 2 'Unstow Packages' 'no valid packages'
      return 1
    fi
    target_pkgs=("${resolved[@]}")
  fi

  # Run unstow with live dashboard.
  _tui_result_init

  # shellcheck disable=SC2034  # mutated via nameref in _tui_dash_render
  declare -A dash_statuses=()
  local pkg
  for pkg in "${target_pkgs[@]}"; do
    dash_statuses[${pkg}]=pending
  done
  _tui_dash_render dash_statuses "${target_pkgs[@]}"

  local ok_count=0 fail_count=0
  local unstow_rc
  for pkg in "${target_pkgs[@]}"; do
    dash_statuses[${pkg}]=run
    _tui_dash_up
    _tui_dash_render dash_statuses "${target_pkgs[@]}"

    unstow_rc=0
    unstow_package "${pkg}" >/dev/null 2>&1 || unstow_rc=$?

    # Determine final status: for unstow, check that it's no longer stowed
    # (or dry_run accepted without error).
    if (( unstow_rc == 0 )); then
      dash_statuses[${pkg}]=ok
      _tui_result_write "${pkg}" ok
      ok_count=$(( ok_count + 1 ))
    else
      # shellcheck disable=SC2034  # mutated via nameref in _tui_dash_render
      dash_statuses[${pkg}]=fail
      _tui_result_write "${pkg}" fail
      fail_count=$(( fail_count + 1 ))
    fi

    _tui_dash_up
    _tui_dash_render dash_statuses "${target_pkgs[@]}"
  done

  local detail="${ok_count}/${#target_pkgs[@]} packages"
  if "${dry_run}"; then
    detail="dry-run · ${detail}"
  fi
  if (( fail_count > 0 )); then
    detail+=" · ${fail_count} failed"
  fi

  if (( fail_count > 0 )); then
    _tui_phase_fail 2 'Unstow Packages' "${detail}"
  else
    _tui_phase_done 2 'Unstow Packages' "${detail}"
  fi
}

cmd_status() {
  local -a pkgs

  if (( $# > 0 )); then
    local -a resolved=()

    if ! validate_pkgs resolved "$@"; then
      return 1
    fi

    pkgs=("${resolved[@]}")
  else
    pkgs=("${PKG_ALL[@]}")
  fi

  _TUI_PHASE_TOTAL=2
  _tui_banner

  # Phase 1: Prerequisites (read-only checks).
  _tui_phase_start 1 'Prerequisites'
  local xcode_st='missing' brew_st='missing' stow_st='missing'
  if _has_xcode_cli; then xcode_st='ok'; fi
  if _has_homebrew; then brew_st='ok'; fi
  if _has_stow; then stow_st='ok'; fi

  _tui_status_prereq_table \
    "Xcode CLI:${xcode_st}" \
    "Homebrew:${brew_st}" \
    "GNU Stow:${stow_st}"
  _tui_phase_done 1 'Prerequisites'

  if [[ "${stow_st}" == missing ]]; then
    printf '\n  %s→%s run: %s./setup.sh install --all%s\n\n' \
      "${C_Y}" "${C_RESET}" "${C_BOLD}" "${C_RESET}"
    return 0
  fi

  # Phase 2: Package Status.
  _tui_phase_start 2 'Package Status'

  local -A pkg_stowed=()
  local stowed_count=0
  local pkg

  for pkg in "${pkgs[@]}"; do
    if is_stowed "${pkg}"; then
      pkg_stowed[${pkg}]=1
      stowed_count=$(( stowed_count + 1 ))
    else
      pkg_stowed[${pkg}]=0
    fi
  done

  # Count stowed packages outside the subset for the summary detail.
  local total_stowed="${stowed_count}"
  if (( ${#pkgs[@]} < ${#PKG_ALL[@]} )); then
    local other_pkg
    for other_pkg in "${PKG_ALL[@]}"; do
      if [[ -z "${pkg_stowed[${other_pkg}]+set}" ]] \
        && is_stowed "${other_pkg}"; then
        total_stowed=$(( total_stowed + 1 ))
      fi
    done
  fi

  _tui_status_pkg_table pkgs pkg_stowed

  local detail="${stowed_count}/${#pkgs[@]} stowed"
  if (( ${#pkgs[@]} < ${#PKG_ALL[@]} )); then
    detail+=" · ${total_stowed}/${#PKG_ALL[@]} total"
  fi
  _tui_phase_done 2 'Package Status' "${detail}"
  printf '\n'
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
