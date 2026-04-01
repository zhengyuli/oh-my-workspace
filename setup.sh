#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-04-01 13:11:14 Wednesday by zhengyu.li>
# =============================================================================
# oh-my-workspace Setup Script
#
# Usage:    ./setup.sh help
#
# Dependencies: bash 4.3+, macOS
#
# References:
#   1. GNU Stow Manual: https://www.gnu.org/software/stow/manual/
#   2. Homebrew Bundle: https://github.com/Homebrew/homebrew-bundle
#
# Note: Uses stow -n -v to simulate operations, delegating all ignore rules,
# tree-folding logic, and edge cases to stow itself.
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

readonly LINE_WIDTH=79

readonly NETWORK_TIMEOUT=60

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

WORKSPACE_DIR="${WORKSPACE_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
readonly WORKSPACE_DIR

BREWFILE="${WORKSPACE_DIR}/pkg/homebrew/Brewfile"
readonly BREWFILE

DRY_RUN=false

# -----------------------------------------------------------------------------
# Colors
# -----------------------------------------------------------------------------

readonly _RED='\033[0;31m'
readonly _GREEN='\033[0;32m'
readonly _YELLOW='\033[0;33m'
readonly _BLUE='\033[0;34m'
readonly _BOLD='\033[1m'
readonly _RESET='\033[0m'

# -----------------------------------------------------------------------------
# Error handling
# -----------------------------------------------------------------------------

_err_handler() {
  local -r code=$?
  printf "  ${_RED}[error]${_RESET} %s() line %d: exit %d\n" \
         "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "${code}" >&2
}
trap '_err_handler' ERR

# -----------------------------------------------------------------------------
# Logging
# -----------------------------------------------------------------------------

die() {
  printf "  ${_RED}[error]${_RESET} %s\n" "$*" >&2
  exit 1
}

_misuse() {
  printf "  ${_RED}[error]${_RESET} %s\n" "$*" >&2
  exit 2
}

log_ok() {
  printf "  ${_GREEN}[ok]${_RESET} %s\n" "$*"
}

log_err() {
  printf "  ${_RED}[error]${_RESET} %s\n" "$*" >&2
}

log_warn() {
  printf "  ${_YELLOW}[warn]${_RESET} %s\n" "$*"
}

log_info() {
  printf "  ${_BLUE}[info]${_RESET} %s\n" "$*"
}

# -----------------------------------------------------------------------------
# UI helpers
# -----------------------------------------------------------------------------

print_header() {
  printf '\n%b' "${_BOLD}"
  printf '=%.0s' $(seq 1 "${LINE_WIDTH}")
  printf '\n  %s\n' "$1"
  printf '=%.0s' $(seq 1 "${LINE_WIDTH}")
  printf '\n%b\n' "${_RESET}"
}

confirm() {
  local prompt="$1"
  local default="${2:-n}"
  local reply

  if [[ "${default}" == y ]]; then
    printf '\n  %s [Y/n]: ' "${prompt}"
  else
    printf '\n  %s [y/N]: ' "${prompt}"
  fi

  if ! read -r reply; then
    reply=''
  fi

  [[ "${reply:-${default}}" =~ ^[Yy]$ ]]
}

# --- Status display (table format — no bracket prefix) ---
# Label widths: "ok" (2) padded to match "missing" (7).
_status_ok() { printf "  ${_GREEN}[ok]${_RESET}       %s\n" "$*"; }
_status_missing() { printf "  ${_RED}[missing]${_RESET}  %s\n" "$*"; }

# --- Package status labels ---
_status_stowed() { printf "  %s ${_GREEN}[stowed]${_RESET}    :\n" "$1"; }
_status_unstowed() { printf "  %s ${_YELLOW}[unstowed]${_RESET}:\n" "$1"; }

# -----------------------------------------------------------------------------
# Package path helpers
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
  local p found pkg

  for p in "$@"; do
    if [[ -z "${p}" ]]; then
      continue
    fi
    found=''

    # Exact "category/name" match
    if _is_valid_pkg "${p}"; then
      _out+=("${p}")
      continue
    fi

    # Fallback: match by short name (e.g. "zsh" → "shell/zsh")
    for pkg in "${PKG_ALL[@]}"; do
      if [[ "$(pkg_name "${pkg}")" == "${p}" ]]; then
        _out+=("${pkg}")
        found=1
        break
      fi
    done

    if [[ -z "${found}" ]]; then
      log_warn "Unknown package: ${p}"
    fi
  done

  (( ${#_out[@]} > 0 ))
}

# -----------------------------------------------------------------------------
# Prerequisites — probes  (pure, no side effects)
# -----------------------------------------------------------------------------

_has_xcode_cli() {
  xcode-select -p &>/dev/null
}

_has_homebrew() {
  command -v brew &>/dev/null
}

_has_stow() {
  command -v stow &>/dev/null
}

# -----------------------------------------------------------------------------
# Prerequisites — bootstrap
# -----------------------------------------------------------------------------

_bootstrap_homebrew_env() {
  if _has_homebrew; then
    return 0
  fi

  local b env_output
  for b in /opt/homebrew/bin/brew /usr/local/bin/brew; do
    if [[ -x "${b}" ]]; then
      env_output=$("${b}" shellenv 2>/dev/null) || continue
      source <(printf '%s\n' "${env_output}")
      if _has_homebrew; then
        return 0
      fi
    fi
  done

  return 1
}

# -----------------------------------------------------------------------------
# Prerequisites — install
# -----------------------------------------------------------------------------

readonly XCODE_POLL_INTERVAL=5
readonly XCODE_POLL_MAX=600

# Install Xcode CLI and poll until available (timeout XCODE_POLL_MAX seconds).
_install_xcode_cli() {
  log_info "Installing Xcode Command Line Tools..."
  log_warn "Complete the dialog that appears."

  if ! xcode-select --install 2>/dev/null; then
    : # already installed — ignore error
  fi

  local waited=0
  while ! _has_xcode_cli; do
    if (( waited >= XCODE_POLL_MAX )); then
      log_err "Xcode CLI installation timed out"
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
  if ! _bootstrap_homebrew_env || ! _has_homebrew; then
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
  major=$(echo "${ver}" | cut -d. -f1)
  minor=$(echo "${ver}" | cut -d. -f2)

  if (( major > MIN_HOMEBREW_MAJOR )) \
       || (( major == MIN_HOMEBREW_MAJOR \
               && minor >= MIN_HOMEBREW_MINOR )); then
    log_ok "Homebrew version: ${ver}"
    return 0
  fi

  local -r min_ver="${MIN_HOMEBREW_MAJOR}.${MIN_HOMEBREW_MINOR}"
  log_warn "Homebrew ${ver} is too old (need >= ${min_ver})"

  if "${DRY_RUN}"; then
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

  # Return 1 intentionally so the caller (ensure_prerequisites) proceeds
  # to re-install Homebrew via _install_homebrew.
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
  log_info "Running brew bundle..."

  if brew bundle --verbose --file="${BREWFILE}"; then
    log_ok "brew bundle: complete"
  else
    log_err "brew bundle: some packages failed"
    return 1
  fi
}

ensure_prerequisites() {
  if ! _has_xcode_cli; then
    if "${DRY_RUN}"; then
      log_info "[dry-run] would install Xcode CLI"
    elif ! _install_xcode_cli; then
      return 1
    fi
  fi

  if ! _bootstrap_homebrew_env; then
    if "${DRY_RUN}"; then
      log_info "[dry-run] would install Homebrew"
    elif ! _install_homebrew; then
      return 1
    fi
  elif ! _ensure_homebrew_version; then
    if "${DRY_RUN}"; then
      log_info "[dry-run] would reinstall Homebrew"
    elif ! _install_homebrew; then
      return 1
    fi
  fi

  if ! _has_stow; then
    if "${DRY_RUN}"; then
      log_info "[dry-run] would install GNU Stow"
    elif ! _install_stow; then
      return 1
    fi
  fi
}

# -----------------------------------------------------------------------------
# Stow state queries  (pure — no side effects)
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

# Show the stow-managed symlinks for a package.  Walks up from each file
# in the package dir to find the highest symlinked ancestor, printing
# one entry per unique symlink (de-duplicated via shown[] hash).
stow_links() {
  local pkg="$1"
  local stow_dir
  stow_dir=$(pkg_stow_dir "${pkg}")

  local pkg_base
  pkg_base=$(pkg_name "${pkg}")

  local pkg_dir="${stow_dir}/${pkg_base}"
  local -A shown=()
  local src rel check

  while IFS= read -r src; do
    rel="${src#"${pkg_dir}"/}"
    check="${HOME}/${rel}"

    while [[ "${check}" != "${HOME}" ]]; do
      if [[ -L "${check}" && -z "${shown[${check}]+set}" ]]; then
        shown[${check}]=1
        printf '      %s -> %s\n' \
               "${check#"${HOME}"/}" "$(readlink "${check}")"
        break
      fi
      check="${check%/*}"
    done
  done < <(find "${pkg_dir}" -not -type d 2>/dev/null)
}

# -----------------------------------------------------------------------------
# Stow operations
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
    if "${DRY_RUN}"; then
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

  if "${DRY_RUN}"; then
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

  # --- Single stow dry-run for all decisions ---
  local -a dry_flags=(-n -v -d "${stow_dir}" -t "${HOME}")
  case "${mode}" in
    restow) dry_flags+=(-R) ;;
    unstow) dry_flags+=(-D) ;;
  esac

  local dry_rc=0
  local dry_output
  dry_output=$(stow "${dry_flags[@]}" "${pkg_base}" 2>&1) || dry_rc=$?

  # --- State check ---
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

  # --- Conflict resolution (stow/restow only) ---
  if [[ "${mode}" != unstow ]]; then
    local target
    while IFS= read -r target; do
      _resolve_stow_conflict "${target}" || return 1
    done < <(_parse_stow_targets "${dry_output}")
  fi

  # --- Dry-run display ---
  if "${DRY_RUN}"; then
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

  # --- Actual execution ---
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
# Internal helpers
# -----------------------------------------------------------------------------

# After stowing zsh, offer to change the user's default login shell.
_offer_shell_switch() {
  if ! is_stowed shell/zsh; then
    return 0
  fi

  if [[ -n "${ZSH_VERSION:-}" || "${SHELL##*/}" == zsh ]]; then
    log_info "Already running zsh"
    return 0
  fi

  if ! confirm "Switch default shell to zsh?" y; then
    log_info "Skipped — run: chsh -s \"\$(which zsh)\""
    return 0
  fi

  local zsh_path
  if ! zsh_path=$(command -v zsh 2>/dev/null); then
    log_err "zsh not in PATH"
    return 0
  fi

  if ! grep -qx "${zsh_path}" /etc/shells 2>/dev/null; then
    if echo "${zsh_path}" | sudo tee -a /etc/shells >/dev/null 2>&1; then
      log_ok "Added ${zsh_path} to /etc/shells"
    else
      log_warn "Cannot add to /etc/shells (need sudo)"
      log_info "Run: echo '${zsh_path}' | sudo tee -a /etc/shells"
      log_info "Then: chsh -s '${zsh_path}'"
      return 0
    fi
  fi

  if chsh -s "${zsh_path}" 2>/dev/null; then
    log_ok "Default shell changed to zsh — open a new terminal."
  else
    log_warn "chsh failed"
    log_info "Run: chsh -s '${zsh_path}'"
  fi
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
      --dry-run) DRY_RUN=true ;;
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

  if "${DRY_RUN}"; then
    log_info "Dry-run mode: no files will be modified."
  fi

  ensure_prerequisites

  if "${do_all}"; then
    if "${DRY_RUN}"; then
      if _has_homebrew && [[ -f "${BREWFILE}" ]]; then
        log_info "[dry-run] brew bundle preview:"
        brew bundle check --file="${BREWFILE}" 2>&1 \
          | while IFS= read -r line; do
              log_info "[dry-run]   ${line}"
            done || true
      else
        log_info "[dry-run] would run brew bundle"
      fi
    else
      if _run_brew_bundle; then
        : # success, proceed to stow
      elif ! confirm "brew bundle had failures. Continue with stow?" n; then
        log_info "Cancelled"
        return 1
      fi
    fi

    local pkg
    for pkg in "${PKG_ALL[@]}"; do
      if "${force}"; then
        restow_package "${pkg}"
      else
        stow_package "${pkg}"
      fi
    done

    if ! "${DRY_RUN}"; then
      _offer_shell_switch
    fi

    printf '\n'
    if "${DRY_RUN}"; then
      log_ok "Dry-run complete. No files were modified."
    else
      log_ok "Done. Source ~/.zshenv or open a new terminal to apply changes."
    fi
    return 0
  fi

  local -a resolved=()
  if ! validate_pkgs resolved "${pkgs[@]}"; then
    die "No valid packages specified"
  fi

  local pkg
  for pkg in "${resolved[@]}"; do
    if "${force}"; then
      restow_package "${pkg}"
    else
      stow_package "${pkg}"
    fi
  done

  if ! "${DRY_RUN}"; then
    local _pkg
    for _pkg in "${resolved[@]}"; do
      if [[ "${_pkg}" == "shell/zsh" ]]; then
        _offer_shell_switch
        break
      fi
    done
  fi
}

cmd_uninstall() {
  local do_all=false
  local -a pkgs=()
  local arg

  for arg in "$@"; do
    case "${arg}" in
      --all) do_all=true ;;
      --dry-run) DRY_RUN=true ;;
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

  if "${DRY_RUN}"; then
    log_info "Dry-run mode: no files will be modified."
  fi

  # Uninstall only needs stow — do NOT call ensure_prerequisites (which
  # would try to *install* missing tools).  Just verify stow is available.
  if ! _has_stow; then
    log_err "GNU Stow is required for uninstall but not found."
    log_info "Run: ./setup.sh install --all"
    return 1
  fi

  if "${do_all}"; then
    local -a stowed=()
    local p

    for p in "${PKG_ALL[@]}"; do
      if is_stowed "${p}"; then
        stowed+=("${p}")
      fi
    done

    if (( ${#stowed[@]} == 0 )); then
      log_info "No packages are currently stowed"
      return 0
    fi

    if "${DRY_RUN}"; then
      log_info "Would uninstall ${#stowed[@]} stowed packages:"
      for p in "${stowed[@]}"; do
        log_info "  $(pkg_name "${p}")"
      done
    elif ! confirm "Uninstall all ${#stowed[@]} stowed packages?" n; then
      log_info "Cancelled"
      return 0
    fi

    for p in "${stowed[@]}"; do
      unstow_package "${p}"
    done
    return 0
  fi

  local -a resolved=()
  if ! validate_pkgs resolved "${pkgs[@]}"; then
    return 1
  fi

  local pkg
  for pkg in "${resolved[@]}"; do
    unstow_package "${pkg}"
  done
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

  print_header "oh-my-workspace status"

  # --- Prerequisites (read-only checks) ---
  printf '  Prerequisites\n\n'

  if _has_xcode_cli; then
    _status_ok "Xcode CLI"
  else
    _status_missing "Xcode CLI"
  fi

  if _has_homebrew; then
    local brew_ver
    brew_ver=$(brew --version 2>/dev/null | head -1 | awk '{print $2}') || true
    _status_ok "Homebrew  ${brew_ver}"
  else
    _status_missing "Homebrew"
  fi

  if _has_stow; then
    local stow_ver
    stow_ver=$(stow --version 2>/dev/null | head -1 | awk '{print $NF}') || true
    _status_ok "GNU Stow  ${stow_ver}"
  else
    _status_missing "GNU Stow"
  fi

  if ! _has_stow; then
    log_warn "Cannot check package status — GNU Stow not installed."
    log_info "Run: ./setup.sh install --all"
    return 0
  fi

  local -A pkg_stowed=()
  local stowed_count=0
  local total_stowed=0
  local pkg

  for pkg in "${pkgs[@]}"; do
    if is_stowed "${pkg}"; then
      pkg_stowed[${pkg}]=1
      stowed_count=$(( stowed_count + 1 ))
    else
      pkg_stowed[${pkg}]=0
    fi
  done

  # Calculate total stowed across all packages (for subset display).
  if (( ${#pkgs[@]} < ${#PKG_ALL[@]} )); then
    total_stowed="${stowed_count}"
    local other_pkg
    for other_pkg in "${PKG_ALL[@]}"; do
      if [[ -z "${pkg_stowed[${other_pkg}]+set}" ]] \
           && is_stowed "${other_pkg}"; then
        total_stowed=$(( total_stowed + 1 ))
      fi
    done
  else
    total_stowed="${stowed_count}"
  fi

  printf '\n  Packages  (%d / %d stowed' "${stowed_count}" "${#pkgs[@]}"
  if (( ${#pkgs[@]} < ${#PKG_ALL[@]} )); then
    printf ', %d / %d total)' "${total_stowed}" "${#PKG_ALL[@]}"
  else
    printf ')'
  fi
  printf '\n'

  local pkg_base
  for pkg in "${pkgs[@]}"; do
    pkg_base=$(pkg_name "${pkg}")
    printf '\n'
    if (( pkg_stowed[${pkg}] )); then
      _status_stowed "${pkg_base}"
      stow_links "${pkg}"
    else
      _status_unstowed "${pkg_base}"
    fi
  done
  printf '\n'
}

# -----------------------------------------------------------------------------
# Help
# -----------------------------------------------------------------------------

show_help() {
  printf '%b\n' \
         "${_BOLD}oh-my-workspace setup${_RESET}" '' \
         'Usage:' \
         '  ./setup.sh <command> [flags] [packages]' '' \
         "${_BOLD}Commands:${_RESET}" \
         '  install   [--all] [--force] [--dry-run] [<pkg>...]   Stow packages' \
         '  uninstall [--all] [--dry-run] [<pkg>...]             Unstow packages' \
         '  status    [<pkg>...]                                 Show status and symlinks' \
         '  help                                                 Show this help' '' \
         "${_BOLD}Flags:${_RESET}" \
         '  --all      Apply to all packages (install / uninstall)' \
         '  --force    Restow even if already stowed (install only).' \
         '             Runs stow -R; conflicts at target paths are deleted.' \
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
         '  ./setup.sh status                           Full status with symlinks' \
         '  ./setup.sh status zsh                       Status for one package' '' \
         "${_BOLD}Note:${_RESET}" \
         '  install without packages or --all shows this help.'
}

# -----------------------------------------------------------------------------
# Entry point
# -----------------------------------------------------------------------------

main() {
  # Platform check (fast, no side effects)
  if [[ "$(uname -s)" != Darwin ]]; then
    printf 'error: macOS required\n' >&2
    exit 1
  fi

  # Parse args before any dependency installation
  if [[ $# -eq 0 ]]; then
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
