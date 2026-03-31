#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-31 21:47:27 Tuesday by zhengyu.li>
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
  printf "  ${_RED}[error]${_RESET} %s\n" "$*" >&2;
  exit 1;
}

_misuse() {
  printf "  ${_RED}[error]${_RESET} %s\n" "$*" >&2;
  exit 2;
}

log_ok() {
  printf "  ${_GREEN}[ok]${_RESET} %s\n"    "$*";
}

log_err() {
  printf "  ${_RED}[error]${_RESET} %s\n"   "$*" >&2;
}

log_warn() {
  printf "  ${_YELLOW}[warn]${_RESET} %s\n" "$*";
}

log_info() {
  printf "  ${_BLUE}[info]${_RESET} %s\n"   "$*";
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

# -----------------------------------------------------------------------------
# Package path helpers
# -----------------------------------------------------------------------------

pkg_category() {
  printf '%s' "${1%/*}";
}

pkg_name() {
  printf '%s' "${1##*/}";
}

pkg_stow_dir() {
  printf '%s/%s' "${WORKSPACE_DIR}" "$(pkg_category "$1")";
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

validate_pkgs() {
  local -n _out="$1"
  shift
  _out=()
  local p found pkg

  for p in "$@"; do
    found=''
    if _is_valid_pkg "${p}"; then
      _out+=("${p}")
      continue
    fi

    for pkg in "${PKG_ALL[@]}"; do
      if [[ "$(pkg_name "${pkg}")" == "${p}" ]]; then
        _out+=("${pkg}")
        found=1
        break
      fi
    done

    if [[ -z "${found}" ]]; then
      local suggestion=''
      for pkg in "${PKG_ALL[@]}"; do
        if [[ "${pkg}" == */"${p}" ]]; then
          suggestion="${pkg}"
          break
        fi
      done

      if [[ -n "${suggestion}" ]]; then
        log_warn "Unknown package: ${p} — did you mean: ${suggestion}?"
      else
        log_warn "Unknown package: ${p}"
      fi
    fi
  done

  (( ${#_out[@]} > 0 ))
}

# -----------------------------------------------------------------------------
# Prerequisites — probes  (pure, no side effects)
# -----------------------------------------------------------------------------

_has_xcode_cli() {
  xcode-select -p &>/dev/null;
}

_has_homebrew() {
  command -v brew  &>/dev/null;
}

_has_stow() {
  command -v stow  &>/dev/null;
}

# -----------------------------------------------------------------------------
# Prerequisites — bootstrap
# -----------------------------------------------------------------------------

_bootstrap_homebrew_env() {
  if _has_homebrew; then
    return 0
  fi

  local b
  for b in /opt/homebrew/bin/brew /usr/local/bin/brew; do
    if [[ -x "${b}" ]]; then
      source <("${b}" shellenv)
      return 0
    fi
  done

  return 1
}

# -----------------------------------------------------------------------------
# Prerequisites — install
# -----------------------------------------------------------------------------

_install_xcode_cli() {
  log_info "Installing Xcode Command Line Tools..."
  log_warn "Complete the dialog that appears, then press any key here."

  xcode-select --install
  if ! read -r -n 1 -s; then
    :
  fi

  if ! _has_xcode_cli; then
    log_err "Xcode CLI installation failed"
    return 1
  fi

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

_ensure_homebrew_version() {
  local ver
  ver=$(brew --version 2>/dev/null | head -1 | awk '{print $2}')

  local major minor
  major=$(echo "${ver}" | cut -d. -f1)
  minor=$(echo "${ver}" | cut -d. -f2)

  if (( major > MIN_HOMEBREW_MAJOR )) \
     || (( major == MIN_HOMEBREW_MAJOR && minor >= MIN_HOMEBREW_MINOR )); then
    log_ok "Homebrew version: ${ver}"
    return 0
  fi

  log_warn "Homebrew ${ver} is too old (need >= ${MIN_HOMEBREW_MAJOR}.${MIN_HOMEBREW_MINOR})"
  log_info "Removing old Homebrew..."

  local -r uninstall_url='https://raw.githubusercontent.com/Homebrew/install/HEAD/uninstall.sh'
  if ! curl --fail --silent --show-error --connect-timeout "${NETWORK_TIMEOUT}" \
       "${uninstall_url}" | /bin/bash; then
    log_err "Homebrew uninstall failed"
    return 1
  fi

  hash -r
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
    if ! _install_xcode_cli; then
      return 1;
    fi
  fi

  if ! _bootstrap_homebrew_env; then
    # No brew found — fresh install
    if ! _install_homebrew; then
      return 1;
    fi
  elif ! _ensure_homebrew_version; then
    # Brew found but too old — uninstalled, now reinstall
    if ! _install_homebrew; then
      return 1;
    fi
  fi
  _bootstrap_homebrew_env

  if ! _has_stow; then
    if ! _install_stow; then
      return 1;
    fi
  fi
}

# -----------------------------------------------------------------------------
# Stow state queries  (pure — no side effects)
# -----------------------------------------------------------------------------

is_stowed() {
  local output

  if ! output=$(stow -n -v -d "$(pkg_stow_dir "$1")" \
                     -t "${HOME}" "$(pkg_name "$1")" 2>&1); then
    return 1
  fi

  if grep -q '^LINK:' <<< "${output}"; then
    return 1
  fi
}

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

# Parse stow dry-run output for conflict/link targets.
_parse_stow_targets() {
  local output="$1"
  local -r link_pat='^LINK: ([^[:space:]]+)'
  local -r conflict_pat='existing target[^:]+: ([^[:space:]]+)'
  local line

  while IFS= read -r line; do
    if [[ "${line}" =~ ${link_pat} || "${line}" =~ ${conflict_pat} ]]; then
      printf '%s\n' "${HOME}/${BASH_REMATCH[1]}"
    fi
  done <<< "${output}"
}

_resolve_stow_conflict() {
  local -r target="$1"

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

  if "${DRY_RUN}"; then
    log_info "[dry-run] would remove conflicting path: ${target}"
    return 0
  fi

  if [[ "${target}" != "${HOME}"/* ]]; then
    log_err "Refusing to remove path outside HOME: ${target}"
    return 1
  fi

  log_warn "Removing conflicting path: ${target}"
  rm -rf "${target}"
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
  if [[ "${mode}" == unstow ]] \
      && ! grep -q '^UNLINK:' <<< "${dry_output}"; then
    log_warn "${pkg_base}: not stowed, skipping"
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
        " — will be created after removal)"
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
    return 1
  fi

  if chsh -s "${zsh_path}" 2>/dev/null; then
    log_ok "Default shell changed to zsh — open a new terminal."
  else
    log_warn "chsh failed — zsh may not be in /etc/shells"
    log_info "Run: echo '${zsh_path}' | sudo tee -a /etc/shells"
    log_info "Then: chsh -s '${zsh_path}'"
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

  if "${DRY_RUN}"; then
    log_info "Dry-run mode: no files will be modified."
  fi

  ensure_prerequisites

  if "${do_all}"; then
    if ! "${DRY_RUN}"; then
      _run_brew_bundle
    fi

    local pkg
    for pkg in "${PKG_ALL[@]}"; do
      if "${force}"; then
        restow_package "${pkg}"
      else
        stow_package "${pkg}"
      fi
    done

    if ! "${force}" && ! "${DRY_RUN}"; then
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

  if (( ${#pkgs[@]} == 0 )); then
    show_help
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

  if "${DRY_RUN}"; then
    log_info "Dry-run mode: no files will be modified."
  fi

  ensure_prerequisites

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
    if ! confirm "Uninstall all ${#stowed[@]} stowed packages?" n; then
      log_info "Cancelled"
      return 0
    fi
    for p in "${stowed[@]}"; do
      unstow_package "${p}"
    done
    return 0
  fi

  if (( ${#pkgs[@]} == 0 )); then
    log_err "No packages specified"
    log_info "Usage: ./setup.sh uninstall [--all | <pkg>...]"
    return 1
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

  ensure_prerequisites

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

  printf '  Prerequisites\n\n'
  printf "  ${_GREEN}ok${_RESET}  Xcode CLI\n"
  printf "  ${_GREEN}ok${_RESET}  Homebrew  %s\n" \
    "$(brew --version 2>/dev/null | head -1 | awk '{print $2}')"
  printf "  ${_GREEN}ok${_RESET}  GNU Stow  %s\n" \
    "$(stow --version 2>/dev/null | head -1 | awk '{print $NF}')"

  local -A pkg_stowed=()
  local stowed_count=0
  local pkg
  for pkg in "${PKG_ALL[@]}"; do
    if is_stowed "${pkg}"; then
      pkg_stowed[${pkg}]=1
      stowed_count=$(( stowed_count + 1 ))
    else
      pkg_stowed[${pkg}]=0
    fi
  done

  printf '\n  Packages  (%d / %d stowed)\n' "${stowed_count}" "${#PKG_ALL[@]}"

  if (( ${#pkgs[@]} < ${#PKG_ALL[@]} )); then
    printf '  (showing %d of %d)\n' "${#pkgs[@]}" "${#PKG_ALL[@]}"
  fi

  local pkg_base
  for pkg in "${pkgs[@]}"; do
    pkg_base=$(pkg_name "${pkg}")
    printf '\n'
    if (( pkg_stowed[${pkg}] )); then
      printf "  %s ${_GREEN}[stowed]${_RESET}:\n" "${pkg_base}"
      stow_links "${pkg}"
    else
      printf "  %s ${_YELLOW}[unstowed]${_RESET}:\n" "${pkg_base}"
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
