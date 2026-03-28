#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-25 15:21:05 Wednesday by zhengyu.li>
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
# Bootstrap checks
# -----------------------------------------------------------------------------

# Require bash 4.3+ for local -n (nameref) and associative arrays.
if (( BASH_VERSINFO[0] < 4 ||
      ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] < 3 ) )); then
  sed "s/BASH_VERSION_PLACEHOLDER/${BASH_VERSION}/" >&2 <<'EOF'
error: bash 4.3 or later required

  Current version: BASH_VERSION_PLACEHOLDER

  Why upgrade?
    • Security: bash 3.2 (macOS default) is from 2007 with known vulnerabilities
    • Features: nameref, associative arrays, better glob patterns
    • Performance: bash 5.x is significantly faster
    • Compatibility: bash 5.x runs virtually all bash 3.2 scripts

  Install modern bash via Homebrew:
    brew install bash

  Then re-run this script in a new terminal.
EOF
  exit 1
fi

# This script targets macOS only (Xcode CLI, Homebrew).
if [[ "$(uname -s)" != Darwin ]]; then
  printf 'error: macOS required\n' >&2
  exit 1
fi

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

readonly NETWORK_TIMEOUT=60

# Format: <category>/<package>  category dirs organize configs by type
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

# Google style: declaration and assignment must be separate when the value comes
# from a command substitution, so that a failure in $() is not masked by the
# exit status of readonly itself.
# cd ... && pwd is the standard idiom for resolving a directory's absolute path
# inside $(); replacing it with if/else would require a subshell-safe helper
# that changes cwd, making it significantly more complex for no practical gain.
WORKSPACE_DIR="${WORKSPACE_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
readonly WORKSPACE_DIR

BREWFILE="${WORKSPACE_DIR}/pkg/homebrew/Brewfile"
readonly BREWFILE

# Mutable script-wide flag set by --dry-run.
# Read by _remove_stow_conflicts and _stow_exec.
DRY_RUN=false

# -----------------------------------------------------------------------------
# Error handling
# -----------------------------------------------------------------------------

readonly _RED='\033[0;31m'
readonly _GREEN='\033[0;32m'
readonly _YELLOW='\033[0;33m'
readonly _BLUE='\033[0;34m'
readonly _BOLD='\033[1m'
readonly _RESET='\033[0m'

_err_handler() {
  local -r code=$?
  printf "  ${_RED}[error]${_RESET} unexpected failure in %s() at line %d (exit %d)\n" \
    "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "${code}" >&2
}
trap '_err_handler' ERR

# -----------------------------------------------------------------------------
# Logging
# -----------------------------------------------------------------------------

die() { printf "  ${_RED}[error]${_RESET} %s\n" "$*" >&2; exit 1; }

log_ok() { printf "  ${_GREEN}[ok]${_RESET} %s\n" "$*"; }

log_err() { printf "  ${_RED}[error]${_RESET} %s\n" "$*" >&2; }

log_warn() { printf "  ${_YELLOW}[warn]${_RESET} %s\n" "$*"; }

log_info() { printf "  ${_BLUE}[info]${_RESET} %s\n" "$*"; }

print_header() {
  printf '\n%b' "${_BOLD}"
  printf '=%.0s' {1..79}
  printf '\n  %s\n' "$1"
  printf '=%.0s' {1..79}
  printf '\n%b\n' "${_RESET}"
}

# Prompts the user for a yes/no confirmation.
# Arguments:
#   $1 - prompt text
#   $2 - default answer: "y" or "n" (default: "n")
# Returns:
#   0 if user answered yes, 1 otherwise
confirm() {
  local prompt="$1"
  local default="${2:-n}"
  local reply

  if [[ "${default}" == y ]]; then
    printf '\n  %s [Y/n]: ' "${prompt}"
  else
    printf '\n  %s [y/N]: ' "${prompt}"
  fi
  # read returns non-zero on EOF (non-interactive stdin); treat as default.
  if ! read -r reply; then
    reply=''
  fi
  [[ "${reply:-${default}}" =~ ^[Yy]$ ]]
}

# -----------------------------------------------------------------------------
# Package path helpers
# -----------------------------------------------------------------------------

# Returns the category portion of a package path ("shell/zsh" -> "shell").
pkg_category() { printf '%s' "${1%/*}"; }

# Returns the name portion of a package path ("shell/zsh" -> "zsh").
pkg_name() { printf '%s' "${1##*/}"; }

# Returns the stow directory for a package ("shell/zsh" -> "$WORKSPACE_DIR/shell").
pkg_stow_dir() { printf '%s/%s' "${WORKSPACE_DIR}" "$(pkg_category "$1")"; }

# Returns 0 if the given string matches a known package path in PKG_ALL.
# Arguments: $1 - package path to test
_is_valid_pkg() {
  local p
  for p in "${PKG_ALL[@]}"; do
    if [[ "$1" == "${p}" ]]; then
      return 0
    fi
  done
  return 1
}

# Resolves package name arguments to full category/name paths.
# Accepts both "shell/zsh" and bare "zsh" forms. Warns on unknowns.
# Uses nameref so the caller's own array is populated directly, avoiding
# a global output variable.
# Arguments:
#   $1   - name of caller's output array variable (nameref)
#   $2.. - package names to resolve
# Returns:
#   0 if at least one package resolved, 1 if none
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
      log_warn "Unknown package: ${p}"
    fi
  done
  (( ${#_out[@]} > 0 ))
}

# -----------------------------------------------------------------------------
# Stow state queries  (pure - no side effects)
# -----------------------------------------------------------------------------

# Returns 0 if pkg is fully stowed (stow -n reports nothing to do).
# Returns 1 if stow is absent or links/conflicts exist.
is_stowed() {
  _has_stow || return 1
  stow -n -d "$(pkg_stow_dir "$1")" -t "${HOME}" "$(pkg_name "$1")" &>/dev/null
}

# Prints $HOME-absolute paths that stow would touch when linking pkg.
# mode=stow    uses stow -n -v     (for packages not yet stowed)
# mode=restow  uses stow -R -n -v  (for --force: detects new conflicts that
#                                   arise after old links are removed)
# Parses LINK and "existing target" lines from stow dry-run output.
# Arguments:
#   $1 - full package path
#   $2 - mode: "stow" (default) or "restow"
# Outputs: one absolute $HOME path per line
stow_targets() {
  local pkg="$1"
  local mode="${2:-stow}"
  local stow_dir
  stow_dir=$(pkg_stow_dir "${pkg}")
  local pkg_base
  pkg_base=$(pkg_name "${pkg}")
  local -a flags=(-n -v -d "${stow_dir}" -t "${HOME}")
  if [[ "${mode}" == restow ]]; then
    flags+=(-R)
  fi

  local -r link_pat='^LINK: ([^[:space:]]+)'
  local -r conflict_pat='existing target ([^[:space:]]+)'
  local output line
  output=$(stow "${flags[@]}" "${pkg_base}" 2>&1) || true
  while IFS= read -r line; do
    if [[ "${line}" =~ ${link_pat} || "${line}" =~ ${conflict_pat} ]]; then
      printf '%s\n' "${HOME}/${BASH_REMATCH[1]}"
    fi
  done <<< "${output}"
}

# Prints the symlinks in $HOME that currently belong to pkg.
# Walks the package dir tree and traces each file's $HOME path upward to find
# its symlink, correctly handling stow tree-folding (whole subtree collapsed
# into one directory symlink).
# Process substitution (< <(find)) keeps the while loop in the current shell
# so the associative array deduplication remains visible throughout.
# Arguments: $1 - full package path
# Outputs: relative path and symlink target, two lines per link
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
    rel="${src#${pkg_dir}/}"
    check="${HOME}/${rel}"
    while [[ "${check}" != "${HOME}" ]]; do
      if [[ -L "${check}" && -z "${shown[${check}]+set}" ]]; then
        shown[${check}]=1
        printf '      %s -> %s\n' \
          "${check#${HOME}/}" "$(readlink "${check}")"
        break
      fi
      check="${check%/*}"
    done
  done < <(find "${pkg_dir}" -not -type d 2>/dev/null)
}

# -----------------------------------------------------------------------------
# Conflict resolution
# -----------------------------------------------------------------------------

# Removes files or symlinks that would block stow from linking pkg.
# In dry-run mode, logs what would be removed without touching the filesystem.
# Globals:
#   DRY_RUN (read)
# Arguments:
#   $1 - full package path
#   $2 - stow_targets mode: "stow" (default) or "restow"
_remove_stow_conflicts() {
  local pkg="$1"
  local mode="${2:-stow}"
  local target

  while IFS= read -r target; do
    if [[ -L "${target}" ]]; then
      if "${DRY_RUN}"; then
        log_info "[dry-run] would remove foreign symlink: ${target} -> $(readlink "${target}")"
      else
        log_warn "Removing foreign symlink: ${target} -> $(readlink "${target}")"
        rm -f "${target}"
      fi
    elif [[ -e "${target}" ]]; then
      if "${DRY_RUN}"; then
        log_info "[dry-run] would remove conflicting path: ${target}"
      else
        log_warn "Removing conflicting path: ${target}"
        rm -rf "${target}"
      fi
    fi
  done < <(stow_targets "${pkg}" "${mode}")
}

# -----------------------------------------------------------------------------
# Stow operations
# -----------------------------------------------------------------------------

# Runs stow (or stow -R for restow mode) for pkg after clearing any conflicts.
# In dry-run mode, prints planned LINK/UNLINK actions without modifying
# the filesystem.
# Globals:
#   DRY_RUN (read)
# Arguments:
#   $1 - full package path
#   $2 - mode: "stow" (default) or "restow"
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

  _remove_stow_conflicts "${pkg}" "${mode}"

  # Pre-create ~/.config so stow links individual items inside it rather
  # than folding the whole directory into a single symlink.
  if ! "${DRY_RUN}"; then
    mkdir -p "${HOME}/.config"
  fi

  local -a flags=(-d "${stow_dir}" -t "${HOME}")
  if [[ "${mode}" == restow ]]; then
    flags+=(-R)
  fi

  if "${DRY_RUN}"; then
    log_info "[dry-run] would ${mode}: ${pkg_base}"
    local output line
    output=$(stow -n -v "${flags[@]}" "${pkg_base}" 2>&1) || true
    while IFS= read -r line; do
      if [[ "${line}" =~ ^(LINK|UNLINK): ]]; then
        log_info "[dry-run]   ${line}"
      fi
    done <<< "${output}"
    return 0
  fi

  local action
  if [[ "${mode}" == restow ]]; then
    action='restowed'
  else
    action='stowed'
  fi

  if stow "${flags[@]}" "${pkg_base}"; then
    log_ok "${pkg_base}: ${action}"
  else
    log_err "${pkg_base}: ${mode} failed"
    return 1
  fi
}

# Stows pkg if not already stowed.
# Arguments: $1 - full package path
stow_package() {
  if is_stowed "$1"; then
    log_info "$(pkg_name "$1"): already stowed"
    return 0
  fi
  _stow_exec "$1" stow
}

# Restows pkg unconditionally (remove existing links, then re-link).
# Arguments: $1 - full package path
restow_package() {
  _stow_exec "$1" restow
}

# Removes stow symlinks for pkg.
# Arguments: $1 - full package path
unstow_package() {
  local pkg="$1"
  local stow_dir
  stow_dir=$(pkg_stow_dir "${pkg}")
  local pkg_base
  pkg_base=$(pkg_name "${pkg}")

  if ! is_stowed "${pkg}"; then
    log_warn "${pkg_base}: not stowed, skipping"
    return 0
  fi
  if stow -D -d "${stow_dir}" -t "${HOME}" "${pkg_base}"; then
    log_ok "${pkg_base}: unstowed"
  else
    log_err "${pkg_base}: unstow failed"
    return 1
  fi
}

# -----------------------------------------------------------------------------
# Prerequisites
# -----------------------------------------------------------------------------

_has_xcode_cli() { xcode-select -p &>/dev/null; }
_has_homebrew() { command -v brew &>/dev/null; }
_has_stow() { command -v stow &>/dev/null; }

_install_xcode_cli() {
  if _has_xcode_cli; then
    log_ok "Xcode CLI: already installed"
    return 0
  fi
  log_info "Installing Xcode Command Line Tools..."
  log_warn "Complete the dialog that appears, then press any key here."
  xcode-select --install
  # read returns non-zero on EOF (non-interactive stdin); treat as default.
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
  if _has_homebrew; then
    log_ok "Homebrew: already installed"
    return 0
  fi
  log_info "Installing Homebrew..."
  # Note: curl | bash is the official Homebrew installation method.
  # Security: URL is hardcoded HTTPS to Homebrew's official GitHub repo.
  local -r url='https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh'
  if ! curl --fail --silent --show-error \
      --connect-timeout "${NETWORK_TIMEOUT}" "${url}" | /bin/bash; then
    log_err "Homebrew installation failed"
    return 1
  fi
  # Note: eval is required per Homebrew's official post-install instructions.
  # Security: paths are hardcoded to official Homebrew locations.
  local b
  for b in /opt/homebrew/bin/brew /usr/local/bin/brew; do
    if [[ -x "${b}" ]]; then
      eval "$("${b}" shellenv)"
      break
    fi
  done
  if ! _has_homebrew; then
    log_err "Homebrew not found after installation"
    return 1
  fi
  log_ok "Homebrew: installed"
}

_install_stow() {
  if _has_stow; then
    log_ok "GNU Stow: already installed"
    return 0
  fi
  log_info "Installing GNU Stow..."
  if brew install stow; then
    log_ok "GNU Stow: installed"
  else
    log_err "GNU Stow installation failed"
    return 1
  fi
}

_run_brew_bundle() {
  if [[ ! -f "${BREWFILE}" ]]; then
    die "Brewfile not found: ${BREWFILE}"
  fi
  log_info "Running brew bundle..."
  if brew bundle --verbose --file="${BREWFILE}"; then
    log_ok "brew bundle: complete"
  else
    log_err "brew bundle: some packages failed"
    return 1
  fi
}

# Ensures Xcode CLI, Homebrew, and GNU Stow are present.
# In install mode, installs any missing tools and short-circuits on failure.
# In check mode, reports all missing tools before returning 1.
# Arguments: $1 - mode: "install" or "check" (default)
ensure_prerequisites() {
  local mode="${1:-check}"
  local ok=1

  if ! _has_xcode_cli; then
    if [[ "${mode}" == install ]]; then
      if ! _install_xcode_cli; then return 1; fi
    else
      log_err "Xcode CLI missing"
      ok=0
    fi
  fi
  if ! _has_homebrew; then
    if [[ "${mode}" == install ]]; then
      if ! _install_homebrew; then return 1; fi
    else
      log_err "Homebrew missing"
      ok=0
    fi
  fi
  if ! _has_stow; then
    if [[ "${mode}" == install ]]; then
      if ! _install_stow; then return 1; fi
    else
      log_err "GNU Stow missing"
      ok=0
    fi
  fi

  if (( ! ok )); then
    log_info "Run: ./setup.sh install --all"
    return 1
  fi
  # Sanity-check: stow must be reachable after installation.
  if ! _has_stow; then
    die "GNU Stow unavailable after installation"
  fi
}

# -----------------------------------------------------------------------------
# Commands
# -----------------------------------------------------------------------------

# Stows one or more packages.
#   --all      Stow all packages (mutually exclusive with <pkg>)
#   --force    Restow even if already stowed (picks up new dotfiles)
#   --dry-run  Preview what would change without modifying anything
# Globals:
#   DRY_RUN (write)
# Arguments: parsed from "$@"
cmd_install() {
  local do_all=false
  local force=false
  local -a pkgs=()
  local arg

  for arg in "$@"; do
    case "${arg}" in
      --all)     do_all=true ;;
      --force)   force=true ;;
      --dry-run) DRY_RUN=true ;;
      -*)        die "Unknown flag: ${arg}" ;;
      *)         pkgs+=("${arg}") ;;
    esac
  done

  if "${do_all}"; then
    if (( ${#pkgs[@]} > 0 )); then
      die "--all and package names are mutually exclusive"
    fi
  fi

  if "${DRY_RUN}"; then
    log_info "Dry-run mode: no files will be modified."
  fi

  if "${do_all}"; then
    # In dry-run mode, only check prerequisites (do not install anything);
    # stow must already be present for stow -n -v calls to work.
    if "${DRY_RUN}"; then
      ensure_prerequisites check
    else
      ensure_prerequisites install
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
    # Only offer shell switch on first-time full install, not on restow or
    # dry-run (dry-run must not modify the login shell).
    if ! "${force}"; then
      if ! "${DRY_RUN}"; then
        _offer_shell_switch
      fi
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
  ensure_prerequisites check
  local pkg
  for pkg in "${resolved[@]}"; do
    if "${force}"; then
      restow_package "${pkg}"
    else
      stow_package "${pkg}"
    fi
  done
}

# Unstows one or more packages.
#   --all  Unstow all currently stowed packages (mutually exclusive with <pkg>)
# Arguments: parsed from "$@"
cmd_uninstall() {
  local do_all=false
  local -a pkgs=()
  local arg

  for arg in "$@"; do
    case "${arg}" in
      --all) do_all=true ;;
      -*)    die "Unknown flag: ${arg}" ;;
      *)     pkgs+=("${arg}") ;;
    esac
  done

  if "${do_all}"; then
    if (( ${#pkgs[@]} > 0 )); then
      die "--all and package names are mutually exclusive"
    fi
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
  ensure_prerequisites check
  local pkg
  for pkg in "${resolved[@]}"; do
    unstow_package "${pkg}"
  done
}

# Displays prerequisite status and stow state for packages.
# Optionally filters to specific packages; defaults to all.
# Arguments: parsed from "$@"
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

  printf '  Prerequisites\n\n'
  if _has_xcode_cli; then
    printf "  ${_GREEN}ok${_RESET}  Xcode CLI\n"
  else
    printf "  ${_RED}--${_RESET}  Xcode CLI\n"
  fi
  if _has_homebrew; then
    printf "  ${_GREEN}ok${_RESET}  Homebrew  %s\n" \
      "$(brew --version 2>/dev/null | head -1 | awk '{print $2}')"
  else
    printf "  ${_RED}--${_RESET}  Homebrew\n"
  fi
  if _has_stow; then
    printf "  ${_GREEN}ok${_RESET}  GNU Stow  %s\n" \
      "$(stow --version 2>/dev/null | head -1 | awk '{print $NF}')"
  else
    printf "  ${_RED}--${_RESET}  GNU Stow\n"
  fi

  # Single pass: run is_stowed() once per package to avoid a second redundant
  # stow -n -v call in the display loop below.
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

  local pkg_base
  for pkg in "${pkgs[@]}"; do
    pkg_base=$(pkg_name "${pkg}")
    printf '\n'
    if (( pkg_stowed[${pkg}] )); then
      printf "  %s ${_GREEN}[stowed]${_RESET}:\n" "${pkg_base}"
      stow_links "${pkg}"
    else
      printf "  %s ${_YELLOW}[------]${_RESET}:\n" "${pkg_base}"
    fi
  done
  printf '\n'
}

# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

# Offers to switch the login shell to zsh after a successful full install.
# No-op if zsh is already the active shell or if shell/zsh was not stowed.
_offer_shell_switch() {
  if ! is_stowed shell/zsh; then
    return 0
  fi
  if [[ -n "${ZSH_VERSION:-}" || "${SHELL##*/}" == zsh ]]; then
    log_info "Already running zsh"
    return 0
  fi
  if ! confirm "Switch default shell to zsh?" y; then
    log_info "Skipped — run: chsh -s \$(which zsh)"
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
# Help
# -----------------------------------------------------------------------------

show_help() {
  cat <<'EOF'
oh-my-workspace setup

Usage:
  ./setup.sh <command> [flags] [packages]

Commands:
  install   [--all] [--force] [--dry-run] [<pkg>...]   Stow packages
  uninstall [--all] [<pkg>...]                         Unstow packages
  status    [<pkg>...]                                 Show status and symlinks
  help                                                 Show this help

Flags:
  --all      Apply to all packages (install / uninstall)
  --force    Restow even if already stowed (install only)
  --dry-run  Preview stow changes; brew bundle is skipped, nothing is linked

Packages (base name or full category/name):
  zsh  starship  vim  emacs  ghostty  git  lazygit  ripgrep  uv  bun

Examples:
  ./setup.sh install --all                    Bootstrap: prereqs + brew + stow all
  ./setup.sh install zsh git                  Stow specific packages
  ./setup.sh install --force zsh              Restow zsh (pick up new dotfiles)
  ./setup.sh install --force --all            Restow everything
  ./setup.sh install --dry-run zsh            Preview what install would do
  ./setup.sh install --force --dry-run --all  Preview a full restow
  ./setup.sh uninstall --all                  Unstow all
  ./setup.sh status                           Full status with symlinks
  ./setup.sh status zsh                       Status for one package

Details:
  install --all
    Installs Xcode CLI + Homebrew + GNU Stow, runs brew bundle,
    stows all packages, offers to switch default shell to zsh.

  install --force
    Runs stow -R (remove + re-link). Conflicting files or symlinks
    at target paths are deleted before stowing. Use after adding new
    dotfiles to a package dir.
EOF
}

# -----------------------------------------------------------------------------
# Entry point
# -----------------------------------------------------------------------------

main() {
  if [[ $# -eq 0 ]]; then
    show_help
    exit 0
  fi
  local cmd="$1"
  shift
  case "${cmd}" in
    install)        cmd_install "$@" ;;
    uninstall)      cmd_uninstall "$@" ;;
    status)         cmd_status "$@" ;;
    help|-h|--help) show_help ;;
    *)
      log_err "Unknown command: ${cmd}"
      printf '\n'
      show_help
      exit 1
      ;;
  esac
}

main "$@"
