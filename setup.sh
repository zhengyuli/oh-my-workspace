#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-05-05 12:37:36 Tuesday by zhengyu.li>
#
# =============================================================================
# oh-my-workspace Setup — Dotfile Management via Symlink Mapping
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: dotfiles, symlink, homebrew, setup
# Dependencies: bash 4.3+, macOS
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   2026-04-01 13:11 zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   Manages dotfiles via declarative symlink mappings with prerequisite
#   bootstrapping (Xcode CLI, Homebrew), Homebrew bundle, and per-package
#   post-install hooks. Uses static directory/file mapping tables to
#   create and manage symlinks from the repo into $HOME.
#
# References:
#   1. XDG Base Directory Specification:
#      https://specifications.freedesktop.org/basedir-spec/latest
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
  tool/starship
  editor/vim
  editor/emacs
  terminal/ghostty
  tool/git
  tool/lazygit
  tool/ripgrep
  tool/yazi
  prog-lang/python/uv
  prog-lang/typescript/bun
)

# Two-step assign-then-seal avoids failure when the variable is
# already exported as readonly from the environment.
WORKSPACE_DIR="${WORKSPACE_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
readonly WORKSPACE_DIR

BREWFILE="${WORKSPACE_DIR}/pkg-manager/homebrew/Brewfile"
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

# Run brew bundle. Always returns 0; partial failures are warnings.
_run_brew_bundle() {
  if _run_indented brew bundle --file="${BREWFILE}"; then
    log_ok "brew bundle: done"
    return 0
  fi
  log_warn "brew bundle had failures, continuing..."
  return 0
}

# Install prerequisites: Xcode CLI → Homebrew.
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
}

# -----------------------------------------------------------------------------
# Symlink Engine
# -----------------------------------------------------------------------------

# --- Package Link Registry ---

# Each package declares its links as an array of "type:source:target" entries.
#   type   = "dir" (directory symlink) or "file" (file symlink)
#   source = relative path within the package directory ("." for package root)
#   target = absolute destination path
#
# Variable naming: _LINKS_<pkg> where slashes/hyphens become underscores.
# Engine resolves via nameref: shell/zsh → _LINKS_shell_zsh

readonly -a _LINKS_shell_zsh=(
  "dir:conf.d:${HOME}/.config/zsh/conf.d"
  "dir:functions:${HOME}/.config/zsh/functions"
  "file:zshenv:${HOME}/.zshenv"
  "file:zshrc:${HOME}/.config/zsh/.zshrc"
  "file:zprofile:${HOME}/.config/zsh/.zprofile"
)

readonly -a _LINKS_tool_starship=(
  "file:starship.toml:${HOME}/.config/starship.toml"
)

readonly -a _LINKS_editor_vim=(
  "dir:.:${HOME}/.config/vim"
)

readonly -a _LINKS_editor_emacs=(
  "dir:.:${HOME}/.config/emacs"
)

readonly -a _LINKS_terminal_ghostty=(
  "dir:.:${HOME}/.config/ghostty"
)

readonly -a _LINKS_tool_git=(
  "dir:.:${HOME}/.config/git"
)

readonly -a _LINKS_tool_lazygit=(
  "dir:.:${HOME}/.config/lazygit"
)

readonly -a _LINKS_tool_ripgrep=(
  "dir:.:${HOME}/.config/ripgrep"
)

readonly -a _LINKS_tool_yazi=(
  "dir:.:${HOME}/.config/yazi"
)

readonly -a _LINKS_prog_lang_python_uv=(
  "dir:.:${HOME}/.config/uv"
)

readonly -a _LINKS_prog_lang_typescript_bun=(
  "file:bunfig.toml:${HOME}/.config/.bunfig.toml"
)

# --- Conflict Resolution ---

# Check recursively if a directory contains only workspace-owned content.
# A directory is "all ours" if every entry is either a symlink pointing into
# WORKSPACE_DIR, or a subdirectory that is itself "all ours".
# Arguments:
#   $1 - Absolute path of directory to check.
# Returns:
#   0 if all contents belong to this workspace, 1 otherwise.
_dir_all_ours() {
  local -r dir="$1"
  local item

  for item in "${dir}"/* "${dir}"/.*; do
    case "$(basename "${item}")" in
      .|..) continue ;;
    esac
    if [[ ! -e "${item}" && ! -L "${item}" ]]; then
      continue
    fi

    if [[ -L "${item}" ]]; then
      local link_target
      link_target="$(readlink "${item}")"
      if [[ "${link_target}" != "${WORKSPACE_DIR}"/* ]]; then
        return 1
      fi
    elif [[ -d "${item}" ]]; then
      if ! _dir_all_ours "${item}"; then
        return 1
      fi
    else
      return 1
    fi
  done
  return 0
}

# Resolve a conflicting file before linking.
# Handles foreign symlinks (remove), missing files (no-op),
# non-empty dirs (refuse), and regular files (back up to
# *.pre-link-backup with incrementing suffix).
# Arguments:
#   $1 - Absolute path of the conflicting file.
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
  local canonical_home
  canonical_home="$(cd "${HOME}" 2>/dev/null && pwd -P)" || true
  if [[ -z "${canonical}" || "${canonical}" != "${canonical_home}"/* ]]; then
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
      # Check if all contents are workspace-owned (symlinks to us or dirs of symlinks)
      if _dir_all_ours "${target}"; then
        if "${dry_run}"; then
          log_info "[dry-run] would replace directory of symlinks: ${target}"
          return 0
        fi
        rm -rf "${target}"
        return 0
      fi

      log_warn "Non-empty directory with non-workspace content: ${target}"
      # Back up the directory (same logic as file backup below)
    fi
  fi

  if "${dry_run}"; then
    log_info "[dry-run] would back up and remove conflicting path: ${target}"
    return 0
  fi

  local backup="${target}.pre-link-backup"
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

# --- Link Helpers ---

# Create a single symlink from source to destination.
# Handles conflict detection, dry-run mode, and force mode.
# Arguments:
#   $1 - Absolute source path (in the repo).
#   $2 - Absolute destination path (in HOME).
#   $3 - Force flag ("true" or "false").
# Returns:
#   0 on success, 1 on failure.
_create_link() {
  local -r src="$1" dest="$2" force="$3"

  if [[ -L "${dest}" ]]; then
    local current
    current="$(readlink "${dest}")"

    if [[ "${current}" == "${src}" ]]; then
      if "${force}"; then
        if "${dry_run}"; then
          log_info "[dry-run]   relink: ${dest}"
          return 0
        fi
        rm -f "${dest}"
        ln -sf "${src}" "${dest}"
      fi
      return 0
    fi

    if ! _resolve_conflict "${dest}"; then
      return 1
    fi
  elif [[ -e "${dest}" ]]; then
    if ! _resolve_conflict "${dest}"; then
      return 1
    fi
  fi

  if "${dry_run}"; then
    log_info "[dry-run]   link: ${dest} -> ${src}"
    return 0
  fi

  # Remove broken directory-level symlinks blocking mkdir (stow migration).
  local parent
  parent="$(dirname "${dest}")"
  local path_component="${parent}"
  while [[ "${path_component}" != "/" && "${path_component}" != "${HOME}" ]]; do
    if [[ -L "${path_component}" && ! -e "${path_component}" ]]; then
      rm -f "${path_component}"
      break
    fi
    path_component="$(dirname "${path_component}")"
  done

  mkdir -p "${parent}"
  ln -sf "${src}" "${dest}"
}

# Resolve the link registry array name for a package.
# Converts "shell/zsh" → "_LINKS_shell_zsh", "prog-lang/python/uv" → "_LINKS_prog_lang_python_uv".
# Arguments:
#   $1 - Package path (e.g., "shell/zsh").
# Output:
#   Prints the array variable name.
_pkg_links_var() {
  local name="$1"
  name="${name//\//_}"
  name="${name//-/_}"
  printf '_LINKS_%s' "${name}"
}

# Collect all links for a package from its registry array.
# Populates two parallel arrays: source paths and dest paths.
# Arguments:
#   $1 - Full package path (e.g., "shell/zsh").
# Globals:
#   _LINK_SRCS (write) - Array of absolute source paths.
#   _LINK_DESTS (write) - Array of absolute destination paths.
# Returns:
#   0 if at least one link found, 1 if package has no registry.
_collect_links() {
  local -r pkg="$1"
  _LINK_SRCS=()
  _LINK_DESTS=()

  local var_name
  var_name="$(_pkg_links_var "${pkg}")"

  local -n links="${var_name}" 2>/dev/null || return 1
  if (( ${#links[@]} == 0 )); then
    return 1
  fi

  local entry type rel_path target src_abs
  for entry in "${links[@]}"; do
    type="${entry%%:*}"
    local rest="${entry#*:}"
    rel_path="${rest%%:*}"
    target="${rest#*:}"

    if [[ "${rel_path}" == "." ]]; then
      src_abs="${WORKSPACE_DIR}/${pkg}"
    else
      src_abs="${WORKSPACE_DIR}/${pkg}/${rel_path}"
    fi

    case "${type}" in
      dir)
        if [[ -d "${src_abs}" ]]; then
          _LINK_SRCS+=("${src_abs}")
          _LINK_DESTS+=("${target}")
        fi
        ;;
      file)
        if [[ -f "${src_abs}" ]]; then
          _LINK_SRCS+=("${src_abs}")
          _LINK_DESTS+=("${target}")
        fi
        ;;
    esac
  done

  (( ${#_LINK_SRCS[@]} > 0 ))
}

# --- Public Interface ---

# Test whether a package is currently linked.
# Checks that all expected symlinks exist and point to correct targets.
# Arguments:
#   $1 - Full package path (e.g., "shell/zsh").
# Returns:
#   0 if fully linked, 1 otherwise.
is_linked() {
  local -r pkg="$1"
  local -a _LINK_SRCS _LINK_DESTS

  if ! _collect_links "${pkg}"; then
    return 1
  fi

  local i
  for (( i = 0; i < ${#_LINK_SRCS[@]}; i++ )); do
    local dest="${_LINK_DESTS[${i}]}"
    local src="${_LINK_SRCS[${i}]}"

    if [[ ! -L "${dest}" ]]; then
      return 1
    fi

    if [[ "$(readlink "${dest}")" != "${src}" ]]; then
      return 1
    fi
  done

  return 0
}

# Link a package: create symlinks (directory-level or file-level).
# Skips if already fully linked (idempotent).
# Arguments:
#   $1 - Full package path (e.g., "tool/git").
# Returns:
#   0 on success, 1 on failure.
link_package() {
  local -r pkg="$1"
  local pkg_base
  pkg_base=$(pkg_name "${pkg}")
  local -a _LINK_SRCS _LINK_DESTS

  if ! _collect_links "${pkg}"; then
    log_err "${pkg_base}: no linkable files found"
    return 1
  fi

  # Check if already linked
  if is_linked "${pkg}"; then
    log_info "${pkg_base}: already linked"
    return 0
  fi

  if "${dry_run}"; then
    log_info "[dry-run] would link: ${pkg_base}"
  fi

  local i fail=false
  for (( i = 0; i < ${#_LINK_SRCS[@]}; i++ )); do
    if ! _create_link "${_LINK_SRCS[${i}]}" "${_LINK_DESTS[${i}]}" false; then
      fail=true
    fi
  done

  if "${fail}"; then
    log_err "${pkg_base}: link failed"
    return 1
  fi

  if ! "${dry_run}"; then
    log_ok "${pkg_base}: linked"
  fi
}

# Relink a package: force re-creation of all symlinks.
# Arguments:
#   $1 - Full package path.
# Returns:
#   0 on success, 1 on failure.
relink_package() {
  local -r pkg="$1"
  local pkg_base
  pkg_base=$(pkg_name "${pkg}")
  local -a _LINK_SRCS _LINK_DESTS

  if ! _collect_links "${pkg}"; then
    log_err "${pkg_base}: no linkable files found"
    return 1
  fi

  if "${dry_run}"; then
    log_info "[dry-run] would relink: ${pkg_base}"
  fi

  local i fail=false
  for (( i = 0; i < ${#_LINK_SRCS[@]}; i++ )); do
    if ! _create_link "${_LINK_SRCS[${i}]}" "${_LINK_DESTS[${i}]}" true; then
      fail=true
    fi
  done

  if "${fail}"; then
    log_err "${pkg_base}: relink failed"
    return 1
  fi

  if ! "${dry_run}"; then
    log_ok "${pkg_base}: relinked"
  fi
}

# Unlink a package: remove symlinks pointing to this repo.
# For directory-level links, removes the single directory symlink.
# For file-level links, removes the file symlink.
# Arguments:
#   $1 - Full package path.
# Returns:
#   0 on success, 1 on failure.
unlink_package() {
  local -r pkg="$1"
  local pkg_base
  pkg_base=$(pkg_name "${pkg}")
  local -a _LINK_SRCS _LINK_DESTS

  if ! _collect_links "${pkg}"; then
    log_warn "${pkg_base}: no linkable files, skipping"
    return 0
  fi

  local i unlinked=0
  for (( i = 0; i < ${#_LINK_SRCS[@]}; i++ )); do
    local dest="${_LINK_DESTS[${i}]}"
    local src="${_LINK_SRCS[${i}]}"

    if [[ ! -L "${dest}" ]]; then
      continue
    fi

    if [[ "$(readlink "${dest}")" != "${src}" ]]; then
      continue
    fi

    if "${dry_run}"; then
      log_info "[dry-run]   unlink: ${dest}"
      (( unlinked += 1 ))
      continue
    fi

    rm -f "${dest}"
    (( unlinked += 1 ))

    # Clean empty parent dirs up to HOME
    local parent
    parent="$(dirname "${dest}")"
    while [[ "${parent}" != "${HOME}" && "${parent}" != "/" ]]; do
      if [[ -d "${parent}" && -z "$(ls -A "${parent}")" ]]; then
        rmdir "${parent}"
        parent="$(dirname "${parent}")"
      else
        break
      fi
    done
  done

  if (( unlinked == 0 )); then
    log_warn "${pkg_base}: not linked, skipping"
    return 0
  fi

  if "${dry_run}"; then
    log_info "[dry-run] would unlink ${unlinked} links from ${pkg_base}"
  else
    log_ok "${pkg_base}: unlinked (${unlinked} links)"
  fi
}

# -----------------------------------------------------------------------------
# Post-Install Hooks
# -----------------------------------------------------------------------------

# --- Shell (zsh) ---

# Set default shell to zsh if not already active.
# Returns:
#   0 on success, 1 on failure, 2 if skipped.
_post_install_shell_zsh() {
  if ! is_linked shell/zsh; then
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

# Install yazi plugins via ya pack.
# Plugin list kept in sync with tool/yazi/init.lua.
# Returns:
#   0 on success, 1 on failure, 2 if skipped.
_post_install_yazi() {
  if ! is_linked tool/yazi; then
    return 2
  fi

  if ! command -v ya >/dev/null 2>&1; then
    return 2
  fi

  local -ra _YAZI_PLUGINS=(
    "yazi-rs/plugins:smart-enter"
    "yazi-rs/plugins:smart-filter"
    "yazi-rs/plugins:full-border"
    "yazi-rs/plugins:toggle-pane"
    "yazi-rs/plugins:zoom"
    "yazi-rs/plugins:git"
  )
  local -ra _YAZI_FLAVORS=(
    "yazi-rs/flavors:catppuccin-mocha"
  )

  local pkg output
  for pkg in "${_YAZI_PLUGINS[@]}" "${_YAZI_FLAVORS[@]}"; do
    output="$(ya pkg add "${pkg}" 2>&1)" || {
      # "already exists" is not a real error — idempotent
      if [[ "${output}" == *"already exists"* ]]; then
        continue
      fi
      log_err "yazi: failed to add ${pkg}"
      return 1
    }
  done

  if ! ya pkg install >/dev/null 2>&1; then
    log_err "yazi plugin install failed"
    return 1
  fi
  return 0
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
    tool/starship) printf 'starship:starship' ;;
    editor/vim) printf 'vim:vim' ;;
    editor/emacs) printf 'emacs:emacs' ;;
    terminal/ghostty)
      printf 'ghostty:ghostty:/Applications/Ghostty.app/Contents/MacOS/ghostty'
      ;;
    tool/git) printf 'git:git' ;;
    tool/lazygit) printf 'lazygit:lazygit' ;;
    tool/ripgrep) printf 'rg:rg' ;;
    tool/yazi) printf 'yazi:yazi' ;;
    prog-lang/python/uv) printf 'uv:uv' ;;
    prog-lang/typescript/bun) printf 'bun:bun' ;;
    *) return 1 ;;
  esac
}

# Verify each linked package's binary is reachable.
_run_health_check() {
  _phase "Health Check"
  local pkg cmd_name friendly fallback_path resolved

  for pkg in "$@"; do
    if ! is_linked "${pkg}"; then
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

# Link or relink a single package based on the force flag.
_link_one() {
  local -r pkg="$1" force="$2"
  if "${force}"; then
    relink_package "${pkg}"
  else
    link_package "${pkg}"
  fi
}

# Command: install packages.
# Handles prerequisites, brew bundle, linking, post-install
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

    _phase "Homebrew Bundle"
    if "${dry_run}"; then
      log_info "[dry-run] would run brew bundle"
    else
      _run_brew_bundle
    fi

    _phase "Link Packages"
    local pkg
    local fail_count=0
    for pkg in "${PKG_ALL[@]}"; do
      if ! _link_one "${pkg}" "${force}"; then
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

  _phase "Link Packages"
  local pkg
  local fail_count=0
  for pkg in "${_VALIDATED_PKGS[@]}"; do
    if ! _link_one "${pkg}" "${force}"; then
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

  local -a target_pkgs=()
  if "${do_all}"; then
    local p
    for p in "${PKG_ALL[@]}"; do
      if is_linked "${p}"; then
        target_pkgs+=("${p}")
      fi
    done

    if (( ${#target_pkgs[@]} == 0 )); then
      log_info "Nothing linked"
      return 0
    fi

    if ! "${dry_run}"; then
      log_warn "This will unlink ${#target_pkgs[@]} packages: ${target_pkgs[*]}"
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

  _phase "Unlink Packages"
  local pkg
  local fail_count=0
  for pkg in "${target_pkgs[@]}"; do
    if ! unlink_package "${pkg}"; then
      (( fail_count += 1 ))
    fi
  done

  return $(( fail_count > 0 ? 1 : 0 ))
}

# Command: display prerequisite health and link status.
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

  _phase_total=2
  _phase "Packages"
  local pkg

  for pkg in "${pkgs[@]}"; do
    if is_linked "${pkg}"; then
      log_ok "${pkg}: linked"
    else
      log_warn "${pkg}: not linked"
    fi
  done
}

cmd_help() {
  cat <<EOF
${_BOLD}oh-my-workspace setup${_RESET}

Usage:
  ./setup.sh <command> [flags] [packages]

${_BOLD}Commands:${_RESET}
  install   [--all] [--force] [--dry-run] [<pkg>...]   Link packages
  uninstall [--all] [--dry-run] [<pkg>...]             Unlink packages
  status    [<pkg>...]                                 Show link status
  help                                                 Show this help

${_BOLD}Flags:${_RESET}
  --all      Apply to all packages (install / uninstall)
  --force    Relink even if already linked (install only).
             Conflicting files are backed up to *.pre-link-backup.
             Use after adding new dotfiles to a package dir.
  --dry-run  Preview changes; brew bundle is skipped, nothing is linked/unlinked

${_BOLD}Packages${_RESET} (base name or full category/name):
  shell:       zsh
  editor:      vim  emacs
  terminal:    ghostty
  tool:        git  lazygit  ripgrep  starship  yazi
  prog-lang:   uv  bun

${_BOLD}Examples:${_RESET}
  ./setup.sh install --all                    Prereqs + brew + link all packages
  ./setup.sh install zsh git                  Link specific packages
  ./setup.sh install --force zsh              Relink (pick up new dotfiles)
  ./setup.sh install --force --all            Relink everything
  ./setup.sh install --dry-run zsh            Preview what install would do
  ./setup.sh install --force --dry-run --all  Preview a full relink
  ./setup.sh uninstall --all                  Unlink all
  ./setup.sh uninstall --dry-run zsh          Preview what uninstall would do
  ./setup.sh status                           Full link status
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
