#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-20 00:45:00 Thursday by zhengyu.li>
# =============================================================================
# oh-my-dotfiles Setup Script
#
# Copyright (C) 2026 zhengyu.li
# Author: zhengyu.li <lizhengyu419@outlook.com>
#
# Location: ~/oh-my-dotfiles/setup.sh  (not stowed; run directly)
# References:
#   1. GNU Stow Manual: https://www.gnu.org/software/stow/manual/
#   2. Homebrew Bundle: https://github.com/Homebrew/homebrew-bundle
#
# Usage:
#   install   [--all | <pkg>...]  [--dry-run]
#   uninstall [--all | <pkg>...] [--dry-run]
#   update    [--all | --pkgs | <pkg>...] [--dry-run]
#   restore   <pkg>...
#   status    [<pkg>...]
#   defaults
#
# Design layers (each layer only calls layers below it):
#   cmd_*       one function per CLI subcommand
#   do_*        workflow helpers; orchestrate operations
#   operations  stow / backup / restore / prerequisites  (side effects)
#   queries     is_stowed, stow_targets, has_*  (pure)
#   data        PKG_ALL, paths, constants
#
# stow -n -v is used throughout to simulate what stow would do, so all
# ignore rules, folding logic, and edge cases are handled by stow itself
# rather than reimplemented here.
#
# Conflict policy (install / update):
#   target absent             -> stow directly
#   target is our symlink     -> skip (install) or restow (update)
#   target is foreign symlink -> mv to .backups/<pkg>/
#   target is real file/dir   -> mv to .backups/<pkg>/
#   mv acts on the link itself, so backup and restore are fully symmetric.
#
# Backup layout:   .backups/<pkg>/<basename>.bak.0  .bak.1  ...
# Restore policy:  mv the highest-numbered backup back to its original path.
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

DRY_RUN=false

MAX_BACKUPS=5
NETWORK_RETRIES=3
NETWORK_TIMEOUT=60

# zsh must come first - subsequent packages rely on XDG env vars it sets.
PKG_ALL=(zsh git vim emacs ghostty ripgrep uv bun starship)

DOTFILES_DIR="${DOTFILES_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
BREWFILE="${DOTFILES_DIR}/homebrew/Brewfile"
DEFAULTS_SCRIPT="${DOTFILES_DIR}/macos/defaults.sh"
BACKUP_DIR="${DOTFILES_DIR}/.backups"

if [[ -z "${SETUP_SH_TEST_MODE:-}" ]]; then
    readonly MAX_BACKUPS NETWORK_RETRIES NETWORK_TIMEOUT
    readonly PKG_ALL
    readonly DOTFILES_DIR BREWFILE DEFAULTS_SCRIPT BACKUP_DIR
fi

# -----------------------------------------------------------------------------
# Output helpers
# -----------------------------------------------------------------------------

readonly _R='\033[0;31m' _G='\033[0;32m' _Y='\033[0;33m'
readonly _B='\033[0;34m' _W='\033[1m' _0='\033[0m'

die()      { printf "  ${_R}[error]${_0} %s\n" "$*" >&2; exit 1; }
log_ok()   { printf "  ${_G}[ok]${_0}    %s\n" "$*"; }
log_err()  { printf "  ${_R}[error]${_0} %s\n" "$*" >&2; }
log_warn() { printf "  ${_Y}[warn]${_0}  %s\n" "$*"; }
log_info() { printf "  ${_B}[info]${_0}  %s\n" "$*"; }

print_header() {
    printf '\n%b' "${_W}"
    printf '=%.0s' {1..79}
    printf '\n  %s\n' "$1"
    printf '=%.0s' {1..79}
    printf '\n%b\n' "${_0}"
}

confirm() {
    local prompt="$1" default="${2:-n}" reply
    [[ "$default" == y ]] \
        && printf '\n  %s [Y/n]: ' "$prompt" \
        || printf '\n  %s [y/N]: ' "$prompt"
    read -r reply
    [[ "${reply:-$default}" =~ ^[Yy]$ ]]
}

# -----------------------------------------------------------------------------
# Dry-run wrapper
# -----------------------------------------------------------------------------

run() {
    if [[ "$DRY_RUN" == true ]]; then
        log_info "[dry-run] $*"
        return 0
    fi
    "$@"
}

# -----------------------------------------------------------------------------
# Queries  (pure - no side effects)
# -----------------------------------------------------------------------------

is_valid_pkg() {
    local p
    for p in "${PKG_ALL[@]}"; do [[ "$1" == "$p" ]] && return 0; done
    return 1
}

# Print the absolute $HOME paths that stow would create for pkg.
# Walks the package directory to find all files/dirs that would be linked.
# This works even when stow reports conflicts (where stow -n -v shows no LINK lines).
stow_targets() {
    local pkg="$1" dir="${DOTFILES_DIR}/${pkg}" rel
    [[ -d "$dir" ]] || return 1
    # Find all non-directory entries in the package
    while IFS= read -r -d '' rel; do
        # Convert package-relative path to $HOME-relative path
        # e.g., "zsh/.zshenv" -> ".zshenv", "starship/.config/starship.toml" -> ".config/starship.toml"
        echo "${HOME}/${rel#${pkg}/}"
    done < <(cd "$DOTFILES_DIR" && find "$pkg" -type f -print0 2>/dev/null)
    # Also include directories that would be folded (stow creates dir symlinks for leaf dirs)
    while IFS= read -r -d '' rel; do
        echo "${HOME}/${rel#${pkg}/}"
    done < <(cd "$DOTFILES_DIR" && find "$pkg" -type d -empty -print0 2>/dev/null)
}

# Returns 0 when stow reports nothing to do for pkg - meaning every symlink
# stow would create already exists and points to the right place.
# Must also check for conflicts - stow outputs no LINK lines in both cases.
is_stowed() {
    local output
    output=$(stow -n -v -d "$DOTFILES_DIR" -t "$HOME" "$1" 2>&1)
    # No LINK lines AND no conflict warnings
    ! grep -q "^LINK:" <<< "$output" && ! grep -q "conflicts\|cannot stow" <<< "$output"
}

# Count backup files for pkg under BACKUP_DIR/<pkg>/.
backup_count() {
    local dir="${BACKUP_DIR}/${1}"
    [[ -d "$dir" ]] || { echo 0; return; }
    find "$dir" -maxdepth 1 -name '*.bak.*' | wc -l | tr -d ' '
}

stowed_count() {
    local n=0 p
    for p in "${PKG_ALL[@]}"; do is_stowed "$p" && (( n++ )) || true; done
    echo "$n"
}

has_xcode_cli() { xcode-select -p &>/dev/null; }
has_homebrew()  { command -v brew &>/dev/null; }
has_stow()      { command -v stow &>/dev/null; }

# Validate package names from "$@"; set _valid_pkgs to the valid subset.
# Warns about unknown names. Returns 1 if no valid packages remain.
validate_pkgs() {
    _valid_pkgs=()
    local p
    for p in "$@"; do
        is_valid_pkg "$p" && _valid_pkgs+=("$p") || log_warn "Unknown package: ${p}"
    done
    (( ${#_valid_pkgs[@]} > 0 ))
}

# Set _stowed to every package in PKG_ALL that is currently stowed.
collect_stowed() {
    _stowed=()
    local p
    for p in "${PKG_ALL[@]}"; do is_stowed "$p" && _stowed+=("$p") || true; done
}

# -----------------------------------------------------------------------------
# Operations: backup and restore
# -----------------------------------------------------------------------------
#
# mv acts on symlinks themselves (not their destinations), so a foreign symlink
# and a regular file are handled identically. Restore is always lossless.

backup_file() {
    local target="$1" pkg="$2"
    [[ -e "$target" || -L "$target" ]] || return 0

    local dir="${BACKUP_DIR}/${pkg}"
    local base; base=$(basename "$target")

    run mkdir -p "$dir"

    local n=0
    while [[ -e "${dir}/${base}.bak.${n}" || -L "${dir}/${base}.bak.${n}" ]]; do
        (( n++ ))
    done

    if (( n >= MAX_BACKUPS )); then
        local i
        for (( i = 0; i <= n - MAX_BACKUPS; i++ )); do
            { [[ -e "${dir}/${base}.bak.${i}" ]] \
                || [[ -L "${dir}/${base}.bak.${i}" ]]; } \
                && run rm -f "${dir}/${base}.bak.${i}" || true
        done
    fi

    log_warn "Backing up: ${target} -> ${dir}/${base}.bak.${n}"
    run mv "$target" "${dir}/${base}.bak.${n}"
}

restore_file() {
    local target="$1" pkg="$2"
    local dir="${BACKUP_DIR}/${pkg}"
    local base; base=$(basename "$target")

    local n=0 max=-1
    while [[ -e "${dir}/${base}.bak.${n}" || -L "${dir}/${base}.bak.${n}" ]]; do
        max=$n; (( n++ ))
    done

    (( max >= 0 )) || {
        log_err "No backup found for $(basename "$target") in ${dir}"
        return 1
    }

    log_info "Restoring: ${dir}/${base}.bak.${max} -> ${target}"
    run mv "${dir}/${base}.bak.${max}" "$target"
}

# -----------------------------------------------------------------------------
# Operations: stow
# -----------------------------------------------------------------------------

stow_package() {
    local pkg="$1"

    [[ -d "${DOTFILES_DIR}/${pkg}" ]] \
        || { log_err "Package directory not found: ${DOTFILES_DIR}/${pkg}"; return 1; }

    is_stowed "$pkg" && { log_info "${pkg}: already stowed, skipping"; return 0; }

    # Use stow's own dry-run to enumerate targets; back up anything blocking.
    local target
    while IFS= read -r target; do
        if [[ -L "$target" ]]; then
            log_warn "Foreign symlink: ${target} -> $(readlink "$target")"
            backup_file "$target" "$pkg"
        elif [[ -e "$target" ]]; then
            backup_file "$target" "$pkg"
        fi
    done < <(stow_targets "$pkg")

    run mkdir -p "${HOME}/.config"

    if run stow -d "$DOTFILES_DIR" -t "$HOME" "$pkg"; then
        log_ok "${pkg}: stowed"
    else
        log_err "${pkg}: stow failed"
        return 1
    fi
}

restow_package() {
    local pkg="$1"
    if run stow -R -d "$DOTFILES_DIR" -t "$HOME" "$pkg"; then
        log_ok "${pkg}: restowed"
    else
        log_err "${pkg}: restow failed"
        return 1
    fi
}

unstow_package() {
    local pkg="$1"
    if ! is_stowed "$pkg"; then
        log_warn "${pkg}: not stowed, skipping"
        return 0
    fi
    if run stow -D -d "$DOTFILES_DIR" -t "$HOME" "$pkg"; then
        log_ok "${pkg}: unstowed"
    else
        log_err "${pkg}: unstow failed"
        return 1
    fi
}

# -----------------------------------------------------------------------------
# Operations: prerequisites
# -----------------------------------------------------------------------------

install_xcode_cli() {
    has_xcode_cli && { log_ok "Xcode CLI: already installed"; return 0; }
    log_info "Installing Xcode Command Line Tools..."
    log_warn "A dialog will appear - complete it, then press any key here."
    xcode-select --install
    read -r -n 1 -s
    has_xcode_cli || { log_err "Xcode CLI installation failed"; return 1; }
    log_ok "Xcode CLI: installed"
}

install_homebrew() {
    has_homebrew && { log_ok "Homebrew: already installed"; return 0; }
    log_info "Installing Homebrew..."
    local url="https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"
    local i
    for (( i = 1; i <= NETWORK_RETRIES; i++ )); do
        (( i > 1 )) && { log_warn "Retry ${i}/${NETWORK_RETRIES}..."; sleep 5; }
        curl --fail --silent --show-error \
             --connect-timeout "$NETWORK_TIMEOUT" \
             "$url" | /bin/bash && break
        (( i == NETWORK_RETRIES )) && {
            log_err "Homebrew installation failed after ${NETWORK_RETRIES} attempts"
            return 1
        }
    done
    local b
    for b in /opt/homebrew/bin/brew /usr/local/bin/brew; do
        [[ -x "$b" ]] && eval "$("$b" shellenv)" && break
    done
    has_homebrew || { log_err "Homebrew not found after installation"; return 1; }
    log_ok "Homebrew: installed"
}

run_brew_bundle() {
    [[ -f "$BREWFILE" ]] || die "Brewfile not found: $BREWFILE"
    log_info "Running brew bundle..."
    if run brew bundle --file="$BREWFILE"; then
        log_ok "brew bundle: complete"
    else
        log_err "brew bundle: some packages failed"
        return 1
    fi
}

# mode=install  install anything missing
# mode=check    report missing and return 1
ensure_prerequisites() {
    local mode="${1:-check}" ok=true

    if ! has_xcode_cli; then
        [[ "$mode" == install ]] \
            && { install_xcode_cli || ok=false; } \
            || { log_err "Xcode CLI missing - run: ./setup.sh install --all"; ok=false; }
    fi
    if ! has_homebrew; then
        [[ "$mode" == install ]] \
            && { install_homebrew || ok=false; } \
            || { log_err "Homebrew missing - run: ./setup.sh install --all"; ok=false; }
    fi
    if ! has_stow; then
        [[ "$mode" == install ]] \
            && { run_brew_bundle || ok=false; } \
            || { log_err "GNU Stow missing - run: ./setup.sh install --all"; ok=false; }
    fi

    "$ok" || return 1
    has_stow || die "GNU Stow unavailable after installation"
}

# -----------------------------------------------------------------------------
# Workflow helpers (do_*)
# -----------------------------------------------------------------------------

do_install_pkgs() {
    local pkg
    for pkg in "$@"; do stow_package "$pkg"; done
    printf '\n'
    log_ok "Done - run 'source ~/.zshenv' or open a new terminal to apply changes."
}

do_uninstall_pkgs() {
    local pkg
    for pkg in "$@"; do unstow_package "$pkg"; done
}

do_update_pkgs() {
    local pkg
    for pkg in "$@"; do
        is_stowed "$pkg" \
            && restow_package "$pkg" \
            || log_warn "${pkg}: not stowed, skipping"
    done
}

# Unstow pkg (if stowed), then restore the highest-numbered backup for each
# target. restore_file returns 1 with an error if no backup exists.
do_restore_pkg() {
    local pkg="$1"

    is_stowed "$pkg" && {
        log_info "${pkg}: unstowing before restore..."
        run stow -D -d "$DOTFILES_DIR" -t "$HOME" "$pkg"
    }

    # After unstow, stow -n -v shows what would be linked - i.e. the original
    # target paths - which is exactly what we need to restore backups to.
    local target
    while IFS= read -r target; do
        if [[ -e "$target" || -L "$target" ]]; then
            log_err "${target} is occupied - remove it manually before restoring"
            return 1
        fi
        restore_file "$target" "$pkg" || return 1
    done < <(stow_targets "$pkg")

    log_ok "${pkg}: restored"
}

do_offer_shell_switch() {
    is_stowed zsh               || return 0
    [[ -z "${ZSH_VERSION:-}" ]] || { log_info "Already running zsh"; return 0; }

    confirm "Switch default shell to zsh?" y || {
        log_info "Skipped - run 'chsh -s \$(which zsh)' manually later."
        return 0
    }

    local zsh_path
    zsh_path=$(command -v zsh) || { log_err "zsh not found in PATH"; return 1; }

    if ! grep -qF "$zsh_path" /etc/shells 2>/dev/null; then
        log_info "Adding ${zsh_path} to /etc/shells..."
        if [[ "$DRY_RUN" == true ]]; then
            log_info "[dry-run] echo ${zsh_path} | sudo tee -a /etc/shells"
        else
            echo "$zsh_path" | sudo tee -a /etc/shells > /dev/null \
                || { log_err "Failed to write /etc/shells"; return 1; }
        fi
    fi

    if run chsh -s "$zsh_path"; then
        log_ok "Default shell changed to zsh - open a new terminal to use it."
    else
        log_err "chsh failed"
        return 1
    fi
}

# -----------------------------------------------------------------------------
# Commands (cmd_*)
# -----------------------------------------------------------------------------

cmd_install() {
    local do_all=false
    local -a pkgs=()

    for arg in "$@"; do
        case "$arg" in
            --all)     do_all=true ;;
            --dry-run) DRY_RUN=true ;;
            -*)        die "Unknown flag: $arg" ;;
            *)         pkgs+=("$arg") ;;
        esac
    done

    if "$do_all"; then
        (( ${#pkgs[@]} == 0 )) || die "--all and package names are mutually exclusive"
        ensure_prerequisites install
        run_brew_bundle
        do_install_pkgs "${PKG_ALL[@]}"
        do_offer_shell_switch
        printf '\n'; log_ok "Full installation complete"
        return
    fi

    (( ${#pkgs[@]} > 0 )) || { show_help; return 0; }
    validate_pkgs "${pkgs[@]}" || die "No valid packages specified"
    ensure_prerequisites check
    do_install_pkgs "${_valid_pkgs[@]}"
}

cmd_uninstall() {
    local do_all=false
    local -a pkgs=()

    for arg in "$@"; do
        case "$arg" in
            --all)     do_all=true ;;
            --dry-run) DRY_RUN=true ;;
            -*)        die "Unknown flag: $arg" ;;
            *)         pkgs+=("$arg") ;;
        esac
    done

    if "$do_all"; then
        (( ${#pkgs[@]} == 0 )) || die "--all and package names are mutually exclusive"
        collect_stowed
        (( ${#_stowed[@]} > 0 )) || { log_info "No packages are currently stowed"; return 0; }
        confirm "Uninstall all ${#_stowed[@]} stowed packages?" n \
            || { log_info "Cancelled"; return 0; }
        do_uninstall_pkgs "${_stowed[@]}"
        return
    fi

    (( ${#pkgs[@]} > 0 )) || {
        log_err "No packages specified"
        log_info "Usage: ./setup.sh uninstall [--all | <pkg>...] [--dry-run]"
        return 1
    }
    validate_pkgs "${pkgs[@]}" || return 1
    do_uninstall_pkgs "${_valid_pkgs[@]}"
}

cmd_update() {
    local do_all=false do_pkgs=false
    local -a pkgs=()

    for arg in "$@"; do
        case "$arg" in
            --all)     do_all=true ;;
            --pkgs)    do_pkgs=true ;;
            --dry-run) DRY_RUN=true ;;
            -*)        die "Unknown flag: $arg" ;;
            *)         pkgs+=("$arg") ;;
        esac
    done

    if "$do_all"; then
        (( ${#pkgs[@]} == 0 )) || die "--all and package names are mutually exclusive"
        ensure_prerequisites install
        run_brew_bundle
        collect_stowed
        (( ${#_stowed[@]} > 0 )) \
            && do_update_pkgs "${_stowed[@]}" \
            || log_info "No packages are currently stowed"
        log_ok "Update complete"
        return
    fi

    if "$do_pkgs"; then
        (( ${#pkgs[@]} == 0 )) || die "--pkgs and package names are mutually exclusive"
        ensure_prerequisites check
        collect_stowed
        (( ${#_stowed[@]} > 0 )) \
            && do_update_pkgs "${_stowed[@]}" \
            || log_info "No packages are currently stowed"
        return
    fi

    (( ${#pkgs[@]} > 0 )) || {
        log_err "No target specified"
        log_info "Usage: ./setup.sh update [--all | --pkgs | <pkg>...] [--dry-run]"
        return 1
    }
    validate_pkgs "${pkgs[@]}" || return 1
    ensure_prerequisites check
    do_update_pkgs "${_valid_pkgs[@]}"
}

cmd_restore() {
    (( $# > 0 )) || {
        log_err "No packages specified"
        log_info "Usage: ./setup.sh restore <pkg>..."
        return 1
    }
    local p
    for p in "$@"; do
        is_valid_pkg "$p" \
            && do_restore_pkg "$p" \
            || log_warn "Unknown package: ${p}"
    done
}

cmd_status() {
    local -a pkgs=("${PKG_ALL[@]}")
    (( $# > 0 )) && pkgs=("$@")

    print_header "oh-my-dotfiles status"

    printf '  Prerequisites\n\n'
    has_xcode_cli \
        && printf "  ${_G}ok${_0}  Xcode CLI\n" \
        || printf "  ${_R}--${_0}  Xcode CLI\n"
    has_homebrew \
        && printf "  ${_G}ok${_0}  Homebrew   %s\n" \
               "$(brew --version 2>/dev/null | head -1 | awk '{print $2}')" \
        || printf "  ${_R}--${_0}  Homebrew\n"
    has_stow \
        && printf "  ${_G}ok${_0}  GNU Stow   %s\n" \
               "$(stow --version 2>/dev/null | head -1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+')" \
        || printf "  ${_R}--${_0}  GNU Stow\n"

    printf '\n  Packages  (%d / %d stowed)\n\n' "$(stowed_count)" "${#PKG_ALL[@]}"
    printf '  %-12s  %s  %s\n' "package" "stow" "backups"
    printf '  %-12s  %s  %s\n' "-------" "----" "-------"

    local pkg
    for pkg in "${pkgs[@]}"; do
        printf '  %-12s  ' "$pkg"
        is_stowed "$pkg" \
            && printf "${_G}ok${_0}" \
            || printf "${_Y}--${_0}"
        printf '  %d\n' "$(backup_count "$pkg")"
    done
}

cmd_defaults() {
    [[ -f "$DEFAULTS_SCRIPT" ]] \
        || { log_err "defaults.sh not found: $DEFAULTS_SCRIPT"; return 1; }
    log_warn "This will modify system preferences. Some changes require logout or restart."
    confirm "Apply macOS defaults?" n || { log_info "Skipped"; return 0; }
    bash "$DEFAULTS_SCRIPT" && log_ok "macOS defaults applied"
}

# -----------------------------------------------------------------------------
# Help
# -----------------------------------------------------------------------------

show_help() {
    cat <<'EOF'
oh-my-dotfiles setup

Usage:
  ./setup.sh <command> [options]

Commands:
  install   [--all | <pkg>...]  [--dry-run]
  uninstall [--all | <pkg>...] [--dry-run]
  update    [--all | --pkgs | <pkg>...] [--dry-run]
  restore   <pkg>...
  status    [<pkg>...]
  defaults
  help, -h

install:
  --all      Prerequisites + brew bundle + all packages + offer shell switch.
  <pkg>...   Stow specific packages. Prerequisites must already be present.

uninstall:
  --all      Unstow all currently stowed packages (prompts for confirmation).
  <pkg>...   Unstow specific packages.

update:
  --all      ensure prerequisites + brew bundle + restow all stowed packages.
  --pkgs     Restow all currently stowed packages; skip prerequisites/brew.
  <pkg>...   Restow specific packages.

restore:
  <pkg>...   Unstow pkg if stowed, then restore the most recent backup from
             .backups/<pkg>/. Refuses if the target path is already occupied.

status:
  [<pkg>...] Show prerequisite versions and stow/backup status per package.
             Defaults to all packages.

defaults:
  Apply macOS system preferences from macos/defaults.sh (prompts first).

Options:
  --dry-run  Print what would be done without executing anything.
             Supported by: install, uninstall, update.

Packages:
  zsh  git  vim  emacs  ghostty  ripgrep  uv  bun  starship

For more information: https://github.com/zhengyuli/oh-my-dotfiles
EOF
}

# -----------------------------------------------------------------------------
# Entry point
# -----------------------------------------------------------------------------

main() {
    [[ $# -eq 0 ]] && { show_help; exit 0; }
    local cmd="$1"; shift
    case "$cmd" in
        install)        cmd_install   "$@" ;;
        uninstall)      cmd_uninstall "$@" ;;
        update)         cmd_update    "$@" ;;
        restore)        cmd_restore   "$@" ;;
        status)         cmd_status    "$@" ;;
        defaults)       cmd_defaults ;;
        help|-h|--help) show_help ;;
        *) log_err "Unknown command: ${cmd}"; printf '\n'; show_help; exit 1 ;;
    esac
}

main "$@"
