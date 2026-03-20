#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-20 00:00:00 Friday by zhengyu.li>
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

die() { printf "  ${_R}[error]${_0} %s\n" "$*" >&2; exit 1; }
log_ok() { printf "  ${_G}[ok]${_0} %s\n" "$*"; }
log_err() { printf "  ${_R}[error]${_0} %s\n" "$*" >&2; }
log_warn() { printf "  ${_Y}[warn]${_0} %s\n" "$*"; }
log_info() { printf "  ${_B}[info]${_0} %s\n" "$*"; }

print_header() {
    printf '\n%b' "${_W}"
    printf '=%.0s' {1..79}
    printf '\n  %s\n' "$1"
    printf '=%.0s' {1..79}
    printf '\n%b\n' "${_0}"
}

confirm() {
    local prompt="$1" default="${2:-n}" reply
    if [[ "$default" == y ]]; then
        printf '\n  %s [Y/n]: ' "$prompt"
    else
        printf '\n  %s [y/N]: ' "$prompt"
    fi
    # Guard against EOF (non-interactive stdin) treating it as the default.
    read -r reply || reply=""
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
# Delegates entirely to the stow binary so that all ignore rules (.stow-local-ignore,
# built-in defaults), tree-folding, and edge cases are handled natively.
# Parses two output patterns from stow -n -v:
#   LINK: <rel> => <src>           - target stow would create (no conflict)
#   existing target ...: <rel>     - occupied target stow cannot overwrite (conflict)
# Both cases are targets we need to handle; covering them enables backup_file
# to clear blockers before the real stow run.
stow_targets() {
    local pkg="$1"
    [[ -d "${DOTFILES_DIR}/${pkg}" ]] || return 1
    local link_pat='^LINK: ([^[:space:]]+)'
    local conflict_pat='existing target [^:]+: (.+)$'
    stow -n -v -d "$DOTFILES_DIR" -t "$HOME" "$pkg" 2>&1 \
        | while IFS= read -r line; do
            if [[ "$line" =~ $link_pat ]]; then
                echo "${HOME}/${BASH_REMATCH[1]}"
            elif [[ "$line" =~ $conflict_pat ]]; then
                echo "${HOME}/${BASH_REMATCH[1]}"
            fi
        done
}

# Returns 0 when stow reports nothing to do for pkg - meaning every symlink
# stow would create already exists and points to the right place.
# Must also check for conflicts - stow outputs no LINK lines in both cases.
# || true prevents set -e from aborting on stow non-zero exit (e.g. conflicts).
is_stowed() {
    local output
    output=$(stow -n -v -d "$DOTFILES_DIR" -t "$HOME" "$1" 2>&1) || true
    if grep -q "^LINK:" <<< "$output"; then
        return 1
    fi
    if grep -q "conflicts\|cannot stow" <<< "$output"; then
        return 1
    fi
    return 0
}

# Count backup files for pkg under BACKUP_DIR/<pkg>/.
backup_count() {
    local dir="${BACKUP_DIR}/${1}"
    if [[ ! -d "$dir" ]]; then
        echo 0
        return
    fi
    find "$dir" -maxdepth 1 -name '*.bak.*' | wc -l | tr -d ' '
}

stowed_count() {
    local n=0 p
    for p in "${PKG_ALL[@]}"; do
        if is_stowed "$p"; then
            # Use prefix increment: (( ++n )) always evaluates non-zero,
            # avoiding the set -e trap that (( n++ )) hits when n is 0.
            (( ++n ))
        fi
    done
    echo "$n"
}

has_xcode_cli() { xcode-select -p &>/dev/null; }
has_homebrew() { command -v brew &>/dev/null; }
has_stow() { command -v stow &>/dev/null; }

# Validate package names from "$@"; set _valid_pkgs to the valid subset.
# Warns about unknown names. Returns 1 if no valid packages remain.
validate_pkgs() {
    _valid_pkgs=()
    local p
    for p in "$@"; do
        if is_valid_pkg "$p"; then
            _valid_pkgs+=("$p")
        else
            log_warn "Unknown package: ${p}"
        fi
    done
    (( ${#_valid_pkgs[@]} > 0 ))
}

# Set _stowed to every package in PKG_ALL that is currently stowed.
collect_stowed() {
    _stowed=()
    local p
    for p in "${PKG_ALL[@]}"; do
        if is_stowed "$p"; then
            _stowed+=("$p")
        fi
    done
}

# -----------------------------------------------------------------------------
# Operations: backup and restore
# -----------------------------------------------------------------------------
#
# mv acts on symlinks themselves (not their destinations), so a foreign symlink
# and a regular file are handled identically. Restore is always lossless.

# Set _backup_max_n to the highest backup number N found in dir for base.bak.N,
# or -1 if no backups exist. Uses find to handle non-sequential numbering after
# old backups are purged (sequential probing would miss high-numbered survivors).
_find_max_backup_n() {
    local dir="$1" base="$2"
    _backup_max_n=-1
    [[ -d "$dir" ]] || return
    local f n
    while IFS= read -r f; do
        n="${f##*.bak.}"
        if [[ "$n" =~ ^[0-9]+$ ]] && (( n > _backup_max_n )); then
            _backup_max_n=$n
        fi
    done < <(find "$dir" -maxdepth 1 -name "${base}.bak.*" 2>/dev/null)
}

backup_file() {
    local target="$1" pkg="$2"
    [[ -e "$target" || -L "$target" ]] || return 0

    local dir="${BACKUP_DIR}/${pkg}"
    local base; base=$(basename "$target")

    run mkdir -p "$dir"

    # New backup always appends after the highest existing number so that
    # purging old backups never creates a gap that resets the slot counter.
    _find_max_backup_n "$dir" "$base"
    local new_n=$(( _backup_max_n + 1 ))

    # Purge backups older than the retention window before creating the new one.
    if (( new_n >= MAX_BACKUPS )); then
        local cutoff=$(( new_n - MAX_BACKUPS ))
        local f n
        while IFS= read -r f; do
            n="${f##*.bak.}"
            if [[ "$n" =~ ^[0-9]+$ ]] && (( n <= cutoff )); then
                run rm -f "$f"
            fi
        done < <(find "$dir" -maxdepth 1 -name "${base}.bak.*" 2>/dev/null)
    fi

    log_warn "Backing up: ${target} -> ${dir}/${base}.bak.${new_n}"
    run mv "$target" "${dir}/${base}.bak.${new_n}"
}

restore_file() {
    local target="$1" pkg="$2"
    local dir="${BACKUP_DIR}/${pkg}"
    local base; base=$(basename "$target")

    _find_max_backup_n "$dir" "$base"

    if (( _backup_max_n < 0 )); then
        log_err "No backup found for $(basename "$target") in ${dir}"
        return 1
    fi

    log_info "Restoring: ${dir}/${base}.bak.${_backup_max_n} -> ${target}"
    run mv "${dir}/${base}.bak.${_backup_max_n}" "$target"
}

# -----------------------------------------------------------------------------
# Operations: stow
# -----------------------------------------------------------------------------

stow_package() {
    local pkg="$1"

    if [[ ! -d "${DOTFILES_DIR}/${pkg}" ]]; then
        log_err "Package directory not found: ${DOTFILES_DIR}/${pkg}"
        return 1
    fi

    if is_stowed "$pkg"; then
        log_info "${pkg}: already stowed, skipping"
        return 0
    fi

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
        if [[ "$DRY_RUN" == true ]]; then
            log_info "${pkg}: would be stowed"
        else
            log_ok "${pkg}: stowed"
        fi
    else
        log_err "${pkg}: stow failed"
        return 1
    fi
}

restow_package() {
    local pkg="$1"

    # Back up conflicts before restowing. stow -R removes existing links then
    # re-stows; if new package files clash with real files in $HOME, stow fails
    # AFTER removing old links, leaving the package partially stowed.
    # stow_targets reports conflict lines for new clashing files even when the
    # package is currently stowed, so we can resolve them proactively.
    local target
    while IFS= read -r target; do
        if [[ -L "$target" ]]; then
            log_warn "Foreign symlink: ${target} -> $(readlink "$target")"
            backup_file "$target" "$pkg"
        elif [[ -e "$target" ]]; then
            backup_file "$target" "$pkg"
        fi
    done < <(stow_targets "$pkg")

    if run stow -R -d "$DOTFILES_DIR" -t "$HOME" "$pkg"; then
        if [[ "$DRY_RUN" == true ]]; then
            log_info "${pkg}: would be restowed"
        else
            log_ok "${pkg}: restowed"
        fi
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
        if [[ "$DRY_RUN" == true ]]; then
            log_info "${pkg}: would be unstowed"
        else
            log_ok "${pkg}: unstowed"
        fi
    else
        log_err "${pkg}: unstow failed"
        return 1
    fi
}

# -----------------------------------------------------------------------------
# Operations: prerequisites
# -----------------------------------------------------------------------------

install_xcode_cli() {
    if has_xcode_cli; then
        log_ok "Xcode CLI: already installed"
        return 0
    fi
    log_info "Installing Xcode Command Line Tools..."
    log_warn "A dialog will appear - complete it, then press any key here."
    xcode-select --install
    # read -n 1 waits for the user to confirm the GUI dialog completed;
    # || true guards against non-interactive stdin returning EOF.
    read -r -n 1 -s || true
    if ! has_xcode_cli; then
        log_err "Xcode CLI installation failed"
        return 1
    fi
    log_ok "Xcode CLI: installed"
}

install_homebrew() {
    if has_homebrew; then
        log_ok "Homebrew: already installed"
        return 0
    fi
    log_info "Installing Homebrew..."
    local url="https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"
    local i
    for (( i = 1; i <= NETWORK_RETRIES; i++ )); do
        if (( i > 1 )); then
            log_warn "Retry ${i}/${NETWORK_RETRIES}..."
            sleep 5
        fi
        # Wrap in if so set -e does not exit the script on pipeline failure;
        # the if condition context is exempt from set -e termination.
        if curl --fail --silent --show-error \
                --connect-timeout "$NETWORK_TIMEOUT" \
                "$url" | /bin/bash; then
            break
        fi
        if (( i == NETWORK_RETRIES )); then
            log_err "Homebrew installation failed after ${NETWORK_RETRIES} attempts"
            return 1
        fi
    done
    local b
    for b in /opt/homebrew/bin/brew /usr/local/bin/brew; do
        if [[ -x "$b" ]]; then
            eval "$("$b" shellenv)"
            break
        fi
    done
    if ! has_homebrew; then
        log_err "Homebrew not found after installation"
        return 1
    fi
    log_ok "Homebrew: installed"
}

install_stow() {
    if has_stow; then
        log_ok "GNU Stow: already installed"
        return 0
    fi
    log_info "Installing GNU Stow..."
    if run brew install stow; then
        log_ok "GNU Stow: installed"
    else
        log_err "GNU Stow installation failed"
        return 1
    fi
}

run_brew_bundle() {
    [[ -f "$BREWFILE" ]] || die "Brewfile not found: $BREWFILE"
    log_info "Running brew bundle..."
    # --verbose shows each package as it is installed or verified.
    if run brew bundle --verbose --file="$BREWFILE"; then
        log_ok "brew bundle: complete"
    else
        log_err "brew bundle: some packages failed"
        return 1
    fi
}

# mode=install  install anything missing
# mode=check    report missing and return 1
#
# Each step short-circuits: a failure at one layer stops subsequent steps
# (e.g. homebrew cannot be installed without Xcode CLI).
# install_stow uses "brew install stow" directly so that run_brew_bundle is
# never called here - it runs exactly once in cmd_install --all / cmd_update
# --all, avoiding the double-bundle problem when stow was absent.
ensure_prerequisites() {
    local mode="${1:-check}"

    if ! has_xcode_cli; then
        if [[ "$mode" == install ]]; then
            install_xcode_cli || return 1
        else
            log_err "Xcode CLI missing - run: ./setup.sh install --all"
            return 1
        fi
    fi

    if ! has_homebrew; then
        if [[ "$mode" == install ]]; then
            install_homebrew || return 1
        else
            log_err "Homebrew missing - run: ./setup.sh install --all"
            return 1
        fi
    fi

    if ! has_stow; then
        if [[ "$mode" == install ]]; then
            install_stow || return 1
        else
            log_err "GNU Stow missing - run: ./setup.sh install --all"
            return 1
        fi
    fi

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
        if is_stowed "$pkg"; then
            restow_package "$pkg"
        else
            log_warn "${pkg}: not stowed, skipping"
        fi
    done
}

# Unstow pkg (if stowed), then restore the highest-numbered backup for each
# target. restore_file returns 1 with an error if no backup exists.
do_restore_pkg() {
    local pkg="$1"

    if is_stowed "$pkg"; then
        log_info "${pkg}: unstowing before restore..."
        run stow -D -d "$DOTFILES_DIR" -t "$HOME" "$pkg"
    fi

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
    is_stowed zsh || return 0
    if [[ -n "${ZSH_VERSION:-}" ]]; then
        log_info "Already running zsh"
        return 0
    fi

    if ! confirm "Switch default shell to zsh?" y; then
        log_info "Skipped - run 'chsh -s \$(which zsh)' manually later."
        return 0
    fi

    local zsh_path
    if ! zsh_path=$(command -v zsh 2>/dev/null); then
        log_err "zsh not found in PATH"
        return 1
    fi

    if ! grep -qF "$zsh_path" /etc/shells 2>/dev/null; then
        log_info "Adding ${zsh_path} to /etc/shells..."
        if [[ "$DRY_RUN" == true ]]; then
            log_info "[dry-run] echo ${zsh_path} | sudo tee -a /etc/shells"
        else
            if ! echo "$zsh_path" | sudo tee -a /etc/shells > /dev/null; then
                log_err "Failed to write /etc/shells"
                return 1
            fi
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
    local arg

    for arg in "$@"; do
        case "$arg" in
            --all) do_all=true ;;
            --dry-run) DRY_RUN=true ;;
            -*) die "Unknown flag: $arg" ;;
            *) pkgs+=("$arg") ;;
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

    if (( ${#pkgs[@]} == 0 )); then
        show_help
        return 0
    fi
    validate_pkgs "${pkgs[@]}" || die "No valid packages specified"
    ensure_prerequisites check
    do_install_pkgs "${_valid_pkgs[@]}"
}

cmd_uninstall() {
    local do_all=false
    local -a pkgs=()
    local arg

    for arg in "$@"; do
        case "$arg" in
            --all) do_all=true ;;
            --dry-run) DRY_RUN=true ;;
            -*) die "Unknown flag: $arg" ;;
            *) pkgs+=("$arg") ;;
        esac
    done

    if "$do_all"; then
        (( ${#pkgs[@]} == 0 )) || die "--all and package names are mutually exclusive"
        collect_stowed
        if (( ${#_stowed[@]} == 0 )); then
            log_info "No packages are currently stowed"
            return 0
        fi
        if ! confirm "Uninstall all ${#_stowed[@]} stowed packages?" n; then
            log_info "Cancelled"
            return 0
        fi
        do_uninstall_pkgs "${_stowed[@]}"
        return
    fi

    if (( ${#pkgs[@]} == 0 )); then
        log_err "No packages specified"
        log_info "Usage: ./setup.sh uninstall [--all | <pkg>...] [--dry-run]"
        return 1
    fi
    validate_pkgs "${pkgs[@]}" || return 1
    do_uninstall_pkgs "${_valid_pkgs[@]}"
}

cmd_update() {
    local do_all=false do_pkgs=false
    local -a pkgs=()
    local arg

    for arg in "$@"; do
        case "$arg" in
            --all) do_all=true ;;
            --pkgs) do_pkgs=true ;;
            --dry-run) DRY_RUN=true ;;
            -*) die "Unknown flag: $arg" ;;
            *) pkgs+=("$arg") ;;
        esac
    done

    if "$do_all"; then
        (( ${#pkgs[@]} == 0 )) || die "--all and package names are mutually exclusive"
        ensure_prerequisites install
        run_brew_bundle
        collect_stowed
        if (( ${#_stowed[@]} > 0 )); then
            do_update_pkgs "${_stowed[@]}"
        else
            log_info "No packages are currently stowed"
        fi
        log_ok "Update complete"
        return
    fi

    if "$do_pkgs"; then
        (( ${#pkgs[@]} == 0 )) || die "--pkgs and package names are mutually exclusive"
        ensure_prerequisites check
        collect_stowed
        if (( ${#_stowed[@]} > 0 )); then
            do_update_pkgs "${_stowed[@]}"
        else
            log_info "No packages are currently stowed"
        fi
        return
    fi

    if (( ${#pkgs[@]} == 0 )); then
        log_err "No target specified"
        log_info "Usage: ./setup.sh update [--all | --pkgs | <pkg>...] [--dry-run]"
        return 1
    fi
    validate_pkgs "${pkgs[@]}" || return 1
    ensure_prerequisites check
    do_update_pkgs "${_valid_pkgs[@]}"
}

cmd_restore() {
    if (( $# == 0 )); then
        log_err "No packages specified"
        log_info "Usage: ./setup.sh restore <pkg>..."
        return 1
    fi
    local p
    for p in "$@"; do
        if is_valid_pkg "$p"; then
            do_restore_pkg "$p"
        else
            log_warn "Unknown package: ${p}"
        fi
    done
}

cmd_status() {
    local -a pkgs
    if (( $# > 0 )); then
        validate_pkgs "$@" || return 1
        pkgs=("${_valid_pkgs[@]}")
    else
        pkgs=("${PKG_ALL[@]}")
    fi

    print_header "oh-my-dotfiles status"

    printf '  Prerequisites\n\n'
    if has_xcode_cli; then
        printf "  ${_G}ok${_0}  Xcode CLI\n"
    else
        printf "  ${_R}--${_0}  Xcode CLI\n"
    fi
    if has_homebrew; then
        printf "  ${_G}ok${_0}  Homebrew   %s\n" \
            "$(brew --version 2>/dev/null | head -1 | awk '{print $2}')"
    else
        printf "  ${_R}--${_0}  Homebrew\n"
    fi
    if has_stow; then
        printf "  ${_G}ok${_0}  GNU Stow   %s\n" \
            "$(stow --version 2>/dev/null | head -1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+')"
    else
        printf "  ${_R}--${_0}  GNU Stow\n"
    fi

    printf '\n  Packages  (%d / %d stowed)\n\n' "$(stowed_count)" "${#PKG_ALL[@]}"
    printf '  %-12s  %s  %s\n' "package" "stow" "backups"
    printf '  %-12s  %s  %s\n' "-------" "----" "-------"

    local pkg
    for pkg in "${pkgs[@]}"; do
        printf '  %-12s  ' "$pkg"
        if is_stowed "$pkg"; then
            printf "${_G}ok${_0}"
        else
            printf "${_Y}--${_0}"
        fi
        printf '  %d\n' "$(backup_count "$pkg")"
    done
}

cmd_defaults() {
    if [[ ! -f "$DEFAULTS_SCRIPT" ]]; then
        log_err "defaults.sh not found: $DEFAULTS_SCRIPT"
        return 1
    fi
    log_warn "This will modify system preferences. Some changes require logout or restart."
    if ! confirm "Apply macOS defaults?" n; then
        log_info "Skipped"
        return 0
    fi
    if bash "$DEFAULTS_SCRIPT"; then
        log_ok "macOS defaults applied"
    else
        log_err "macOS defaults script failed"
        return 1
    fi
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
    if [[ $# -eq 0 ]]; then
        show_help
        exit 0
    fi
    local cmd="$1"; shift
    case "$cmd" in
        install) cmd_install "$@" ;;
        uninstall) cmd_uninstall "$@" ;;
        update) cmd_update "$@" ;;
        restore) cmd_restore "$@" ;;
        status) cmd_status "$@" ;;
        defaults) cmd_defaults ;;
        help|-h|--help) show_help ;;
        *) log_err "Unknown command: ${cmd}"; printf '\n'; show_help; exit 1 ;;
    esac
}

main "$@"
