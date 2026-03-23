#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-21 08:00:00 Saturday by zhengyu.li>
# =============================================================================
# oh-my-workspace Setup Script
#
# Location: $WORKSPACE_DIR/setup.sh (run directly, not stowed)
# Usage: ./setup.sh help
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

MAX_BACKUPS=5
NETWORK_RETRIES=3
NETWORK_TIMEOUT=60

# zsh must come first - subsequent packages rely on XDG env vars it sets.
PKG_ALL=(zsh git vim emacs ghostty ripgrep uv bun starship)

WORKSPACE_DIR="${WORKSPACE_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
BREWFILE="${WORKSPACE_DIR}/homebrew/Brewfile"
DEFAULTS_SCRIPT="${WORKSPACE_DIR}/macos/defaults.sh"
BACKUP_DIR="${WORKSPACE_DIR}/.backups"

if [[ -z "${SETUP_SH_TEST_MODE:-}" ]]; then
    readonly MAX_BACKUPS NETWORK_RETRIES NETWORK_TIMEOUT
    readonly PKG_ALL
    readonly WORKSPACE_DIR BREWFILE DEFAULTS_SCRIPT BACKUP_DIR
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
# Queries  (pure - no side effects)
# -----------------------------------------------------------------------------

is_valid_pkg() {
    local p
    for p in "${PKG_ALL[@]}"; do [[ "$1" == "$p" ]] && return 0; done
    return 1
}

# Print paths stow would link or that block linking for pkg.
# Parses stow -n -v output for LINK and conflict patterns.
# Returns absolute $HOME paths for backup_file to clear before real stow run.
stow_targets() {
    local pkg="$1"
    [[ -d "${WORKSPACE_DIR}/${pkg}" ]] || return 1
    local link_pat='^LINK: ([^[:space:]]+)'
    local conflict_pat='existing target [^:]+: (.+)$'
    local output line
    # Capture output (including when stow exits non-zero for conflicts) so the
    # while loop runs in the current shell rather than a subshell, consistent
    # with the is_stowed() pattern.
    output=$(stow -n -v -d "$WORKSPACE_DIR" -t "$HOME" "$pkg" 2>&1) || true
    while IFS= read -r line; do
        if [[ "$line" =~ $link_pat || "$line" =~ $conflict_pat ]]; then
            printf '%s\n' "${HOME}/${BASH_REMATCH[1]}"
        fi
    done <<< "$output"
}

# Returns 0 if pkg is fully stowed (stow -n -v reports nothing to do).
# Returns 1 if stow is absent, LINK lines exist, or conflicts are reported.
is_stowed() {
    has_stow || return 1
    local output
    output=$(stow -n -v -d "$WORKSPACE_DIR" -t "$HOME" "$1" 2>&1) || true
    if grep -qE "^LINK:|conflicts|cannot stow" <<< "$output"; then
        return 1
    fi
    return 0
}

# Count backup files for pkg under BACKUP_DIR/<pkg>/.
backup_count() {
    local dir="${BACKUP_DIR}/${1}"
    if [[ ! -d "$dir" ]]; then
        printf '0\n'
        return
    fi
    find "$dir" -maxdepth 1 -name '*.bak.*' | wc -l | tr -d ' '
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

# mv acts on symlinks themselves (not their destinations), so a foreign symlink
# and a regular file are handled identically. Restore is always lossless.
#
# Set _backup_max_n to the highest backup number N found in dir for base.bak.N,
# or -1 if no backups exist. Uses find to handle non-sequential numbering after
# old backups are purged (sequential probing would miss high-numbered ones).
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

# Back up target file to BACKUP_DIR/<pkg>/ with auto-incrementing number.
# Purges oldest backups when MAX_BACKUPS limit is reached.
backup_file() {
    local target="$1" pkg="$2"
    [[ -e "$target" || -L "$target" ]] || return 0

    local dir="${BACKUP_DIR}/${pkg}"
    local base; base=$(basename "$target")

    if ! mkdir -p "$dir"; then
        log_err "Failed to create backup directory: ${dir}"
        return 1
    fi

    # New backup always appends after the highest existing number so that
    # purging old backups never creates a gap that resets the slot counter.
    _find_max_backup_n "$dir" "$base"
    local new_n=$(( _backup_max_n + 1 ))

    # Purge backups beyond the retention limit before creating the new one.
    if (( new_n >= MAX_BACKUPS )); then
        local cutoff=$(( new_n - MAX_BACKUPS ))
        local f n
        while IFS= read -r f; do
            n="${f##*.bak.}"
            if [[ "$n" =~ ^[0-9]+$ ]] && (( n <= cutoff )); then
                rm -f "$f"
            fi
        done < <(find "$dir" -maxdepth 1 -name "${base}.bak.*" 2>/dev/null)
    fi

    log_warn "Backing up: ${target} -> ${dir}/${base}.bak.${new_n}"
    # Re-check before mv to mitigate TOCTOU race (file could disappear
    # between initial check at line 190 and this operation).
    if [[ -e "$target" || -L "$target" ]]; then
        mv "$target" "${dir}/${base}.bak.${new_n}"
    else
        log_warn "Target disappeared: ${target}"
    fi
}

# Restore the most recent backup for target from BACKUP_DIR/<pkg>/.
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
    mv "${dir}/${base}.bak.${_backup_max_n}" "$target"
}

# -----------------------------------------------------------------------------
# Operations: stow
# -----------------------------------------------------------------------------

# Back up any paths that would block stow from linking pkg. Covers both
# LINK targets already occupied by a foreign symlink or real file, and
# conflict paths stow explicitly cannot overwrite.
backup_stow_conflicts() {
    local pkg="$1" target
    while IFS= read -r target; do
        if [[ -L "$target" ]]; then
            log_warn "Foreign symlink: ${target} -> $(readlink "$target")"
            backup_file "$target" "$pkg"
        elif [[ -e "$target" ]]; then
            backup_file "$target" "$pkg"
        fi
    done < <(stow_targets "$pkg")
}

# Stow a single package, backing up any conflicting files first.
stow_package() {
    local pkg="$1"

    if [[ ! -d "${WORKSPACE_DIR}/${pkg}" ]]; then
        log_err "Package directory not found: ${WORKSPACE_DIR}/${pkg}"
        return 1
    fi

    if is_stowed "$pkg"; then
        log_info "${pkg}: already stowed, skipping"
        return 0
    fi

    # Use stow's own dry-run to enumerate targets; back up anything blocking.
    backup_stow_conflicts "$pkg"

    # Pre-create ~/.config so stow links individual items inside it rather
    # than folding the whole directory into a single symlink.
    mkdir -p "${HOME}/.config"

    if stow -d "$WORKSPACE_DIR" -t "$HOME" "$pkg"; then
        log_ok "${pkg}: stowed"
    else
        log_err "${pkg}: stow failed"
        return 1
    fi
}

# Restow a package (remove and re-link), backing up any new conflicts.
restow_package() {
    local pkg="$1"

    # Back up conflicts before restowing. stow -R removes existing links then
    # re-stows; if new package files clash with real files in $HOME, stow
    # fails AFTER removing old links, leaving the package partially stowed.
    # stow_targets reports conflict lines for new clashing files even when
    # the package is currently stowed, so we can resolve them proactively.
    backup_stow_conflicts "$pkg"

    if stow -R -d "$WORKSPACE_DIR" -t "$HOME" "$pkg"; then
        log_ok "${pkg}: restowed"
    else
        log_err "${pkg}: restow failed"
        return 1
    fi
}

# Remove stow symlinks for a package.
unstow_package() {
    local pkg="$1"
    if ! is_stowed "$pkg"; then
        log_warn "${pkg}: not stowed, skipping"
        return 0
    fi
    if stow -D -d "$WORKSPACE_DIR" -t "$HOME" "$pkg"; then
        log_ok "${pkg}: unstowed"
    else
        log_err "${pkg}: unstow failed"
        return 1
    fi
}

# -----------------------------------------------------------------------------
# Operations: prerequisites
# -----------------------------------------------------------------------------

# Install Xcode Command Line Tools if not present.
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

# Install Homebrew package manager if not present.
install_homebrew() {
    if has_homebrew; then
        log_ok "Homebrew: already installed"
        return 0
    fi
    log_info "Installing Homebrew..."
    # Note: curl | bash is the official Homebrew installation method.
    # Security: URL is hardcoded (not user input), uses HTTPS with SSL
    # verification, and points to Homebrew's official GitHub repository.
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
            log_err "Homebrew installation failed after" \
                "${NETWORK_RETRIES} attempts"
            return 1
        fi
    done
    # Note: eval is required here per Homebrew's official installation guide.
    # Security: Paths are hardcoded to official Homebrew locations
    # (/opt/homebrew for Apple Silicon, /usr/local for Intel), so this is safe.
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

# Install GNU Stow via Homebrew if not present.
install_stow() {
    if has_stow; then
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

# Run brew bundle to install/upgrade all packages from Brewfile.
run_brew_bundle() {
    [[ -f "$BREWFILE" ]] || die "Brewfile not found: $BREWFILE"
    log_info "Running brew bundle..."
    # --verbose shows each package as it is installed or verified.
    if brew bundle --verbose --file="$BREWFILE"; then
        log_ok "brew bundle: complete"
    else
        log_err "brew bundle: some packages failed"
        return 1
    fi
}

# Ensure Xcode CLI, Homebrew, and GNU Stow are installed.
# mode=install: install missing prerequisites; mode=check: report only.
# Steps short-circuit on failure; install_stow uses brew directly to avoid
# double-bundle when stow was absent.
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

# Install multiple packages in sequence.
do_install_pkgs() {
    local pkg
    for pkg in "$@"; do stow_package "$pkg"; done
    printf '\n'
    log_ok "Done. Source ~/.zshenv or open a new terminal to apply changes."
}

# Uninstall multiple packages in sequence.
do_uninstall_pkgs() {
    local pkg
    for pkg in "$@"; do unstow_package "$pkg"; done
}

# Restow multiple packages in sequence.
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
        if ! stow -D -d "$WORKSPACE_DIR" -t "$HOME" "$pkg"; then
            log_err "${pkg}: unstow failed"
            return 1
        fi
    fi

    # After unstow, stow -n -v shows what would be linked - i.e. the original
    # target paths - which is exactly what we need to restore backups to.
    local target
    while IFS= read -r target; do
        if [[ -e "$target" || -L "$target" ]]; then
            log_err "${target}: occupied, remove it manually before restoring"
            return 1
        fi
        restore_file "$target" "$pkg" || return 1
    done < <(stow_targets "$pkg")

    log_ok "${pkg}: restored"
}

# Offer to switch default shell to zsh after successful installation.
do_offer_shell_switch() {
    is_stowed zsh || return 0
    # ZSH_VERSION is set when running inside zsh; SHELL reflects the user's
    # default shell (may differ if the script is invoked via bash setup.sh).
    if [[ -n "${ZSH_VERSION:-}" || "${SHELL##*/}" == "zsh" ]]; then
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

    if chsh -s "$zsh_path" 2>/dev/null; then
        log_ok "Default shell changed to zsh"
        log_info "Open a new terminal to use it."
    else
        log_warn "chsh failed - zsh may not be in /etc/shells"
        log_info "Run: echo '$zsh_path' | sudo tee -a /etc/shells"
        log_info "Then: chsh -s '$zsh_path'"
    fi
}

# -----------------------------------------------------------------------------
# Commands (cmd_*)
# -----------------------------------------------------------------------------

# Handle 'install' command.
cmd_install() {
    local do_all=false
    local -a pkgs=()
    local arg

    for arg in "$@"; do
        case "$arg" in
            --all) do_all=true ;;
            -*) die "Unknown flag: $arg" ;;
            *) pkgs+=("$arg") ;;
        esac
    done

    if "$do_all"; then
        if (( ${#pkgs[@]} != 0 )); then
            die "--all and package names are mutually exclusive"
        fi
        ensure_prerequisites install
        run_brew_bundle
        do_install_pkgs "${PKG_ALL[@]}"
        do_offer_shell_switch
        printf '\n'
        log_ok "Full installation complete"
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

# Handle 'uninstall' command.
cmd_uninstall() {
    local do_all=false
    local -a pkgs=()
    local arg

    for arg in "$@"; do
        case "$arg" in
            --all) do_all=true ;;
            -*) die "Unknown flag: $arg" ;;
            *) pkgs+=("$arg") ;;
        esac
    done

    if "$do_all"; then
        if (( ${#pkgs[@]} != 0 )); then
            die "--all and package names are mutually exclusive"
        fi
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
        log_info "Usage: ./setup.sh uninstall [--all | <pkg>...]"
        return 1
    fi
    validate_pkgs "${pkgs[@]}" || return 1
    ensure_prerequisites check
    do_uninstall_pkgs "${_valid_pkgs[@]}"
}

# Handle 'update' command.
cmd_update() {
    local do_all=false do_pkgs=false
    local -a pkgs=()
    local arg

    for arg in "$@"; do
        case "$arg" in
            --all) do_all=true ;;
            --pkgs) do_pkgs=true ;;
            -*) die "Unknown flag: $arg" ;;
            *) pkgs+=("$arg") ;;
        esac
    done

    if "$do_all" && "$do_pkgs"; then
        die "--all and --pkgs are mutually exclusive"
    fi

    if "$do_all"; then
        if (( ${#pkgs[@]} != 0 )); then
            die "--all and package names are mutually exclusive"
        fi
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
        if (( ${#pkgs[@]} != 0 )); then
            die "--pkgs and package names are mutually exclusive"
        fi
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
        log_info "Usage: ./setup.sh update [--all | --pkgs | <pkg>...]"
        return 1
    fi
    validate_pkgs "${pkgs[@]}" || return 1
    ensure_prerequisites check
    do_update_pkgs "${_valid_pkgs[@]}"
}

# Handle 'restore' command.
cmd_restore() {
    if (( $# == 0 )); then
        log_err "No packages specified"
        log_info "Usage: ./setup.sh restore <pkg>..."
        return 1
    fi
    ensure_prerequisites check
    local p
    for p in "$@"; do
        if is_valid_pkg "$p"; then
            do_restore_pkg "$p"
        else
            log_warn "Unknown package: ${p}"
        fi
    done
}

# Handle 'status' command.
cmd_status() {
    local -a pkgs
    if (( $# > 0 )); then
        validate_pkgs "$@" || return 1
        pkgs=("${_valid_pkgs[@]}")
    else
        pkgs=("${PKG_ALL[@]}")
    fi

    print_header "oh-my-workspace status"

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
            "$(stow --version 2>/dev/null | head -1 | awk '{print $NF}')"
    else
        printf "  ${_R}--${_0}  GNU Stow\n"
    fi

    # Single pass over all packages to determine stow status, so each package
    # runs stow -n -v exactly once (avoids a second pass in the display loop).
    local -A _pkg_stowed=()
    local pkg stowed=0
    for pkg in "${PKG_ALL[@]}"; do
        if is_stowed "$pkg"; then
            _pkg_stowed[$pkg]=1
            (( ++stowed ))
        else
            _pkg_stowed[$pkg]=0
        fi
    done
    local total=${#PKG_ALL[@]}
    printf '\n  Packages  (%d / %d stowed)\n\n' "$stowed" "$total"
    printf '  %-12s  %s  %s\n' "package" "stow" "backups"
    printf '  %-12s  %s  %s\n' "-------" "----" "-------"

    for pkg in "${pkgs[@]}"; do
        printf '  %-12s  ' "$pkg"
        if (( _pkg_stowed[$pkg] )); then
            printf "${_G}ok${_0}"
        else
            printf "${_Y}--${_0}"
        fi
        printf '  %d\n' "$(backup_count "$pkg")"
    done
}

# Handle 'defaults' command.
cmd_defaults() {
    if [[ ! -f "$DEFAULTS_SCRIPT" ]]; then
        log_err "defaults.sh not found: $DEFAULTS_SCRIPT"
        return 1
    fi
    log_warn "This will modify system preferences."
    log_warn "Some changes require logout or restart."
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

# Display usage information and available commands.
show_help() {
    cat <<'EOF'
oh-my-workspace setup

Usage:
  ./setup.sh <command> [arguments]

Commands:
  install   <pkg>... [--all]    Install packages
  uninstall <pkg>... [--all]    Uninstall packages
  update    <pkg>... [--all|--pkgs]  Update packages
  restore   <pkg>...            Restore from backups
  status    [<pkg>...]          Show status
  defaults                      Apply macOS defaults
  help                          Show this help

Arguments:
  <pkg>...  One or more packages: zsh, git, vim, emacs,
            ghostty, ripgrep, uv, bun, starship
  --all     Apply to all packages (install/uninstall/update)
  --pkgs    Update only stowed packages, skip brew (update only)

Examples:
  ./setup.sh install --all          Full setup
  ./setup.sh install zsh git        Install specific packages
  ./setup.sh update --all           Update everything
  ./setup.sh restore zsh            Restore zsh backups

Details:
  install --all:
    Installs Xcode CLI, Homebrew, GNU Stow, runs brew bundle,
    stows all packages, offers to switch default shell to zsh.

  update --all:
    Ensures prerequisites, runs brew bundle, restows all packages.

  update --pkgs:
    Restows currently stowed packages without touching brew.

  restore:
    Unstows package if stowed, then restores the most recent
    backup from .backups/<pkg>/.

For more information: https://github.com/zhengyuli/oh-my-workspace
EOF
}

# -----------------------------------------------------------------------------
# Entry point
# -----------------------------------------------------------------------------

# Entry point - dispatch to appropriate command handler.
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
