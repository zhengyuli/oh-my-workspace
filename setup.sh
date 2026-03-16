#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-15 23:45:00 Saturday by zhengyuli>
#
# ==============================================================================
# File: setup.sh
# Role: Dotfiles setup and maintenance utility for macOS
#
# Author: zhengyuli <lizhengyu419@outlook.com>
# Copyright (C) 2026 zhengyu li
#
# Load context : Called directly by user: ./setup.sh <command>
# Dependencies : bash 4.3+, git, curl, stow
# Side effects :
#   - Creates symlinks in $HOME via GNU Stow
#   - Installs Homebrew, Node.js, Python
#   - Modifies /etc/shells (requires sudo)
#
# Usage:
#   ./setup.sh full-setup      # Full setup
#   ./setup.sh create-links    # Stow packages
#   ./setup.sh show-status     # Check current status
#   ./setup.sh help            # Show all commands
# ==============================================================================

set -euo pipefail

# ==============================================================================
# Error Handling
# ==============================================================================

# Trap errors to provide meaningful feedback instead of silent exit
trap_error() {
    local exit_code=$?
    local line_no=$1
    log_error "Script failed at line $line_no (exit code: $exit_code)"
    log_error "Run with VERBOSE=1 for more details, or use bash -x ./setup.sh for tracing"
    exit $exit_code
}
trap 'trap_error $LINENO' ERR

# ==============================================================================
# Bootstrap
# ==============================================================================

DOTFILES_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export DOTFILES_ROOT

# ==============================================================================
# XDG Base Directory Specification
# ==============================================================================

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# ==============================================================================
# Color & Logging
# ==============================================================================

readonly RED=$'\033[0;31m'
readonly GREEN=$'\033[0;32m'
readonly YELLOW=$'\033[0;33m'
readonly BLUE=$'\033[0;34m'
readonly CYAN=$'\033[0;36m'
readonly BOLD=$'\033[1m'
readonly NC=$'\033[0m'

log_info()  { printf '%b\n' "${BLUE}[INFO]${NC}  $*"; }
log_ok()    { printf '%b\n' "${GREEN}[OK]${NC}    $*"; }
log_skip()  { printf '%b\n' "${CYAN}[SKIP]${NC}  $*"; }
log_warn()  { printf '%b\n' "${YELLOW}[WARN]${NC}  $*"; }
log_error() { printf '%b\n' "${RED}[ERROR]${NC} $*" >&2; }

print_header() {
    echo ""
    echo -e "${BOLD}${CYAN}==> $*${NC}"
    echo ""
}

die() { log_error "$*"; exit 1; }

# ==============================================================================
# Config
# ==============================================================================

VERBOSE="${VERBOSE:-1}"

# Stow packages - directories containing dotfile configurations
# Note: homebrew and macos are NOT stow packages (they provide scripts only)
readonly -a STOW_PACKAGES=(zsh git vim emacs)

# Each entry: "name|version_flag" — used by cmd_status
readonly -a STATUS_TOOLS=(
    "brew|--version"
    "zsh|--version"
    "git|--version"
    "fnm|--version"
    "pyenv|--version"
    "node|--version"
    "python|--version"
)

# Homebrew zsh candidates in priority order: "arch|path"
readonly -a BREW_ZSH_CANDIDATES=(
    "arm64|/opt/homebrew/bin/zsh"
    "x86_64|/usr/local/bin/zsh"
)

# ==============================================================================
# Utilities
# ==============================================================================

# Run command verbosely (VERBOSE=1, default) or silently (VERBOSE=0)
quietly() {
    if [[ "$VERBOSE" == "1" ]]; then
        "$@"
    else
        "$@" &>/dev/null
    fi
}

require_command() {
    local cmd="$1" hint="${2:-}"
    command -v "$cmd" &>/dev/null && return 0
    log_error "'$cmd' not found.${hint:+ $hint}"
    return 1
}

# Return the appropriate Homebrew zsh for the current arch
# Returns: 0 and path if found, 1 if not found
brew_zsh_path() {
    local arch
    arch="$(uname -m)"
    for entry in "${BREW_ZSH_CANDIDATES[@]}"; do
        local candidate_arch="${entry%%|*}"
        local candidate_path="${entry##*|}"
        if [[ "$arch" == "$candidate_arch" && -f "$candidate_path" ]]; then
            echo "$candidate_path"
            return 0
        fi
    done
    return 1
}

# ==============================================================================
# GNU Stow Integration
# ==============================================================================

# Get list of available stow packages that exist in the repository.
#
# Arguments:
#   None
#
# Output:
#   Space-separated list of package names
#
# Returns:
#   0    always
get_stow_packages() {
    local -a available=()
    for pkg in "${STOW_PACKAGES[@]}"; do
        if [[ -d "$DOTFILES_ROOT/$pkg" ]]; then
            available+=("$pkg")
        fi
    done
    echo "${available[@]}"
}

# Stow packages to $HOME using GNU Stow.
#
# Arguments:
#   $@  Package names (optional, defaults to all available)
#
# Returns:
#   0    always (non-fatal)
#
# Side effects:
#   Creates symlinks in $HOME for each stow package
stow_packages() {
    local -a packages
    if (( $# > 0 )); then
        packages=("$@")
    else
        read -ra packages <<< "$(get_stow_packages)"
    fi

    (( ${#packages[@]} > 0 )) || { log_warn "No stow packages found"; return; }

    for pkg in "${packages[@]}"; do
        if [[ -d "$DOTFILES_ROOT/$pkg" ]]; then
            if quietly stow -d "$DOTFILES_ROOT" -t "$HOME" "$pkg"; then
                log_ok "Stowed: $pkg"
            else
                log_warn "Conflict or error stowing: $pkg"
            fi
        else
            log_warn "Package not found: $pkg"
        fi
    done
}

# Unstow (remove) packages from $HOME using GNU Stow.
#
# Arguments:
#   $@  Package names (optional, defaults to all available)
#
# Returns:
#   0    always (non-fatal)
#
# Side effects:
#   Removes symlinks from $HOME for each stow package
unstow_packages() {
    local -a packages
    if (( $# > 0 )); then
        packages=("$@")
    else
        read -ra packages <<< "$(get_stow_packages)"
    fi

    (( ${#packages[@]} > 0 )) || { log_warn "No stow packages found"; return; }

    for pkg in "${packages[@]}"; do
        if [[ -d "$DOTFILES_ROOT/$pkg" ]]; then
            if quietly stow -d "$DOTFILES_ROOT" -t "$HOME" -D "$pkg"; then
                log_ok "Unstowed: $pkg"
            else
                log_warn "Error unstowing: $pkg"
            fi
        fi
    done
}

# Restow (refresh) packages in $HOME using GNU Stow.
#
# Arguments:
#   $@  Package names (optional, defaults to all available)
#
# Returns:
#   0    always (non-fatal)
#
# Side effects:
#   Removes and recreates symlinks for each stow package
restow_packages() {
    local -a packages
    if (( $# > 0 )); then
        packages=("$@")
    else
        read -ra packages <<< "$(get_stow_packages)"
    fi

    (( ${#packages[@]} > 0 )) || { log_warn "No stow packages found"; return; }

    for pkg in "${packages[@]}"; do
        if [[ -d "$DOTFILES_ROOT/$pkg" ]]; then
            if quietly stow -d "$DOTFILES_ROOT" -t "$HOME" -R "$pkg"; then
                log_ok "Restowed: $pkg"
            else
                log_warn "Error restowing: $pkg"
            fi
        fi
    done
}

# ==============================================================================
# Prerequisites
# ==============================================================================

check_os() {
    [[ "$(uname)" == "Darwin" ]] || die "This setup is designed for macOS only."
    log_ok "macOS $(sw_vers -productVersion)"
}

check_command_line_tools() {
    if ! xcode-select -p &>/dev/null; then
        log_info "Requesting Xcode Command Line Tools installation..."
        xcode-select --install 2>/dev/null || true
        die "Re-run this script after Command Line Tools installation completes."
    fi
    log_ok "Xcode Command Line Tools"
}

# ==============================================================================
# Homebrew
# ==============================================================================

setup_homebrew() {
    if command -v brew &>/dev/null; then
        log_skip "Homebrew already installed"
        return
    fi

    log_info "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    # shellcheck disable=SC1091  # Homebrew shellenv generates shell init code
    if [[ -f "/opt/homebrew/bin/brew" ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    elif [[ -f "/usr/local/bin/brew" ]]; then
        eval "$(/usr/local/bin/brew shellenv)"
    fi
    log_ok "Homebrew installed"
}

install_brew_packages() {
    local brewfile="$DOTFILES_ROOT/homebrew/Brewfile"
    if [[ ! -f "$brewfile" ]]; then
        log_warn "Brewfile not found at $brewfile — skipping"
        return
    fi

    log_info "Installing packages from Brewfile..."
    if quietly brew bundle --file "$brewfile"; then
        log_ok "Brewfile packages installed"
    else
        log_warn "Some Brewfile packages failed — continuing"
    fi
}

# ==============================================================================
# Node.js (fnm)
# ==============================================================================

setup_node() {
    require_command fnm "Install via Homebrew: brew install fnm" || return 1
    log_ok "fnm $(fnm --version)"

    # shellcheck disable=SC1091  # fnm env generates shell init code
    eval "$(fnm env --shell bash)"

    # Check if any LTS version is already installed
    # Use multiple detection methods for robustness
    if fnm list 2>/dev/null | grep -qE '(lts|LTS)' || command -v node &>/dev/null; then
        log_skip "Node.js already installed: $(node --version 2>/dev/null)"
        return
    fi

    log_info "Installing Node.js LTS..."
    quietly fnm install --lts
    quietly fnm use lts-latest
    quietly fnm default lts-latest
    log_ok "Node.js $(node --version)  npm $(npm --version)"
}

# ==============================================================================
# Python (pyenv)
# ==============================================================================

_openssl_prefix() {
    local -a candidates=(
        "/opt/homebrew/opt/openssl@3"
        "/opt/homebrew/opt/openssl"
        "/usr/local/opt/openssl@3"
        "/usr/local/opt/openssl"
    )
    for dir in "${candidates[@]}"; do
        [[ -d "$dir" ]] && echo "$dir" && return
    done
}

# Setup Python environment using pyenv.
#
# Arguments:
#   None
#
# Returns:
#   0    success (Python ready to use)
#   1    pyenv not found
#
# Side effects:
#   Installs latest stable Python via pyenv if not present
#   Sets Python as global default via pyenv global
#   Installs virtualenvwrapper via pip
#   Creates $XDG_DATA_HOME/virtualenvs if WORKON_HOME not set
setup_python() {
    require_command pyenv "Install via Homebrew: brew install pyenv" || return 1
    log_ok "pyenv $(pyenv --version | awk '{print $2}')"

    # shellcheck disable=SC1091  # pyenv init generates shell init code
    eval "$(pyenv init -)"

    local current
    current="$(pyenv version-name 2>/dev/null || echo "")"

    if [[ -n "$current" && "$current" != "system" ]]; then
        log_skip "Python already active: $current"
    else
        local latest
        latest="$(pyenv install --list \
            | grep -E '^\s+[0-9]+\.[0-9]+\.[0-9]+$' \
            | tr -d ' ' \
            | sort -V \
            | tail -1)"
        [[ -n "$latest" ]] || latest="3.12.0"

        log_info "Installing Python $latest..."

        local ssl_prefix
        ssl_prefix="$(_openssl_prefix)"
        if [[ -n "$ssl_prefix" ]]; then
            # Append to existing flags rather than overwriting
            export LDFLAGS="-L${ssl_prefix}/lib${LDFLAGS:+ $LDFLAGS}"
            export CPPFLAGS="-I${ssl_prefix}/include${CPPFLAGS:+ $CPPFLAGS}"
            export PKG_CONFIG_PATH="${ssl_prefix}/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
        fi

        quietly pyenv install -s "$latest"
        pyenv global "$latest"
        log_ok "Python $(python --version 2>&1 | awk '{print $2}')  pip $(pip --version | awk '{print $2}')"
    fi

    setup_virtualenvwrapper
}

setup_virtualenvwrapper() {
    local workon_home="${WORKON_HOME:-${XDG_DATA_HOME}/virtualenvs}"
    [[ -d "$workon_home" ]] || { mkdir -p "$workon_home"; log_ok "Created virtualenv dir: $workon_home"; }

    if quietly pip show virtualenvwrapper; then
        log_skip "virtualenvwrapper already installed"
    else
        log_info "Installing virtualenvwrapper..."
        quietly pip install virtualenvwrapper
        log_ok "virtualenvwrapper installed"
    fi
}


# ==============================================================================
# Git
# ==============================================================================

_git_prompt_field() {
    local field="$1" key="$2" git_local="$HOME/.gitconfig.local"

    local existing
    existing="$(git config --global "$key" 2>/dev/null || true)"
    if [[ -n "$existing" ]]; then
        log_skip "Git $key: $existing"
        return
    fi

    # Check if running interactively
    if [[ ! -t 0 ]]; then
        log_warn "Git $key not set and stdin is not a TTY — skipping"
        log_warn "Run './setup.sh config-git' interactively to configure"
        return
    fi

    log_info "Git $key not set. Enter your $field:"
    local value
    read -r value || { log_warn "Failed to read input — skipping"; return; }
    [[ -n "$value" ]] && git config -f "$git_local" "$key" "$value"
}

setup_git() {
    _git_prompt_field "name"  "user.name"
    _git_prompt_field "email" "user.email"
}

# ==============================================================================
# Shell
# ==============================================================================

switch_shell() {
    local target_zsh
    if ! target_zsh="$(brew_zsh_path)"; then
        log_warn "Homebrew zsh not found — skipping shell switch"
        return
    fi

    if ! grep -qxF "$target_zsh" /etc/shells 2>/dev/null; then
        log_info "Registering $target_zsh in /etc/shells (requires sudo)..."
        echo "$target_zsh" | sudo tee -a /etc/shells >/dev/null
    fi

    if [[ "$SHELL" == "$target_zsh" ]]; then
        log_skip "Default shell is already $target_zsh"
    else
        log_info "Changing default shell to $target_zsh..."
        chsh -s "$target_zsh"
        log_warn "Shell changed — restart your terminal to apply"
    fi
}

# ==============================================================================
# Commands
# ==============================================================================

cmd_full_setup() {
    print_header "Dotfiles — Full Setup"

    print_header "Prerequisites"
    check_os
    check_command_line_tools

    print_header "Homebrew"
    setup_homebrew
    install_brew_packages

    print_header "Node.js"
    setup_node || log_warn "Node.js setup skipped"

    print_header "Python"
    setup_python || log_warn "Python setup skipped"

    print_header "Stow Packages"
    stow_packages

    print_header "Git"
    setup_git

    print_header "Shell"
    switch_shell

    print_header "Done!"
    echo -e "  ${GREEN}Next steps:${NC}"
    echo "    1. Restart terminal or run: source ~/.zshrc"
    echo "    2. Create local overrides in ~/.config/zsh/conf.d/99-local.zsh"
    echo ""
}

cmd_create_links() {
    print_header "Dotfiles — Stow Packages"
    stow_packages "$@"
}

cmd_remove_links() {
    print_header "Dotfiles — Unstow Packages"
    echo -e "${YELLOW}This will remove all managed stow symlinks from \$HOME.${NC}"
    echo -n "Continue? [y/N] "
    read -r confirm || { log_warn "Failed to read input — aborting."; return; }
    [[ "$confirm" =~ ^[Yy]$ ]] || { log_info "Aborted."; return; }
    unstow_packages "$@"
    log_ok "Unstow complete. Homebrew packages and shell config untouched."
}

cmd_restow() {
    print_header "Dotfiles — Restow Packages"
    restow_packages "$@"
}

# Individual step commands
cmd_check_prereq() {
    print_header "Dotfiles — Check Prerequisites"
    check_os
    check_command_line_tools
}

cmd_install_homebrew() {
    print_header "Dotfiles — Install Homebrew"
    setup_homebrew
    install_brew_packages
}

cmd_setup_node() {
    print_header "Dotfiles — Setup Node.js"
    setup_node || log_warn "Node.js setup failed"
}

cmd_setup_python() {
    print_header "Dotfiles — Setup Python"
    setup_python || log_warn "Python setup failed"
}

cmd_config_git() {
    print_header "Dotfiles — Configure Git"
    setup_git
}

cmd_switch_shell() {
    print_header "Dotfiles — Switch Shell"
    switch_shell
}

cmd_show_status() {
    print_header "Dotfiles — Show Status"

    echo -e "${BOLD}Stow Packages:${NC}"
    for pkg in "${STOW_PACKAGES[@]}"; do
        if [[ ! -d "$DOTFILES_ROOT/$pkg" ]]; then
            echo -e "  ${RED}✘${NC}  $pkg  ${RED}(not found)${NC}"
            continue
        fi

        # Check if package is stowed (dry-run to returns 0 if already stowed)
        if stow -d "$DOTFILES_ROOT" -t "$HOME" -n "$pkg" &>/dev/null; then
            echo -e "  ${GREEN}✔${NC}  $pkg"
        else
            echo -e "  ${YELLOW}!${NC}  $pkg  ${YELLOW}(unstowed or conflict)${NC}"
        fi
    done

    echo ""
    echo -e "${BOLD}Tools:${NC}"
    for entry in "${STATUS_TOOLS[@]}"; do
        local tool="${entry%%|*}" flag="${entry##*|}"
        if command -v "$tool" &>/dev/null; then
            local ver
            ver="$("$tool" "$flag" 2>&1 | head -1)"
            echo -e "  ${GREEN}✔${NC}  $tool  $ver"
        else
            echo -e "  ${RED}✘${NC}  $tool  (not found)"
        fi
    done

    echo ""
    printf '%b\n' "${BOLD}Shell:${NC}"
    local target_zsh
    if target_zsh="$(brew_zsh_path)"; then
        if [[ "$SHELL" == "$target_zsh" ]]; then
            printf '%b\n' "  ${GREEN}✔${NC}  $SHELL"
        else
            printf '%b\n' "  ${YELLOW}!${NC}  $SHELL  ${YELLOW}(expected $target_zsh)${NC}"
        fi
    else
        printf '%b\n' "  ${YELLOW}!${NC}  $SHELL  ${YELLOW}(Homebrew zsh not found)${NC}"
    fi
    echo ""
}

# ==============================================================================
# Help
# ==============================================================================

show_help() {
    echo ""
    echo -e "${BOLD}dotfiles${NC} — Setup and maintenance utility"
    echo ""
    echo -e "${BOLD}Usage:${NC}"
    echo "  ./setup.sh <command>"
    echo ""
    echo -e "${BOLD}Workflow:${NC}"
    echo "  full-setup      Full setup: all steps in sequence"
    echo "  show-status     Show stow package and tool status"
    echo "  restow          Refresh all stow package symlinks"
    echo ""
    echo -e "${BOLD}Stow Packages:${NC}"
    echo "  create-links    Stow packages to \$HOME (stow)"
    echo "  remove-links    Unstow packages from \$HOME (stow -D)"
    echo ""
    echo -e "${BOLD}Individual Steps:${NC}"
    echo "  install-homebrew Install Homebrew and Brewfile packages"
    echo "  setup-node      Setup Node.js via fnm"
    echo "  setup-python    Setup Python via pyenv"
    echo "  config-git      Configure git user.name and user.email"
    echo "  switch-shell    Switch default shell to Homebrew zsh"
    echo ""
    echo -e "${BOLD}Options:${NC}"
    echo "  VERBOSE=0       Suppress command output (default: 1, verbose)"
    echo ""
    echo -e "${BOLD}Requirements:${NC}"
    echo "  bash 4.3+       Required for array features (brew install bash)"
    echo "  stow            GNU Stow for symlink management (brew install stow)"
    echo ""
    echo -e "${BOLD}Examples:${NC}"
    echo "  ./setup.sh                    # Show this help"
    echo "  ./setup.sh full-setup         # Full setup"
    echo "  ./setup.sh create-links       # Stow all packages"
    echo "  ./setup.sh restow zsh git     # Restow specific packages"
    echo ""
}

# ==============================================================================
# Main
# ==============================================================================

main() {
    local cmd="${1:-help}"
    shift || true

    case "$cmd" in
        full-setup)
            cmd_full_setup "$@" ;;
        create-links)
            cmd_create_links "$@" ;;
        remove-links)
            cmd_remove_links "$@" ;;
        restow)
            cmd_restow "$@" ;;
        show-status)
            cmd_show_status "$@" ;;
        install-homebrew)
            cmd_install_homebrew "$@" ;;
        setup-node)
            cmd_setup_node "$@" ;;
        setup-python)
            cmd_setup_python "$@" ;;
        config-git)
            cmd_config_git "$@" ;;
        switch-shell)
            cmd_switch_shell "$@" ;;
        help|--help|-h)
            show_help ;;
        *)
            log_error "Unknown command: '$cmd'"
            show_help
            exit 1
            ;;
    esac
}

main "$@"
