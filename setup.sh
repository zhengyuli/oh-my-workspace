#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-14 16:30:00 Saturday by zhengyuli>
#
# ==============================================================================
# File: setup.sh
# Role: Dotfiles setup and maintenance utility for macOS
#
# Author: zhengyuli <lizhengyu419@outlook.com>
# Copyright (C) 2026 zhengyu li
#
# Load context : Called directly by user: ./setup.sh <command>
# Dependencies : bash 4.3+, git, curl
# Side effects :
#   - Creates symlinks in $HOME from *.symlink files
#   - Installs Homebrew, zsh plugins, Node.js, Python
#   - Modifies /etc/shells (requires sudo)
#   - Creates ~/.dotfiles-backup for conflicting files
#
# Usage:
#   ./setup.sh full-setup      # Full setup
#   ./setup.sh create-links    # Re-link symlinks only
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

ZSH_PLUGIN_DIR="${ZSH_PLUGIN_DIR:-${XDG_DATA_HOME}/zsh/plugins}"
VERBOSE="${VERBOSE:-1}"

# Each entry: "name|url"
readonly -a ZSH_PLUGINS=(
    "zsh-completions|https://github.com/zsh-users/zsh-completions"
    "zsh-autosuggestions|https://github.com/zsh-users/zsh-autosuggestions"
    "zsh-history-substring-search|https://github.com/zsh-users/zsh-history-substring-search"
    "zsh-syntax-highlighting|https://github.com/zsh-users/zsh-syntax-highlighting"
)

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
brew_zsh_path() {
    local arch
    arch="$(uname -m)"
    for entry in "${BREW_ZSH_CANDIDATES[@]}"; do
        local candidate_arch="${entry%%|*}"
        local candidate_path="${entry##*|}"
        if [[ "$arch" == "$candidate_arch" && -f "$candidate_path" ]]; then
            echo "$candidate_path"
            return
        fi
    done
    echo ""
}

# Collect *.symlink sources into a named array (null-safe)
# Usage: collect_symlinks sources_array_name
# Requires bash 4.3+ (nameref). Homebrew bash satisfies this.
collect_symlinks() {
    if (( BASH_VERSINFO[0] < 4 || ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] < 3 ) )); then
        die "collect_symlinks requires bash 4.3+. Run: brew install bash"
    fi
    local -n _out="$1"
    _out=()
    while IFS= read -r -d '' src; do
        _out+=("$src")
    done < <(find "$DOTFILES_ROOT" -name '*.symlink' -print0 | sort -z)
}

# Resolve *.symlink path -> ~/.filename
symlink_dst() {
    local filename
    filename="$(basename "$1")"
    echo "$HOME/.${filename%.symlink}"
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
    # shellcheck disable=SC1091
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
# Zsh Plugins
# ==============================================================================

check_zsh() {
    require_command zsh || return 1
    log_ok "zsh $(zsh --version | awk '{print $2}')"
}

ensure_plugin_dir() {
    [[ -d "$ZSH_PLUGIN_DIR" ]] && return
    mkdir -p "$ZSH_PLUGIN_DIR"
    log_info "Created plugin directory: $ZSH_PLUGIN_DIR"
}

_plugin_default_branch() {
    local branch
    branch="$(git ls-remote --symref "$1" HEAD 2>/dev/null \
        | awk '/^ref:/ { sub("refs/heads/", "", $2); print $2; exit }')"

    if [[ -z "$branch" ]]; then
        log_warn "Could not detect default branch for $1 (network issue or repository not found)"
    fi

    echo "$branch"
}

# Install or update a zsh plugin from a git repository.
#
# Arguments:
#   $1  name  Plugin directory name
#   $2  url   Git repository URL
#
# Returns:
#   0    success (installed or updated)
#   1    installation failed (update failures are logged but not fatal)
#
# Side effects:
#   Creates directory under $ZSH_PLUGIN_DIR/$name
#   Clones with --depth=1 for faster downloads
install_or_update_plugin() {
    local name="$1" url="$2"
    local plugin_path="$ZSH_PLUGIN_DIR/$name"

    if [[ -d "$plugin_path" ]]; then
        log_info "Updating $name..."
        local current_branch
        current_branch="$(git -C "$plugin_path" symbolic-ref --short HEAD 2>/dev/null || true)"

        if [[ -z "$current_branch" ]]; then
            log_warn "$name is in detached HEAD state — skipping update"
        else
            quietly git -C "$plugin_path" pull --rebase --autostash -q origin "$current_branch" \
                || log_warn "Failed to update $name"
        fi
    else
        log_info "Installing $name..."
        local branch
        branch="$(_plugin_default_branch "$url")"
        quietly git clone --depth=1 ${branch:+--branch "$branch"} "$url" "$plugin_path" \
            || { log_error "Failed to clone $name"; return 1; }
        log_ok "Installed $name"
    fi
}

install_all_plugins() {
    local ok=0 fail=0

    for entry in "${ZSH_PLUGINS[@]}"; do
        local name="${entry%%|*}" url="${entry##*|}"
        if install_or_update_plugin "$name" "$url"; then
            ok=$(( ok + 1 ))
        else
            fail=$(( fail + 1 ))
        fi
    done

    echo ""
    log_ok "$ok plugin(s) up to date"
    if (( fail > 0 )); then
        log_warn "$fail plugin(s) failed"
        return 1
    fi
}

# ==============================================================================
# Node.js (fnm)
# ==============================================================================

setup_node() {
    require_command fnm "Install via Homebrew: brew install fnm" || return 1
    log_ok "fnm $(fnm --version)"

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

    eval "$(pyenv init -)"

    local current
    current="$(pyenv version-name 2>/dev/null || echo "")"

    if [[ -n "$current" && "$current" != "system" ]]; then
        log_skip "Python already active: $current"
    else
        local latest
        latest="$(pyenv install --list \
            | grep -E '^\s+[0-9]+\.[0-9]+\.[0-9]+$' \
            | tail -1 | tr -d ' ')"
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
# Symlinks  (*.symlink -> ~/.<name>)
# ==============================================================================

# Create symlinks from all *.symlink files to $HOME/.<name>.
#
# Arguments:
#   None
#
# Returns:
#   0    always (non-fatal)
#
# Side effects:
#   Creates symlinks in $HOME for each *.symlink file found
#   Backs up conflicting regular files to ~/.dotfiles-backup/
#   Removes existing symlinks that point to different targets
create_symlinks() {
    local backup_dir="$HOME/.dotfiles-backup" count=0
    local -a sources=()
    collect_symlinks sources

    (( ${#sources[@]} > 0 )) || { log_warn "No *.symlink files found"; return; }

    for src in "${sources[@]}"; do
        local dst
        dst="$(symlink_dst "$src")"

        # Already correct
        if [[ -L "$dst" && "$(readlink "$dst")" == "$src" ]]; then
            log_skip "$dst"
            continue
        fi

        # Conflict: only back up regular files (not symlinks)
        if [[ -e "$dst" && ! -L "$dst" ]]; then
            mkdir -p "$backup_dir"
            mv "$dst" "$backup_dir/"
            log_warn "Backed up: $(basename "$dst") -> $backup_dir/"
        elif [[ -L "$dst" ]]; then
            # Existing symlink (pointing elsewhere) — just remove it
            rm -f "$dst"
            log_warn "Removed old symlink: $dst"
        fi

        ln -s "$src" "$dst"
        log_ok "Linked: $dst"
        count=$(( count + 1 ))
    done

    echo ""
    log_ok "Created $count new symlink(s)"
    if [[ -d "$backup_dir" ]]; then
        log_warn "Backups saved to: $backup_dir"
    fi
}

# Remove all managed symlinks and restore from backups if available.
#
# Arguments:
#   None
#
# Returns:
#   0    always (non-fatal)
#
# Side effects:
#   Removes symlinks from $HOME that point to $DOTFILES_ROOT
#   Restores original files from ~/.dotfiles-backup/ if present
#   Removes backup directory if empty after restoration
remove_symlinks() {
    local count=0
    local backup_dir="$HOME/.dotfiles-backup"
    local -a sources=()
    collect_symlinks sources

    log_info "Removing managed symlinks..."

    for src in "${sources[@]}"; do
        local dst basename
        dst="$(symlink_dst "$src")"
        basename="$(basename "$dst")"
        if [[ -L "$dst" && "$(readlink "$dst")" == "$src" ]]; then
            rm -f "$dst"
            log_ok "Removed: $dst"
            # Restore from backup if available
            if [[ -e "$backup_dir/$basename" ]]; then
                mv "$backup_dir/$basename" "$dst"
                log_ok "Restored: $dst"
            fi
            count=$(( count + 1 ))
        fi
    done

    # Clean up backup directory if empty
    if [[ -d "$backup_dir" ]]; then
        if rmdir "$backup_dir" 2>/dev/null; then
            log_ok "Removed empty backup directory: $backup_dir"
        fi
    fi

    log_ok "Removed $count symlink(s)"
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
    target_zsh="$(brew_zsh_path)"

    if [[ -z "$target_zsh" ]]; then
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

    print_header "Zsh Plugins"
    check_zsh && { ensure_plugin_dir; install_all_plugins; }

    print_header "Node.js"
    setup_node || log_warn "Node.js setup skipped"

    print_header "Python"
    setup_python || log_warn "Python setup skipped"

    print_header "Symlinks"
    create_symlinks

    print_header "Git"
    setup_git

    print_header "Shell"
    switch_shell

    print_header "Done!"
    echo -e "  ${GREEN}Next steps:${NC}"
    echo "    1. Restart terminal or run: source ~/.zshrc"
    echo "    2. Create ~/.zshrc.local for machine-local overrides"
    echo ""
}

cmd_create_links() {
    print_header "Dotfiles — Create Links"
    create_symlinks
}

cmd_remove_links() {
    print_header "Dotfiles — Remove Links"
    echo -e "${YELLOW}This will remove all managed symlinks from \$HOME.${NC}"
    echo -n "Continue? [y/N] "
    read -r confirm || { log_warn "Failed to read input — aborting."; return; }
    [[ "$confirm" =~ ^[Yy]$ ]] || { log_info "Aborted."; return; }
    remove_symlinks
    log_ok "Unlink complete. Homebrew packages and shell config untouched."
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

cmd_install_plugins() {
    print_header "Dotfiles — Install Plugins"
    check_zsh || die "zsh is required"
    ensure_plugin_dir
    install_all_plugins
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

cmd_update_plugins() {
    print_header "Zsh Plugins — Update"
    check_zsh || die "zsh is required"
    ensure_plugin_dir
    install_all_plugins
}

cmd_show_status() {
    print_header "Dotfiles — Show Status"

    echo -e "${BOLD}Symlinks:${NC}"
    local -a sources=()
    collect_symlinks sources

    for src in "${sources[@]}"; do
        local dst
        dst="$(symlink_dst "$src")"
        if [[ -L "$dst" && "$(readlink "$dst")" == "$src" ]]; then
            echo -e "  ${GREEN}✔${NC}  $dst"
        elif [[ -e "$dst" ]]; then
            echo -e "  ${YELLOW}!${NC}  $dst  ${YELLOW}(conflict)${NC}"
        else
            echo -e "  ${RED}✘${NC}  $dst  ${RED}(missing)${NC}"
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
    echo -e "${BOLD}Shell:${NC}"
    local target_zsh
    target_zsh="$(brew_zsh_path)"
    if [[ "$SHELL" == "$target_zsh" ]]; then
        echo -e "  ${GREEN}✔${NC}  $SHELL"
    else
        echo -e "  ${YELLOW}!${NC}  $SHELL  ${YELLOW}(expected $target_zsh)${NC}"
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
    echo "  show-status     Show symlink and tool status"
    echo "  update-plugins  Update zsh plugins"
    echo ""
    echo -e "${BOLD}Symlinks:${NC}"
    echo "  create-links    Create symlinks from *.symlink files"
    echo "  remove-links    Remove all managed symlinks"
    echo ""
    echo -e "${BOLD}Individual Steps:${NC}"
    echo "  install-homebrew Install Homebrew and Brewfile packages"
    echo "  install-plugins Install zsh plugins"
    echo "  setup-node      Setup Node.js via fnm"
    echo "  setup-python    Setup Python via pyenv"
    echo "  config-git      Configure git user.name and user.email"
    echo "  switch-shell    Switch default shell to Homebrew zsh"
    echo ""
    echo -e "${BOLD}Options:${NC}"
    echo "  VERBOSE=0       Suppress command output (default: 1, verbose)"
    echo ""
    echo -e "${BOLD}Requirements:${NC}"
    echo "  bash 4.3+       Required for nameref feature (brew install bash)"
    echo ""
    echo -e "${BOLD}Examples:${NC}"
    echo "  ./setup.sh                    # Show this help"
    echo "  ./setup.sh full-setup         # Full setup"
    echo "  ./setup.sh create-links       # Re-link symlinks only"
    echo "  ./setup.sh setup-node setup-python  # Setup Node.js and Python"
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
        update-plugins)
            cmd_update_plugins "$@" ;;
        show-status)
            cmd_show_status "$@" ;;
        install-homebrew)
            cmd_install_homebrew "$@" ;;
        install-plugins)
            cmd_install_plugins "$@" ;;
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
