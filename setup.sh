#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-13 21:52:47 Friday by zhengyu.li>
#
# Copyright (C) 2026 zhengyu li
#
# Author: zhengyuli <lizhengyu419@outlook.com>
# Description: Dotfiles setup and maintenance utility

set -euo pipefail

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

log_info()  { echo -e "${BLUE}[INFO]${NC}  $*"; }
log_ok()    { echo -e "${GREEN}[OK]${NC}    $*"; }
log_skip()  { echo -e "${CYAN}[SKIP]${NC}  $*"; }
log_warn()  { echo -e "${YELLOW}[WARN]${NC}  $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*" >&2; }

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
    git ls-remote --symref "$1" HEAD 2>/dev/null \
        | awk '/^ref:/ { sub("refs/heads/", "", $2); print $2; exit }'
}

install_or_update_plugin() {
    local name="$1" url="$2"
    local plugin_path="$ZSH_PLUGIN_DIR/$name"

    if [[ -d "$plugin_path" ]]; then
        log_info "Updating $name..."
        quietly git -C "$plugin_path" pull --rebase --autostash -q origin \
            "$(git -C "$plugin_path" symbolic-ref --short HEAD)" \
            || log_warn "Failed to update $name"
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
            (( ok++ )) || true
        else
            (( fail++ )) || true
        fi
    done

    echo ""
    log_ok "$ok plugin(s) up to date"
    (( fail > 0 )) && log_warn "$fail plugin(s) failed"
    return $(( fail > 0 ? 1 : 0 ))
}

# ==============================================================================
# Node.js (fnm)
# ==============================================================================

setup_node() {
    require_command fnm "Install via Homebrew: brew install fnm" || return 1
    log_ok "fnm $(fnm --version)"

    eval "$(fnm env --shell bash)"

    if fnm list 2>/dev/null | grep -qE '\(lts'; then
        log_skip "Node.js LTS already installed: $(node --version 2>/dev/null)"
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
            export LDFLAGS="-L${ssl_prefix}/lib"
            export CPPFLAGS="-I${ssl_prefix}/include"
            export PKG_CONFIG_PATH="${ssl_prefix}/lib/pkgconfig"
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

create_symlinks() {
    local backup_dir="" count=0
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

        # Conflict — back up first
        if [[ -e "$dst" || -L "$dst" ]]; then
            if [[ -z "$backup_dir" ]]; then
                backup_dir="$HOME/.dotfiles-backup-$(date +%Y%m%d_%H%M%S)"
                mkdir -p "$backup_dir"
            fi
            mv "$dst" "$backup_dir/"
            log_warn "Backed up: $(basename "$dst") -> $backup_dir/"
        fi

        ln -s "$src" "$dst"
        log_ok "Linked: $dst"
        (( count++ )) || true
    done

    echo ""
    log_ok "Created $count new symlink(s)"
    [[ -n "$backup_dir" ]] && log_warn "Backups saved to: $backup_dir"
}

remove_symlinks() {
    local count=0
    local -a sources=()
    collect_symlinks sources

    log_info "Removing managed symlinks..."

    for src in "${sources[@]}"; do
        local dst
        dst="$(symlink_dst "$src")"
        if [[ -L "$dst" && "$(readlink "$dst")" == "$src" ]]; then
            rm -f "$dst"
            log_ok "Removed: $dst"
            (( count++ )) || true
        fi
    done

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

    log_info "Git $key not set. Enter your $field:"
    local value
    read -r value
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

    grep -qxF "$target_zsh" /etc/shells 2>/dev/null || {
        log_info "Registering $target_zsh in /etc/shells (requires sudo)..."
        echo "$target_zsh" | sudo tee -a /etc/shells >/dev/null
    }

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

cmd_setup() {
    print_header "Dotfiles — Setup"

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

cmd_link() {
    print_header "Dotfiles — Link"
    create_symlinks
}

cmd_unlink() {
    print_header "Dotfiles — Unlink"
    echo -e "${YELLOW}This will remove all managed symlinks from \$HOME.${NC}"
    echo -n "Continue? [y/N] "
    read -r confirm
    [[ "$confirm" =~ ^[Yy]$ ]] || { log_info "Aborted."; return; }
    remove_symlinks
    log_ok "Unlink complete. Homebrew packages and shell config untouched."
}

cmd_update() {
    print_header "Zsh Plugins — Update"
    check_zsh || die "zsh is required"
    ensure_plugin_dir
    install_all_plugins
}

cmd_status() {
    print_header "Dotfiles — Status"

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
    echo "  ./setup.sh [command]"
    echo ""
    echo -e "${BOLD}Commands:${NC}"
    echo "  setup           Full setup: Homebrew, plugins, languages, symlinks, shell  ${CYAN}(default)${NC}"
    echo "  link      Create symlinks from *.symlink files into \$HOME"
    echo "  unlink    Remove all managed symlinks from \$HOME"
    echo "  update    Update zsh plugins"
    echo "  status    Show symlink and tool status"
    echo "  help      Show this message"
    echo ""
    echo -e "${BOLD}Options:${NC}"
    echo "  VERBOSE=0   Suppress command output  (e.g. VERBOSE=0 ./setup.sh setup)"
    echo ""
    echo -e "${BOLD}Examples:${NC}"
    echo "  ./setup.sh                    # Full setup"
    echo "  ./setup.sh link               # Re-link symlinks only"
    echo "  ./setup.sh update-plugins     # Update zsh plugins"
    echo "  ./setup.sh status             # Check current state"
    echo "  VERBOSE=0 ./setup.sh setup    # Run silently"
    echo ""
}

# ==============================================================================
# Main
# ==============================================================================

main() {
    local cmd="${1:-setup}"
    shift || true

    case "$cmd" in
        setup)
            cmd_setup  "$@" ;;
        link)
            cmd_link   "$@" ;;
        unlink)
            cmd_unlink "$@" ;;
        update)
            cmd_update "$@" ;;
        status)
            cmd_status "$@" ;;
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
