#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-17 22:00:00 Tuesday by zhengyu.li>
# =============================================================================
# oh-my-dotfiles Interactive Setup Script
#
# Copyright (C) 2026 zhengyu.li
# Author: zhengyu.li <lizhengyu419@outlook.com>
#
# Location: ~/oh-my-dotfiles/setup.sh (not stowed; run directly)
# References:
#   1. GNU Stow Manual: https://www.gnu.org/software/stow/manual/
#   2. Homebrew Bundle: https://github.com/Homebrew/homebrew-bundle
# Note: This script manages installation, uninstallation, and updates for
#       dotfiles packages using GNU Stow symlink management.
# =============================================================================

set -euo pipefail

# =============================================================================
# Constants
# =============================================================================

# In test mode, skip readonly declarations to allow overrides
if [[ -z "${SETUP_SH_TEST_MODE:-}" ]]; then
    # --- Configuration Constants ---
    readonly MAX_BACKUPS=5                    # Maximum backup files per target
    readonly NETWORK_TIMEOUT=60               # Network request timeout (seconds)
    readonly NETWORK_RETRIES=3                # Number of retries for network requests

    # --- Package Type Configuration ---
    # Packages with single file at $HOME (e.g., .zshenv)
    readonly PKG_TYPE_HOME_FILE="home_file"
    # Packages with .config/pkg directory
    readonly PKG_TYPE_CONFIG_DIR="config_dir"
    # Packages with single .config/pkg.toml file
    readonly PKG_TYPE_CONFIG_FILE="config_file"

    # Package-to-type mapping (used by is_stowed and stow_package)
    readonly PKG_TYPE_ZSH="${PKG_TYPE_HOME_FILE}"
    readonly PKG_TYPE_STARSHIP="${PKG_TYPE_CONFIG_FILE}"
    # All other packages are PKG_TYPE_CONFIG_DIR

    # Colors for output
    readonly COLOR_RED='\033[0;31m'
    readonly COLOR_GREEN='\033[0;32m'
    readonly COLOR_YELLOW='\033[0;33m'
    readonly COLOR_BLUE='\033[0;34m'
    readonly COLOR_PURPLE='\033[0;35m'
    readonly COLOR_CYAN='\033[0;36m'
    readonly COLOR_WHITE='\033[0;37m'
    readonly COLOR_BOLD='\033[1m'
    readonly COLOR_RESET='\033[0m'

    # Package definitions
    readonly STOW_PACKAGES=(zsh git vim emacs ghostty ripgrep uv bun starship)
    readonly ESSENTIAL_PACKAGES=(zsh git)
    readonly TOOL_PACKAGES=(ghostty ripgrep uv bun starship)
    readonly EDITOR_PACKAGES=(vim emacs)
else
    # --- Configuration Constants (non-readonly in test mode) ---
    MAX_BACKUPS=5
    NETWORK_TIMEOUT=60
    NETWORK_RETRIES=3

    # --- Package Type Configuration (non-readonly in test mode) ---
    PKG_TYPE_HOME_FILE="home_file"
    PKG_TYPE_CONFIG_DIR="config_dir"
    PKG_TYPE_CONFIG_FILE="config_file"

    # Package-to-type mapping (non-readonly in test mode)
    PKG_TYPE_ZSH="${PKG_TYPE_HOME_FILE}"
    PKG_TYPE_STARSHIP="${PKG_TYPE_CONFIG_FILE}"
    # All other packages are PKG_TYPE_CONFIG_DIR

    # Colors for output (non-readonly in test mode)
    COLOR_RED='\033[0;31m'
    COLOR_GREEN='\033[0;32m'
    COLOR_YELLOW='\033[0;33m'
    COLOR_BLUE='\033[0;34m'
    COLOR_PURPLE='\033[0;35m'
    COLOR_CYAN='\033[0;36m'
    COLOR_WHITE='\033[0;37m'
    COLOR_BOLD='\033[1m'
    COLOR_RESET='\033[0m'

    # Package definitions (non-readonly in test mode)
    STOW_PACKAGES=(zsh git vim emacs ghostty ripgrep uv bun starship)
    ESSENTIAL_PACKAGES=(zsh git)
    TOOL_PACKAGES=(ghostty ripgrep uv bun starship)
    EDITOR_PACKAGES=(vim emacs)
fi

# Paths - compatible with both bash and zsh
get_script_dir() {
    if [[ -n "${BASH_SOURCE[0]:-}" ]]; then
        cd "$(dirname "${BASH_SOURCE[0]}")" && pwd
    elif [[ -n "${ZSH_VERSION:-}" ]]; then
        cd "$(dirname "${0}")" && pwd
    else
        cd "$(dirname "${0}")" && pwd
    fi
}

# In test mode, allow SCRIPT_DIR to be overridden
if [[ -z "${SETUP_SH_TEST_MODE:-}" ]]; then
    readonly SCRIPT_DIR="$(get_script_dir)"
    readonly BREWFILE="${SCRIPT_DIR}/homebrew/Brewfile"
    readonly DEFAULTS_SCRIPT="${SCRIPT_DIR}/macos/defaults.sh"
else
    SCRIPT_DIR="${SCRIPT_DIR:-$(get_script_dir)}"
    BREWFILE="${SCRIPT_DIR}/homebrew/Brewfile"
    DEFAULTS_SCRIPT="${SCRIPT_DIR}/macos/defaults.sh"
fi

# =============================================================================
# Shell Detection
# =============================================================================

# Check if running in zsh
is_zsh() {
    [[ -n "${ZSH_VERSION:-}" ]]
}

# Check prerequisites without installing them
# Returns 0 if all present, 1 if any missing
check_prerequisites_only() {
    local -a missing=()

    if [[ $(check_xcode_cli) != "installed" ]]; then
        missing+=("Xcode CLI")
    fi
    if [[ $(check_homebrew) != "installed" ]]; then
        missing+=("Homebrew")
    fi
    if [[ $(check_stow) != "installed" ]]; then
        missing+=("GNU Stow")
    fi

    if (( ${#missing[@]} > 0 )); then
        print_error "Missing prerequisites: ${missing[*]}"
        print_info "Run './setup.sh install-full' to install all prerequisites."
        return 1
    fi
    return 0
}

# Offer to change default login shell to zsh after installation
offer_shell_switch() {
    # Only offer if zsh package was installed and current shell is not zsh
    if ! is_stowed "zsh"; then
        return 0
    fi

    if is_zsh; then
        print_info "Already running in zsh."
        return 0
    fi

    printf '\n'
    if confirm "Switch default shell to zsh?" "y"; then
        local zsh_path
        zsh_path=$(command -v zsh)

        if [[ -z "${zsh_path}" ]]; then
            print_error "zsh not found in PATH"
            return 1
        fi

        # Check if zsh is in /etc/shells
        if ! grep -q "${zsh_path}" /etc/shells 2>/dev/null; then
            print_info "Adding ${zsh_path} to /etc/shells..."
            if ! echo "${zsh_path}" | sudo tee -a /etc/shells > /dev/null; then
                print_error "Failed to add ${zsh_path} to /etc/shells"
                return 1
            fi
            # Verify the write succeeded
            if ! grep -q "${zsh_path}" /etc/shells 2>/dev/null; then
                print_error "Verification failed: ${zsh_path} not found in /etc/shells"
                return 1
            fi
            print_success "Added ${zsh_path} to /etc/shells"
        fi

        print_info "Changing default shell..."
        if ! chsh -s "${zsh_path}"; then
            print_error "Failed to change default shell"
            return 1
        fi

        print_success "Default shell changed to zsh"
        print_info "Run 'zsh' or open a new terminal to use zsh."
    else
        print_info "Skipped shell switch. Run 'chsh -s \$(which zsh)' manually later."
    fi
}

# =============================================================================
# Output Functions
# =============================================================================

print_header() {
    printf '\n'
    printf '%b' "${COLOR_BOLD}${COLOR_CYAN}"
    printf '=%.0s' {1..79}
    printf '\n'
    printf '%b' "${COLOR_RESET}"
    printf '  %b\n' "${1}"
    printf '%b' "${COLOR_BOLD}${COLOR_CYAN}"
    printf '=%.0s' {1..79}
    printf '\n'
    printf '%b' "${COLOR_RESET}"
}

print_section() {
    printf '\n'
    printf '%b' "${COLOR_BOLD}${COLOR_BLUE}"
    printf -- '-%.0s' {1..79}
    printf '\n'
    printf '  %b\n' "${1}"
    printf '%b' "${COLOR_RESET}"
}

print_success() {
    printf '  %b[OK]%b %s\n' "${COLOR_GREEN}" "${COLOR_RESET}" "${1}"
}

print_error() {
    printf '  %b[ERROR]%b %s\n' "${COLOR_RED}" "${COLOR_RESET}" "${1}" >&2
}

print_warning() {
    printf '  %b[WARN]%b %s\n' "${COLOR_YELLOW}" "${COLOR_RESET}" "${1}"
}

print_info() {
    printf '  %b[INFO]%b %s\n' "${COLOR_BLUE}" "${COLOR_RESET}" "${1}"
}

print_status() {
    local state="${1}"
    local label="${2}"

    if [[ "${state}" == "yes" || "${state}" == "installed" ]]; then
        printf '    %-20s %b[%b]%b\n' "${label}:" "${COLOR_GREEN}" "Installed" "${COLOR_RESET}"
    elif [[ "${state}" == "no" || "${state}" == "not_installed" ]]; then
        printf '    %-20s %b[%b]%b\n' "${label}:" "${COLOR_YELLOW}" "Not Installed" "${COLOR_RESET}"
    else
        printf '    %-20s [%b]\n' "${label}:" "${state}"
    fi
}

# =============================================================================
# Utility Functions
# =============================================================================

command_exists() {
    command -v "${1}" &>/dev/null
}

confirm() {
    local prompt="${1}"
    local default="${2:-n}"
    local response

    if [[ "${default}" == "y" ]]; then
        printf '%s [Y/n]: ' "${prompt}"
    else
        printf '%s [y/N]: ' "${prompt}"
    fi

    read -r response
    response="${response:-${default}}"

    [[ "${response}" =~ ^[Yy]$ ]]
}

backup_file() {
    local target="${1}"
    local backup_num=0
    local backup_path

    if [[ ! -e "${target}" ]]; then
        return 0
    fi

    # Find next available backup number
    while true; do
        backup_path="${target}.bak.${backup_num}"
        if [[ ! -e "${backup_path}" ]]; then
            break
        fi
        ((backup_num++))
    done

    # Cleanup old backups if exceeding MAX_BACKUPS limit
    if (( backup_num >= MAX_BACKUPS )); then
        local cleanup_num=0
        local keep_start=$((backup_num - MAX_BACKUPS + 1))
        print_info "Cleaning up old backups (keeping last ${MAX_BACKUPS})..."
        while (( cleanup_num < keep_start )); do
            local old_backup="${target}.bak.${cleanup_num}"
            if [[ -e "${old_backup}" ]]; then
                rm -f "${old_backup}"
                print_info "Removed old backup: ${old_backup}"
            fi
            ((cleanup_num++))
        done
    fi

    print_warning "Backing up existing file: ${target} -> ${backup_path}"
    mv "${target}" "${backup_path}"
}

is_valid_package() {
    local pkg="${1}"
    local valid_pkg

    for valid_pkg in "${STOW_PACKAGES[@]}"; do
        if [[ "${pkg}" == "${valid_pkg}" ]]; then
            return 0
        fi
    done
    return 1
}

list_valid_packages() {
    printf '  Available packages: '
    printf '%b%s%b ' "${COLOR_GREEN}" "${STOW_PACKAGES[0]}" "${COLOR_RESET}"
    local pkg
    for pkg in "${STOW_PACKAGES[@]:1}"; do
        printf '%s ' "${pkg}"
    done
    printf '\n'
}

# =============================================================================
# Detection Functions
# =============================================================================

check_xcode_cli() {
    if xcode-select -p &>/dev/null; then
        echo "installed"
    else
        echo "not_installed"
    fi
}

check_homebrew() {
    if command_exists brew; then
        echo "installed"
    else
        echo "not_installed"
    fi
}

check_stow() {
    if command_exists stow; then
        echo "installed"
    else
        echo "not_installed"
    fi
}

# Resolve a path to its absolute form (portable for macOS/Linux)
resolve_path() {
    local path="${1}"
    local base_dir="${2:-.}"

    # If path is absolute, use it directly
    if [[ "${path}" == /* ]]; then
        if [[ -d "${path}" ]]; then
            cd "${path}" 2>/dev/null && pwd || echo "${path}"
        else
            echo "${path}"
        fi
        return
    fi

    # Resolve relative path from base directory
    local orig_dir="$(pwd)"
    cd "${base_dir}" 2>/dev/null || return 1

    # Handle parent directory traversals (.., ../.., etc.)
    # These are directory-only paths, so just resolve the directory
    if [[ "${path}" == ".." ]] || [[ "${path}" == "../"* ]]; then
        local resolved
        resolved=$(cd "${path}" 2>/dev/null && pwd)
        cd "${orig_dir}" 2>/dev/null
        echo "${resolved}"
        return
    fi

    local dir_part="$(dirname "${path}")"
    local base_part="$(basename "${path}")"

    if [[ "${dir_part}" == "." ]]; then
        # Path is in current directory
        echo "$(pwd)/${base_part}"
    else
        # Path has directory component
        if cd "${dir_part}" 2>/dev/null; then
            echo "$(pwd)/${base_part}"
        else
            echo "${path}"
        fi
    fi

    cd "${orig_dir}" 2>/dev/null
}

# Get package type for symlink detection
get_package_type() {
    local pkg="${1}"

    case "${pkg}" in
        zsh) echo "${PKG_TYPE_ZSH}" ;;
        starship) echo "${PKG_TYPE_STARSHIP}" ;;
        *) echo "${PKG_TYPE_CONFIG_DIR}" ;;
    esac
}

# Resolve symlink to absolute path (helper for is_stowed)
resolve_symlink() {
    local link="${1}"
    local base_dir="${2:-.}"

    if [[ "${link}" == /* ]]; then
        echo "${link}"
    else
        local dir_part
        dir_part=$(cd "${base_dir}" 2>/dev/null && cd "$(dirname "${link}")" 2>/dev/null && pwd)
        if [[ -n "${dir_part}" ]]; then
            echo "${dir_part}/$(basename "${link}")"
        else
            echo "${link}"
        fi
    fi
}

is_stowed() {
    local pkg="${1}"
    local stow_dir="${SCRIPT_DIR}/${pkg}"
    local pkg_type
    local target
    local expected_target

    pkg_type=$(get_package_type "${pkg}")

    case "${pkg_type}" in
        "${PKG_TYPE_HOME_FILE}")
            # Package with file at $HOME (e.g., .zshenv)
            target="${HOME}/.${pkg}env"
            expected_target="${stow_dir}/.${pkg}env"
            ;;
        "${PKG_TYPE_CONFIG_FILE}")
            # Package with single .config/pkg.toml file
            target="${HOME}/.config/${pkg}.toml"
            expected_target="${stow_dir}/.config/${pkg}.toml"
            ;;
        "${PKG_TYPE_CONFIG_DIR}"|*)
            # Package with .config/pkg directory (default)
            target="${HOME}/.config/${pkg}"
            expected_target="${stow_dir}/.config/${pkg}"
            ;;
    esac

    if [[ -L "${target}" ]]; then
        local link_target
        local base_dir
        base_dir=$(dirname "${target}")
        link_target=$(readlink "${target}")
        local resolved_target
        resolved_target=$(resolve_symlink "${link_target}" "${base_dir}")

        if [[ "${resolved_target}" == "${expected_target}" ]]; then
            return 0
        fi
    fi
    return 1
}

count_stowed_packages() {
    local count=0
    local pkg

    for pkg in "${STOW_PACKAGES[@]}"; do
        if is_stowed "${pkg}"; then
            ((count++))
        fi
    done

    echo "${count}"
}

# =============================================================================
# Core Operations
# =============================================================================

install_xcode_cli() {
    print_section "Installing Xcode Command Line Tools"

    if [[ $(check_xcode_cli) == "installed" ]]; then
        print_success "Xcode Command Line Tools already installed"
        return 0
    fi

    print_info "Installing Xcode Command Line Tools..."
    print_warning "A popup window will appear. Please follow the prompts."

    xcode-select --install

    print_info "Waiting for Xcode CLI installation to complete..."
    print_info "Press any key once the installation is finished..."
    read -r -n 1 -s

    if [[ $(check_xcode_cli) == "installed" ]]; then
        print_success "Xcode Command Line Tools installed successfully"
        return 0
    else
        print_error "Xcode Command Line Tools installation failed"
        return 1
    fi
}

install_homebrew() {
    print_section "Installing Homebrew"

    if [[ $(check_homebrew) == "installed" ]]; then
        print_success "Homebrew already installed"
        return 0
    fi

    print_info "Installing Homebrew..."
    print_warning "This may take a few minutes."

    local install_url="https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"
    local retry_count=0
    local install_success=false

    while (( retry_count < NETWORK_RETRIES )); do
        if (( retry_count > 0 )); then
            print_info "Retry ${retry_count}/${NETWORK_RETRIES}..."
        fi

        if curl --fail --silent --show-error \
                --connect-timeout "${NETWORK_TIMEOUT}" \
                --max-time "$((NETWORK_TIMEOUT * 10))" \
                "${install_url}" | /bin/bash; then
            install_success=true
            break
        fi

        ((retry_count++))
        if (( retry_count < NETWORK_RETRIES )); then
            print_warning "Download failed, retrying in 5 seconds..."
            sleep 5
        fi
    done

    if [[ "${install_success}" != "true" ]]; then
        print_error "Homebrew installation failed after ${NETWORK_RETRIES} attempts"
        return 1
    fi

    # Add Homebrew to PATH for current session (Apple Silicon Macs)
    if [[ -f /opt/homebrew/bin/brew ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    elif [[ -f /usr/local/bin/brew ]]; then
        eval "$(/usr/local/bin/brew shellenv)"
    fi

    if [[ $(check_homebrew) == "installed" ]]; then
        print_success "Homebrew installed successfully"
        return 0
    else
        print_error "Homebrew installation verification failed"
        return 1
    fi
}

install_brew_packages() {
    print_section "Installing Homebrew Packages"

    if [[ ! -f "${BREWFILE}" ]]; then
        print_error "Brewfile not found: ${BREWFILE}"
        return 1
    fi

    print_info "Running brew bundle..."
    print_warning "This may take several minutes for first-time installation."

    if brew bundle --file="${BREWFILE}"; then
        print_success "Homebrew packages installed successfully"
        return 0
    else
        print_error "Some packages failed to install"
        return 1
    fi
}

stow_package() {
    local pkg="${1}"
    local stow_dir="${SCRIPT_DIR}/${pkg}"

    print_info "Stowing ${pkg}..."

    # Check if package directory exists
    if [[ ! -d "${stow_dir}" ]]; then
        print_error "Package directory not found: ${stow_dir}"
        return 1
    fi

    # Handle potential conflicts for zsh (.zshenv)
    if [[ "${pkg}" == "zsh" ]]; then
        if [[ -e "${HOME}/.zshenv" && ! -L "${HOME}/.zshenv" ]]; then
            backup_file "${HOME}/.zshenv"
        fi
    fi

    # Handle potential conflicts for other packages (.config/pkg or .config/pkg.toml)
    if [[ -d "${stow_dir}/.config/${pkg}" ]]; then
        if [[ -e "${HOME}/.config/${pkg}" && ! -L "${HOME}/.config/${pkg}" ]]; then
            backup_file "${HOME}/.config/${pkg}"
        fi
    fi

    # Handle starship (single file: .config/starship.toml)
    if [[ "${pkg}" == "starship" ]]; then
        if [[ -e "${HOME}/.config/starship.toml" && ! -L "${HOME}/.config/starship.toml" ]]; then
            backup_file "${HOME}/.config/starship.toml"
        fi
    fi

    # Create .config directory if it doesn't exist
    mkdir -p "${HOME}/.config"

    # Run stow
    if stow -d "${SCRIPT_DIR}" -t "${HOME}" "${pkg}"; then
        print_success "${pkg} stowed successfully"
        return 0
    else
        print_error "Failed to stow ${pkg}"
        return 1
    fi
}

unstow_package() {
    local pkg="${1}"

    print_info "Unstowing ${pkg}..."

    if ! is_stowed "${pkg}"; then
        print_warning "${pkg} is not currently stowed"
        return 0
    fi

    if stow -D -d "${SCRIPT_DIR}" -t "${HOME}" "${pkg}"; then
        print_success "${pkg} unstowed successfully"
        return 0
    else
        print_error "Failed to unstow ${pkg}"
        return 1
    fi
}

apply_macos_defaults() {
    print_section "Applying macOS System Defaults"

    if [[ ! -f "${DEFAULTS_SCRIPT}" ]]; then
        print_error "macOS defaults script not found: ${DEFAULTS_SCRIPT}"
        return 1
    fi

    print_warning "This will modify system preferences."
    print_info "Some changes may require logout or restart."

    if confirm "Apply macOS defaults?" "n"; then
        if bash "${DEFAULTS_SCRIPT}"; then
            print_success "macOS defaults applied successfully"
            return 0
        else
            print_error "Failed to apply some macOS defaults"
            return 1
        fi
    else
        print_info "Skipped macOS defaults"
        return 0
    fi
}

update_repository() {
    print_section "Updating Repository"

    print_info "Pulling latest changes..."
    if git -C "${SCRIPT_DIR}" pull; then
        print_success "Repository updated successfully"
    else
        print_error "Failed to update repository"
        return 1
    fi

    print_info "Updating Homebrew packages..."
    if brew bundle --file="${BREWFILE}"; then
        print_success "Homebrew packages updated"
    else
        print_warning "Some packages may not have updated"
    fi

    # Restow all currently stowed packages
    local pkg
    for pkg in "${STOW_PACKAGES[@]}"; do
        if is_stowed "${pkg}"; then
            print_info "Restowing ${pkg}..."
            stow -R -d "${SCRIPT_DIR}" -t "${HOME}" "${pkg}" || true
        fi
    done

    print_success "Update complete"
}

# =============================================================================
# Menu Functions
# =============================================================================

show_status() {
    print_header "Current Status"

    printf '\n'
    print_status "$(check_xcode_cli)" "Xcode CLI"
    print_status "$(check_homebrew)" "Homebrew"
    print_status "$(check_stow)" "GNU Stow"
    printf '\n'

    print_section "Stowed Packages ($(count_stowed_packages)/${#STOW_PACKAGES[@]})"

    local pkg
    for pkg in "${STOW_PACKAGES[@]}"; do
        if is_stowed "${pkg}"; then
            printf '    %b[*]%b %s\n' "${COLOR_GREEN}" "${COLOR_RESET}" "${pkg}"
        else
            printf '    [ ] %s\n' "${pkg}"
        fi
    done
}

show_help() {
    cat <<'EOF'
oh-my-dotfiles Setup Script

Usage:
  ./setup.sh <command> [args]

Commands:
  install-full          Full install: prerequisites + all packages + shell switch
  install all           Same as install-full
  install <pkg>...      Install specific packages (prerequisites must exist)
  uninstall <pkg>...    Uninstall specified packages
  update                Update repository and restow packages
  status                Show current installation status
  defaults              Apply macOS system defaults
  interactive, -I       Enter interactive menu mode
  help, -h              Show this help message

Packages:
  zsh, git, vim, emacs, ghostty, ripgrep, uv, bun, starship

Examples:
  ./setup.sh                        # Show this help
  ./setup.sh install-full           # Full installation
  ./setup.sh install zsh git        # Install specific packages
  ./setup.sh uninstall vim          # Uninstall vim package
  ./setup.sh update                 # Update and restow
  ./setup.sh status                 # View status

Installation Order (install-full):
  1. Xcode Command Line Tools
  2. Homebrew
  3. Brew packages (including stow)
  4. Stow packages (zsh first for XDG env vars)
  5. Offer to switch default shell to zsh

For more information, see: https://github.com/zhengyuli/oh-my-dotfiles
EOF
}

show_main_menu() {
    local choice

    while true; do
        print_header "oh-my-dotfiles Setup"

        printf '\n'
        print_status "$(check_xcode_cli)" "Xcode CLI"
        print_status "$(check_homebrew)" "Homebrew"
        print_status "$(check_stow)" "GNU Stow"
        printf '    %-20s %d/%d\n' "Stowed Packages:" "$(count_stowed_packages)" "${#STOW_PACKAGES[@]}"
        printf '\n'

        printf '  %bMain Menu:%b\n' "${COLOR_BOLD}" "${COLOR_RESET}"
        printf '    1. Install Packages\n'
        printf '    2. Uninstall Packages\n'
        printf '    3. Update Repository & Packages\n'
        printf '    4. Apply macOS Defaults\n'
        printf '    5. View Status\n'
        printf '    q. Quit\n'
        printf '\n'
        printf '  Enter choice: '
        read -r choice

        case "${choice}" in
            1) show_install_menu ;;
            2) show_uninstall_menu ;;
            3) update_repository ;;
            4) apply_macos_defaults ;;
            5) show_status ;;
            q|Q)
                printf '\n'
                print_info "Goodbye!"
                exit 0
                ;;
            *)
                print_warning "Invalid choice. Please try again."
                ;;
        esac

        printf '\n'
        printf '  Press any key to continue...'
        read -r -n 1 -s
        printf '\n'
    done
}

show_install_menu() {
    local choice
    local -a selected=()
    local pkg

    while true; do
        print_header "Install Packages"

        # Show prerequisites status
        printf '\n'
        printf '  %bPrerequisites:%b\n' "${COLOR_BOLD}" "${COLOR_RESET}"

        if [[ $(check_xcode_cli) == "installed" ]]; then
            printf '    %b[X]%b Xcode CLI\n' "${COLOR_GREEN}" "${COLOR_RESET}"
        else
            printf '    [ ] Xcode CLI\n'
        fi

        if [[ $(check_homebrew) == "installed" ]]; then
            printf '    %b[X]%b Homebrew\n' "${COLOR_GREEN}" "${COLOR_RESET}"
        else
            printf '    [ ] Homebrew\n'
        fi

        if [[ $(check_stow) == "installed" ]]; then
            printf '    %b[X]%b GNU Stow\n' "${COLOR_GREEN}" "${COLOR_RESET}"
        else
            printf '    [ ] GNU Stow\n'
        fi

        printf '\n'
        printf '  %bAvailable Packages:%b\n' "${COLOR_BOLD}" "${COLOR_RESET}"
        printf '\n'
        printf '    Essential:\n'

        local i=1
        for pkg in "${ESSENTIAL_PACKAGES[@]}"; do
            if is_stowed "${pkg}"; then
                printf '      %b[X]%b %d. %s\n' "${COLOR_GREEN}" "${COLOR_RESET}" "${i}" "${pkg}"
            else
                printf '      [ ] %d. %s\n' "${i}" "${pkg}"
            fi
            ((i++))
        done

        printf '\n'
        printf '    Editors:\n'

        for pkg in "${EDITOR_PACKAGES[@]}"; do
            if is_stowed "${pkg}"; then
                printf '      %b[X]%b %d. %s\n' "${COLOR_GREEN}" "${COLOR_RESET}" "${i}" "${pkg}"
            else
                printf '      [ ] %d. %s\n' "${i}" "${pkg}"
            fi
            ((i++))
        done

        printf '\n'
        printf '    Tools:\n'

        for pkg in "${TOOL_PACKAGES[@]}"; do
            if is_stowed "${pkg}"; then
                printf '      %b[X]%b %d. %s\n' "${COLOR_GREEN}" "${COLOR_RESET}" "${i}" "${pkg}"
            else
                printf '      [ ] %d. %s\n' "${i}" "${pkg}"
            fi
            ((i++))
        done

        printf '\n'
        printf '  %bQuick Install:%b\n' "${COLOR_BOLD}" "${COLOR_RESET}"
        printf '    a. Install all\n'
        printf '    e. Essential only\n'
        printf '    r. Return\n'
        printf '\n'
        printf '  Enter choice: '
        read -r choice

        case "${choice}" in
            a|A)
                run_install_prerequisites
                run_install_all
                return
                ;;
            e|E)
                run_install_prerequisites
                run_install_packages "${ESSENTIAL_PACKAGES[@]}"
                return
                ;;
            r|R)
                return
                ;;
            [0-9]*)
                # Handle numeric selection
                local idx=$((choice - 1))
                if (( idx >= 0 && idx < ${#STOW_PACKAGES[@]} )); then
                    run_install_prerequisites
                    run_install_packages "${STOW_PACKAGES[idx]}"
                    return
                else
                    print_warning "Invalid selection. Please try again."
                fi
                ;;
            *)
                print_warning "Invalid choice. Please try again."
                ;;
        esac
    done
}

show_uninstall_menu() {
    local choice
    local pkg
    local i=1

    print_header "Uninstall Packages"

    # Count stowed packages
    local stowed_count=0
    for pkg in "${STOW_PACKAGES[@]}"; do
        if is_stowed "${pkg}"; then
            ((stowed_count++))
        fi
    done

    if (( stowed_count == 0 )); then
        print_info "No packages are currently stowed."
        return
    fi

    printf '\n'
    printf '  %bStowed Packages:%b\n' "${COLOR_BOLD}" "${COLOR_RESET}"
    printf '\n'

    for pkg in "${STOW_PACKAGES[@]}"; do
        if is_stowed "${pkg}"; then
            printf '    %d. %s\n' "${i}" "${pkg}"
            ((i++))
        fi
    done

    printf '\n'
    printf '    a. Uninstall all\n'
    printf '    r. Return\n'
    printf '\n'
    printf '  Enter choice: '
    read -r choice

    case "${choice}" in
        a|A)
            if confirm "Uninstall all stowed packages?" "n"; then
                for pkg in "${STOW_PACKAGES[@]}"; do
                    if is_stowed "${pkg}"; then
                        unstow_package "${pkg}"
                    fi
                done
            fi
            ;;
        r|R)
            return
            ;;
        [0-9]*)
            local idx=1
            for pkg in "${STOW_PACKAGES[@]}"; do
                if is_stowed "${pkg}"; then
                    if (( choice == idx )); then
                        unstow_package "${pkg}"
                        return
                    fi
                    ((idx++))
                fi
            done
            print_warning "Invalid selection."
            ;;
        *)
            print_warning "Invalid choice."
            ;;
    esac
}

# =============================================================================
# Install Helpers
# =============================================================================

run_install_prerequisites() {
    # Check and install Xcode CLI
    if [[ $(check_xcode_cli) != "installed" ]]; then
        install_xcode_cli
    fi

    # Check and install Homebrew
    if [[ $(check_homebrew) != "installed" ]]; then
        install_homebrew
    fi

    # Check and install stow via brew bundle
    if [[ $(check_stow) != "installed" ]]; then
        print_info "GNU Stow not found. Installing via brew bundle..."
        install_brew_packages
    fi

    # Verify stow is available
    if ! command_exists stow; then
        print_error "GNU Stow is required but not available. Please install it first."
        exit 1
    fi
}

run_install_packages() {
    local -a packages=("$@")
    local pkg

    if (( ${#packages[@]} == 0 )); then
        packages=("${STOW_PACKAGES[@]}")
    fi

    print_section "Installing Packages"

    for pkg in "${packages[@]}"; do
        if is_stowed "${pkg}"; then
            print_info "${pkg} is already stowed, skipping..."
        else
            stow_package "${pkg}"
        fi
    done

    printf '\n'
    print_success "Installation complete!"
    print_info "Run 'source ~/.zshenv' or restart your shell to apply changes."
}

run_uninstall_packages() {
    local -a packages=("$@")
    local pkg

    if (( ${#packages[@]} == 0 )); then
        print_error "No packages specified for uninstallation."
        print_info "Usage: ./setup.sh uninstall <package>..."
        list_valid_packages
        return 1
    fi

    print_section "Uninstalling Packages"

    for pkg in "${packages[@]}"; do
        if is_valid_package "${pkg}"; then
            unstow_package "${pkg}"
        else
            print_warning "Invalid package: ${pkg}"
        fi
    done
}

# Full installation workflow: prerequisites + all packages + shell switch
run_full_install() {
    # 1. Install prerequisites
    run_install_prerequisites

    # 2. Install all packages
    run_install_packages "${STOW_PACKAGES[@]}"

    # 3. Offer shell switch
    printf '\n'
    offer_shell_switch

    printf '\n'
    print_success "Full installation complete!"
}

# Partial installation: check prerequisites, error if missing, stow specified packages only
run_partial_install() {
    local -a packages=("$@")

    # 1. Check prerequisites (don't install)
    if ! check_prerequisites_only; then
        exit 1
    fi

    # 2. Validate package names
    local -a valid_packages=()
    local pkg
    for pkg in "${packages[@]}"; do
        if is_valid_package "${pkg}"; then
            valid_packages+=("${pkg}")
        else
            print_warning "Invalid package: ${pkg}"
        fi
    done

    if (( ${#valid_packages[@]} == 0 )); then
        print_error "No valid packages specified."
        list_valid_packages
        exit 1
    fi

    # 3. Stow specified packages
    print_section "Installing Packages"

    for pkg in "${valid_packages[@]}"; do
        if is_stowed "${pkg}"; then
            print_info "${pkg} is already stowed, skipping..."
        else
            stow_package "${pkg}"
        fi
    done

    printf '\n'
    print_success "Package installation complete!"
    print_info "Run 'source ~/.zshenv' or restart your shell to apply changes."
}

# =============================================================================
# CLI Mode
# =============================================================================

run_cli_mode() {
    local mode="${1}"
    shift
    local -a args=("$@")

    case "${mode}" in
        install-full)
            run_full_install
            ;;
        install)
            if (( ${#args[@]} == 0 )); then
                # No args = show help
                show_help
                exit 0
            elif [[ "${args[0]}" == "all" ]]; then
                run_full_install
            else
                run_partial_install "${args[@]}"
            fi
            ;;
        uninstall)
            run_uninstall_packages "${args[@]}"
            ;;
        update)
            update_repository
            ;;
        status)
            show_status
            ;;
        defaults)
            apply_macos_defaults
            ;;
        interactive)
            show_main_menu
            ;;
        help)
            show_help
            ;;
        *)
            print_error "Unknown command: ${mode}"
            show_help
            exit 1
            ;;
    esac
}

# =============================================================================
# Main Entry
# =============================================================================

main() {
    local mode="help"
    local -a args=()

    # Parse arguments (subcommand style, no -- prefix required)
    while (( $# > 0 )); do
        case "${1}" in
            install-full)
                mode="install-full"
                shift
                ;;
            install)
                mode="install"
                shift
                while (( $# > 0 )) && [[ ! "${1}" =~ ^- ]]; do
                    args+=("${1}")
                    shift
                done
                ;;
            uninstall)
                mode="uninstall"
                shift
                while (( $# > 0 )) && [[ ! "${1}" =~ ^- ]]; do
                    args+=("${1}")
                    shift
                done
                ;;
            update)
                mode="update"
                shift
                ;;
            status)
                mode="status"
                shift
                ;;
            defaults)
                mode="defaults"
                shift
                ;;
            interactive|-I)
                mode="interactive"
                shift
                ;;
            help|-h|--help)
                mode="help"
                shift
                ;;
            -*)
                print_error "Unknown option: ${1}"
                print_info "Did you mean a subcommand? Try './setup.sh help'"
                show_help
                exit 1
                ;;
            *)
                # Treat as subcommand
                mode="${1}"
                shift
                while (( $# > 0 )) && [[ ! "${1}" =~ ^- ]]; do
                    args+=("${1}")
                    shift
                done
                ;;
        esac
    done

    # Run in appropriate mode
    run_cli_mode "${mode}" "${args[@]}"
}

# Run main (skip if in test mode)
if [[ -z "${SETUP_SH_TEST_MODE:-}" ]]; then
    main "$@"
fi
