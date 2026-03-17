#!/usr/bin/env bash
# setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-17 21:00:00 Monday by zhengyu.li>
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
readonly SCRIPT_DIR="$(get_script_dir)"
readonly BREWFILE="${SCRIPT_DIR}/homebrew/Brewfile"
readonly DEFAULTS_SCRIPT="${SCRIPT_DIR}/macos/defaults.sh"

# =============================================================================
# Shell Detection & Switching
# =============================================================================

# Check if running in zsh
is_zsh() {
    [[ -n "${ZSH_VERSION:-}" ]]
}

# Switch to zsh if not already running in zsh
ensure_zsh() {
    if is_zsh; then
        return 0
    fi

    # Find zsh binary
    local zsh_bin=""
    if [[ -x "/opt/homebrew/bin/zsh" ]]; then
        zsh_bin="/opt/homebrew/bin/zsh"
    elif [[ -x "/usr/local/bin/zsh" ]]; then
        zsh_bin="/usr/local/bin/zsh"
    elif [[ -x "/bin/zsh" ]]; then
        zsh_bin="/bin/zsh"
    elif command -v zsh &>/dev/null; then
        zsh_bin="$(command -v zsh)"
    fi

    if [[ -z "${zsh_bin}" ]]; then
        printf '%b[WARN]%b zsh not found. Continuing with current shell.\n' \
               "${COLOR_YELLOW}" "${COLOR_RESET}"
        return 1
    fi

    printf '%b[INFO]%b Switching to zsh...\n' "${COLOR_BLUE}" "${COLOR_RESET}"

    # Re-execute this script with zsh
    exec "${zsh_bin}" "$0" "$@"
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

is_stowed() {
    local pkg="${1}"
    local target_dir
    local stow_dir="${SCRIPT_DIR}/${pkg}"

    case "${pkg}" in
        zsh)
            # zsh has .zshenv at $HOME and .config/zsh at $HOME/.config/zsh
            if [[ -L "${HOME}/.zshenv" ]]; then
                local link_target
                link_target=$(readlink "${HOME}/.zshenv")
                if [[ "${link_target}" == "${stow_dir}/.zshenv" ]]; then
                    return 0
                fi
            fi
            return 1
            ;;
        git|vim|emacs|ghostty|ripgrep|uv|bun)
            target_dir="${HOME}/.config/${pkg}"
            if [[ -L "${target_dir}" ]]; then
                local link_target
                link_target=$(readlink "${target_dir}")
                if [[ "${link_target}" == "${stow_dir}/.config/${pkg}" ]]; then
                    return 0
                fi
            fi
            return 1
            ;;
        starship)
            # starship has a single file .config/starship.toml
            local target_file="${HOME}/.config/starship.toml"
            if [[ -L "${target_file}" ]]; then
                local link_target
                link_target=$(readlink "${target_file}")
                if [[ "${link_target}" == "${stow_dir}/.config/starship.toml" ]]; then
                    return 0
                fi
            fi
            return 1
            ;;
        *)
            return 1
            ;;
    esac
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

    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

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
        print_error "Homebrew installation failed"
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
  ./setup.sh [OPTIONS] [COMMAND] [ARGS...]

Commands:
  --install [PKG...]      Install packages (default: all)
  --uninstall PKG...      Uninstall specified packages
  --update                Update repository and restow packages
  --status                Show current installation status
  --defaults              Apply macOS system defaults
  --interactive, -I       Enter interactive menu mode
  --help, -h              Show this help message

Packages:
  zsh, git, vim, emacs, ghostty, ripgrep, uv, bun, starship

Examples:
  ./setup.sh                          # Show this help
  ./setup.sh --install                # Install all packages
  ./setup.sh --install zsh git        # Install specific packages
  ./setup.sh --uninstall vim          # Uninstall vim package
  ./setup.sh --update                 # Update and restow
  ./setup.sh --interactive            # Interactive menu mode

Installation Order:
  1. Xcode Command Line Tools
  2. Homebrew
  3. Brew packages (including stow)
  4. Stow packages (zsh first for XDG env vars)
  5. Source ~/.zshenv

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
        print_info "Usage: ./setup.sh --uninstall <package>..."
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

# =============================================================================
# CLI Mode
# =============================================================================

run_cli_mode() {
    local mode="${1}"
    shift
    local -a args=("$@")

    case "${mode}" in
        install)
            run_install_prerequisites

            if (( ${#args[@]} == 0 )); then
                # No arguments = install all
                run_install_packages "${STOW_PACKAGES[@]}"
            elif [[ "${args[0]}" == "all" ]]; then
                run_install_packages "${STOW_PACKAGES[@]}"
            else
                # Validate package names
                local -a valid_packages=()
                local pkg
                for pkg in "${args[@]}"; do
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

                run_install_packages "${valid_packages[@]}"
            fi
            ;;
        uninstall)
            run_uninstall_packages "${args[@]}"
            ;;
        update)
            run_install_prerequisites
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
    # Switch to zsh if not already running in zsh
    ensure_zsh "$@"

    local mode="help"
    local -a args=()

    # Parse arguments
    while (( $# > 0 )); do
        case "${1}" in
            --install)
                mode="install"
                shift
                # Collect remaining arguments as package names
                while (( $# > 0 )) && [[ ! "${1}" =~ ^- ]]; do
                    args+=("${1}")
                    shift
                done
                ;;
            --uninstall)
                mode="uninstall"
                shift
                while (( $# > 0 )) && [[ ! "${1}" =~ ^- ]]; do
                    args+=("${1}")
                    shift
                done
                ;;
            --update)
                mode="update"
                shift
                ;;
            --status)
                mode="status"
                shift
                ;;
            --defaults)
                mode="defaults"
                shift
                ;;
            --interactive|-I)
                mode="interactive"
                shift
                ;;
            --help|-h)
                mode="help"
                shift
                ;;
            *)
                print_error "Unknown option: ${1}"
                show_help
                exit 1
                ;;
        esac
    done

    # Run in appropriate mode
    run_cli_mode "${mode}" "${args[@]}"
}

# Run main
main "$@"
