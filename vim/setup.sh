#!/bin/bash

#---------------------------------------------------------------------------------
# Name: setup.sh
# Purpose: Vim configurations setup script for macOS
#
# Time-stamp: <2026-02-16 14:30:00 Sunday by zhengyuli>
#
# Author: zhengyu li
# Created: 2014-03-26
# Optimized: Symlink mode with dependency guide
#
# Copyright (c) 2014, 2022, 2023, 2024, 2025, 2026 zhengyu li <lizhengyu419@gmail.com>
#---------------------------------------------------------------------------------

set -euo pipefail

# Global variables
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly VIM_CONFIG_FILE="$HOME/.vimrc"

# Color codes for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m' # No Color

# Logging function
log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp
    timestamp=$(date '+%Y-%m-%d %H:%M:%S')

    if [[ "$level" == "ERROR" ]]; then
        echo -e "${timestamp} [${level}] ${message}" >&2
        echo -e "${RED}Error: $*${NC}" >&2
    else
        echo -e "${timestamp} [${level}] ${message}"
        if [[ "$level" == "SUCCESS" ]]; then
            echo -e "${GREEN}$*${NC}"
        elif [[ "$level" == "WARNING" ]]; then
            echo -e "${YELLOW}$*${NC}"
        fi
    fi
}

log_info() {
    log "INFO" "$*"
}

log_error() {
    log "ERROR" "$*"
}

log_success() {
    log "SUCCESS" "$*"
}

log_warning() {
    log "WARNING" "$*"
}

# Function to handle errors
error_exit() {
    log_error "$1"
    exit 1
}

# Function to check if running on macOS
check_macos() {
    if [[ "$OSTYPE" != "darwin"* ]]; then
        error_exit "This script only supports macOS. Current OS: $OSTYPE"
    fi
    log_info "Running on macOS"
}

# Function to check if Vim is installed
check_vim_installed() {
    if ! command -v vim &> /dev/null; then
        error_exit "Vim is not installed. Please install Vim first:
  brew install vim"
    fi

    local vim_version
    vim_version=$(vim --version | head -1 | grep -oE '[0-9]+\.[0-9]+')
    log_info "Vim version: $vim_version"

    # Check minimum version (8.0)
    local major minor
    major=$(echo "$vim_version" | cut -d. -f1)
    minor=$(echo "$vim_version" | cut -d. -f2)

    if [[ "$major" -lt 8 ]]; then
        error_exit "Vim version must be >= 8.0. Current: $vim_version"
    fi

    # Check for Python support (optional but recommended)
    if vim --version | grep -q '+python3'; then
        log_info "Vim has Python3 support"
    else
        log_warning "Vim does not have Python3 support. Some plugins may not work."
    fi
}

# Function to validate source file
validate_source_file() {
    local source_file="$1"
    if [[ ! -f "$source_file" ]]; then
        error_exit "Source configuration file not found: $source_file"
    fi
    if [[ ! -r "$source_file" ]]; then
        error_exit "Cannot read source configuration file: $source_file"
    fi
}

# Function to create necessary directories
create_vim_directories() {
    local dirs=(
        "$HOME/.vim"
        "$HOME/.vim/undo"
        "$HOME/.vim/backup"
        "$HOME/.vim/swap"
        "$HOME/.vim/pack/plugins/start"
    )

    for dir in "${dirs[@]}"; do
        if [[ ! -d "$dir" ]]; then
            log_info "Creating directory: $dir"
            mkdir -p "$dir"
        fi
    done
}

# Function to validate installation
validate_installation() {
    local config_file="$1"
    local source_file="$2"

    if [[ ! -L "$config_file" ]]; then
        log_error "Installation failed: Symlink not created"
        return 1
    fi

    # Check if symlink points to correct target
    local link_target
    link_target=$(readlink "$config_file")
    if [[ "$link_target" != "$source_file" ]]; then
        log_error "Installation failed: Symlink points to wrong target"
        log_error "Expected: $source_file"
        log_error "Actual: $link_target"
        return 1
    fi

    log_success "Installation validation passed"
    return 0
}

# Function to print dependency installation guide
print_dependency_guide() {
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════════════════════════════════════════${NC}"
    echo -e "${CYAN}                    External Dependencies Installation Guide                  ${NC}"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "${YELLOW}Vim configuration works out of the box!${NC}"
    echo -e "${YELLOW}Optional tools for enhanced experience:${NC}"
    echo ""

    echo -e "${BLUE}【Recommended Tools】${NC}"
    echo "  # Better grep"
    echo "  brew install ripgrep"
    echo ""
    echo "  # Better find"
    echo "  brew install fd"
    echo ""
    echo "  # Git integration"
    echo "  brew install git"
    echo ""

    echo -e "${BLUE}【Optional Plugins (vim-plug)】${NC}"
    echo "  # Install vim-plug"
    echo "  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \\"
    echo "    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    echo ""
    echo "  # Then uncomment the plugin section in ~/.vimrc and run :PlugInstall"
    echo ""

    echo -e "${BLUE}【For Neovim Users】${NC}"
    echo "  # The vimrc is compatible with Neovim"
    echo "  # Link to neovim config:"
    echo "  mkdir -p ~/.config/nvim"
    echo "  ln -s ~/.vimrc ~/.config/nvim/init.vim"
    echo ""

    echo -e "${CYAN}═══════════════════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}Quick Install (copy and paste):${NC}"
    echo ""
    echo "  # Install vim-plug (optional)"
    echo "  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \\"
    echo "    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    echo ""
    echo "  # Install useful tools"
    echo "  brew install ripgrep fd git"
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════════════════════════════════════════${NC}"
}

# Main installation function
main() {
    # Check prerequisites
    check_macos
    check_vim_installed

    log_info "Starting Vim configuration installation (symlink mode)"
    log_info "Script directory: $SCRIPT_DIR"
    log_info "Target configuration file: $VIM_CONFIG_FILE"

    # Validate source file
    validate_source_file "$SCRIPT_DIR/vimrc"

    # Create necessary directories
    create_vim_directories

    # Remove existing .vimrc if present (symlink or file)
    if [[ -e "$VIM_CONFIG_FILE" ]] || [[ -L "$VIM_CONFIG_FILE" ]]; then
        log_info "Removing existing: $VIM_CONFIG_FILE"
        rm -f "$VIM_CONFIG_FILE"
    fi

    # Create symlink
    log_info "Creating symlink: $VIM_CONFIG_FILE -> $SCRIPT_DIR/vimrc"
    if ! ln -s "$SCRIPT_DIR/vimrc" "$VIM_CONFIG_FILE"; then
        error_exit "Failed to create symlink"
    fi

    # Validate installation
    if validate_installation "$VIM_CONFIG_FILE" "$SCRIPT_DIR/vimrc"; then
        log_success "Setup Vim configuration successfully!"
        log_info "Configuration file: $VIM_CONFIG_FILE -> $SCRIPT_DIR/vimrc"
        log_info "Note: Any changes to vimrc in the project will be immediately effective"

        # Print dependency guide
        print_dependency_guide

        return 0
    else
        error_exit "Installation validation failed"
    fi
}

# Run main function with error handling
if ! main "$@"; then
    exit 1
fi
