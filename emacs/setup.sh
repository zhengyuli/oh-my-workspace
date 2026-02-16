#!/bin/bash

#---------------------------------------------------------------------------------
# Name: setup.sh
# Purpose: Emacs configurations setup script for macOS
#
# Time-stamp: <2026-02-16 14:00:00 Sunday by zhengyuli>
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
readonly EMACS_CONFIG_FILE="$HOME/.emacs"

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

# Function to check if Emacs is installed
check_emacs_installed() {
    if ! command -v emacs &> /dev/null; then
        error_exit "Emacs is not installed. Please install Emacs first:
  brew install --cask emacs"
    fi

    local emacs_version
    emacs_version=$(emacs --version | head -1 | awk '{print $3}')
    log_info "Emacs version: $emacs_version"

    # Check minimum version (30.2)
    local major minor
    major=$(echo "$emacs_version" | cut -d. -f1)
    minor=$(echo "$emacs_version" | cut -d. -f2)

    if [[ "$major" -lt 30 ]] || { [[ "$major" -eq 30 ]] && [[ "$minor" -lt 2 ]]; }; then
        error_exit "Emacs version must be >= 30.2. Current: $emacs_version"
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

# Function to validate destination directory
validate_destination_dir() {
    local dir="$1"
    if [[ ! -d "$dir" ]]; then
        log_warning "Destination directory does not exist: $dir"
        if ! mkdir -p "$dir"; then
            error_exit "Failed to create directory: $dir"
        fi
        log_info "Created directory: $dir"
    fi
    if [[ ! -w "$dir" ]]; then
        error_exit "Destination directory is not writable: $dir"
    fi
}

# Function to safely set proxy
setup_proxy() {
    if [[ -n "${http_proxy:-}" ]]; then
        log_warning "Proxy detected: $http_proxy"
        log_warning "Since using symlink mode, proxy configuration will modify init.el directly"
        log_warning "Consider setting proxy via environment variable or custom_settings.el instead"
        log_info "Skipping automatic proxy configuration to preserve project file integrity"
    else
        log_info "No proxy environment variable found"
    fi
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
    echo -e "${YELLOW}Please install the following tools manually to enable all features:${NC}"
    echo ""

    echo -e "${BLUE}【Required - Core Functionality】${NC}"
    echo "  brew install git aspell pandoc the_silver_searcher ripgrep coreutils libvterm"
    echo ""

    echo -e "${BLUE}【LSP Servers - Code Completion & Navigation】${NC}"
    echo "  # Python LSP"
    echo "  pip install 'python-lsp-server[all]'"
    echo ""
    echo "  # Go LSP"
    echo "  go install golang.org/x/tools/gopls@latest"
    echo ""
    echo "  # C/C++ LSP (via LLVM or Xcode Command Line Tools)"
    echo "  brew install llvm"
    echo ""
    echo "  # Haskell LSP (via ghcup)"
    echo "  ghcup install hls"
    echo ""
    echo "  # YAML, Bash, Dockerfile LSP (Node.js)"
    echo "  npm install -g yaml-language-server bash-language-server dockerfile-language-server-nodejs"
    echo ""
    echo "  # CMake LSP"
    echo "  pip install cmake-language-server"
    echo ""
    echo "  # Markdown LSP (optional)"
    echo "  brew install marksman"
    echo ""

    echo -e "${BLUE}【Code Formatters】${NC}"
    echo "  # Go formatter"
    echo "  go install mvdan.cc/gofumpt@latest"
    echo ""
    echo "  # Python formatters"
    echo "  pip install black black-macchiato isort"
    echo ""

    echo -e "${BLUE}【Code Linters】${NC}"
    echo "  # Python linter"
    echo "  pip install pylint"
    echo ""

    echo -e "${BLUE}【Debug Tools】${NC}"
    echo "  # Python debugger"
    echo "  pip install debugpy"
    echo ""

    echo -e "${BLUE}【Fonts - GUI Mode】${NC}"
    echo "  brew install --cask font-source-code-pro font-source-serif-pro"
    echo "  # After installation, run in Emacs: M-x all-the-icons-install-fonts"
    echo ""

    echo -e "${CYAN}═══════════════════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}Quick Install (copy and paste):${NC}"
    echo ""
    echo "  # Core tools"
    echo "  brew install git aspell pandoc the_silver_searcher ripgrep coreutils libvterm marksman fd"
    echo ""
    echo "  # Node.js LSP servers"
    echo "  npm install -g yaml-language-server bash-language-server dockerfile-language-server-nodejs"
    echo ""
    echo "  # Python tools"
    echo "  pip install 'python-lsp-server[all]' pylint black black-macchiato isort debugpy cmake-language-server"
    echo ""
    echo "  # Go tools"
    echo "  go install golang.org/x/tools/gopls@latest mvdan.cc/gofumpt@latest"
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════════════════════════════════════════${NC}"
}

# Main installation function
main() {
    # Check prerequisites
    check_macos
    check_emacs_installed

    log_info "Starting Emacs configuration installation (symlink mode)"
    log_info "Script directory: $SCRIPT_DIR"
    log_info "Target configuration file: $EMACS_CONFIG_FILE"

    # Validate source file
    validate_source_file "$SCRIPT_DIR/init.el"

    # Validate destination directory
    validate_destination_dir "$(dirname "$EMACS_CONFIG_FILE")"

    # Remove existing .emacs if present (symlink or file)
    if [[ -e "$EMACS_CONFIG_FILE" ]] || [[ -L "$EMACS_CONFIG_FILE" ]]; then
        log_info "Removing existing: $EMACS_CONFIG_FILE"
        rm -f "$EMACS_CONFIG_FILE"
    fi

    # Create symlink
    log_info "Creating symlink: $EMACS_CONFIG_FILE -> $SCRIPT_DIR/init.el"
    if ! ln -s "$SCRIPT_DIR/init.el" "$EMACS_CONFIG_FILE"; then
        error_exit "Failed to create symlink"
    fi

    # Setup proxy if configured
    setup_proxy

    # Validate installation
    if validate_installation "$EMACS_CONFIG_FILE" "$SCRIPT_DIR/init.el"; then
        log_success "Setup Emacs configuration successfully!"
        log_info "Configuration file: $EMACS_CONFIG_FILE -> $SCRIPT_DIR/init.el"
        log_info "Note: Any changes to init.el in the project will be immediately effective"

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
