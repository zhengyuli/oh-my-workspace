#!/bin/bash

#---------------------------------------------------------------------------------
# Name: setup.sh
# Purpose: Emacs configurations setup script for macOS
#
# Time-stamp: <2026-03-03 23:12:12 Tuesday by zhengyu.li>
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
        log_warning "Consider setting proxy via environment variable or $HOME/.emacs.d/custom.el instead"
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

# Function to check if LSP servers are installed
check_lsp_servers() {
    echo ""
    echo -e "${CYAN}Checking LSP servers...${NC}"
    echo ""

    local missing_servers=()

    # Python
    if ! command -v pylsp &> /dev/null; then
        missing_servers+=("pylsp (pip install python-lsp-server[all])")
    else
        echo -e "  ${GREEN}✓${NC} Python LSP (pylsp)"
    fi

    # Go
    if ! command -v gopls &> /dev/null; then
        missing_servers+=("gopls (go install golang.org/x/tools/gopls@latest)")
    else
        echo -e "  ${GREEN}✓${NC} Go LSP (gopls)"
    fi

    # TypeScript
    if ! command -v typescript-language-server &> /dev/null; then
        missing_servers+=("typescript-language-server (npm install -g typescript-language-server)")
    else
        echo -e "  ${GREEN}✓${NC} TypeScript LSP"
    fi

    # YAML
    if ! command -v yaml-language-server &> /dev/null; then
        missing_servers+=("yaml-language-server (npm install -g yaml-language-server)")
    else
        echo -e "  ${GREEN}✓${NC} YAML LSP"
    fi

    # Bash
    if ! command -v bash-language-server &> /dev/null; then
        missing_servers+=("bash-language-server (npm install -g bash-language-server)")
    else
        echo -e "  ${GREEN}✓${NC} Bash LSP"
    fi

    # Dockerfile
    if ! command -v docker-langserver &> /dev/null; then
        missing_servers+=("docker-langserver (npm install -g dockerfile-language-server-nodejs)")
    else
        echo -e "  ${GREEN}✓${NC} Dockerfile LSP"
    fi

    # CMake
    if ! command -v cmake-language-server &> /dev/null; then
        missing_servers+=("cmake-language-server (pip install cmake-language-server)")
    else
        echo -e "  ${GREEN}✓${NC} CMake LSP"
    fi

    # Marksman (Markdown)
    if ! command -v marksman &> /dev/null; then
        missing_servers+=("marksman (brew install marksman)")
    else
        echo -e "  ${GREEN}✓${NC} Markdown LSP (marksman)"
    fi

    if [ ${#missing_servers[@]} -gt 0 ]; then
        echo ""
        echo -e "${YELLOW}Missing LSP servers:${NC}"
        for server in "${missing_servers[@]}"; do
            echo -e "  ${RED}✗${NC} $server"
        done
        return 1
    else
        echo ""
        echo -e "${GREEN}All LSP servers installed!${NC}"
        return 0
    fi
}

# Function to print dependency installation guide
print_dependency_guide() {
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════════════════════════════════════════${NC}"
    echo -e "${CYAN}                    External Dependencies Installation Guide                  ${NC}"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "${CYAN}Configuration Structure:${NC}"
    echo "  lisp/          - Core modules (UI, editing, completion)"
    echo "  lisp/tools/    - Tool integrations (AI, auth, terminal, VC)"
    echo "  lisp/lang/     - Language-specific configs (10 languages)"
    echo "  site-packages/ - Custom Emacs packages"
    echo ""
    echo -e "${YELLOW}Quick Install (copy and paste):${NC}"
    echo ""
    echo "  # Core tools (P0 - Required)"
    echo "  brew install git aspell pandoc the_silver_searcher ripgrep coreutils libvterm marksman fd"
    echo ""
    echo "  # LSP servers (P1 - Development)"
    echo "  npm install -g \\"
    echo "    typescript-language-server typescript \\"
    echo "    yaml-language-server \\"
    echo "    bash-language-server \\"
    echo "    dockerfile-language-server-nodejs"
    echo ""
    echo "  pip install 'python-lsp-server[all]' cmake-language-server"
    echo "  go install golang.org/x/tools/gopls@latest"
    echo ""
    echo "  # Formatters & Linters"
    echo "  pip install black black-macchiato isort pylint debugpy"
    echo "  go install mvdan.cc/gofumpt@latest"
    echo ""
    echo -e "${GREEN}Verify installation:${NC}"
    echo "  M-x emacs-config-validate-all"
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

    # Check LSP servers (informational, doesn't fail setup)
    check_lsp_servers || log_warning "Some LSP servers are missing. See installation guide above."

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
