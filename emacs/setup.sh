#!/bin/bash

#---------------------------------------------------------------------------------
# Name: setup.sh
# Purpose: Emacs configurations setup script for macOS
#
# Time-stamp: <2026-03-05 13:26:00 Thursday by zhengyu.li>
#
# Author: zhengyu li
# Created: 2014-03-26
# Optimized: Symlink mode with interactive dependency installation
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

# Global arrays for missing dependencies
declare -a MISSING_CORE_TOOLS=()
declare -a MISSING_LSP_SERVERS=()
declare -a MISSING_FORMATTERS=()

# Add missing dependency to global array
add_missing_dependency() {
    local category="$1"
    local install_cmd="$2"

    case "$category" in
        core)
            MISSING_CORE_TOOLS+=("$install_cmd")
            ;;
        lsp)
            MISSING_LSP_SERVERS+=("$install_cmd")
            ;;
        formatter)
            MISSING_FORMATTERS+=("$install_cmd")
            ;;
        *)
            log_error "Unknown dependency category: $category"
            return 1
            ;;
    esac
    return 0
}

# Dependency configuration data
# Format: "check_command|display_name|install_command|category|priority"
# Priority levels: P0 (Required), P1 (Development)
readonly DEPENDENCY_CONFIG=(
    # Core Tools (P0)
    "command -v pinentry|pinentry|brew install pinentry|core|P0"
    "command -v git|git|brew install git|core|P0"
    "command -v aspell|aspell|brew install aspell|core|P0"
    "command -v pandoc|pandoc|brew install pandoc|core|P0"
    "command -v ag|the_silver_searcher (ag)|brew install the_silver_searcher|core|P0"
    "command -v rg|ripgrep (rg)|brew install ripgrep|core|P0"
    "command -v gls|coreutils (gls)|brew install coreutils|core|P0"
    "command -v fd|fd|brew install fd|core|P0"
    "brew list libvterm &> /dev/null 2>&1|libvterm|brew install libvterm|core|P0"

    # LSP Servers (P1)
    "command -v pylsp|Python LSP (pylsp)|pip install 'python-lsp-server[all]'|lsp|P1"
    "command -v gopls|Go LSP (gopls)|go install golang.org/x/tools/gopls@latest|lsp|P1"
    "command -v typescript-language-server|TypeScript LSP|npm install -g typescript-language-server|lsp|P1"
    "command -v yaml-language-server|YAML LSP|npm install -g yaml-language-server|lsp|P1"
    "command -v bash-language-server|Bash LSP|npm install -g bash-language-server|lsp|P1"
    "command -v docker-langserver|Dockerfile LSP|npm install -g dockerfile-language-server-nodejs|lsp|P1"
    "command -v cmake-language-server|CMake LSP|pip install cmake-language-server|lsp|P1"
    "command -v marksman|Markdown LSP (marksman)|brew install marksman|lsp|P1"

    # Formatters & Linters (P1)
    "command -v black|Python formatter (black)|pip install black black-macchiato|formatter|P1"
    "command -v isort|Python import sorter (isort)|pip install isort|formatter|P1"
    "command -v pylint|Python linter (pylint)|pip install pylint|formatter|P1"
    "command -v gofumpt|Go formatter (gofumpt)|go install mvdan.cc/gofumpt@latest|formatter|P1"
    "python3 -c \"import debugpy\" &> /dev/null 2>&1|Python debugger (debugpy)|pip install debugpy|formatter|P1"
)

# Core logging function with timestamp and colored output
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

# Log informational message
log_info() {
    log "INFO" "$*"
}

# Log error message
log_error() {
    log "ERROR" "$*"
}

# Log success message
log_success() {
    log "SUCCESS" "$*"
}

# Log warning message
log_warning() {
    log "WARNING" "$*"
}

# Log section header with bordered formatting
log_section() {
    local title="$1"
    local border="═══════════════════════════════════════════════════════════════════════════"
    echo ""
    echo -e "${CYAN}${border}${NC}"
    echo -e "${CYAN}$(printf "%-73s" "$title")${NC}"
    echo -e "${CYAN}${border}${NC}"
}

# Log error message and exit with failure status
#
# Used for fatal errors that should immediately stop script execution.
# Log error message and exit with status 1
error_exit() {
    log_error "$1"
    exit 1
}

# Verify OS is macOS
check_macos() {
    if [[ "$OSTYPE" != "darwin"* ]]; then
        error_exit "This script only supports macOS. Current OS: $OSTYPE"
    fi
    log_info "Running on macOS"
}

# Verify Emacs is installed and meets minimum version (30.2)
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

# Validate source file exists and is readable
validate_source_file() {
    local source_file="$1"
    if [[ ! -f "$source_file" ]]; then
        error_exit "Source configuration file not found: $source_file"
    fi
    if [[ ! -r "$source_file" ]]; then
        error_exit "Cannot read source configuration file: $source_file"
    fi
}

# Validate destination directory exists and is writable
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

# Check for proxy configuration and warn about limitations
setup_proxy() {
    if [[ -n "${http_proxy:-}" ]]; then
        log_info "Proxy detected: $http_proxy"
        log_info "Consider setting proxy in $HOME/.emacs.d/custom.el by appending (setq omw/http-proxy $http_proxy)"
    else
        log_info "No proxy environment variable found"
    fi
}

# Validate symlink installation was successful
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

# Check dependencies by category using data-driven configuration
check_dependencies_by_category() {
    local category="$1"
    local priority="$2"
    local missing_count=0
    local category_name=""

    case "$category" in
        core) category_name="Core Tools" ;;
        lsp) category_name="LSP Servers" ;;
        formatter) category_name="Formatters & Linters" ;;
    esac

    echo ""
    echo -e "${CYAN}Checking $category_name ($priority - ${priority/P0/Required/P1:Development})...${NC}"
    echo ""

    for dep_config in "${DEPENDENCY_CONFIG[@]}"; do
        IFS='|' read -r check_cmd display_name install_cmd dep_category dep_priority <<< "$dep_config"

        # Skip if not matching category and priority
        [[ "$dep_category" != "$category" ]] && continue
        [[ "$dep_priority" != "$priority" ]] && continue

        # Check if dependency exists
        if eval "$check_cmd"; then
            echo -e "  ${GREEN}✓${NC} $display_name"
        else
            add_missing_dependency "$category" "$install_cmd"
            ((missing_count++))
            echo -e "  ${RED}✗${NC} $display_name"
        fi
    done

    echo ""
    if [[ $missing_count -eq 0 ]]; then
        echo -e "${GREEN}All $category_name installed!${NC}"
    else
        echo -e "${YELLOW}Missing $missing_count $category_name(s)${NC}"
    fi

    return $missing_count
}

# Wrapper functions for backward compatibility and clarity
check_core_tools() {
    check_dependencies_by_category "core" "P0"
}

check_lsp_servers_enhanced() {
    check_dependencies_by_category "lsp" "P1"
}

check_formatters_linters() {
    check_dependencies_by_category "formatter" "P1"
}

# Collect all missing dependencies across all categories
collect_all_dependencies() {
    log_section "Checking External Dependencies"

    local missing_core=0
    local missing_lsp=0
    local missing_formatters=0

    check_core_tools || missing_core=$?
    check_lsp_servers_enhanced || missing_lsp=$?
    check_formatters_linters || missing_formatters=$?

    local total_missing=$((missing_core + missing_lsp + missing_formatters))
    return $total_missing
}

# Display formatted summary of missing dependencies
display_missing_dependencies() {
    local total_missing=${#MISSING_CORE_TOOLS[@]}
    total_missing=$((total_missing + ${#MISSING_LSP_SERVERS[@]}))
    total_missing=$((total_missing + ${#MISSING_FORMATTERS[@]}))

    if [[ $total_missing -eq 0 ]]; then
        return 0
    fi

    log_section "Missing Dependencies Summary"
    echo ""

    # Core Tools
    if [[ ${#MISSING_CORE_TOOLS[@]} -gt 0 ]]; then
        echo -e "${YELLOW}Core Tools (P0 - Required):${NC}"
        for cmd in "${MISSING_CORE_TOOLS[@]}"; do
            echo "  $cmd"
        done
        echo ""
    fi

    # LSP Servers
    if [[ ${#MISSING_LSP_SERVERS[@]} -gt 0 ]]; then
        echo -e "${YELLOW}LSP Servers (P1 - Development):${NC}"
        for cmd in "${MISSING_LSP_SERVERS[@]}"; do
            echo "  $cmd"
        done
        echo ""
    fi

    # Formatters & Linters
    if [[ ${#MISSING_FORMATTERS[@]} -gt 0 ]]; then
        echo -e "${YELLOW}Formatters & Linters (P1 - Development):${NC}"
        for cmd in "${MISSING_FORMATTERS[@]}"; do
            echo "  $cmd"
        done
        echo ""
    fi
}

# Prompt user to install missing dependencies
prompt_install_dependencies() {
    local total_missing=${#MISSING_CORE_TOOLS[@]}
    total_missing=$((total_missing + ${#MISSING_LSP_SERVERS[@]}))
    total_missing=$((total_missing + ${#MISSING_FORMATTERS[@]}))

    if [[ $total_missing -eq 0 ]]; then
        return 1
    fi

    log_section "Install Missing Dependencies?"
    echo ""
    echo "Found $total_missing missing dependencies across all categories."
    echo ""
    echo "Would you like to install them now?"
    echo "  [y] Yes - Install all missing dependencies"
    echo "  [N] No  - Skip installation (you can install manually later)"
    echo ""

    local choice
    read -p "Your choice (y/N): " choice
    echo ""

    if [[ "$choice" =~ ^[Yy]$ ]]; then
        return 0
    else
        return 1
    fi
}

# Execute single install command (type-safe, no eval)
execute_install_command() {
    local cmd="$1"
    local category="$2"

    echo -e "Installing: ${BLUE}$cmd${NC}"
    echo -e "Category:   ${YELLOW}$category${NC}"

    local exit_code=0

    # Type-safe execution based on command prefix
    case "$cmd" in
        brew\ install*)
            local package="${cmd#brew install }"
            if brew install $package &> /dev/null; then
                echo -e "${GREEN}✓ Success: $cmd${NC}"
            else
                echo -e "${RED}✗ Failed: $cmd${NC}"
                exit_code=1
            fi
            ;;
        pip\ install*)
            local package="${cmd#pip install }"
            if pip install $package &> /dev/null; then
                echo -e "${GREEN}✓ Success: $cmd${NC}"
            else
                echo -e "${RED}✗ Failed: $cmd${NC}"
                exit_code=1
            fi
            ;;
        npm\ install\ -g*)
            local package="${cmd#npm install -g }"
            if npm install -g $package &> /dev/null; then
                echo -e "${GREEN}✓ Success: $cmd${NC}"
            else
                echo -e "${RED}✗ Failed: $cmd${NC}"
                exit_code=1
            fi
            ;;
        go\ install*)
            local package="${cmd#go install }"
            if go install $package &> /dev/null; then
                echo -e "${GREEN}✓ Success: $cmd${NC}"
            else
                echo -e "${RED}✗ Failed: $cmd${NC}"
                exit_code=1
            fi
            ;;
        *)
            echo -e "${RED}✗ Unknown command type: $cmd${NC}"
            exit_code=1
            ;;
    esac

    echo ""
    return $exit_code
}

# Install all missing dependencies with progress tracking
install_missing_dependencies() {
    declare -a failed_commands=()
    local success_count=0
    local fail_count=0
    local step=1

    # Install Core Tools
    if [[ ${#MISSING_CORE_TOOLS[@]} -gt 0 ]]; then
        echo -e "${CYAN}[$step/3] Installing Core Tools (P0)...${NC}"
        echo ""
        for cmd in "${MISSING_CORE_TOOLS[@]}"; do
            if execute_install_command "$cmd" "Core Tools"; then
                ((success_count++))
            else
                ((fail_count++))
                failed_commands+=("$cmd")
            fi
        done
        ((step++))
    fi

    # Install LSP Servers
    if [[ ${#MISSING_LSP_SERVERS[@]} -gt 0 ]]; then
        echo -e "${CYAN}[$step/3] Installing LSP Servers (P1)...${NC}"
        echo ""
        for cmd in "${MISSING_LSP_SERVERS[@]}"; do
            if execute_install_command "$cmd" "LSP Servers"; then
                ((success_count++))
            else
                ((fail_count++))
                failed_commands+=("$cmd")
            fi
        done
        ((step++))
    fi

    # Install Formatters & Linters
    if [[ ${#MISSING_FORMATTERS[@]} -gt 0 ]]; then
        echo -e "${CYAN}[$step/3] Installing Formatters & Linters (P1)...${NC}"
        echo ""
        for cmd in "${MISSING_FORMATTERS[@]}"; do
            if execute_install_command "$cmd" "Formatters & Linters"; then
                ((success_count++))
            else
                ((fail_count++))
                failed_commands+=("$cmd")
            fi
        done
    fi

    # Print installation summary
    log_section "Installation Summary"
    echo ""
    echo -e "${GREEN}Successfully installed: $success_count${NC}"

    if [[ $fail_count -gt 0 ]]; then
        echo -e "${RED}Failed to install:    $fail_count${NC}"
        echo ""
        echo -e "${YELLOW}Failed packages:${NC}"
        for cmd in "${failed_commands[@]}"; do
            echo "  • $cmd"
        done
    fi
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════════════════════════════════════════${NC}"
}

# Offer interactive dependency installation workflow
offer_install_dependencies() {
    # Clear arrays before collecting
    MISSING_CORE_TOOLS=()
    MISSING_LSP_SERVERS=()
    MISSING_FORMATTERS=()

    # Collect all dependencies
    collect_all_dependencies
    local total_missing=$?

    if [[ $total_missing -eq 0 ]]; then
        echo ""
        echo -e "${GREEN}All dependencies already installed!${NC}"
        echo ""
        echo -e "${CYAN}Verify installation:${NC}"
        echo "  M-x emacs-config-validate-all"
        return 0
    fi

    # Display missing dependencies
    display_missing_dependencies

    # Prompt user to install
    if prompt_install_dependencies; then
        # User wants to install
        install_missing_dependencies

        echo ""
        echo -e "${GREEN}Setup completed!${NC}"
        echo ""
        echo -e "${CYAN}Verify installation:${NC}"
        echo "  M-x emacs-config-validate-all"
    else
        # User declined installation
        echo ""
        echo -e "${YELLOW}Installation skipped.${NC}"
        echo ""
        echo -e "${CYAN}To install manually, run the commands listed above.${NC}"
        echo ""
        echo -e "${CYAN}Verify installation:${NC}"
        echo "  M-x emacs-config-validate-all"
    fi
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

        # Offer interactive dependency installation
        offer_install_dependencies

        return 0
    else
        error_exit "Installation validation failed"
    fi
}

# Run main function with error handling
if ! main "$@"; then
    exit 1
fi
