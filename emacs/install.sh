#!/bin/bash

#---------------------------------------------------------------------------------
# Name: install.sh
# Purpose: Emacs configurations setup script
#
# Time-stamp: <2025-10-18 18:42:10 Saturday by zhengyuli>
#
# Author: zhengyu li
# Created: 2014-03-26
# Optimized: Enhanced error handling and validation
#
# Copyright (c) 2014, 2022, 2023, 2024, 2025 zhengyu li <lizhengyu419@gmail.com>
#---------------------------------------------------------------------------------

set -euo pipefail

# Global variables
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly EMACS_CONFIG_FILE="$HOME/.emacs"

# Color codes for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
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
        log_info "Setting up proxy configuration: $http_proxy"
        # Validate proxy URL format
        if [[ ! "$http_proxy" =~ ^http:// ]]; then
            log_error "Invalid proxy format: $http_proxy"
            return 1
        fi
        # Safely escape the proxy string
        local escaped_proxy
        escaped_proxy=$(printf '%s\n' "$http_proxy" | sed 's/[[\.*^$()+?{|]/\\&/g')
        if sed -i.tmp "s#emacs-http-proxy nil#emacs-http-proxy \"${escaped_proxy}\"#g" "$EMACS_CONFIG_FILE"; then
            rm -f "${EMACS_CONFIG_FILE}.tmp"
            log_success "Proxy configuration updated successfully"
        else
            log_error "Failed to update proxy configuration"
            return 1
        fi
    else
        log_info "No proxy environment variable found"
    fi
}

# Function to validate installation
validate_installation() {
    local config_file="$1"
    if [[ ! -f "$config_file" ]]; then
        error_exit "Installation failed: Configuration file not created"
    fi

    # Check if required substitutions were made
    if ! grep -q "emacs-config-root-path" "$config_file"; then
        log_error "Installation failed: Missing emacs-config-root-path variable"
        return 1
    fi

    # Check if the placeholder was replaced with actual path
    local expected_path="${SCRIPT_DIR}/"
    if ! grep -q "${expected_path}" "$config_file"; then
        log_error "Installation failed: Base path substitution failed"
        return 1
    fi

    log_success "Installation validation passed"
    return 0
}

# Main installation function
main() {
    log_info "Starting Emacs configuration installation"
    log_info "Script directory: $SCRIPT_DIR"
    log_info "Target configuration file: $EMACS_CONFIG_FILE"

    # Validate source file
    validate_source_file "$SCRIPT_DIR/init.el"

    # Validate destination directory
    validate_destination_dir "$(dirname "$EMACS_CONFIG_FILE")"

    # Copy and configure
    log_info "Copying configuration from $SCRIPT_DIR/init.el to $EMACS_CONFIG_FILE"
    if ! cp -v "$SCRIPT_DIR/init.el" "$EMACS_CONFIG_FILE"; then
        error_exit "Failed to copy configuration file"
    fi

    # Determine sed command for platform
    local sed_cmd
    if [[ "$OSTYPE" == "darwin"* ]]; then
        sed_cmd="sed -i ''"
    else
        sed_cmd="sed -i"
    fi

    # Perform substitutions
    log_info "Performing configuration substitutions"

    # Replace init.el with .emacs
    if ! $sed_cmd 's#init\.el#\.emacs#g' "$EMACS_CONFIG_FILE"; then
        error_exit "Failed to update emacs reference"
    fi

    # Replace path placeholder
    if ! $sed_cmd "s#_EMACS_CONFIG_ROOT_PATH_#${SCRIPT_DIR}/#g" "$EMACS_CONFIG_FILE"; then
        error_exit "Failed to update base path"
    fi

    # Setup proxy if configured
    setup_proxy

    # Validate installation
    if validate_installation "$EMACS_CONFIG_FILE"; then
        log_success "Emacs configuration installed successfully!"
        log_info "Configuration file: $EMACS_CONFIG_FILE"
        return 0
    else
        error_exit "Installation validation failed"
    fi
}

# Run main function with error handling
if ! main "$@"; then
    exit 1
fi
