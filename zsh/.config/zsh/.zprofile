# zprofile.symlink -*- mode: zsh; -*-
# Time-stamp: <2026-03-14 16:00:00 Saturday by zhengyu.li>
#
# ==============================================================================
# File: .zprofile
# Role: Login shell bootstrap - PATH construction and runtime init
#
# Load context : Sourced once per login session (before .zshrc)
# Dependencies : DOTFILES (from .zshenv), XDG_* (from .zshenv)
# Side effects : Modifies PATH, initializes Homebrew/fnm/pyenv
# ==============================================================================

# ==============================================================================
# Setup
# ==============================================================================

# DOTFILES is set by ~/.zshenv via symlink resolution
# Verify it's set correctly before proceeding
if [[ -z "$DOTFILES" ]] || [[ ! -d "$DOTFILES" ]]; then
    echo "ERROR: DOTFILES not set or invalid. Ensure ~/.zshenv is a symlink." >&2
    return 1 2>/dev/null || exit 1
fi

# ==============================================================================
# Homebrew Initialization
# ==============================================================================

if [[ "$(uname)" == "Darwin" ]]; then
    # Apple Silicon Macs
    if [[ -d "/opt/homebrew" ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    # Intel Macs
    elif [[ -d "/usr/local/Homebrew" ]]; then
        eval "$(/usr/local/bin/brew shellenv)"
    fi
fi

# ==============================================================================
# PATH Construction
# ==============================================================================

# User binaries
typeset -U path  # Remove duplicates

# Add user bin directories if they exist
[[ -d "$HOME/.local/bin" ]] && path=("$HOME/.local/bin" "${path[@]}")

# ==============================================================================
# Language Runtimes (Order: Node.js → Python → Go)
# ==============================================================================

# ------------------------------------------------------------------------------
# Node.js (fnm)
# ------------------------------------------------------------------------------

# FNM_DIR is set in zshenv
# Initialize fnm for PATH setup (without interactive hooks)
# Interactive shells get --use-on-cd hook via nodejs.zsh
# Set version-file-strategy here so it's consistent across all shells
if command -v fnm &>/dev/null; then
    eval "$(fnm env --shell zsh --version-file-strategy=recursive)"
fi

# ------------------------------------------------------------------------------
# Python (pyenv)
# ------------------------------------------------------------------------------

if command -v pyenv &>/dev/null; then
    # Use pyenv init --path for proper PATH setup in login shells
    eval "$(pyenv init --path)"
fi

# ------------------------------------------------------------------------------
# Go
# ------------------------------------------------------------------------------

# GOPATH is set in zshenv, only add to PATH here
[[ -d "$GOPATH/bin" ]] && path=("$GOPATH/bin" "${path[@]}")

# ==============================================================================
# Local PATH Extensions
# ==============================================================================

[[ -f "$XDG_CONFIG_HOME/zsh/path" ]] && source "$XDG_CONFIG_HOME/zsh/path"
