# 99-local.example.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-30 13:47:02 Monday by zhengyu.li>
#
# =============================================================================
# Machine-Local Configuration Template - Overrides for specific machines
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: local, template, machine-specific, overrides
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   2026-03-30 13:47 zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   Loaded by: .zshrc (interactive shells only, loaded last)
#   Load order: 99 (after all other conf.d modules)
#
#   Usage:
#     cp 99-local.example.zsh 99-local.zsh
#     Edit 99-local.zsh with machine-specific settings
#
#   Note:
#     99-local.zsh is listed in .gitignore and must never be committed.
#     This template file (.example) is committed to the repo as a reference.
# =============================================================================

# -----------------------------------------------------------------------------
# Secrets and API Keys
# -----------------------------------------------------------------------------

# export GITHUB_TOKEN=""
# export ANTHROPIC_API_KEY=""
# export AWS_PROFILE="default"

# -----------------------------------------------------------------------------
# Machine-Specific PATH Additions
# -----------------------------------------------------------------------------

# path+=("$HOME/bin/scripts")
# path+=("/opt/custom-tool/bin")

# -----------------------------------------------------------------------------
# Machine-Specific Tool Configuration
# -----------------------------------------------------------------------------

# export JAVA_HOME="/opt/homebrew/opt/openjdk@17"
# export ANDROID_HOME="$HOME/Library/Android/sdk"

# -----------------------------------------------------------------------------
# Project Directory Shortcuts
# -----------------------------------------------------------------------------

# alias work='cd ~/Projects/mycompany'
# alias proj='cd ~/Projects/myproject'

# -----------------------------------------------------------------------------
# Machine-Specific Aliases
# -----------------------------------------------------------------------------

# alias deploy='cd ~/work/deploy && ./run.sh'
# alias k='kubectl'
# alias tf='terraform'

# -----------------------------------------------------------------------------
# SSH Agent
# -----------------------------------------------------------------------------

# Ensure SSH key is loaded in macOS keychain on shell start
# ssh-add -l &>/dev/null \
#   || ssh-add --apple-use-keychain ~/.ssh/id_ed25519 2>/dev/null

# -----------------------------------------------------------------------------
# Network Proxy
# -----------------------------------------------------------------------------

# --- Proxy Settings (edit these three values) ---
# readonly _PROXY_URL="http://proxy.company.com:8080"
# readonly _PROXY_BYPASS_COMPANY=".company.com,company.com"
# readonly _PROXY_BYPASS_PRIVATE="localhost,127.0.0.1,::1,10.0.0.0/8,172.16.0.0/12,192.168.0.0/16"

# set_proxy() {
#   local -r bypass="${_PROXY_BYPASS_PRIVATE},${_PROXY_BYPASS_COMPANY}"
#   # Uppercase — Go, npm, bun, Java, etc.
#   export HTTP_PROXY="$_PROXY_URL"
#   export HTTPS_PROXY="$_PROXY_URL"
#   export NO_PROXY="$bypass"
#   # Lowercase — curl, wget, Python requests, etc.
#   export http_proxy="$_PROXY_URL"
#   export https_proxy="$_PROXY_URL"
#   export no_proxy="$bypass"
#   # Catch-all for tools that only read ALL_PROXY
#   export ALL_PROXY="$_PROXY_URL"
#   export all_proxy="$_PROXY_URL"
#   print "Proxy set: $_PROXY_URL"
# }

# unset_proxy() {
#   unset HTTP_PROXY HTTPS_PROXY NO_PROXY
#   unset http_proxy https_proxy no_proxy
#   unset ALL_PROXY all_proxy
#   print "Proxy unset"
# }

# set_proxy
