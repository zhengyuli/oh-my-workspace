# 99-local.zsh.example
# Time-stamp: <2026-03-23 14:33:26 Monday by zhengyu.li>
# =============================================================================
# Machine-Local Configuration Template
#
# Loaded by: .zshrc (interactive shells only, loaded last)
# Load order: 99 (after all other conf.d modules)
#
# Responsibilities:
#   1. Provide a template for machine-specific overrides (99-local.zsh)
#   2. Document common local customization patterns
#
# Usage:
#   cp 99-local.zsh.example 99-local.zsh
#   Edit 99-local.zsh with machine-specific settings
#
# Note:
#   99-local.zsh is listed in .gitignore and must never be committed.
#   This template file (.example) is committed to the repo as a reference.
#   Loaded last — can override anything defined in earlier conf.d fragments.
# =============================================================================

# -----------------------------------------------------------------------------
# Project directory shortcuts
# -----------------------------------------------------------------------------
# alias work='cd ~/Projects/mycompany'
# alias proj='cd ~/Projects/myproject'

# -----------------------------------------------------------------------------
# Machine-specific PATH additions
# -----------------------------------------------------------------------------
# path+=("$HOME/bin/scripts")
# path+=("/opt/custom-tool/bin")

# -----------------------------------------------------------------------------
# Secrets and API keys (local only -- never commit)
# -----------------------------------------------------------------------------
# export GITHUB_TOKEN=""
# export ANTHROPIC_API_KEY=""
# export AWS_PROFILE="default"

# -----------------------------------------------------------------------------
# Machine-specific tool configuration
# -----------------------------------------------------------------------------
# export JAVA_HOME="/opt/homebrew/opt/openjdk@17"
# export ANDROID_HOME="$HOME/Library/Android/sdk"

# -----------------------------------------------------------------------------
# Corporate proxy (office network, VPN, etc.)
# -----------------------------------------------------------------------------
_PROXY_URL="http://proxy.nioint.com:8080"
_NO_PROXY_LIST="localhost,127.0.0.1,::1,10.0.0.0/8,172.16.0.0/12,192.168.0.0/16,.nioint.com,nioint.com"

set_proxy() {
  # Uppercase — Go, npm, bun, Java, etc.
  export HTTP_PROXY="$_PROXY_URL"
  export HTTPS_PROXY="$_PROXY_URL"
  export NO_PROXY="$_NO_PROXY_LIST"
  # Lowercase — curl, wget, Python requests, etc.
  export http_proxy="$_PROXY_URL"
  export https_proxy="$_PROXY_URL"
  export no_proxy="$_NO_PROXY_LIST"
  # Catch-all for tools that only read ALL_PROXY
  export ALL_PROXY="$_PROXY_URL"
  export all_proxy="$_PROXY_URL"
  echo "Proxy set: $_PROXY_URL"
}

unset_proxy() {
  unset HTTP_PROXY HTTPS_PROXY NO_PROXY
  unset http_proxy https_proxy no_proxy
  unset ALL_PROXY all_proxy
  echo "Proxy unset"
}

set_proxy

# -----------------------------------------------------------------------------
# Experimental settings (move to the appropriate conf.d fragment once stable)
# -----------------------------------------------------------------------------
