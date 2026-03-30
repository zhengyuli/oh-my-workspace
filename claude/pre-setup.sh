#!/usr/bin/env bash
# pre-setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-03-30 21:03:20 Monday by zhengyu.li>
# =============================================================================
# Claude Code Pre-Setup
#
# Run this script before entering Claude Code for the first time.
# Covers prerequisites check, claude CLI install, and GLM API configuration.
# After completion, enter Claude Code and follow setup.md for the rest.
#
# Usage: ./claude/pre-setup.sh
# Dependencies: bash 4.3+, macOS 13+
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

readonly MIN_MACOS_VERSION="13.0"

_err_handler() {
  local -r code=$?
  printf '[error] %s() line %d: exit %d\n' \
    "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
}
trap '_err_handler' ERR

_info() {
  printf '[info] %s\n' "$*"
}

_warn() {
  printf '[warn] %s\n' "$*" >&2
}

_pass() {
  printf '  ✓ %s\n' "$*"
}

_fail() {
  printf '  ✗ %s\n' "$*" >&2
}

_check_cmd() {
  local -r name="$1"
  if command -v "$name" >/dev/null 2>&1; then
    _pass "$name: $(command "$name" --version 2>/dev/null | head -1)"
  else
    _fail "$name: NOT INSTALLED"
    return 1
  fi
}

# -----------------------------------------------------------------------------
# Step 1: Prerequisites Check
# -----------------------------------------------------------------------------

_check_prerequisites() {
  _info "Checking prerequisites..."

  # macOS version
  if [[ "$(uname -s)" != Darwin ]]; then
    _fail "Not macOS ($(uname -s))"
    return 1
  fi

  local os_version
  os_version="$(sw_vers -productVersion)"
  if ! python3 -c "
import sys
from packaging.version import Version
sys.exit(0 if Version('$os_version') >= Version('$MIN_MACOS_VERSION') else 1)
" 2>/dev/null; then
    # Fallback: simple string comparison
    if [[ "$os_version" < "$MIN_MACOS_VERSION" ]]; then
      _fail "macOS $os_version (need $MIN_MACOS_VERSION+)"
      return 1
    fi
  fi
  _pass "macOS $os_version"

  # Required tools
  local missing=0
  for cmd in git jq curl bun uv; do
    if ! _check_cmd "$cmd"; then
      missing=$((missing + 1))
    fi
  done

  if [[ "$missing" -gt 0 ]]; then
    _warn "$missing tool(s) missing — run: brew bundle --file=pkg/homebrew/Brewfile"
    return 1
  fi

  # Network
  if curl -s --max-time 5 https://api.github.com >/dev/null 2>&1; then
    _pass "GitHub API accessible"
  else
    _fail "Cannot reach GitHub API"
    return 1
  fi
}

# -----------------------------------------------------------------------------
# Step 2: Install Claude CLI
# -----------------------------------------------------------------------------

_install_claude() {
  if command -v claude >/dev/null 2>&1; then
    _pass "claude CLI already installed: $(claude --version 2>/dev/null)"
    return 0
  fi

  _info "Installing claude CLI..."
  curl -fSL https://claude.ai/install.sh | bash
  _pass "claude CLI installed: $(claude --version 2>/dev/null)"
}

# -----------------------------------------------------------------------------
# Step 3: GLM Configuration via ZAI
# -----------------------------------------------------------------------------

_configure_glm() {
  _info "Running GLM configuration helper (interactive)..."
  _info "Follow the prompts: select language, enter API key, choose plan"

  # Interactive tool — user may cancel (Ctrl+C) or tool may return non-zero.
  # Do not let set -e kill the script; check exit code explicitly.
  set +e
  bunx @z_ai/coding-helper
  local -r rc=$?
  set -e

  if [[ "$rc" -ne 0 ]]; then
    _warn "ZAI helper exited with code $rc (may need re-run later)"
    _warn "Continuing with post-configuration fixes..."
  fi
}

# -----------------------------------------------------------------------------
# Step 4: Post-ZAI Fixes
# -----------------------------------------------------------------------------

_apply_post_fixes() {
  local -r settings="$HOME/.claude/settings.json"

  if [[ ! -f "$settings" ]]; then
    _fail "settings.json not found at $settings"
    return 1
  fi

  _info "Applying post-ZAI configuration fixes..."

  # Set default model to Sonnet with 1M context
  local tmp
  tmp="$(mktemp)"
  jq '.model = "sonnet[1m]"' "$settings" > "$tmp" && mv "$tmp" "$settings"
  _pass "Default model set to sonnet[1m]"

  # Set model environment variables
  tmp="$(mktemp)"
  jq '.env.ANTHROPIC_DEFAULT_HAIKU_MODEL = "glm-4.5-air" | .env.ANTHROPIC_DEFAULT_SONNET_MODEL = "glm-5-turbo" | .env.ANTHROPIC_DEFAULT_OPUS_MODEL = "glm-5.1"' \
    "$settings" > "$tmp" && mv "$tmp" "$settings"
  _pass "Model env vars configured (haiku/sonnet/opus → GLM)"
}

# -----------------------------------------------------------------------------
# Step 5: Verification
# -----------------------------------------------------------------------------

_verify() {
  local -r settings="$HOME/.claude/settings.json"
  local ok=0

  printf '\n'
  _info "Verifying configuration..."

  # Check required env vars
  local -a required=(
    ANTHROPIC_BASE_URL
    ANTHROPIC_AUTH_TOKEN
    ENABLE_TOOL_SEARCH
    CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS
    CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC
    ANTHROPIC_DEFAULT_HAIKU_MODEL
    ANTHROPIC_DEFAULT_SONNET_MODEL
    ANTHROPIC_DEFAULT_OPUS_MODEL
  )

  for var in "${required[@]}"; do
    if jq -e ".env.\"$var\"" "$settings" >/dev/null 2>&1; then
      _pass "$var"
    else
      _fail "$var missing"
      ok=1
    fi
  done

  # Check model
  local model
  model="$(jq -r '.model' "$settings")"
  if [[ "$model" == "sonnet[1m]" ]]; then
    _pass "model = $model"
  else
    _fail "model = $model (expected sonnet[1m])"
    ok=1
  fi

  # Check claude CLI
  if command -v claude >/dev/null 2>&1; then
    _pass "claude CLI: $(claude --version 2>/dev/null)"
  else
    _fail "claude CLI not found"
    ok=1
  fi

  if [[ "$ok" -ne 0 ]]; then
    return 1
  fi
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

main() {
  printf '=========================================\n'
  printf ' Claude Code Pre-Setup\n'
  printf '=========================================\n\n'

  _check_prerequisites
  printf '\n'

  _install_claude
  printf '\n'

  _configure_glm
  printf '\n'

  _apply_post_fixes
  printf '\n'

  _verify

  printf '\n'
  printf '=========================================\n'
  printf ' Pre-Setup Complete!\n'
  printf '=========================================\n'
  printf '\nNext steps:\n'
  printf '  1. Open a new terminal (reload shell)\n'
  printf '  2. Run: claude\n'
  printf '  3. In Claude Code, follow claude/setup.md\n'
  printf '     for plugins, MCP servers, hooks, etc.\n'
}

main "$@"
