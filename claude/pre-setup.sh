#!/usr/bin/env bash
# pre-setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-04-04 13:29:08 Saturday by zhengyu.li>
# =============================================================================
# Claude Code Pre-Setup
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: claude-code, glm, setup, zai
# Dependencies: bash 4.3+, macOS 13+
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   2026-04-01 14:02 zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   Run this script before entering Claude Code for the first time.
#   Covers prerequisites check, claude CLI install, and GLM API
#   configuration via the ZAI helper. After completion, enter
#   Claude Code and follow setup.md for the rest.
#
# Usage: ./claude/pre-setup.sh
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

# --- Timeouts ---
readonly NETWORK_TIMEOUT=10
readonly INSTALL_TIMEOUT=120

# --- Platform ---
readonly MIN_MACOS_VERSION="13.0"

# -----------------------------------------------------------------------------
# Error Handling
# -----------------------------------------------------------------------------

_err_handler() {
  local -r code=$?
  printf '[error] %s() line %d: exit %d\n' \
         "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
}
trap '_err_handler' ERR

# -----------------------------------------------------------------------------
# Colors
# -----------------------------------------------------------------------------

readonly _RED=$'\033[0;31m'
readonly _GREEN=$'\033[0;32m'
readonly _YELLOW=$'\033[0;33m'
readonly _BLUE=$'\033[0;34m'
readonly _RESET=$'\033[0m'

# -----------------------------------------------------------------------------
# Logging
# -----------------------------------------------------------------------------

_info() {
  printf '  %s[info]%s %s\n' "$_BLUE" "$_RESET" "$*"
}

_warn() {
  printf '  %s[warn]%s %s\n' "$_YELLOW" "$_RESET" "$*" >&2
}

_pass() {
  printf '  %s✓%s %s\n' "$_GREEN" "$_RESET" "$*"
}

_fail() {
  printf '  %s✗%s %s\n' "$_RED" "$_RESET" "$*" >&2
}

# Check if a command exists and print its version (best-effort).
# Some tools do not support --version; the version line will be blank.
_check_cmd() {
  local -r name="$1"

  if [[ -z "$name" ]]; then
    return 1
  fi

  if command -v "$name" >/dev/null 2>&1; then
    local ver
    ver="$(command "$name" --version 2>/dev/null | head -1)" || true
    if [[ -z "$ver" ]]; then
      ver="(unknown version)"
    fi
    _pass "$name: $ver"
  else
    _fail "$name: NOT INSTALLED"
    return 1
  fi
}

# -----------------------------------------------------------------------------
# Prerequisites Check
# -----------------------------------------------------------------------------

# Compare two dot-separated version strings (e.g. "13.0" vs "13.0").
# Uses python3 with packaging.version if available, falls back to awk.
# Returns 0 if os_version >= min_version, 1 otherwise.
_check_macos_version() {
  local -r os_version="$1"
  local -r min_version="$2"

  # Guard: empty or non-numeric version strings cannot be compared safely.
  if [[ -z "$os_version" || ! "$os_version" =~ ^[0-9] ]]; then
    return 1
  fi
  if [[ -z "$min_version" || ! "$min_version" =~ ^[0-9] ]]; then
    return 1
  fi

  # Primary: python3 + packaging (handles pre-release tags, etc.)
  # Guard: avoid triggering macOS "install developer tools" popup.
  if command -v python3 >/dev/null 2>&1 \
    && python3 -c "from packaging.version import Version" 2>/dev/null \
    && python3 -c "
import sys
from packaging.version import Version
sys.exit(0 if Version(sys.argv[1]) >= Version(sys.argv[2]) else 1)
" "$os_version" "$min_version" 2>/dev/null; then
    return 0
  fi

  # Fallback: numeric field comparison via awk.
  # Compares up to 3 fields (major.minor.patch); missing fields treated as 0.
  # Uses ARGV (not -v) to avoid backslash-escape interpretation.
  if awk 'BEGIN {
      ver = ARGV[1]; min = ARGV[2]
      split(ver, v, "."); split(min, m, ".")
      for (i = 1; i <= 3; i++) {
        if ((v[i]+0) < (m[i]+0)) exit 1
        if ((v[i]+0) > (m[i]+0)) exit 0
      }
      exit 0
    }' "$os_version" "$min_version"; then
    return 0
  fi

  return 1
}

_check_prerequisites() {
  _info "Checking prerequisites..."

  # --- Platform Check ---
  if [[ "$(uname -s)" != Darwin ]]; then
    _fail "Not macOS ($(uname -s))"
    return 1
  fi

  local os_version
  os_version="$(sw_vers -productVersion)"

  if ! _check_macos_version "$os_version" "$MIN_MACOS_VERSION"; then
    _fail "macOS $os_version (need $MIN_MACOS_VERSION+)"
    return 1
  fi
  _pass "macOS $os_version"

  # --- Required Tools ---
  local missing=0
  local cmd

  for cmd in git jq curl bun uv; do
    if ! _check_cmd "$cmd"; then
      missing=$((missing + 1))
    fi
  done

  if (( missing > 0 )); then
    local workspace_dir
    workspace_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
    _warn "$missing tool(s) missing —" \
      "run: brew bundle --file=${workspace_dir}/pkg/homebrew/Brewfile"
    return 1
  fi

  # --- Network ---
  if curl -s --max-time "$NETWORK_TIMEOUT" \
    https://api.github.com >/dev/null 2>&1; then
    _pass "GitHub API accessible"
  else
    _fail "Cannot reach GitHub API (timeout: ${NETWORK_TIMEOUT}s)"
    return 1
  fi
}

# -----------------------------------------------------------------------------
# CLI Installation
# -----------------------------------------------------------------------------

_install_claude() {
  if command -v claude >/dev/null 2>&1; then
    _pass "claude CLI already installed: $(claude --version 2>/dev/null)"
    return 0
  fi

  _info "Installing claude CLI..."

  # Pipe curl output directly to bash — pipefail ensures failure
  # propagates if either curl or bash fails.
  if ! curl -fSL --connect-timeout "$NETWORK_TIMEOUT" \
    --max-time "$INSTALL_TIMEOUT" \
    https://claude.ai/install.sh | bash; then
    _fail "claude CLI installation failed"
    return 1
  fi

  # The install script may add claude to a directory not yet in PATH
  # (e.g. ~/.claude/bin); verify availability before declaring success.
  if command -v claude >/dev/null 2>&1; then
    _pass "claude CLI installed: $(claude --version 2>/dev/null)"
  else
    _warn "claude CLI installed but not yet in PATH"
    _warn "Will be available after opening a new terminal"
  fi
}

# -----------------------------------------------------------------------------
# GLM Configuration
# -----------------------------------------------------------------------------

_configure_glm() {
  local -r settings="$HOME/.claude/settings.json"

  # Skip if ZAI has already configured the essential auth vars.
  if [[ -f "$settings" ]] \
    && jq -e '.env.ANTHROPIC_BASE_URL' "$settings" >/dev/null 2>&1 \
    && jq -e '.env.ANTHROPIC_AUTH_TOKEN' "$settings" >/dev/null 2>&1; then
    _pass "GLM credentials already configured — skipping ZAI helper"
    return 0
  fi

  _info "Running GLM configuration helper (interactive)..."
  _info "Follow the prompts: select language, enter API key, choose plan"

  # Interactive tool — user may cancel (Ctrl+C) or tool may return non-zero.
  # Disable both set -e and the ERR trap so we can inspect the exit code.
  # Without disabling the trap, it would overwrite $? before we capture it.
  set +e
  trap - ERR
  bunx @z_ai/coding-helper
  local -r rc=$?
  set -e
  trap '_err_handler' ERR

  if (( rc != 0 )); then
    _warn "ZAI helper exited with code $rc (may need re-run later)"
    _warn "Continuing with post-configuration fixes..."
  fi
}

# -----------------------------------------------------------------------------
# Post-Install Fixes
# -----------------------------------------------------------------------------

# Rewrite settings.json atomically: jq writes to a temp file, then mv
# replaces the original.  A RETURN trap ensures cleanup on early return.
_apply_post_fixes() {
  local -r settings="$HOME/.claude/settings.json"

  if [[ ! -f "$settings" ]]; then
    _fail "settings.json not found at $settings"
    return 1
  fi

  _info "Applying post-ZAI configuration fixes..."

  local orig_perms
  orig_perms="$(/usr/bin/stat -f '%Lp' "$settings" 2>/dev/null \
    || printf '%s' "600")"

  local tmp
  tmp="$(mktemp)"

  if [[ -z "$tmp" ]]; then
    _fail "mktemp failed"
    return 1
  fi

  # Remove temp file on any early return (before mv succeeds).
  trap 'rm -f "${tmp:-}"' RETURN

  if ! jq '.model = "sonnet[1m]"
    | .env.ANTHROPIC_DEFAULT_HAIKU_MODEL = "glm-4.5-air"
    | .env.ANTHROPIC_DEFAULT_SONNET_MODEL = "glm-5-turbo"
    | .env.ANTHROPIC_DEFAULT_OPUS_MODEL = "glm-5.1"
    | .env.ENABLE_TOOL_SEARCH = "0"
    | .env.CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS = "1"
    | .env.CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1"' \
    "$settings" > "$tmp"; then
    _fail "jq failed to process settings.json"
    return 1
  fi

  if ! mv "$tmp" "$settings"; then
    _fail "mv failed — temp file will be cleaned up: $tmp"
    return 1
  fi

  # Temp file already renamed — clear cleanup trap.
  trap - RETURN

  if ! chmod "$orig_perms" "$settings"; then
    _warn "Could not restore permissions ($orig_perms) on $settings"
  fi

  _pass "Model set to sonnet[1m] with GLM env vars and compatibility flags"
}

# -----------------------------------------------------------------------------
# Verification
# -----------------------------------------------------------------------------

_verify() {
  local -r settings="$HOME/.claude/settings.json"
  local failed=0

  printf '\n'
  _info "Verifying configuration..."

  if [[ ! -f "$settings" ]]; then
    _fail "settings.json not found at $settings"
    return 1
  fi

  if ! jq empty "$settings" 2>/dev/null; then
    _fail "settings.json is not valid JSON"
    return 1
  fi

  # --- Check Required Env Vars ---
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

  local var val
  for var in "${required[@]}"; do
    val="$(jq -r ".env.\"$var\"" "$settings")"
    if [[ -n "$val" && "$val" != "null" ]]; then
      _pass "$var"
    else
      _fail "$var missing or empty"
      failed=1
    fi
  done

  # --- Check Model ---
  local model
  model="$(jq -r '.model' "$settings")"

  if [[ "$model" == "sonnet[1m]" ]]; then
    _pass "model = $model"
  else
    _fail "model = $model (expected sonnet[1m])"
    failed=1
  fi

  # --- Check Claude CLI ---
  # May need a new terminal if just installed (PATH not yet updated).
  if command -v claude >/dev/null 2>&1; then
    _pass "claude CLI: $(claude --version 2>/dev/null)"
  else
    _warn "claude CLI not in PATH (available after new terminal)"
  fi

  if (( failed != 0 )); then
    return 1
  fi

  return 0
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

main() {
  printf '=========================================\n'
  printf ' Claude Code Pre-Setup\n'
  printf '=========================================\n\n'

  if ! _check_prerequisites; then
    printf '\n'
    _warn "Prerequisites check failed — resolve issues above, then re-run"
    trap - ERR
    exit 1
  fi
  printf '\n'

  if ! _install_claude; then
    printf '\n'
    _warn "Claude CLI installation failed — check network, then re-run"
    trap - ERR
    exit 1
  fi
  printf '\n'

  _configure_glm
  printf '\n'

  # Guard: ZAI helper must have created settings.json before post-fixes.
  if [[ ! -f "$HOME/.claude/settings.json" ]]; then
    _warn "settings.json not created — ZAI helper may have been cancelled"
    _warn "Re-run ./claude/pre-setup.sh and complete the ZAI prompts"
    trap - ERR
    exit 1
  fi

  if ! _apply_post_fixes; then
    printf '\n'
    _warn "Post-configuration failed — check ~/.claude/settings.json"
    trap - ERR
    exit 1
  fi
  printf '\n'

  if ! _verify; then
    printf '\n'
    _warn "Verification found issues — review the output above"
    printf '\nNext steps:\n'
    printf '  1. Re-run: ./claude/pre-setup.sh\n'
    printf '  2. Or manually check: ~/.claude/settings.json\n'
    trap - ERR
    exit 1
  fi

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
