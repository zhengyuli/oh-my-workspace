#!/usr/bin/env bash
# pre-setup.sh -*- mode: sh; -*-
# Time-stamp: <2026-04-24 15:48:00 Thursday by zhengyu.li>
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
#   Output uses simple tree-structured log lines with step indicators.
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

# --- Files ---
# settings.json holds the API token; default to owner-only if stat fails.
readonly DEFAULT_SETTINGS_PERMS=600

# -----------------------------------------------------------------------------
# Color System
# -----------------------------------------------------------------------------

# Standard ANSI colors. All constants are empty when NO_COLOR is set
# or stdout is not a TTY, ensuring no escape codes in piped output.

_IS_TTY=false
if [[ -t 1 ]]; then
  _IS_TTY=true
fi
readonly _IS_TTY

if [[ -n "${NO_COLOR:-}" ]] || ! "${_IS_TTY}"; then
  readonly C_R=''
  readonly C_G=''
  readonly C_Y=''
  readonly C_B=''
  readonly C_BOLD=''
  readonly C_DIM=''
  readonly C_RESET=''
else
  readonly C_R=$'\033[0;31m'
  readonly C_G=$'\033[0;32m'
  readonly C_Y=$'\033[0;33m'
  readonly C_B=$'\033[0;34m'
  readonly C_BOLD=$'\033[1m'
  readonly C_DIM=$'\033[2m'
  readonly C_RESET=$'\033[0m'
fi

# -----------------------------------------------------------------------------
# Logging
# -----------------------------------------------------------------------------

# Content indent: 4 spaces for items nested under a phase header.
readonly _LOG_INDENT='    '

# Log a colored, tagged message to stdout or stderr.
# Arguments:
#   color  - ANSI escape sequence.
#   tag    - Label shown in brackets.
#   stream - "out" for stdout, "err" for stderr.
#   ...    - Message text.
_log() {
  local -r color="$1" tag="$2" stream="$3"
  shift 3
  if [[ "${stream}" == err ]]; then
    printf '%s%b[%s]%b %s\n' "${_LOG_INDENT}" "${color}" "${tag}" "${C_RESET}" "$*" >&2
  else
    printf '%s%b[%s]%b %s\n' "${_LOG_INDENT}" "${color}" "${tag}" "${C_RESET}" "$*"
  fi
}

# Convenience log wrappers: colored output at various severity levels.
log_ok()   { _log "${C_G}" ok    out "$*"; }
log_err()  { _log "${C_R}" error err "$*"; }
log_warn() { _log "${C_Y}" warn  err "$*"; }
log_info() { _log "${C_B}" info  out "$*"; }

# Phase header: top-level, bold, with step indicator [N/M].
# Set _PHASE_TOTAL before the first call. Counter auto-increments.
_PHASE_TOTAL=1
_PHASE_INDEX=0

_phase() {
  _PHASE_INDEX=$(( _PHASE_INDEX + 1 ))
  printf '\n%b[%d/%d]%b %b%s%b\n' \
    "${C_DIM}" "${_PHASE_INDEX}" "${_PHASE_TOTAL}" "${C_RESET}" "${C_BOLD}" "$*" "${C_RESET}"
}

# Print a warning and exit 1.
# Used by main() to abort cleanly after each phase failure with a recovery hint.
_abort() {
  printf '\n'
  log_warn "$*"
  trap - ERR
  exit 1
}

# -----------------------------------------------------------------------------
# Error Handling
# -----------------------------------------------------------------------------

# ERR trap: log failing function, line number, and exit code.
_err_handler() {
  local -r code=$?
  printf '%s%b[error]%b %s() line %d: exit %d\n' \
    "${_LOG_INDENT}" "${C_R}" "${C_RESET}" "${FUNCNAME[1]:-main}" "${BASH_LINENO[0]}" "$code" >&2
}
trap '_err_handler' ERR

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

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
    log_ok "$name: $ver"
  else
    log_err "$name: NOT INSTALLED"
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

# Verify platform, required tools, and network connectivity.
# Returns:
#   0 if all checks pass, 1 on the first critical failure.
_check_prerequisites() {
  # --- Platform Check ---
  if [[ "$(uname -s)" != Darwin ]]; then
    log_err "Not macOS ($(uname -s))"
    return 1
  fi

  local os_version
  os_version="$(sw_vers -productVersion)"

  if ! _check_macos_version "$os_version" "$MIN_MACOS_VERSION"; then
    log_err "macOS $os_version (need $MIN_MACOS_VERSION+)"
    return 1
  fi
  log_ok "macOS $os_version"

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
    log_warn "$missing tool(s) missing — run: brew bundle --file=${workspace_dir}/pkg/homebrew/Brewfile"
    return 1
  fi

  # --- Network ---
  if curl -s --max-time "$NETWORK_TIMEOUT" https://api.github.com >/dev/null 2>&1; then
    log_ok "GitHub API accessible"
  else
    log_err "Cannot reach GitHub API (timeout: ${NETWORK_TIMEOUT}s)"
    return 1
  fi
}

# -----------------------------------------------------------------------------
# CLI Installation
# -----------------------------------------------------------------------------

# Install the claude CLI via the official installer script.
# Skips installation if the CLI is already on PATH.
_install_claude() {
  if command -v claude >/dev/null 2>&1; then
    log_ok "claude CLI already installed: $(claude --version 2>/dev/null)"
    return 0
  fi

  log_info "Installing claude CLI..."

  # Pipe curl output directly to bash — pipefail ensures failure
  # propagates if either curl or bash fails.
  if ! curl -fSL --connect-timeout "$NETWORK_TIMEOUT" --max-time "$INSTALL_TIMEOUT" \
    https://claude.ai/install.sh | bash; then
    log_err "claude CLI installation failed"
    return 1
  fi

  # The install script may add claude to a directory not yet in PATH
  # (e.g. ~/.claude/bin); verify availability before declaring success.
  if command -v claude >/dev/null 2>&1; then
    log_ok "claude CLI installed: $(claude --version 2>/dev/null)"
  else
    log_warn "claude CLI installed but not yet in PATH"
    log_warn "Will be available after opening a new terminal"
  fi
}

# -----------------------------------------------------------------------------
# GLM Configuration
# -----------------------------------------------------------------------------

# Run the ZAI coding-helper to configure GLM API credentials.
# Skips if ANTHROPIC_BASE_URL and ANTHROPIC_AUTH_TOKEN are
# already present in ~/.claude/settings.json.
_configure_glm() {
  local -r settings="$HOME/.claude/settings.json"

  # Skip if ZAI has already configured the essential auth vars.
  if [[ -f "$settings" ]] \
    && jq -e '.env.ANTHROPIC_BASE_URL' "$settings" >/dev/null 2>&1 \
    && jq -e '.env.ANTHROPIC_AUTH_TOKEN' "$settings" >/dev/null 2>&1; then
    log_ok "GLM credentials already configured — skipping ZAI helper"
    return 0
  fi

  log_info "Running GLM configuration helper (interactive)..."
  log_info "Follow the prompts: select language, enter API key, choose plan"

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
    log_warn "ZAI helper exited with code $rc (may need re-run later)"
    log_warn "Continuing with post-configuration fixes..."
  fi
}

# -----------------------------------------------------------------------------
# Post-Install Fixes
# -----------------------------------------------------------------------------

# Apply post-ZAI fixes to settings.json (model defaults, compatibility flags).
# Rewrites atomically: jq writes to a temp file, then mv replaces the original.
# A RETURN trap ensures cleanup on early return.
_apply_post_fixes() {
  local -r settings="$HOME/.claude/settings.json"

  if [[ ! -f "$settings" ]]; then
    log_err "settings.json not found at $settings"
    return 1
  fi

  log_info "Applying post-ZAI configuration fixes..."

  local orig_perms
  orig_perms="$(/usr/bin/stat -f '%Lp' "$settings" 2>/dev/null || printf '%s' "$DEFAULT_SETTINGS_PERMS")"

  local tmp
  tmp="$(mktemp)"

  if [[ -z "$tmp" ]]; then
    log_err "mktemp failed"
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
    log_err "jq failed to process settings.json"
    return 1
  fi

  if ! mv "$tmp" "$settings"; then
    log_err "mv failed — temp file will be cleaned up: $tmp"
    return 1
  fi

  # Temp file already renamed — clear cleanup trap.
  trap - RETURN

  if ! chmod "$orig_perms" "$settings"; then
    log_warn "Could not restore permissions ($orig_perms) on $settings"
  fi

  log_ok "Model set to sonnet[1m] with GLM env vars and compatibility flags"
}

# -----------------------------------------------------------------------------
# Verification
# -----------------------------------------------------------------------------

# Verify settings.json contains all required env vars and correct model.
# Returns:
#   0 if all checks pass, 1 if any required value is missing.
_verify() {
  local -r settings="$HOME/.claude/settings.json"

  if [[ ! -f "$settings" ]]; then
    log_err "settings.json not found at $settings"
    return 1
  fi

  if ! jq empty "$settings" 2>/dev/null; then
    log_err "settings.json is not valid JSON"
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

  local failed=0
  local var val
  for var in "${required[@]}"; do
    val="$(jq -r ".env.\"$var\"" "$settings")"
    if [[ -n "$val" && "$val" != "null" ]]; then
      log_ok "$var"
    else
      log_err "$var missing or empty"
      failed=1
    fi
  done

  # --- Check Model ---
  local model
  model="$(jq -r '.model' "$settings")"

  if [[ "$model" == "sonnet[1m]" ]]; then
    log_ok "model = $model"
  else
    log_err "model = $model (expected sonnet[1m])"
    failed=1
  fi

  # --- Check Claude CLI ---
  # May need a new terminal if just installed (PATH not yet updated).
  if command -v claude >/dev/null 2>&1; then
    log_ok "claude CLI: $(claude --version 2>/dev/null)"
  else
    log_warn "claude CLI not in PATH (available after new terminal)"
  fi

  if (( failed != 0 )); then
    return 1
  fi

  return 0
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

# Entry point: run all setup phases in sequence.
main() {
  _PHASE_TOTAL=5
  _PHASE_INDEX=0

  _phase "Prerequisites"
  if ! _check_prerequisites; then
    _abort "Prerequisites check failed — resolve issues above, then re-run"
  fi

  _phase "Claude CLI"
  if ! _install_claude; then
    _abort "Claude CLI installation failed — check network, then re-run"
  fi

  _phase "GLM Configuration"
  _configure_glm

  # Guard: ZAI helper must have created settings.json before post-fixes.
  if [[ ! -f "$HOME/.claude/settings.json" ]]; then
    log_warn "settings.json not created — ZAI helper may have been cancelled"
    _abort "Re-run ./claude/pre-setup.sh and complete the ZAI prompts"
  fi

  if ! _apply_post_fixes; then
    _abort "Post-configuration failed — check ~/.claude/settings.json"
  fi

  _phase "Verification"
  if ! _verify; then
    log_warn "Verification found issues — review the output above"
    log_info "Re-run: ./claude/pre-setup.sh"
    log_info "Or manually check: ~/.claude/settings.json"
    trap - ERR
    exit 1
  fi

  _phase "Post Setup"
  log_info "Next steps:"
  log_info "  1. Open a new terminal (reload shell)"
  log_info "  2. Run: claude"
  log_info "  3. In Claude Code, follow claude/setup.md"
}

main "$@"
