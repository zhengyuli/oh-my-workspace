# lib/tui.sh -*- mode: sh; -*-
# Time-stamp: <2026-04-23 00:00:00 Wednesday by zhengyu.li>
#
# =============================================================================
# TUI Primitives Library
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: tui, ansi, terminal, progress
# Dependencies: bash 4.3+, tput (ncurses)
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   2026-04-23 00:00 zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   Rich terminal UI primitives for setup.sh. Provides color system,
#   status icons, phase headers, dashboard grid, summary table, and
#   health check rendering. All output uses ANSI escape codes with
#   NO_COLOR support and TTY detection for piped-output fallback.
# =============================================================================

# Guard against double-sourcing.
# shellcheck shell=bash
if [[ -n "${_TUI_LOADED:-}" ]]; then
  return 0
fi
_TUI_LOADED=1

# -----------------------------------------------------------------------------
# Terminal Capability Detection
# -----------------------------------------------------------------------------

# --- Safe tput Wrapper ---
# Defined first so capability probes below can use it at source-time.
# Returns empty string on failure (safe under set -euo pipefail).
_tput_safe() {
  tput "$@" 2>/dev/null || true
}

# --- Is Stdout a TTY? ---
_TUI_IS_TTY=false
if [[ -t 1 ]]; then
  _TUI_IS_TTY=true
fi
readonly _TUI_IS_TTY

# --- Terminal Width ---
readonly _TUI_DEFAULT_COLS=80
readonly _TUI_MIN_USABLE_COLS=40
_TUI_COLS=${_TUI_DEFAULT_COLS}
if "${_TUI_IS_TTY}"; then
  _cols=$(_tput_safe cols)
  if [[ -n "${_cols}" ]] && (( _cols >= _TUI_MIN_USABLE_COLS )); then
    _TUI_COLS=${_cols}
  fi
fi
readonly _TUI_COLS

# --- Fixed Viewport Support ---
_TUI_HAS_CUP=false
if "${_TUI_IS_TTY}"; then
  _cup_out=$(_tput_safe cup 0 0)
  if [[ -n "${_cup_out}" ]]; then
    _TUI_HAS_CUP=true
  fi
fi
readonly _TUI_HAS_CUP

# --- Unicode Support ---
_TUI_HAS_UTF=false
if [[ "${LC_ALL:-${LC_CTYPE:-${LANG:-}}}" =~ UTF ]]; then
  _TUI_HAS_UTF=true
fi
readonly _TUI_HAS_UTF

# --- Responsive Threshold ---
# Below this column count, fall back to linear progress.
# Dashboard grid is 89 chars wide (2 indent + 87 box). Require at least that.
readonly _TUI_MIN_COLS=89

# Use fixed-viewport dashboard only when terminal supports it AND is
# wide enough.
_TUI_USE_GRID=false
if "${_TUI_HAS_CUP}" && (( _TUI_COLS >= _TUI_MIN_COLS )); then
  _TUI_USE_GRID=true
fi
readonly _TUI_USE_GRID

# --- Spinner Characters ---
if "${_TUI_HAS_UTF}"; then
  readonly -a _SPINNER_FRAMES=(⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏)
else
  readonly -a _SPINNER_FRAMES=(- \\ \| /)
fi
readonly _SPINNER_INTERVAL=0.1

# -----------------------------------------------------------------------------
# Color System
# -----------------------------------------------------------------------------

# Standard ANSI colors. All constants are empty when NO_COLOR is set
# or stdout is not a TTY, ensuring no escape codes in piped output.

if [[ -n "${NO_COLOR:-}" ]] || ! "${_TUI_IS_TTY}"; then
  readonly C_R=''
  readonly C_G=''
  readonly C_Y=''
  readonly C_B=''
  readonly C_C=''
  readonly C_BOLD=''
  readonly C_DIM=''
  readonly C_RESET=''
else
  readonly C_R=$'\033[0;31m'
  readonly C_G=$'\033[0;32m'
  readonly C_Y=$'\033[0;33m'
  readonly C_B=$'\033[0;34m'
  readonly C_C=$'\033[0;36m'
  readonly C_BOLD=$'\033[1m'
  readonly C_DIM=$'\033[2m'
  readonly C_RESET=$'\033[0m'
fi

# --- Hide/Show Cursor ---
_tui_cursor_hide() {
  "${_TUI_IS_TTY}" || return 0
  printf '%b' '\033[?25l'
}
_tui_cursor_show() {
  "${_TUI_IS_TTY}" || return 0
  printf '%b' '\033[?25h'
}

# -----------------------------------------------------------------------------
# Icons
# -----------------------------------------------------------------------------

# Static icons are pre-rendered to avoid subshell forks at every call site.
# _icon_run remains a function because it animates with the elapsed second.
if "${_TUI_HAS_UTF}"; then
  readonly _ICON_OK="${C_G}✔${C_RESET}"
  readonly _ICON_FAIL="${C_R}✖${C_RESET}"
  readonly _ICON_SKIP="${C_DIM}·${C_RESET}"
else
  readonly _ICON_OK="${C_G}[ok]${C_RESET}"
  readonly _ICON_FAIL="${C_R}[X]${C_RESET}"
  readonly _ICON_SKIP="${C_DIM}.${C_RESET}"
fi

_icon_ok()   { printf '%s' "${_ICON_OK}"; }
_icon_fail() { printf '%s' "${_ICON_FAIL}"; }
_icon_skip() { printf '%s' "${_ICON_SKIP}"; }

_icon_run() {
  local -r idx=$(( SECONDS % ${#_SPINNER_FRAMES[@]} ))
  printf '%s%s%s' "${C_Y}" "${_SPINNER_FRAMES[idx]}" "${C_RESET}"
}

# -----------------------------------------------------------------------------
# Logging
# -----------------------------------------------------------------------------

# Worker: prints "  [tag] msg\n" with the given color, on stdout or stderr.
_log() {
  local -r color="$1" tag="$2" stream="$3"
  shift 3
  if [[ "${stream}" == err ]]; then
    printf '  %b[%s]%b %s\n' "${color}" "${tag}" "${C_RESET}" "$*" >&2
  else
    printf '  %b[%s]%b %s\n' "${color}" "${tag}" "${C_RESET}" "$*"
  fi
}

die()      { _log "${C_R}" error err "$*"; exit 1; }
_misuse()  { _log "${C_R}" error err "$*"; exit 2; }
log_ok()   { _log "${C_G}" ok    out "$*"; }
log_err()  { _log "${C_R}" error err "$*"; }
log_warn() { _log "${C_Y}" warn  err "$*"; }
log_info() { _log "${C_B}" info  out "$*"; }

# -----------------------------------------------------------------------------
# Elapsed Timer
# -----------------------------------------------------------------------------

# Each timer gets a unique slot in the associative array.
declare -gA _TUI_TIMERS=()

start_timer() {
  local -r name="${1:-default}"
  _TUI_TIMERS[${name}]=${SECONDS}
}

readonly _TUI_SECONDS_PER_MINUTE=60

elapsed() {
  local -r name="${1:-default}"
  local -r start="${_TUI_TIMERS[${name}]:-${SECONDS}}"
  local -r diff=$(( SECONDS - start ))
  if (( diff >= _TUI_SECONDS_PER_MINUTE )); then
    printf '%dm%ds' \
      $(( diff / _TUI_SECONDS_PER_MINUTE )) \
      $(( diff % _TUI_SECONDS_PER_MINUTE ))
  else
    printf '%ds' "${diff}"
  fi
}

# -----------------------------------------------------------------------------
# Result File
# -----------------------------------------------------------------------------

# Temp file for tracking per-package results during install.
# Format: package<TAB>status (e.g. shell/zsh<TAB>ok)

_TUI_RESULT_FILE=''
_TUI_HOOK_ERR_FILE=''
_TUI_HOOK_OK_FILE=''

# Allocate or truncate a state tempfile by nameref. Idempotent across
# multiple init calls so we don't leak temps from re-entrant entry points.
_tui_init_tmpfile() {
  local -n _ref="$1"
  if [[ -n "${_ref:-}" && -f "${_ref}" ]]; then
    : > "${_ref}"
  else
    _ref=$(mktemp)
    : > "${_ref}"
  fi
}

_tui_result_init() {
  _tui_init_tmpfile _TUI_RESULT_FILE
  _tui_init_tmpfile _TUI_HOOK_ERR_FILE
  _tui_init_tmpfile _TUI_HOOK_OK_FILE
  export _TUI_HOOK_ERR_FILE _TUI_HOOK_OK_FILE
}

_tui_result_write() {
  local -r pkg="$1"
  local -r status="$2"
  printf '%s\t%s\n' "${pkg}" "${status}" >> "${_TUI_RESULT_FILE}"
}

_tui_result_path() {
  printf '%s' "${_TUI_RESULT_FILE}"
}

# Print buffered post-install notes (success messages then error tails),
# then truncate. Called by setup.sh after the post-install table renders.
_tui_hook_msg_flush() {
  if [[ -n "${_TUI_HOOK_OK_FILE:-}" && -s "${_TUI_HOOK_OK_FILE}" ]]; then
    local _line
    while IFS= read -r _line; do
      printf '  %b%s%b\n' "${C_G}" "${_line}" "${C_RESET}"
    done < "${_TUI_HOOK_OK_FILE}"
    : > "${_TUI_HOOK_OK_FILE}"
  fi
  if [[ -n "${_TUI_HOOK_ERR_FILE:-}" && -s "${_TUI_HOOK_ERR_FILE}" ]]; then
    printf '\n  %bpost-install errors:%b\n' "${C_R}" "${C_RESET}" >&2
    local _line
    while IFS= read -r _line; do
      printf '  %b│%b %s\n' "${C_R}" "${C_RESET}" "${_line}" >&2
    done < "${_TUI_HOOK_ERR_FILE}"
    : > "${_TUI_HOOK_ERR_FILE}"
  fi
}

_tui_result_cleanup() {
  local f
  for f in "${_TUI_RESULT_FILE:-}" "${_TUI_HOOK_ERR_FILE:-}" \
           "${_TUI_HOOK_OK_FILE:-}" "${_TUI_BREW_TMPFILE:-}"; do
    if [[ -n "${f}" && -f "${f}" ]]; then
      rm -f "${f}"
    fi
  done
}

# -----------------------------------------------------------------------------
# Brewfile Parsing
# -----------------------------------------------------------------------------

# Parse a Brewfile and assign the count of formulae/casks/taps to the
# named variables. Skips comments and blank lines (handles tabs too).
# Usage: _tui_parse_brewfile <brewfile> formulae_var casks_var taps_var
_tui_parse_brewfile() {
  local -r brewfile="$1"
  local -n _f_ref="$2"
  local -n _c_ref="$3"
  local -n _t_ref="$4"
  _f_ref=0
  _c_ref=0
  _t_ref=0

  if [[ ! -f "${brewfile}" ]]; then
    return 0
  fi

  local line trimmed
  while IFS= read -r line; do
    trimmed="${line#"${line%%[![:space:]]*}"}"
    if [[ -z "${trimmed}" || "${trimmed:0:1}" == '#' ]]; then
      continue
    fi
    case "${trimmed}" in
      tap[[:space:]]*)  _t_ref=$(( _t_ref + 1 )) ;;
      cask[[:space:]]*) _c_ref=$(( _c_ref + 1 )) ;;
      brew[[:space:]]*) _f_ref=$(( _f_ref + 1 )) ;;
    esac
  done < "${brewfile}"
}

# -----------------------------------------------------------------------------
# Banner
# -----------------------------------------------------------------------------

_tui_banner() {
  local arch os_name os_ver host user shell_path workspace rev pkg_count
  arch=$(uname -m 2>/dev/null) || arch='unknown'
  os_name=$(uname -s 2>/dev/null) || os_name='unknown'
  os_ver=$(uname -r 2>/dev/null) || os_ver='?'
  host=$(hostname -s 2>/dev/null) || host='?'
  user="${USER:-$(id -un 2>/dev/null)}"
  shell_path="${SHELL:-?}"
  workspace="${WORKSPACE_DIR:-$PWD}"
  rev=$(git -C "${workspace}" rev-parse --short HEAD 2>/dev/null) || rev='-'
  pkg_count="${#PKG_ALL[@]}"
  if (( pkg_count == 0 )); then
    pkg_count='-'
  fi

  # Collapse $HOME prefix to ~ for compact display.
  local workspace_short="${workspace/#$HOME/\~}"

  local -ra _ART=(
    '   ###  #   #   ## ## #   #   #   #  ###  ####  #   #  #### ####   ###   #### #####'
    '  #   # #   #   # # # #   #   #   # #   # #   # #  #  #     #   # #   # #     #    '
    '  #   # #####   #   #  ###    # # # #   # ####  ###    ###  ####  ##### #     #### '
    '  #   # #   #   #   #   #     # # # #   # # #   #  #      # #     #   # #     #    '
    '   ###  #   #   #   #   #      # #   ###  #  #  #   # ####  #     #   #  #### #####'
  )

  printf '\n'
  local row
  for row in "${_ART[@]}"; do
    printf '  %s%s%s\n' "${C_C}" "${row}" "${C_RESET}"
  done
  printf '\n'
  printf '  %smacOS dotfiles · GNU Stow%s\n\n' "${C_DIM}" "${C_RESET}"

  # Inline 2-col info table sharing the standard border style. Uses a
  # narrow FIELD column + wide VALUE column (inverse of the package
  # tables, which are wide-item + narrow-status).
  local -r _F_W=12
  local -r _V_W=68
  printf '%s%s┌─%s─┐%s\n' "${_TBL_INDENT}" "${C_DIM}" "${_TBL_BORDER}" "${C_RESET}"
  printf '%s%s│%s   %s%-*s %-*s%s %s│%s\n' \
    "${_TBL_INDENT}" "${C_DIM}" "${C_RESET}" \
    "${C_BOLD}" "${_F_W}" 'FIELD' "${_V_W}" 'VALUE' "${C_RESET}" \
    "${C_DIM}" "${C_RESET}"
  printf '%s%s├─%s─┤%s\n' "${_TBL_INDENT}" "${C_DIM}" "${_TBL_BORDER}" "${C_RESET}"
  _banner_row() {
    printf '%s%s│%s %s %-*s %-*s %s│%s\n' \
      "${_TBL_INDENT}" "${C_DIM}" "${C_RESET}" \
      "${_ICON_SKIP}" "${_F_W}" "$1" "${_V_W}" "$2" \
      "${C_DIM}" "${C_RESET}"
  }
  _banner_row 'system'    "${os_name} ${os_ver} (${arch})"
  _banner_row 'user'      "${user}@${host}"
  _banner_row 'shell'     "${shell_path}"
  _banner_row 'workspace' "${workspace_short} (rev ${rev})"
  _banner_row 'stow pkgs' "${pkg_count} package(s) to stow"
  unset -f _banner_row
  printf '%s%s└─%s─┘%s\n' "${_TBL_INDENT}" "${C_DIM}" "${_TBL_BORDER}" "${C_RESET}"
  printf '\n'
}

# -----------------------------------------------------------------------------
# Phase Headers
# -----------------------------------------------------------------------------

# Phase numbering: [N/total]. Set per-command before calling phases.
# Default 5 (install --all). Override for uninstall (2) or single-pkg (2).
_TUI_PHASE_TOTAL=5
_TUI_PHASE_INDEX=0

# Pending icon (dim circle, in place of spinner for phases not yet started).
if "${_TUI_HAS_UTF}"; then
  readonly _ICON_PENDING="${C_DIM}○${C_RESET}"
else
  readonly _ICON_PENDING="${C_DIM}o${C_RESET}"
fi
_icon_pending() { printf '%s' "${_ICON_PENDING}"; }

# Print the "running" phase line.
# Stores how many extra lines of sub-content follow it (for overwrite math).
_TUI_PHASE_SUBLINES=0

_tui_phase_start() {
  local -r num="$1"
  local -r title="$2"
  start_timer "phase_${num}"
  _TUI_PHASE_SUBLINES=0
  _TUI_PHASE_INDEX="${num}"

  printf '  %s  %s[%d/%d]%s %s%s%s\n' \
    "$(_icon_run)" \
    "${C_B}" "${num}" "${_TUI_PHASE_TOTAL}" "${C_RESET}" \
    "${C_BOLD}" "${title}" "${C_RESET}"
}

# Worker: overwrite the phase line with a final state.
# Args: icon detail_color num title detail
_tui_phase_finish() {
  local -r icon="$1" detail_color="$2"
  local -r num="$3" title="$4"
  local -r detail="${5:-}"
  local dur
  dur=$(elapsed "phase_${num}")

  # On a TTY, overwrite the spinner/sub-content with the final state.
  # On non-TTY (logs, pipes), append a fresh line so both progress and
  # completion are visible without raw cursor escapes.
  if "${_TUI_IS_TTY}"; then
    printf '\033[%dA\033[2K' $(( _TUI_PHASE_SUBLINES + 1 ))
  fi
  if [[ -n "${detail}" ]]; then
    printf '  %s  %s[%d/%d]%s %s%s%s  %s%s%s  %s%s%s\n' \
      "${icon}" \
      "${C_B}" "${num}" "${_TUI_PHASE_TOTAL}" "${C_RESET}" \
      "${C_BOLD}" "${title}" "${C_RESET}" \
      "${detail_color}" "${detail}" "${C_RESET}" \
      "${C_DIM}" "${dur}" "${C_RESET}"
  else
    printf '  %s  %s[%d/%d]%s %s%s%s  %s%s%s\n' \
      "${icon}" \
      "${C_B}" "${num}" "${_TUI_PHASE_TOTAL}" "${C_RESET}" \
      "${C_BOLD}" "${title}" "${C_RESET}" \
      "${C_DIM}" "${dur}" "${C_RESET}"
  fi

  if "${_TUI_IS_TTY}" && (( _TUI_PHASE_SUBLINES > 0 )); then
    printf '\033[%dB' "${_TUI_PHASE_SUBLINES}"
  fi
  _TUI_PHASE_SUBLINES=0
}

_tui_phase_done() {
  _tui_phase_finish "${_ICON_OK}" "${C_DIM}" "$@"
}

_tui_phase_fail() {
  _tui_phase_finish "${_ICON_FAIL}" "${C_R}" "$@"
}

# -----------------------------------------------------------------------------
# Static Info Table
# -----------------------------------------------------------------------------
# A lightweight table for phases 1, 2, 4, 5 with optional live-redraw animation.
# Columns: ITEM (71) | STATUS (9) — identical layout to the dash grid.
# Bumps _TUI_PHASE_SUBLINES for cursor math.

# Table columns fill the same 89-char width as the banner box.
# Row:  2(indent) + │ + sp + icon(1) + sp + item(71) + sp + status(9) + sp + │ = 89
# Border: 2 + ┌─ + 83_dashes + ─┐ = 89
readonly _TBL_COL_ITEM=71
readonly _TBL_COL_STATUS=9
# Header col1 absorbs the icon+sp that rows allocate ahead of item text.
readonly _TBL_COL_HEADER=$(( _TBL_COL_ITEM + 2 ))
# 2-space indent, matching banner.
readonly _TBL_INDENT='  '
# Indent for sub-content under a phase line (aligns under the phase
# title, past the leading "  ✔  " prefix).
readonly _TBL_SUB_INDENT='     '
# Pre-rendered border bar — 83 dashes, matching banner's 89-char outer width.
readonly _TBL_BORDER='───────────────────────────────────────────────────────────────────────────────────'

_tui_tbl_header() {
  local -r col1="${1:-ITEM}"
  local -r col2="${2:-STATUS}"
  printf '%s%s┌─%s─┐%s\n' "${_TBL_INDENT}" "${C_DIM}" "${_TBL_BORDER}" "${C_RESET}"
  printf '%s%s│%s %s%-*s%s %s%-*s%s %s│%s\n' \
    "${_TBL_INDENT}" "${C_DIM}" "${C_RESET}" \
    "${C_BOLD}" "${_TBL_COL_HEADER}" "${col1}" "${C_RESET}" \
    "${C_BOLD}" "${_TBL_COL_STATUS}" "${col2}" "${C_RESET}" \
    "${C_DIM}" "${C_RESET}"
  printf '%s%s├─%s─┤%s\n' "${_TBL_INDENT}" "${C_DIM}" "${_TBL_BORDER}" "${C_RESET}"
  _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 3 ))
}

_tui_tbl_footer() {
  printf '%s%s└─%s─┘%s\n' "${_TBL_INDENT}" "${C_DIM}" "${_TBL_BORDER}" "${C_RESET}"
  _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 1 ))
}

# Print one table row.
# Args: icon item_label status_text status_color
_tui_tbl_row() {
  local -r icon="$1"
  local -r item="$2"
  local -r status_text="$3"
  local -r status_color="$4"

  printf '%s%s│%s %s %-*s %s%-*s%s %s│%s\n' \
    "${_TBL_INDENT}" "${C_DIM}" "${C_RESET}" \
    "${icon}" "${_TBL_COL_ITEM}" "${item}" \
    "${status_color}" "${_TBL_COL_STATUS}" "${status_text}" "${C_RESET}" \
    "${C_DIM}" "${C_RESET}"
  _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 1 ))
}

# Overwrite the last N rows in place (for in-table spinner animation).
# Args: n — number of rows to move up and redraw.
_tui_tbl_up() {
  local -r n="$1"
  if "${_TUI_USE_GRID}" && (( n > 0 )); then
    printf '\033[%dA' "${n}"
  fi
}

# -----------------------------------------------------------------------------
# Dashboard Grid
# -----------------------------------------------------------------------------

# Dashboard columns fill the same 89-char width as the banner box.
# Row: 2(indent) + │ + sp + icon(1) + sp + pkg(71) + sp + status(9) + sp + │ = 89
# Border: 2 + ╭ + ─ + 83_dashes + ─ + ╮ = 89 — shares _TBL_BORDER.
readonly _DASH_COL_PKG=71
readonly _DASH_COL_STATUS=9
# Header col1 absorbs the icon+sp that rows allocate ahead of pkg text.
readonly _DASH_COL_HEADER=$(( _DASH_COL_PKG + 2 ))
# Indent prefix — 2 spaces, matching banner and table indent.
readonly _DASH_INDENT='  '

# Track how many rows the dashboard occupies on screen (for _tui_dash_up).
_TUI_DASH_ROWS=0

_tui_dash_header() {
  printf '%s%s┌─%s─┐%s\n' "${_DASH_INDENT}" "${C_DIM}" "${_TBL_BORDER}" "${C_RESET}"
  # Color codes outside %-*s to avoid escape-byte padding inflation.
  printf '%s%s│%s %s%-*s%s %s%-*s%s %s│%s\n' \
    "${_DASH_INDENT}" "${C_DIM}" "${C_RESET}" \
    "${C_BOLD}" "${_DASH_COL_HEADER}" "PACKAGE" "${C_RESET}" \
    "${C_BOLD}" "${_DASH_COL_STATUS}" "STATUS" "${C_RESET}" \
    "${C_DIM}" "${C_RESET}"
  printf '%s%s├─%s─┤%s\n' "${_DASH_INDENT}" "${C_DIM}" "${_TBL_BORDER}" "${C_RESET}"
}

_tui_dash_footer() {
  printf '%s%s└─%s─┘%s\n' "${_DASH_INDENT}" "${C_DIM}" "${_TBL_BORDER}" "${C_RESET}"
}

_tui_dash_row() {
  local -r pkg="$1"
  local -r status="$2"

  local icon status_text status_color
  case "${status}" in
    ok) icon="${_ICON_OK}"; status_text='ok'; status_color="${C_G}" ;;
    fail) icon="${_ICON_FAIL}"; status_text='fail'; status_color="${C_R}" ;;
    run) icon=$(_icon_run); status_text='...'; status_color="${C_Y}" ;;
    skip) icon="${_ICON_SKIP}"; status_text='skip'; status_color="${C_DIM}" ;;
    pending) icon="${_ICON_PENDING}"; status_text='pending'; status_color="${C_DIM}" ;;
    *) icon="${_ICON_SKIP}"; status_text="${status}"; status_color="${C_DIM}" ;;
  esac

  # Color codes outside %-*s fields to avoid escape-byte padding inflation.
  printf '%s%s│%s %s %-*s %s%-*s%s %s│%s\n' \
    "${_DASH_INDENT}" "${C_DIM}" "${C_RESET}" \
    "${icon}" \
    "${_DASH_COL_PKG}" "${pkg}" \
    "${status_color}" "${_DASH_COL_STATUS}" "${status_text}" "${C_RESET}" \
    "${C_DIM}" "${C_RESET}"
}

# Render the full dashboard for a set of packages.
# Args: statuses_array_name [ordered_pkg_list...]
_tui_dash_render() {
  local -rn _statuses="$1"
  shift
  local -a _order=("$@")
  local -r total=${#_order[@]}

  if "${_TUI_USE_GRID}"; then
    _tui_cursor_hide

    _tui_dash_header

    local pkg status
    for pkg in "${_order[@]}"; do
      status="${_statuses[${pkg}]:-pending}"
      _tui_dash_row "${pkg}" "${status}"
    done

    _tui_dash_footer

    # header=3 lines (top border + column header + divider), footer=1 line.
    _TUI_DASH_ROWS=$(( total + 4 ))
    # Also track as phase sub-content so _tui_phase_done can move past them.
    _TUI_PHASE_SUBLINES=${_TUI_DASH_ROWS}
    _tui_cursor_show
  else
    # Linear fallback: accumulate results silently; on the final render call
    # (when no package is still pending or running) draw the full box once.
    local pkg status _any_active=false
    for pkg in "${_order[@]}"; do
      status="${_statuses[${pkg}]:-pending}"
      if [[ "${status}" == pending || "${status}" == run ]]; then
        _any_active=true
        break
      fi
    done
    if "${_any_active}"; then
      return 0
    fi

    # All packages settled — draw the complete box.
    _tui_dash_header
    for pkg in "${_order[@]}"; do
      status="${_statuses[${pkg}]:-pending}"
      _tui_dash_row "${pkg}" "${status}"
    done
    _tui_dash_footer

    _TUI_DASH_ROWS=$(( total + 4 ))
    _TUI_PHASE_SUBLINES=${_TUI_DASH_ROWS}
  fi
}

# Move cursor up to overwrite the dashboard for in-place updates.
_tui_dash_up() {
  if "${_TUI_USE_GRID}" && (( _TUI_DASH_ROWS > 0 )); then
    printf '\033[%dA' "${_TUI_DASH_ROWS}"
  fi
}

# -----------------------------------------------------------------------------
# Phase 1: Prerequisites Table
# -----------------------------------------------------------------------------
# Args: pairs "label:status"
#   status: "ok" | "installed" | "skip" | "fail" | "dry-run"
# Renders with spinner per-row when _TUI_USE_GRID is true.
_tui_prereq_table() {
  _tui_render_status_table 'PREREQUISITE' _tui_visual_prereq "$@"
}

# -----------------------------------------------------------------------------
# Phase 2: Homebrew Bundle Table
# -----------------------------------------------------------------------------
# Args: formulae_count casks_count taps_count [dry_run]
_tui_brew_table() {
  local -r formulae="${1:-0}"
  local -r casks="${2:-0}"
  local -r taps="${3:-0}"

  local icon
  icon=${_ICON_OK}
  local color="${C_G}"

  _tui_tbl_header 'CATEGORY' 'COUNT'
  _tui_tbl_row "${icon}" 'Formulae' "${formulae}" "${color}"
  _tui_tbl_row "${icon}" 'Casks' "${casks}" "${color}"
  _tui_tbl_row "${icon}" 'Taps' "${taps}" "${color}"
  _tui_tbl_footer
}

# -----------------------------------------------------------------------------
# Phase 4: Post-Install Hooks Table
# -----------------------------------------------------------------------------
# Args: pairs "label:status" — status: ok|skip|fail|dry-run
_tui_hooks_table() {
  _tui_render_status_table 'HOOK' _tui_visual_hooks "$@"
}

# Status→(icon,color) for prereq table (skip is success).
_tui_visual_prereq() {
  case "$1" in
    ok|skip|installed) printf '%s\t%s' "${_ICON_OK}" "${C_G}" ;;
    fail) printf '%s\t%s' "${_ICON_FAIL}" "${C_R}" ;;
    *) printf '%s\t%s' "${_ICON_SKIP}" "${C_DIM}" ;;
  esac
}

# Status→(icon,color) for hooks table (skip is dim).
_tui_visual_hooks() {
  case "$1" in
    ok) printf '%s\t%s' "${_ICON_OK}" "${C_G}" ;;
    fail) printf '%s\t%s' "${_ICON_FAIL}" "${C_R}" ;;
    *) printf '%s\t%s' "${_ICON_SKIP}" "${C_DIM}" ;;
  esac
}

# Shared renderer for prereq/hooks status tables.
# Args: header_label visual_fn entry...
_tui_render_status_table() {
  local -r header="$1" visual_fn="$2"
  shift 2
  local -a entries=("$@")
  local -r total=${#entries[@]}

  _tui_tbl_header "${header}" 'STATUS'

  local entry item st icon color visual
  if "${_TUI_USE_GRID}"; then
    _tui_cursor_hide
    for entry in "${entries[@]}"; do
      _tui_tbl_row "$(_icon_run)" "${entry%%:*}" '...' "${C_Y}"
    done
    _tui_tbl_up "${total}"
    for entry in "${entries[@]}"; do
      item="${entry%%:*}"; st="${entry#*:}"
      visual=$("${visual_fn}" "${st}")
      icon="${visual%$'\t'*}"; color="${visual#*$'\t'}"
      printf '\033[2K'
      _tui_tbl_row "${icon}" "${item}" "${st}" "${color}"
      _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES - 1 ))
    done
    _tui_cursor_show
  else
    for entry in "${entries[@]}"; do
      item="${entry%%:*}"; st="${entry#*:}"
      visual=$("${visual_fn}" "${st}")
      icon="${visual%$'\t'*}"; color="${visual#*$'\t'}"
      _tui_tbl_row "${icon}" "${item}" "${st}" "${color}"
    done
  fi

  _tui_tbl_footer
}

# -----------------------------------------------------------------------------
# Status Command Tables
# -----------------------------------------------------------------------------

# Prerequisites table for cmd_status (shows version in note column).
# Args: pairs of "label:status:version" — status: ok | missing
_tui_status_prereq_table() {
  _tui_tbl_header 'PREREQUISITE' 'STATUS'
  local entry item st icon color
  for entry in "$@"; do
    item="${entry%%:*}"
    st="${entry#*:}"
    case "${st}" in
      ok) icon=${_ICON_OK}; color="${C_G}" ;;
      missing) icon=${_ICON_FAIL}; color="${C_R}" ;;
      *) icon=${_ICON_SKIP}; color="${C_DIM}" ;;
    esac
    _tui_tbl_row "${icon}" "${item}" "${st}" "${color}"
  done
  _tui_tbl_footer
}

# Package status table for cmd_status.
# Args: pkgs_array_name stowed_map_name
#   pkgs_array_name  — nameref to array of pkg canonical names
#   stowed_map_name  — nameref to assoc array pkg→0|1
_tui_status_pkg_table() {
  local -rn _sp_pkgs="$1"
  local -rn _sp_stowed="$2"

  _tui_tbl_header 'PACKAGE' 'STATUS'

  local pkg pkg_base icon color st
  for pkg in "${_sp_pkgs[@]}"; do
    pkg_base="${pkg##*/}"
    # shellcheck disable=SC2004  # assoc array key needs $ or it's literal
    if (( _sp_stowed[${pkg}] )); then
      icon=${_ICON_OK}
      color="${C_G}"
      st='stowed'
    else
      icon=${_ICON_SKIP}
      color="${C_DIM}"
      st='unstowed'
    fi
    _tui_tbl_row "${icon}" "${pkg_base}" "${st}" "${color}"
  done

  _tui_tbl_footer
}

# -----------------------------------------------------------------------------
# Brew Bundle Output Parser
# -----------------------------------------------------------------------------

# Parse brew bundle output and return a short detail string for the phase line.
# Prints: "N packages · M new" or "N packages · M new · K failed"
# Also sets _TUI_BREW_DETAIL (global) for use by _tui_phase_done.
_TUI_BREW_DETAIL=''
_tui_brew_summary() {
  local -r output="$1"
  local installed=0 skipped=0 failed=0

  local line
  while IFS= read -r line; do
    case "${line}" in
      Installing*|Upgrading*) installed=$(( installed + 1 )) ;;
      Skipping*|Using*)       skipped=$(( skipped + 1 )) ;;
      'Error:'*)              failed=$(( failed + 1 )) ;;
    esac
  done <<< "${output}"

  local total=$(( installed + skipped + failed ))
  if (( failed == 0 )); then
    if (( installed > 0 )); then
      _TUI_BREW_DETAIL="${total} packages · ${installed} new"
    else
      _TUI_BREW_DETAIL="${total} packages · up-to-date"
    fi
  else
    _TUI_BREW_DETAIL="${total} packages · ${installed} new · ${failed} failed"
  fi
}

# Run brew bundle with a live spinner on the phase line itself.
# On failure, print a collapsed error block as sub-content.
readonly _TUI_BREW_ERROR_TAIL=10

_TUI_BREW_TMPFILE=''

_tui_brew_run() {
  local -r brewfile="$1"
  _TUI_BREW_TMPFILE=$(mktemp)
  export _TUI_BREW_TMPFILE
  local -r tmp="${_TUI_BREW_TMPFILE}"

  brew bundle --file="${brewfile}" >"${tmp}" 2>&1 &
  local -r brew_pid=$!

  if "${_TUI_IS_TTY}"; then
    # Animate the phase line spinner while brew runs.
    local frame=0
    local idx
    while kill -0 "${brew_pid}" 2>/dev/null; do
      idx=$(( frame % ${#_SPINNER_FRAMES[@]} ))
      # Move up to the phase line, rewrite the spinner, move back down.
      printf '\033[1A\033[2K'
      printf '  %s  %s[%d/%d]%s %sHomebrew Bundle%s  %s…%s\n' \
        "${_SPINNER_FRAMES[idx]}" \
        "${C_B}" "${_TUI_PHASE_INDEX}" "${_TUI_PHASE_TOTAL}" "${C_RESET}" \
        "${C_BOLD}" "${C_RESET}" \
        "${C_DIM}" "${C_RESET}"
      sleep "${_SPINNER_INTERVAL}"
      frame=$(( frame + 1 ))
    done
  fi

  local brew_rc=0
  wait "${brew_pid}" || brew_rc=$?

  local output
  output=$(cat "${tmp}")
  rm -f "${tmp}"
  _TUI_BREW_TMPFILE=''

  _tui_brew_summary "${output}"

  if (( brew_rc != 0 )); then
    # Print collapsed error block as sub-content under the phase line.
    printf '%s%s┌─ brew bundle errors %s\n' \
      "${_TBL_SUB_INDENT}" "${C_R}" "${C_RESET}"
    local line
    while IFS= read -r line; do
      printf '%s%s│%s %s\n' \
        "${_TBL_SUB_INDENT}" "${C_R}" "${C_RESET}" "${line}"
      _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 1 ))
    done < <(printf '%s\n' "${output}" | tail -n "${_TUI_BREW_ERROR_TAIL}")
    printf '%s%s└%s\n' "${_TBL_SUB_INDENT}" "${C_R}" "${C_RESET}"
    _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 2 ))
  fi

  return "${brew_rc}"
}

# -----------------------------------------------------------------------------
# Health Check
# -----------------------------------------------------------------------------

# Health check result storage.
declare -gA _TUI_HEALTH=()
declare -ga _TUI_HEALTH_ORDER=()

# Check that a tool is reachable on $PATH. Sets check_result to either
# "ok:<resolved-path>" or "fail:<friendly message>".
_tui_health_probe() {
  local -r cmd_name="$1"
  local -r friendly="${2:-${cmd_name}}"
  local resolved
  resolved=$(command -v "${cmd_name}" 2>/dev/null)
  if [[ -n "${resolved}" ]]; then
    check_result="ok:${resolved}"
  else
    check_result="fail:${friendly} not found"
  fi
}

_tui_health_check() {
  local -r result_file="$1"
  local -a pkgs=()
  local failed_count=0

  local pkg status
  while IFS=$'\t' read -r pkg status; do
    if [[ "${status}" == ok ]]; then
      pkgs+=("${pkg}")
    fi
  done < "${result_file}"

  _tui_phase_start 5 'Health Check'

  # Tool existence checks.
  local check_result
  for pkg in "${pkgs[@]}"; do
    check_result=''
    case "${pkg}" in
      shell/zsh)            _tui_health_probe zsh      'zsh not in PATH' ;;
      shell/starship)       _tui_health_probe starship 'starship' ;;
      editor/vim)           _tui_health_probe vim      'vim' ;;
      editor/emacs)         _tui_health_probe emacs    'emacs' ;;
      term/ghostty)         _tui_health_probe ghostty  'ghostty' ;;
      tool/git)             _tui_health_probe git      'git' ;;
      tool/lazygit)         _tui_health_probe lazygit  'lazygit' ;;
      tool/ripgrep)         _tui_health_probe rg       'rg' ;;
      tool/yazi)            _tui_health_probe yazi     'yazi' ;;
      lang/python/uv)       _tui_health_probe uv       'uv' ;;
      lang/typescript/bun)  _tui_health_probe bun      'bun' ;;
    esac
    if [[ -z "${check_result}" ]]; then
      continue
    fi
    _TUI_HEALTH[${pkg}]="${check_result}"
    _TUI_HEALTH_ORDER+=("${pkg}")
    if [[ "${check_result}" == fail:* ]]; then
      failed_count=$(( failed_count + 1 ))
    fi
  done

  _tui_health_render "${failed_count}"

  local total=${#_TUI_HEALTH_ORDER[@]}
  local issues=${failed_count}
  local detail
  if (( issues == 0 )); then
    detail="${total} checks passed"
    _tui_phase_done 5 'Health Check' "${detail}"
  else
    detail="${issues} issues"
    _tui_phase_fail 5 'Health Check' "${detail}"
  fi
}

_tui_health_render() {
  local -r failed_count="$1"
  local total=${#_TUI_HEALTH_ORDER[@]}

  _tui_tbl_header 'TOOL' 'STATUS'

  if "${_TUI_USE_GRID}"; then
    _tui_cursor_hide
    # Print all rows as running first.
    local pkg
    for pkg in "${_TUI_HEALTH_ORDER[@]}"; do
      _tui_tbl_row "$(_icon_run)" "${pkg##*/}" '...' "${C_Y}"
    done
    # Overwrite each row with its real result.
    _tui_tbl_up "${total}"
    local check_result pkg_base
    for pkg in "${_TUI_HEALTH_ORDER[@]}"; do
      check_result="${_TUI_HEALTH[${pkg}]}"
      pkg_base="${pkg##*/}"
      printf '\033[2K'
      if [[ "${check_result}" == ok:* ]]; then
        _tui_tbl_row "${_ICON_OK}" "${pkg_base}" 'ok' "${C_G}"
      else
        _tui_tbl_row "${_ICON_FAIL}" "${pkg_base}" 'fail' "${C_R}"
      fi
      _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES - 1 ))
    done
    _tui_cursor_show
  else
    local pkg check_result pkg_base
    for pkg in "${_TUI_HEALTH_ORDER[@]}"; do
      check_result="${_TUI_HEALTH[${pkg}]}"
      pkg_base="${pkg##*/}"
      if [[ "${check_result}" == ok:* ]]; then
        _tui_tbl_row "${_ICON_OK}" "${pkg_base}" 'ok' "${C_G}"
      else
        _tui_tbl_row "${_ICON_FAIL}" "${pkg_base}" 'fail' "${C_R}"
      fi
    done
  fi

  _tui_tbl_footer

  # Suggestions on failure — printed below the table.
  local issues=${failed_count}
  if (( issues > 0 )); then
    printf '\n'
    _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 1 ))
    local pkg check_result pkg_base
    for pkg in "${_TUI_HEALTH_ORDER[@]}"; do
      check_result="${_TUI_HEALTH[${pkg}]}"
      if [[ "${check_result}" == fail:* ]]; then
        pkg_base="${pkg##*/}"
        case "${pkg}" in
          shell/zsh)
            printf '     %s→%s run: %s./setup.sh install --all%s\n' \
              "${C_Y}" "${C_RESET}" "${C_BOLD}" "${C_RESET}" ;;
          tool/yazi)
            printf '     %s→%s run: %s./setup.sh install --force yazi%s\n' \
              "${C_Y}" "${C_RESET}" "${C_BOLD}" "${C_RESET}" ;;
          *)
            printf '     %s→%s check %s%s%s installation\n' \
              "${C_Y}" "${C_RESET}" "${C_BOLD}" "${pkg_base}" "${C_RESET}" ;;
        esac
        _TUI_PHASE_SUBLINES=$(( _TUI_PHASE_SUBLINES + 1 ))
      fi
    done
  fi
}

# -----------------------------------------------------------------------------
# UI Helpers (from setup.sh)
# -----------------------------------------------------------------------------

confirm() {
  local prompt="$1"
  local default="${2:-n}"
  local reply
  local out=/dev/tty
  if [[ ! -w /dev/tty ]]; then
    out=/dev/stderr
  fi

  if [[ "${default}" == y ]]; then
    printf '\n  %s [Y/n]: ' "${prompt}" > "${out}"
  else
    printf '\n  %s [y/N]: ' "${prompt}" > "${out}"
  fi

  if ! read -r reply; then
    reply=''
  fi

  [[ "${reply:-${default}}" =~ ^[Yy]$ ]]
}
