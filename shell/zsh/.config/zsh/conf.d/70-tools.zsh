# 70-tools.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-20 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Tool Shell Integrations - Completions, shell hooks, and git wrapper
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Copyright (C) 2026 zhengyu li
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 70 (after 60-keybinds.zsh, before 99-local.zsh)
#
# Prerequisites:
#   - compinit (30-completion.zsh), Zinit (40-plugins.zsh)
#
# Do NOT add: env vars, aliases
#             → Env vars in 00-env.zsh
#             → Aliases in 20-aliases.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Bun
# -----------------------------------------------------------------------------

# Bun is a fast native binary, no lazy loading needed.
if [[ -f "$BUN_INSTALL/_bun" ]]; then
  source "$BUN_INSTALL/_bun"
fi

# -----------------------------------------------------------------------------
# UV
# -----------------------------------------------------------------------------

# UV is a fast native binary, no lazy loading needed.
if command -v uv &>/dev/null; then
  _uv_comp="$XDG_CACHE_HOME/zsh/uv-completion.zsh"

  if [[ ! -f "$_uv_comp" ]] || [[ "$(command -v uv)" -nt "$_uv_comp" ]]; then
    mkdir -p "${_uv_comp:h}"
    uv generate-shell-completion zsh >! "$_uv_comp"
  fi
  source "$_uv_comp"
  unset _uv_comp
fi

# -----------------------------------------------------------------------------
# Carapace
# -----------------------------------------------------------------------------

# Must load AFTER compinit (30-completion.zsh) and fzf-tab (40-plugins.zsh).
if command -v carapace &>/dev/null; then
  _carapace_comp="$XDG_CACHE_HOME/zsh/carapace-completion.zsh"
  if [[ ! -f "$_carapace_comp" ]] || \
     [[ "$(command -v carapace)" -nt "$_carapace_comp" ]]; then
    mkdir -p "${_carapace_comp:h}"
    carapace _carapace zsh >! "$_carapace_comp"
  fi
  source "$_carapace_comp"
  unset _carapace_comp
fi

# -----------------------------------------------------------------------------
# FZF
# -----------------------------------------------------------------------------

# Load key bindings only. Must load AFTER fzf-tab (40-plugins.zsh).
#
# FZF ships two integration files:
#   key-bindings.zsh — Ctrl-R/Ctrl-T/Alt-C widgets ← load this
#   completion.zsh — **<Tab> trigger + rebinds ^I ← do NOT load
# Completion.zsh is redundant with fzf-tab which replaces the entire Tab UI.
if command -v fzf &>/dev/null; then
  _fzf_prefix="$HOMEBREW_PREFIX/opt/fzf"

  if [[ -f "$_fzf_prefix/shell/key-bindings.zsh" ]]; then
    source "$_fzf_prefix/shell/key-bindings.zsh"
  fi

  unset _fzf_prefix
fi

# -----------------------------------------------------------------------------
# Zoxide
# -----------------------------------------------------------------------------

# Fast directory jumping with frecency algorithm. Replaces z, autojump.
if command -v zoxide &>/dev/null; then
  eval "$(zoxide init zsh --cmd z)"
  # zinit's alias zi=zinit shadows zoxide's zi() — remove it.
  unalias zi 2>/dev/null
fi

# -----------------------------------------------------------------------------
# Git
# -----------------------------------------------------------------------------

# --- Config Classification Helpers ---

# Print 'r' for read flags, 'w' for write flags, empty otherwise.
_git_check_flag() {
  case "$1" in
    --get|--get-all|--get-regexp|--get-urlmatch|--list|\
      --show-origin|--show-scope)
      print -r -- r ;;
    --add|--unset|--unset-all|--rename-section|\
      --remove-section|--replace-all|--edit|-e|--comment)
      print -r -- w ;;
  esac
}

# Return 0 if the flag takes a value argument.
_git_flag_takes_value() {
  case "$1" in
    --default|--type|--value-type|--fixed-value|--pattern|\
      --expires-after)
      return 0 ;;
    *) return 1 ;;
  esac
}

# Print 'r' for read operations, 'w' for write operations.
_git_classify_config_op() {
  local _a=
  local _result=

  for _a in "${@}"; do
    _result="$(_git_check_flag "$_a")"
    if [[ -n "$_result" ]]; then
      print -r -- "$_result"
      return
    fi
  done

  # No explicit flag: infer from positional arg count
  local -a _pos=()
  local _skip=0
  for _a in "${@}"; do
    if (( _skip )); then
      _skip=0
      continue
    fi
    if _git_flag_takes_value "$_a"; then
      _skip=1
      continue
    fi
    case "$_a" in
      --*) ;;
      *) _pos+=("$_a") ;;
    esac
  done

  if (( ${#_pos[@]} >= 2 )); then
    print -r -- w
  else
    print -r -- r
  fi
}

# --- Config Wrapper ---

# Redirect git config --global writes to config.local (machine-specific,
# not tracked). Reads use --includes to transparently merge both files.
git() {
  if [[ "$1" != config || "$2" != --global ]]; then
    command git "$@"
    return
  fi

  local -a _args=("${@:3}")
  local _mode=
  _mode="$(_git_classify_config_op "${_args[@]}")"

  if [[ "$_mode" == w ]]; then
    command git config -f "$XDG_CONFIG_HOME/git/config.local" \
      "${_args[@]}"
  else
    command git config --global --includes "${_args[@]}"
  fi
}
