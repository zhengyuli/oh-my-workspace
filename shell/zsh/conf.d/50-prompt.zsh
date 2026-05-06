# 50-prompt.zsh -*- mode: sh; -*-
# Time-stamp: <2026-04-07 07:15:23 Tuesday by zhengyu.li>
#
# =============================================================================
# Prompt Configuration - Starship primary, vcs_info fallback
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: prompt, starship, vcs_info, PS1
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   2026-04-07 07:15 zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   Loaded by: .zshrc (interactive shells only)
#   Load order: 50 (after 40-plugins.zsh, before 60-keybinds.zsh)
#
#   Prerequisites:
#     - 40-plugins.zsh (Zinit); starship optional (falls back to vcs_info)
#
#   Do NOT add: aliases, env vars
#               → Aliases in 20-aliases.zsh
#               → Env vars in 00-env.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Starship
# -----------------------------------------------------------------------------

# Config: $XDG_CONFIG_HOME/starship.toml
if command -v starship &>/dev/null; then
  _starship_cache="$XDG_CACHE_HOME/zsh/starship-init.zsh"
  if [[ ! -f "$_starship_cache" ]] || \
     [[ "$(command -v starship)" -nt "$_starship_cache" ]]; then
    mkdir -p "${_starship_cache:h}"
    starship init zsh >! "$_starship_cache"
  fi
  source "$_starship_cache"
  unset _starship_cache

  # Not all terminals support OSC 0 title escapes; check known-good ones.
  local _osc_terms='(iTerm.app|WezTerm|ghostty|Apple_Terminal)'
  if [[ "$TERM_PROGRAM" == ${~_osc_terms} ]] || \
     [[ "$TERM_PROGRAM" == tmux ]] || \
     [[ "$TERM" == alacritty* ]] || \
     [[ "$TERM" == kitty* ]] || \
     [[ -n "$TMUX" ]]; then
    autoload -Uz add-zsh-hook
    _omw_set_window_title() { print -Pn "\e]0;%~\a" }
    add-zsh-hook precmd _omw_set_window_title
  fi
  unset _osc_terms
else
  # Used when no external prompt tools are installed. No configuration needed.
  autoload -Uz vcs_info add-zsh-hook
  _omw_vcs_info_precmd() { vcs_info }
  add-zsh-hook precmd _omw_vcs_info_precmd

  zstyle ':vcs_info:git:*' formats ' %F{green}(%b)%f'
  zstyle ':vcs_info:*' enable git

  setopt PROMPT_SUBST
  PROMPT='%F{blue}%~%f${vcs_info_msg_0_} %F{yellow}>%f '
  RPROMPT='%F{240}%*%f'
fi
