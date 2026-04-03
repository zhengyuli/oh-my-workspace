# 50-prompt.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-20 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Prompt Configuration
#
# Loaded by: Interactive shells (.zshrc)
# Load order: 50 (after 40-plugins.zsh, before 60-keybinds.zsh)
#
# Prerequisites:
#   - 40-plugins.zsh (Zinit for Pure/P10k async loading)
#
# Responsibilities:
#   1. Initialize prompt theme (Starship/Pure/Powerlevel10k)
#   2. Configure XDG-compliant paths for prompt tools
#   3. Provide fallback vcs_info prompt when no external tools available
#
# Do NOT add: Aliases, functions, PATH modifications, environment variables
#             → Aliases: 20-aliases.zsh
#             → Functions: functions/ directory (autoloaded)
#             → PATH: 05-path.zsh
#             → Env vars: 00-env.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Starship
# -----------------------------------------------------------------------------

# Cross-shell prompt written in Rust.
# Prerequisites: brew install starship
# Config: $XDG_CONFIG_HOME/starship.toml
if command -v starship &>/dev/null; then
  eval "$(starship init zsh)"

# --- Window Title ---
# Not all terminals support OSC 0 title escapes; check known-good ones.
# Use add-zsh-hook (NOT precmd() directly) to keep starship's hook.
  if [[ "$TERM_PROGRAM" == (iTerm.app|WezTerm|ghostty|Apple_Terminal) ]] || \
     [[ "$TERM_PROGRAM" == tmux ]] || \
     [[ "$TERM" == alacritty* ]] || \
     [[ "$TERM" == kitty* ]] || \
     [[ -n "$TMUX" ]]; then
    autoload -Uz add-zsh-hook
    _omw_set_window_title() { print -Pn "\e]0;%~\a" }
    add-zsh-hook precmd _omw_set_window_title
  fi
else
# --- Fallback: Native VCS Info Prompt ---
# Built-in Zsh prompt with git branch display. Used when no external
# prompt tools are installed. No configuration needed.
  autoload -Uz vcs_info add-zsh-hook
  _omw_vcs_info_precmd() { vcs_info }
  add-zsh-hook precmd _omw_vcs_info_precmd

  zstyle ':vcs_info:git:*' formats ' %F{green}(%b)%f'
  zstyle ':vcs_info:*' enable git

  setopt PROMPT_SUBST
  PROMPT='%F{blue}%~%f${vcs_info_msg_0_} %F{yellow}>%f '
  RPROMPT='%F{240}%*%f'
fi
