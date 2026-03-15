# ==============================================================================
# File: conf.d/prompt.zsh
# Role: Prompt configuration with fallbacks
#
# Load context : Sourced by .zshrc after tools are loaded
# Dependencies : ZSH_PLUGIN_DIR (for Pure plugin)
# Side effects : Sets PROMPT, RPROMPT environment variables
# ==============================================================================

# ── Option 1: Starship (recommended) ─────────────────────────────────────────

if command -v starship &>/dev/null; then
  eval "$(starship init zsh)"

# ── Option 2: Pure (fallback) ─────────────────────────────────────────────

elif [[ -f "$ZSH_PLUGIN_DIR/pure/pure.zsh" ]]; then
  # Pure requires async
  if [[ -f "$ZSH_PLUGIN_DIR/async/async.zsh" ]]; then
    source "$ZSH_PLUGIN_DIR/async/async.zsh"
    source "$ZSH_PLUGIN_DIR/pure/pure.zsh"

    # Pure settings
    zstyle :prompt:pure:git:branch color yellow
    zstyle :prompt:pure:git:action color yellow
    zstyle :prompt:pure:git:dirty color red
    zstyle :prompt:pure:prompt:success color green
    zstyle :prompt:pure:prompt:error color red
    zstyle :prompt:pure:path color cyan
  fi

# ── Option 3: vcs_info (zero-dependency fallback) ───────────────────────────

else
  autoload -Uz vcs_info
  zstyle ':vcs_info:*' enable git
  zstyle ':vcs_info:*' check-for-changes true
  zstyle ':vcs_info:*' unstagedstr '%F{red}*%f'
  zstyle ':vcs_info:*' stagedstr '%F{green}+%f'
  zstyle ':vcs_info:git:*' formats ' %F{yellow}(%b%u%c)%f'
  zstyle ':vcs_info:git:*' actionformats ' %F{yellow}(%b|%a%u%c)%f'

  # Use precmd_functions array to avoid overwriting other plugins' hooks
  _vcs_info_precmd() { vcs_info }
  precmd_functions+=(_vcs_info_precmd)

  # Basic prompt with git info
  PROMPT='%F{cyan}%~%f${vcs_info_msg_0_} %F{green}❯%f '
  RPROMPT='%F{8}%n@%m%f'  # User@host on right side
fi
