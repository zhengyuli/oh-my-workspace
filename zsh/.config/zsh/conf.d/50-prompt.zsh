# 50-prompt.zsh
# =============================================================================
# Prompt Configuration
#
# Loaded by: Interactive shells (.zshrc)
# Load order: After 40-plugins.zsh, before 60-keybinds.zsh
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
# Option A: Starship (Default)
# -----------------------------------------------------------------------------
# Cross-shell prompt written in Rust. Best for users who want:
# - Consistent prompts across bash/fish/zsh/PowerShell
# - Minimal configuration with TOML
# - Fast startup with good performance
#
# Prerequisites: brew install starship
# Config file: $XDG_CONFIG_HOME/starship.toml
# Documentation: https://starship.rs/config/
# -----------------------------------------------------------------------------
if command -v starship &>/dev/null; then
  export STARSHIP_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/starship.toml"
  export STARSHIP_CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/starship"
  eval "$(starship init zsh)"

# -----------------------------------------------------------------------------
# Option B: Pure (Minimal)
# -----------------------------------------------------------------------------
# Lightweight native Zsh prompt with async git status. Best for users who want:
# - Minimal dependencies (no Rust required)
# - Fast, simple, beautiful defaults
# - Zsh-only environment
#
# Prerequisites: Zinit plugin manager (see 40-plugins.zsh)
# Uncomment the block below to enable
# -----------------------------------------------------------------------------
# elif [[ -n "$ZINIT_HOME" ]]; then
#   zinit ice pick'async.zsh' src'pure.zsh'
#   zinit light sindresorhus/pure
#   zstyle ':prompt:pure:path' color '#89b4fa'
#   zstyle ':prompt:pure:prompt:*' color '#cba6f7'
#   zstyle ':prompt:pure:git:branch' color '#a6e3a1'

# -----------------------------------------------------------------------------
# Option C: Powerlevel10k (Feature-rich)
# -----------------------------------------------------------------------------
# Most configurable Zsh prompt with wizard. Best for users who want:
# - Maximum customization with visual wizard
# - Instant prompt (shows prompt before zsh finishes loading)
# - Transient prompt (compact history in scrollback)
# - Zsh-only environment with heavy customization needs
#
# Prerequisites: Zinit plugin manager (see 40-plugins.zsh)
# Config wizard: p10k configure
# Config file: $ZDOTDIR/.p10k.zsh
# Uncomment the block below to enable
# -----------------------------------------------------------------------------
# elif [[ -n "$ZINIT_HOME" ]]; then
#   zinit ice depth=1
#   zinit light romkatv/powerlevel10k
#   if [[ -f "${ZDOTDIR:-$HOME}/.p10k.zsh" ]]; then
#     source "${ZDOTDIR:-$HOME}/.p10k.zsh"
#   fi

else
  # ---------------------------------------------------------------------------
  # Fallback: Native vcs_info Prompt
  # ---------------------------------------------------------------------------
  # Built-in Zsh prompt with git branch display. Used when no external
  # prompt tools are installed. No configuration needed.
  # ---------------------------------------------------------------------------
  autoload -Uz vcs_info
  precmd() { vcs_info }

  zstyle ':vcs_info:git:*' formats ' %F{green}(%b)%f'
  zstyle ':vcs_info:*' enable git

  setopt PROMPT_SUBST
  PROMPT='%F{blue}%~%f${vcs_info_msg_0_} %F{yellow}>%f '
  RPROMPT='%F{240}%*%f'
fi
