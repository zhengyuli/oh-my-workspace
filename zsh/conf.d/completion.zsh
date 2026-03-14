# ==============================================================================
# File: conf.d/completion.zsh
# Role: Completion system configuration (styles and options)
#
# Load context : Sourced by .zshrc BEFORE compinit
# Dependencies : None
# Side effects : Configures completion behavior
#
# NOTE: This file only configures completion options and styles.
# compinit is called in zshrc.symlink AFTER sourcing this file.
# ==============================================================================

# ── Completion Options ─────────────────────────────────────────────────────

setopt ALWAYS_TO_END       # Move cursor to end after completion
setopt AUTO_MENU           # Show menu on multiple completions
setopt AUTO_LIST           # List choices on ambiguous completion
setopt AUTO_PARAM_SLASH    # Add trailing slash to directories
setopt COMPLETE_IN_WORD    # Complete from cursor position
setopt MENU_COMPLETE       # Insert first match immediately

# ── Completion Styles ─────────────────────────────────────────────────────

# Case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'

# Enable menu selection
zstyle ':completion:*' menu select

# Enable caching for slow completions
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompcache"

# Group completions by category
zstyle ':completion:*' group-name ''

# Verbose completion descriptions
zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format '%F{purple}-- %d --%f'
zstyle ':completion:*:warnings' format '%F{red}-- no matches found --%f'
zstyle ':completion:*:corrections' format '%F{green}-- %d (errors: %e) --%f'

# Colors for file completions
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# Ignore current directory in cd completion
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# Process completion with colors
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=36=31'

# SSH/SCP/RSYNC hosts from known_hosts and ssh_config
zstyle ':completion:*:(ssh|scp|rsync):*' hosts off

# Function to safely extract SSH hosts
_ssh_hosts() {
  local -a hosts=()

  # Parse known_hosts if exists
  if [[ -f ~/.ssh/known_hosts ]]; then
    hosts+=(${${${(f)"$(<~/.ssh/known_hosts 2>/dev/null)"}:#[\[]*}%% *})
  fi

  # Parse ssh_config if exists
  if [[ -f ~/.ssh/config ]]; then
    hosts+=(${${${(M)${(f)"$(<~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**})
  fi

  # Return unique hosts filtered to valid hostname characters only
  # Prevents potential injection from malformed host entries
  # Use 'command grep' to bypass potential rg alias
  echo "${(u)hosts}" | tr ' ' '\n' | command grep -E '^[a-zA-Z0-9._-]+$' | tr '\n' ' '
}

zstyle -e ':completion:*:(ssh|scp|rsync):*:hosts' hosts 'reply=($(_ssh_hosts))'
