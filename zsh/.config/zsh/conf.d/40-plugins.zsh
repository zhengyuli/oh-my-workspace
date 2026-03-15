# 40-plugins.zsh
# =============================================================================
# Zinit Plugin Management
#
# Loaded by: Interactive shells (.zshrc)
# Load order: After 30-completion.zsh, before 50-prompt.zsh
#
# Responsibilities:
#   1. Bootstrap and configure Zinit plugin manager
#   2. Load core plugins (syntax highlighting, autosuggestions, etc.)
#   3. Load utility plugins (z, autopair)
#   4. Configure plugin-specific settings
#
# Do NOT add: Tool initialization (pyenv, fnm, direnv, etc.)
#             → Put these in 70-tools.zsh (separate concern)
# =============================================================================

# -----------------------------------------------------------------------------
# Zinit bootstrap
# Data directory follows XDG: $XDG_DATA_HOME/zinit/
# ----------------------------------------------------------------------------
# Idempotency guard - prevent double loading
(( ${+ZINIT_INITIALIZED} )) && return 0

ZINIT_HOME="$XDG_DATA_HOME/zinit/zinit.git"

if [[ ! -f "$ZINIT_HOME/zinit.zsh" ]]; then
  print -P "%F{cyan}Installing Zinit...%f"
  if mkdir -p "$(dirname "$ZINIT_HOME")"; then
    if ! git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"; then
      print -P "%F{red}Failed to clone Zinit%f"
      return 1
    fi
  else
    print -P "%F{red}Failed to create Zinit directory%f"
    return 1
  fi
fi

source "$ZINIT_HOME/zinit.zsh"
ZINIT_INITIALIZED=1

# -----------------------------------------------------------------------------
# Zinit annexes (extend Zinit functionality; recommended to keep)
# ----------------------------------------------------------------------------
zinit light-mode for \
  zdharma-continuum/zinit-annex-as-monitor \
  zdharma-continuum/zinit-annex-bin-gem-node \
  zdharma-continuum/zinit-annex-patch-dl \
  zdharma-continuum/zinit-annex-rust

# -----------------------------------------------------------------------------
# Core plugins -- turbo mode (wait: load after prompt appears; lucid: no output)
# ----------------------------------------------------------------------------

# Syntax highlighting -- must load before autosuggestions
zinit ice wait lucid atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay"
zinit light zdharma-continuum/fast-syntax-highlighting

# History substring search (enhanced Ctrl-R)
# Keybindings set here via atload to avoid "unhandled ZLE widget" errors
# (widgets must exist before bindkey can reference them)
zinit ice wait lucid atload'
  bindkey "^[[A" history-substring-search-up
  bindkey "^[[B" history-substring-search-down
  bindkey "^[OA" history-substring-search-up
  bindkey "^[OB" history-substring-search-down
'
zinit light zsh-users/zsh-history-substring-search

# Autosuggestions from history
zinit ice wait lucid atload"_zsh_autosuggest_start"
zinit light zsh-users/zsh-autosuggestions

# fzf-tab -- replace default Tab completion menu with fzf (load after compinit)
zinit ice wait lucid
zinit light Aloxaf/fzf-tab

# Additional completions
zinit ice wait lucid blockf
zinit light zsh-users/zsh-completions

# -----------------------------------------------------------------------------
# Utility plugins
# ----------------------------------------------------------------------------

# autopair -- auto-close brackets and quotes
zinit ice wait lucid
zinit light hlissner/zsh-autopair

# -----------------------------------------------------------------------------
# Plugin configuration
# ----------------------------------------------------------------------------

# zsh-autosuggestions
ZSH_AUTOSUGGEST_STRATEGY=(history completion)   # history first, then completion
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20              # no suggestion for long buffers
ZSH_AUTOSUGGEST_USE_ASYNC=1                     # fetch suggestions asynchronously
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#6c7086'    # suggestion text color (Catppuccin overlay0)

# zsh-history-substring-search (key bindings set via atload above)
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=green,fg=black,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=white,bold'
HISTORY_SUBSTRING_SEARCH_FUZZY=1

# fzf-tab preview and style
zstyle ':fzf-tab:complete:cd:*'     fzf-preview 'eza -1 --icons --color=always $realpath 2>/dev/null || ls -1 $realpath'
zstyle ':fzf-tab:complete:*:*'      fzf-preview 'bat --style=numbers --color=always --line-range :50 $realpath 2>/dev/null || cat $realpath 2>/dev/null || echo $realpath'
zstyle ':fzf-tab:*'                 fzf-flags --color=fg:#cdd6f4,bg:#1e1e2e,hl:#89b4fa
zstyle ':fzf-tab:*'                 switch-group ',' '.'   # , . to switch completion groups
