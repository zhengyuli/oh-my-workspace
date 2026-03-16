# 40-plugins.zsh
# =============================================================================
# Zinit Plugin Management
#
# Loaded by: Interactive shells (.zshrc)
# Load order: 40 (after 30-completion.zsh, before 50-prompt.zsh)
#
# Responsibilities:
#   1. Configure plugin-specific environment variables (FZF, etc.)
#   2. Bootstrap and configure Zinit plugin manager
#   3. Load core plugins (syntax highlighting, autosuggestions, etc.)
#   4. Load utility plugins (z, autopair)
#   5. Configure plugin-specific settings
#
# Do NOT add: Tool initialization (pyenv, fnm, direnv, etc.)
#             → Put these in 70-tools.zsh (separate concern)
# =============================================================================

# -----------------------------------------------------------------------------
# Zinit bootstrap
# Data directory follows XDG: $XDG_DATA_HOME/zinit/
# -----------------------------------------------------------------------------
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

# Guard against missing file even after clone (e.g. disk full)
[[ -f "$ZINIT_HOME/zinit.zsh" ]] || return 1
source "$ZINIT_HOME/zinit.zsh"
ZINIT_INITIALIZED=1

# -----------------------------------------------------------------------------
# Core plugins -- turbo mode (wait: load after prompt appears; lucid: no output)
# -----------------------------------------------------------------------------

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

# fzf-tab -- replace default Tab completion menu with fzf
# Load synchronously (no wait) to register widget immediately after compinit.
# Async loading causes widget registration timing issues with Tab key.
# fzf completion.zsh moved to 70-tools.zsh to avoid widget conflicts.
zinit ice
zinit light Aloxaf/fzf-tab

# Additional completions
# blockf: prevent compinit from rebuilding completion files on every load
# (zsh-completions provides many extra completions that would slow down rebuild)
zinit ice wait lucid blockf
zinit light zsh-users/zsh-completions

# -----------------------------------------------------------------------------
# Utility plugins
# -----------------------------------------------------------------------------

# autopair -- auto-close brackets and quotes
zinit ice wait lucid
zinit light hlissner/zsh-autopair

# -----------------------------------------------------------------------------
# Plugin configuration
# -----------------------------------------------------------------------------

# zsh-autosuggestions
ZSH_AUTOSUGGEST_STRATEGY=(history completion)   # history first, then completion
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20              # no suggestion for long buffers
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#7f8c98'    # Doom Xcode comment color

# zsh-history-substring-search (key bindings set via atload above)
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=green,fg=black,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=white,bold'
# HISTORY_SUBSTRING_SEARCH_FUZZY=1              # disabled: perf cost on large histories

# fzf-tab preview and style (Doom One theme)
# Disable default menu
zstyle ':completion:*' menu no

# Group descriptions
zstyle ':completion:*:descriptions' format '[%d]'

# File colors
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Git branch sorting
zstyle ':completion:*:git-checkout:*' sort false

# --- fzf-tab Preview Configurations ---

# cd: preview directory
zstyle ':fzf-tab:complete:cd:*' fzf-preview \
  'eza -1 --color=always --icons $realpath'

# Files: preview content — scoped to argument completion only to avoid
# triggering on command names, users, hostnames, etc.
zstyle ':fzf-tab:complete:*:argument*' fzf-preview \
  'bat --color=always --style=plain --line-range=:50 $realpath 2>/dev/null \
   || eza -1 --color=always --icons $realpath 2>/dev/null'

# Environment variables: preview value
zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset):*' \
  fzf-preview 'echo ${(P)word}'

# kill: preview process info
# Use BSD-compatible -p flag (macOS ps does not support --pid)
zstyle ':completion:*:*:*:*:processes' \
  command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  '[[ $group == "[process ID]" ]] && ps -p $word -o comm= 2>/dev/null'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' \
  fzf-flags --preview-window=down:3:wrap

# brew: preview package info
zstyle ':fzf-tab:complete:brew-(install|uninstall|search|info):*' fzf-preview \
  'brew info $word'

# man: preview man page
zstyle ':fzf-tab:complete:(\\|*/|)man:*' fzf-preview \
  'man $word 2>/dev/null | head -50'

# git: preview diff/log
zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview \
  'git diff --color=always $word'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview \
  'git log --oneline --color=always $word'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
  'git log --oneline --color=always $word 2>/dev/null \
   || git show --color=always $word'

# Switch groups with , and .
zstyle ':fzf-tab:*' switch-group ',' '.'
