# 20-aliases.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-28 22:10:58 Saturday by zhengyu.li>
# =============================================================================
# Command Aliases
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 20 (after 15-history.zsh, before 30-completion.zsh)
#
# Prerequisites: None (standalone configuration)
#
# Responsibilities:
#   1. Provide essential file/directory aliases
#   2. Provide git shortcuts for daily workflow
#   3. Provide zsh config shortcuts
#
# Do NOT add: functions, environment variables, rarely used aliases
#             → Put functions in functions/ directory (autoloaded)
#             → Put env vars in 00-env.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# File Listing (eza or fallback to ls)
# -----------------------------------------------------------------------------

if command -v eza &>/dev/null; then
  alias ls='eza --icons --group-directories-first'
  alias la='eza -a --icons --group-directories-first'
  alias ll='eza -la --icons --group-directories-first --git'
  alias lt='eza --tree --level=2 --icons'
else
  # macOS BSD ls uses -G for color; GNU ls (Linux) uses --color=auto
  if [[ "$OSTYPE" == darwin* ]]; then
    alias ls='ls -G'
    alias la='ls -AG'
    alias ll='ls -lAhG'
  else
    alias ls='ls --color=auto'
    alias la='ls -A --color=auto'
    alias ll='ls -lAh --color=auto'
  fi
  alias lt='tree -L 2'
fi

# -----------------------------------------------------------------------------
# File Operations (safe: interactive + verbose)
# -----------------------------------------------------------------------------

alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'
alias mkdir='mkdir -pv'

# -----------------------------------------------------------------------------
# Directory Navigation
# -----------------------------------------------------------------------------

alias ..='cd ..'
alias ...='cd ../..'
alias -- -='cd -'

# -----------------------------------------------------------------------------
# Editor
# -----------------------------------------------------------------------------

# Vim
alias v='nvim'
alias vi='nvim'
alias vim='nvim'
alias vimdiff='nvim -d'

# Emacs
alias e='emacs -nw'

# -----------------------------------------------------------------------------
# Utilities
# -----------------------------------------------------------------------------

alias path='printf "%s\n" "${path[@]}"'

if command -v bat &>/dev/null; then
  alias cat='bat --style=plain --paging=never'
  alias cats='bat --style=numbers,changes'
fi

alias reload-zsh='exec zsh -l'

alias brew-upgrade='brew update && brew upgrade && brew cleanup'
