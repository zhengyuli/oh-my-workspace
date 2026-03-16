# 20-aliases.zsh
# =============================================================================
# Command Aliases
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 20 (after history config, before completion)
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
  alias ls='ls --color=auto'
  alias la='ls -A'
  alias ll='ls -lAh'
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
# Navigation
# -----------------------------------------------------------------------------
alias ..='cd ..'
alias ...='cd ../..'
alias -- -='cd -'

# -----------------------------------------------------------------------------
# Editor & Viewing
# -----------------------------------------------------------------------------
alias vim='nvim'

if command -v bat &>/dev/null; then
  alias cat='bat --style=plain --paging=never'
  alias cats='bat --style=numbers,changes'
fi

# -----------------------------------------------------------------------------
# System
# -----------------------------------------------------------------------------
alias df='df -h'
alias du='du -sh'

# -----------------------------------------------------------------------------
# Zsh Config
# -----------------------------------------------------------------------------
alias zrc='$EDITOR $ZDOTDIR/.zshrc'
alias reload='source $ZDOTDIR/.zshrc && echo "reloaded"'

# -----------------------------------------------------------------------------
# Git
# -----------------------------------------------------------------------------
alias g='git'
alias gst='git status -sb'
alias ga='git add'
alias gaa='git add -A'
alias gc='git commit'
alias gcm='git commit -m'
alias gd='git diff'
alias gds='git diff --staged'
alias gl='git log --oneline --graph --decorate'
alias gp='git push'
alias gpl='git pull'
alias gsw='git switch'
alias gswc='git switch -c'

# -----------------------------------------------------------------------------
# Utility
# -----------------------------------------------------------------------------
alias path='printf "%s\n" "${(s/:/)PATH}"'
alias tmp='cd "$(mktemp -d)"'
