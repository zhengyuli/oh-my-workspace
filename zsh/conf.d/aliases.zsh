# ==============================================================================
# File: conf.d/aliases.zsh
# Role: Shell aliases for common commands
#
# Load context : Sourced by .zshrc after options.zsh
# Dependencies : None
# Side effects : Defines aliases in global namespace
# ==============================================================================

# ── grep Replacements ─────────────────────────────────────────────────────

if command -v rg &>/dev/null; then
  alias grep='rg'
  alias fgrep='rg -F'
else
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

# ── File Operations ─────────────────────────────────────────────────────

alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'
alias mkdir='mkdir -pv'

# ── Disk Usage ─────────────────────────────────────────────────────

alias du='du -h'
alias df='df -h'
alias ducks='du -sh * | sort -hr | head -10'

# ── Network ─────────────────────────────────────────────────────

alias myip='curl -s ifconfig.me'
alias localip='ipconfig getifaddr en0 2>/dev/null || ipconfig getifaddr en1'
alias ping='ping -c 5'
alias ports='lsof -i -P | grep LISTEN'

# ── Development ─────────────────────────────────────────────────────

# Python
alias python='python3'
alias pip='pip3'
alias venv='python3 -m venv'
alias serve='python3 -m http.server'

# ── macOS Specific ─────────────────────────────────────────────────────

if [[ "$(uname)" == "Darwin" ]]; then
  alias showhidden='defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder'
  alias hidehidden='defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder';
  alias showdesktop='defaults write com.apple.finder CreateDesktop -bool true && killall Finder';
  alias hidedesktop='defaults write com.apple.finder CreateDesktop -bool false && killall Finder';
  alias flushdns='sudo dscacheutil -flushcache && sudo killall -HUP mDNSResponder';
  alias airport='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport';
  alias brewup='brew update && brew upgrade && brew cleanup'
fi

# ── Safety ─────────────────────────────────────────────────────

# --preserve-root is GNU coreutils specific, not available on macOS BSD tools
if [[ "$(uname)" != "Darwin" ]]; then
  alias chown='chown --preserve-root'
  alias chmod='chmod --preserve-root'
  alias chgrp='chgrp --preserve-root'
fi

# ── Fun ─────────────────────────────────────────────────────

alias weather='curl wttr.in'
alias moon='curl wttr.in/Moon'
