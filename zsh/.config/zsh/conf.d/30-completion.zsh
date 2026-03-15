# 30-completion.zsh
# =============================================================================
# Completion System
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 30 (after 05-path.zsh sets fpath, before 40-plugins.zsh)
#
# Responsibilities:
#   1. Initialize zsh completion system (compinit)
#   2. Configure completion styles (zstyle)
#   3. Load completion modules (complist, bashcompinit)
#   4. Set up completion caching for performance
#
# Prerequisites:
#   - 05-path.zsh must set fpath with completion directories
#   - XDG_CACHE_HOME must be defined (00-env.zsh)
#
# Do NOT add:
#   - Custom completion functions → completions/ directory
#   - Plugin completions → 40-plugins.zsh
#   - Tool-specific initialization → 70-tools.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Load completion modules
# Must load complist before compinit for menu-select to work
# -i: suppress error if module already loaded
# -----------------------------------------------------------------------------
zmodload -i zsh/complist

# -----------------------------------------------------------------------------
# compinit -- initialize the completion system
# Stores zcompdump in XDG cache directory
# -----------------------------------------------------------------------------
autoload -Uz compinit

_zcompdump="$XDG_CACHE_HOME/zsh/zcompdump"

# Rebuild zcompdump at most once per day; use -C (skip security check) otherwise
if [[ -f "$_zcompdump" && $(date +%j) == $(date -r "$_zcompdump" +%j 2>/dev/null) ]]; then
  compinit -C -d "$_zcompdump"
else
  compinit -d "$_zcompdump"
fi
unset _zcompdump

# -----------------------------------------------------------------------------
# Completion styles
# -----------------------------------------------------------------------------

# Use menu selection (arrow keys / Tab navigate candidates)
zstyle ':completion:*' menu select

# Matcher list: zsh tries each pattern in order until one matches
#   1. m:{a-z}={A-Za-z}  - case-insensitive (fo → Foo, FO, foo)
#   2. r:|[._-]=* r:|=*  - partial-word after separators (f_b → foo_bar, f-b → foo-bar)
#   3. l:|=* r:|=*       - substring anywhere (bar → foo_bar, oob → foo_bar)
zstyle ':completion:*' matcher-list \
  'm:{a-z}={A-Za-z}' \
  'r:|[._-]=* r:|=*' \
  'l:|=* r:|=*'

# Colored completion list (uses LS_COLORS)
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# Group header format
zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
zstyle ':completion:*:warnings'     format '%F{red}-- no matches for: %d --%f'
zstyle ':completion:*:messages'     format '%F{purple}-- %d --%f'
zstyle ':completion:*:corrections'  format '%F{green}-- %d (errors: %e) --%f'

# Display completions in named groups
zstyle ':completion:*' group-name ''

# Ignore patterns - reduce noise
zstyle ':completion:*' ignored-patterns '*?.o' '*?.pyc' '*?.class' '*?~'

# Show processes from all users in process completion
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:processes-names' command 'ps -e -o comm='

# kill completion
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# ssh / scp / rsync host completion (from known_hosts)
zstyle ':completion:*:(ssh|scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(ssh|scp|rsync):*' group-order users hosts-domain hosts-host users hosts-ipaddr

# List directories before files
zstyle ':completion:*' list-dirs-first true

# Completion cache (speeds up completion on large systems)
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/completion-cache"

# Fuzzy matching with up to 2 errors
zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*:approximate:*' max-errors 'reply=( $((($#PREFIX+$#SUFFIX)/3 )) numeric )'

# Do not offer current directory when completing cd ../
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# -----------------------------------------------------------------------------
# Additional completion helpers
# -----------------------------------------------------------------------------

# bashcompinit: allows use of bash-style complete commands
autoload -Uz bashcompinit && bashcompinit

# Load color variables (used by list-colors above)
autoload -Uz colors && colors
