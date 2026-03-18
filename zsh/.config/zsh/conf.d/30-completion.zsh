# 30-completion.zsh
# Time-stamp: <2026-03-18 00:00:00 Wednesday by zhengyu.li>
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
# Do NOT add: custom completion functions, plugin completions, tool initialization
#             → Custom completion functions in completions/ directory
#             → Plugin completions in 40-plugins.zsh
#             → Tool-specific initialization in 70-tools.zsh
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

# Rebuild zcompdump if missing or older than 20 hours; use -C otherwise (fast).
# Uses zsh glob qualifiers — no subshells, no external commands, cross-platform:
#   N  = nullglob (empty array instead of error when no match)
#   .  = regular file (not a directory or symlink)
#   mh-20 = modification time < 20 hours ago  (i.e., the dump is still fresh)
_zcompdump_fresh=( ${_zcompdump}(N.mh-20) )
if (( ${#_zcompdump_fresh} )); then
  # fresh: skip security check for speed
  compinit -C -d "$_zcompdump"
else
  # stale/missing: full rebuild
  compinit -d "$_zcompdump"
fi
unset _zcompdump _zcompdump_fresh

# -----------------------------------------------------------------------------
# Completion styles
# -----------------------------------------------------------------------------

# Use menu selection (arrow keys / Tab navigate candidates)
# NOTE: This is overridden by fzf-tab in 40-plugins.zsh which sets 'menu no'
# to allow fzf-tab to handle the completion UI instead of zsh's built-in menu.
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
zstyle ':completion:*:warnings'  format '%F{red}-- no matches for: %d --%f'
zstyle ':completion:*:messages'  format '%F{purple}-- %d --%f'
# Note: ':completion:*:corrections' is intentionally omitted — it only
# triggers with _approximate, which is not in the completer list here.

# Display completions in named groups
zstyle ':completion:*' group-name ''

# Ignore patterns - reduce noise (object files, bytecode, backups, logs, temp files, deps)
zstyle ':completion:*' ignored-patterns '*?.o' '*?.pyc' '*?.class' '*?~' '*.log' '*.tmp' 'node_modules'

# Show processes from all users in process completion
# NOTE: The 'command' style for processes is set in 40-plugins.zsh for fzf-tab preview
zstyle ':completion:*:processes-names' command 'ps -e -o comm='

# kill completion
# Regex breakdown: (#b) enables backreferences, captures PID ($word[1]) and command ($word[2])
#   =01;34=0=01 means: match=bold blue, first group=default, second group=bold
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

# Completer order: try exact extension match first, then standard completion.
# _approximate is intentionally omitted: fzf-tab's fzf UI provides superior
# fuzzy matching. Including _approximate alongside fzf causes double-fuzzy
# behaviour (approximate expands candidates that fzf then re-fuzzy-matches),
# degrading performance and producing confusing results.
zstyle ':completion:*' completer _extensions _complete

# Do not offer current directory when completing cd ../
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# -----------------------------------------------------------------------------
# Additional completion helpers
# -----------------------------------------------------------------------------

# bashcompinit: allows use of bash-style complete commands
autoload -Uz bashcompinit
bashcompinit
