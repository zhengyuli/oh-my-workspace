# 30-completion.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-20 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Completion System
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 30 (after 05-path.zsh sets fpath, before 40-plugins.zsh)
#
# Responsibilities:
#   1. Initialize zsh completion system (compinit)
#   2. Configure completion styles (zstyle)
#   3. Load completion modules (complist)
#   4. Set up completion caching for performance
#
# Prerequisites:
#   - 05-path.zsh must set fpath with completion directories
#   - XDG_CACHE_HOME must be defined (00-env.zsh)
#
# Do NOT add: custom completion functions, plugin completions, tool init
#             → Custom completion functions in completions/ directory
#             → Plugin completions in 40-plugins.zsh
#             → Tool-specific initialization in 70-tools.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Completion Modules
# -----------------------------------------------------------------------------

# Must load complist before compinit for menu-select to work
# -i: suppress error if module already loaded
zmodload -i zsh/complist

# -----------------------------------------------------------------------------
# Compinit
# -----------------------------------------------------------------------------

# Stores zcompdump in XDG cache directory
autoload -Uz compinit

_zcompdump="$XDG_CACHE_HOME/zsh/zcompdump"

# Rebuild zcompdump if missing or older than threshold; use -C otherwise.
# Uses zsh glob qualifiers (no subshells, no external commands, portable):
#   N  = nullglob (empty array instead of error when no match)
#   .  = regular file (not a directory or symlink)
#   mh-N = modification time < N hours ago  (i.e., the dump is still fresh)
# Override: set COMPDUMP_MAX_AGE_HOURS in 00-env.zsh (before this file loads).
if (( ! ${+COMPDUMP_MAX_AGE_HOURS} )); then
  readonly COMPDUMP_MAX_AGE_HOURS=20
fi
_zcompdump_fresh=( ${_zcompdump}(N.mh-${COMPDUMP_MAX_AGE_HOURS}) )
if (( ${#_zcompdump_fresh} )); then
  # fresh: skip security audit and dump rebuild for speed.
  # Tradeoff: insecure fpath directories are only detected on full rebuild
  # (once per COMPDUMP_MAX_AGE_HOURS). Acceptable for single-user machines.
  compinit -C -d "$_zcompdump"
else
  # stale/missing: full rebuild
  compinit -d "$_zcompdump"
fi

# Compile dump to bytecode for faster subsequent loads.
# .zwc is loaded automatically when newer than the source dump.
# Atomic lock via mkdir prevents concurrent zcompile corruption
# (ohmyzsh/ohmyzsh#11341).
if [[ -s "$_zcompdump" && ( ! -s "${_zcompdump}.zwc" \
  || "$_zcompdump" -nt "${_zcompdump}.zwc" ) ]]; then
  if mkdir "${_zcompdump}.lock" 2>/dev/null; then
    zcompile "$_zcompdump"
    rm -rf "${_zcompdump}.lock"
  fi
fi
unset _zcompdump _zcompdump_fresh

# -----------------------------------------------------------------------------
# Completion Styles
# -----------------------------------------------------------------------------

# Completion menu is managed entirely by fzf-tab (40-plugins.zsh sets menu no).
# Do NOT set 'menu select' here - fzf-tab overrides it and the dead config
# creates confusion about which system is in control.

# Matcher list: zsh tries each pattern in order until one matches
#   1. m:{a-z}={A-Za-z} - smart case: lowercase matches both cases
#                         (fo → Foo, FO, foo; but FO does NOT match foo).
#                         Chosen for UX consistency with ripgrep/fzf
#                         smart-case. Additionally avoids zsh 5.9
#                         cfp_matcher_range() bug (Debian #1013434).
#   2. r:|[._-]=* r:|=* - partial-word after separators (f_b → foo_bar)
#   3. l:|=* r:|=* - substring anywhere (bar → foo_bar, oob → foo_bar)
zstyle ':completion:*' matcher-list \
  'm:{a-z}={A-Za-z}' \
  'r:|[._-]=* r:|=*' \
  'l:|=* r:|=*'

# Colored completion list (uses LS_COLORS)
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# Group header format
zstyle ':completion:*:warnings' format '%F{red}-- no matches for: %d --%f'
zstyle ':completion:*:messages' format '%F{purple}-- %d --%f'

# Display completions in named groups
zstyle ':completion:*' group-name ''

# Ignore patterns - reduce noise (object files, bytecode, backups, logs, deps)
zstyle ':completion:*' ignored-patterns \
  '*?.o' '*?.pyc' '*?.class' '*?~' '*.log' '*.tmp' 'node_modules'

# Show processes from all users in process completion
# NOTE: The 'command' style for processes is set in 40-plugins.zsh for fzf-tab
zstyle ':completion:*:processes-names' command 'ps -e -o comm='

# kill completion
# Regex: (#b) backreferences; captures PID ($match[1])
#   =0=01;31 means: unmatched parts=normal, PID group=bold red
# Canonical pattern from zsh Guide (Peter Stephenson, Chapter 6).
zstyle ':completion:*:*:kill:*:processes' list-colors \
  '=(#b) #([0-9]#)*=0=01;31'
# Fallback for when fzf-tab is disabled: use built-in menu selection for kill.
# When fzf-tab is active, these are dead code (fzf-tab intercepts completions
# before zsh's menu system is invoked; see 40-plugins.zsh kill fzf-preview).
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
# insert-ids controls PID insertion behavior (not display), remains effective.
zstyle ':completion:*:*:kill:*' insert-ids single

# ssh / scp / rsync host completion (from known_hosts)
zstyle ':completion:*:(ssh|scp|rsync):*' tag-order \
  'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(ssh|scp|rsync):*' group-order \
  users hosts-domain hosts-host hosts-ipaddr

# List directories before files
zstyle ':completion:*' list-dirs-first true

# Collapse consecutive slashes in path completion (foo//bar → foo/bar)
zstyle ':completion:*' squeeze-slashes true

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

# Auto-detect newly installed commands (brew install, cargo install, etc.)
# without manual `rehash`. Minimal perf cost on local filesystems.
zstyle ':completion:*' rehash true
