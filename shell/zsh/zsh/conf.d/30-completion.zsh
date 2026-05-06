# 30-completion.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-20 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Completion System - compinit, zstyle, and caching
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Copyright (C) 2026 zhengyu li
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 30 (after 05-path.zsh sets fpath, before 40-plugins.zsh)
#
# Prerequisites:
#   - 05-path.zsh (fpath with completion directories)
#
# Do NOT add: custom completion functions, tool init
#             → Custom functions in completions/
#             → Tool init in 70-tools.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Completion Modules
# -----------------------------------------------------------------------------

# Must load complist before compinit for menu-select to work.
# -i: no-op for load form (module is silently skipped if already loaded).
zmodload -i zsh/complist

# -----------------------------------------------------------------------------
# Compinit
# -----------------------------------------------------------------------------

# Stores zcompdump in XDG cache directory
autoload -Uz compinit

_zcompdump="$XDG_CACHE_HOME/zsh/zcompdump"

# Rebuild zcompdump if missing or older than threshold; use -C otherwise.
# Glob qualifiers: N=nullglob, .=regular file, mh-N=modified < N hours ago
# Override: set COMPDUMP_MAX_AGE_HOURS in 00-env.zsh (before this file loads).
if (( ! ${+COMPDUMP_MAX_AGE_HOURS} )); then
  readonly COMPDUMP_MAX_AGE_HOURS=20
fi
_zcompdump_fresh=( ${_zcompdump}(N.mh-${COMPDUMP_MAX_AGE_HOURS}) )
if (( ${#_zcompdump_fresh} )); then
  # fresh: skip security audit for speed
  compinit -C -d "$_zcompdump"
else
  # stale/missing: full rebuild
  compinit -d "$_zcompdump"
fi

# Compile dump to bytecode for faster subsequent loads.
# Atomic lock via mkdir prevents concurrent zcompile corruption
# (ohmyzsh#11341).
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
# Do NOT set 'menu select' here — fzf-tab overrides it.

# --- Matcher List ---
# Matcher list: zsh tries each pattern in order until one matches
#   1. m:{a-z}={A-Za-z} — smart case (avoids zsh 5.9 cfp_matcher_range bug)
#   2. r:|[._-]=* r:|=* — partial-word after separators
#   3. l:|=* r:|=* — substring anywhere
zstyle ':completion:*' matcher-list \
  'm:{a-z}={A-Za-z}' \
  'r:|[._-]=* r:|=*' \
  'l:|=* r:|=*'

# --- List Colors ---
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# --- Display Format ---
zstyle ':completion:*:warnings' format '%F{red}-- no matches for: %d --%f'
zstyle ':completion:*:messages' format '%F{purple}-- %d --%f'
zstyle ':completion:*' group-name ''

# --- Filtering ---
zstyle ':completion:*' ignored-patterns '*?.o' '*?.pyc' '*?.class' '*?~' '*.log' '*.tmp' 'node_modules'

# --- Process Completion ---
# NOTE: The 'command' style for processes is set in 40-plugins.zsh for fzf-tab
zstyle ':completion:*:processes-names' command 'ps -e -o comm='

# --- Kill Completion ---
# Kill completion — canonical list-colors pattern from zsh Guide (Ch. 6)
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:*:kill:*' force-list always
# Insert-ids controls PID insertion behavior (not display)
zstyle ':completion:*:*:kill:*' insert-ids single

# --- SSH / SCP / RSYNC ---
# SSH / SCP / RSYNC host completion (from known_hosts)
zstyle ':completion:*:(ssh|scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(ssh|scp|rsync):*' group-order users hosts-domain hosts-host hosts-ipaddr

# --- Path Completion ---
zstyle ':completion:*' list-dirs-first true
zstyle ':completion:*' squeeze-slashes true

# --- Cache ---
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/completion-cache"

# --- Completer Order ---
# Completer order: _approximate omitted — fzf-tab provides superior fuzzy
# matching; including both causes double-fuzzy degradation.
# _ignored: retry completion with normally-ignored patterns (e.g., hidden
# files excluded by ignored-patterns) when no match is found.
zstyle ':completion:*' completer _extensions _complete _ignored

# --- CD Completion ---
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# --- Rehash ---
# Auto-detect newly installed commands (brew install, cargo install, etc.)
# without manual `rehash`. Minimal perf cost on local filesystems.
zstyle ':completion:*' rehash true
