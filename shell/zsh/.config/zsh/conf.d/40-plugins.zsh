# 40-plugins.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-28 15:46:57 Saturday by zhengyu.li>
# =============================================================================
# Zinit Plugin Management - Bootstrap, load, and configure plugins
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Copyright (C) 2026 zhengyu li
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 40 (after 30-completion.zsh, before 50-prompt.zsh)
#
# Do NOT add: Tool initialization (pyenv, fnm, etc.)
#             → Put these in 70-tools.zsh (separate concern)
# =============================================================================

# -----------------------------------------------------------------------------
# Zinit Bootstrap
# -----------------------------------------------------------------------------

# Data directory follows XDG: $XDG_DATA_HOME/zinit/
# Idempotency guard - prevent double loading
if (( ${+ZINIT_INITIALIZED} )); then
  return 0
fi

typeset -g ZINIT_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/zinit/zinit.git"

if [[ ! -f "$ZINIT_HOME/zinit.zsh" ]]; then
  print -P "%F{cyan}Installing Zinit...%f"
  if ! mkdir -p "$(dirname "$ZINIT_HOME")"; then
    print -P -u2 "%F{red}Failed to create Zinit directory%f"
    return 1
  fi
  if ! git clone \
    https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"; then
    print -P -u2 "%F{red}Failed to clone Zinit%f"
    return 1
  fi
fi

# Guard against missing file even after clone (e.g. disk full)
if [[ ! -f "$ZINIT_HOME/zinit.zsh" ]]; then
  return 1
fi

# Set compdump path BEFORE sourcing zinit so ALL internal compinit calls
# (zicompinit, automatic rebuilds, self-update) use the XDG cache path.
# Without this, zinit defaults to ${ZDOTDIR}/.zcompdump
# (~/.config/zsh/.zcompdump).
typeset -gA ZINIT
ZINIT[ZCOMPDUMP_PATH]="$XDG_CACHE_HOME/zsh/zcompdump"

source "$ZINIT_HOME/zinit.zsh"
typeset -g ZINIT_INITIALIZED=1

# -----------------------------------------------------------------------------
# Plugin Loading Order
# -----------------------------------------------------------------------------

# Sync: fzf-tab (owns ^I before turbo plugins can override)
# Turbo 0a: zsh-completions (register early for zicdreplay)
# Turbo 0b: history-substring-search, autosuggestions, autopair
# Turbo 0c: fast-syntax-highlighting (LAST — wraps all ZLE widgets)

# --- Sync: FZF Tab ---
# Must own ^I before any turbo plugin can override it.
zinit light Aloxaf/fzf-tab

# --- Turbo 0a: Completions ---
# Blockf: skip fpath insertion (avoids triggering compinit rebuild)
zinit ice wait"0a" lucid blockf
zinit light zsh-users/zsh-completions

# --- Turbo 0b: Interactive Enhancement Plugins ---
# History substring search — config vars set before ice/light for co-location
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=green,fg=black,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=white,bold'

# Disabled: perf cost on large histories
# HISTORY_SUBSTRING_SEARCH_FUZZY=1
# Keybindings via atload: widgets must exist before bindkey can reference them.
# terminfo keys + CSI/application-mode fallbacks for maximum compatibility.
zinit ice wait"0b" lucid atload'
  bindkey "$terminfo[kcuu1]" history-substring-search-up
  bindkey "$terminfo[kcud1]" history-substring-search-down
  bindkey "^[[A" history-substring-search-up
  bindkey "^[[B" history-substring-search-down
  bindkey "^[OA" history-substring-search-up
  bindkey "^[OB" history-substring-search-down
'
zinit light zsh-users/zsh-history-substring-search

# Autosuggestions from history
# Config vars set BEFORE ice/light so they're available at activation time.
# ! on atload: redraw prompt so first suggestion appears without keypress.
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#7f8c98'
zinit ice wait"0b" lucid atload"!_zsh_autosuggest_start"
zinit light zsh-users/zsh-autosuggestions

# Autopair -- auto-close brackets and quotes
zinit ice wait"0b" lucid
zinit light hlissner/zsh-autopair

# --- Turbo 0c: Fast Syntax Highlighting ---
# atinit: -C (cached dump), zicompinit (pick up new completions),
#   zicdreplay (replay compdef from turbo plugins)
zinit ice wait"0c" lucid atinit'\
  ZINIT[COMPINIT_OPTS]=-C;\
  zicompinit; zicdreplay'
zinit light zdharma-continuum/fast-syntax-highlighting

# -----------------------------------------------------------------------------
# Plugin Configuration
# -----------------------------------------------------------------------------

# FZF-tab owns the Tab UI — disable zsh's default menu.
# Do NOT use %F{...} in descriptions format (fzf-tab limitation).
zstyle ':completion:*' menu no
zstyle ':completion:*:descriptions' format '[%d]'

# Git branch sorting
zstyle ':completion:*:git-checkout:*' sort false

# --- FZF Tab Preview Configurations ---
# Variable quoting is intentional: double quotes expand at source time,
# single quotes (preview) expand at completion time. Do NOT add extra quoting.
#
# ${(U)word} normalizes to uppercase for case-insensitive regex matching.
# Sensitive keywords: TOKEN, KEY, SECRET, PASSWORD, API, CREDENTIAL, PRIVATE,
#   AUTH, DSN, CERT.
zstyle ':fzf-tab:*' use-fzf-default-opts yes

# Global preview window — per-command fzf-flags override where needed.
zstyle ':fzf-tab:*' fzf-flags --preview-window=right:55%:wrap

zstyle ':fzf-tab:complete:cd:*' fzf-preview \
  'eza -1 --color=always --icons "$realpath"'

# (scoped to argument completion only)
zstyle ':fzf-tab:complete:*:argument*' fzf-preview \
  'if ! bat --color=always --style=plain --line-range=:50'\
  ' "$realpath" 2>/dev/null; then eza -1 --color=always'\
  ' --icons "$realpath" 2>/dev/null; fi'

# (hide sensitive ones)
zstyle ':fzf-tab:complete:(-command-|-parameter-'\
'|-brace-parameter-|export|unset|expand):*' \
  fzf-preview \
  'if [[ "${(U)word}" =~ '\
  '(TOKEN|KEY|SECRET|PASSWORD|API|CREDENTIAL|PRIVATE|AUTH|DSN|CERT) ]]; then'\
  ' print "(hidden)"; else print -r -- "${(P)word}"; fi'

# Kill: preview process info (BSD-compatible ps flags for macOS)
zstyle ':completion:*:*:*:*:processes' \
  command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  'if [[ $group == "[process ID]" ]]; then'\
  ' ps -p $word -o comm= 2>/dev/null; fi'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' \
  fzf-flags --preview-window=down:3:wrap

zstyle ':fzf-tab:complete:brew-(install|uninstall|search|info):*' fzf-preview \
  'brew info $word 2>/dev/null'

zstyle ':fzf-tab:complete:(\\|*/|)man:*' fzf-preview \
  'MANPAGER=cat man $word 2>/dev/null | head -50'

zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview \
  'git diff --color=always $word'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview \
  'git log --oneline --color=always -20 $word'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
  'if ! git log --oneline --color=always $word'\
  ' 2>/dev/null; then git show --color=always'\
  ' $word 2>/dev/null; fi'

# Switch groups with , and .
zstyle ':fzf-tab:*' switch-group ',' '.'

# -----------------------------------------------------------------------------
# Direnv
# -----------------------------------------------------------------------------

# Must load before starship (50-prompt.zsh) so .envrc vars are visible
# to the prompt on first render.
if command -v direnv &>/dev/null; then
  # Suppress "direnv: loading..." / "direnv: export..." log clutter
  export DIRENV_LOG_FORMAT=""
  eval "$(direnv hook zsh)"
fi
