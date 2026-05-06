# 00-env.zsh -*- mode: sh; -*-
# Time-stamp: <2026-04-02 12:01:02 Thursday by zhengyu.li>
# =============================================================================
# Core Environment Variables - Editor, pager, locale, tool paths
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Copyright (C) 2026 zhengyu li
#
# Loaded by: .zprofile (login), .zshrc (interactive)
# Load order: 00 (first conf.d module)
#
# Prerequisites:
#   - XDG_* variables must be set in ~/.zshenv (loaded before this file)
#
# Do NOT add: PATH, aliases, functions, tool init
#             → PATH in 05-path.zsh
#             → Aliases in 20-aliases.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Editor / Pager
# -----------------------------------------------------------------------------

export EDITOR=nvim
export VISUAL=nvim
export PAGER=less

# -----------------------------------------------------------------------------
# Locale
# -----------------------------------------------------------------------------

# Use LANG only; avoid LC_ALL as it overrides all LC_* settings.
export LANG=en_US.UTF-8

# -----------------------------------------------------------------------------
# File Permissions
# -----------------------------------------------------------------------------

# Ensure predictable permissions regardless of system default.
# Files: rw-r--r-- (644)  Directories: rwxr-xr-x (755)
umask 022

# -----------------------------------------------------------------------------
# Zsh History
# -----------------------------------------------------------------------------

# History is STATE (persists, cannot be regenerated) → XDG_STATE_HOME.
# Do NOT use XDG_CACHE_HOME: cache directories are cleared by OS maintenance
# tools and "free disk space" scripts, which would silently destroy history.
export HISTFILE="$XDG_STATE_HOME/zsh/history"

# Ensure required zsh directories exist on first run (idempotent).
#   XDG_STATE_HOME/zsh  - history (state: persistent, not regeneratable)
#   XDG_CACHE_HOME/zsh  - zcompdump, completion caches (regeneratable)
# These are distinct XDG categories; do NOT consolidate them.
if [[ ! -d "$XDG_STATE_HOME/zsh" ]]; then
  mkdir -p "$XDG_STATE_HOME/zsh"
fi
if [[ ! -d "$XDG_CACHE_HOME/zsh" ]]; then
  mkdir -p "$XDG_CACHE_HOME/zsh"
fi

# -----------------------------------------------------------------------------
# Build And Development Tools
# -----------------------------------------------------------------------------

# Redirect tool data from ~/.toolname to XDG directories.
# See: https://wiki.archlinux.org/title/XDG_Base_Directory

export GOPATH="${GOPATH:-$XDG_DATA_HOME/go}"

# Bun -- https://github.com/oven-sh/bun/issues/1678
export BUN_INSTALL="${BUN_INSTALL:-$XDG_DATA_HOME/bun}"

export CARGO_HOME="${CARGO_HOME:-$XDG_DATA_HOME/cargo}"
export RUSTUP_HOME="${RUSTUP_HOME:-$XDG_DATA_HOME/rustup}"

# -----------------------------------------------------------------------------
# Editor Tools
# -----------------------------------------------------------------------------

# Vim -- no native XDG support, must redirect via VIMINIT
export VIMINIT="set nocp | execute 'source' fnameescape('${XDG_CONFIG_HOME}/vim/vimrc')"

# -----------------------------------------------------------------------------
# CLI Tools
# -----------------------------------------------------------------------------

# Ripgrep -- no auto-discovery; requires explicit config path via env var
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/rc"

# Needs to be set before carapace initializes (70-tools.zsh)
export CARAPACE_BRIDGES='zsh,fish,bash'

# STARSHIP_CONFIG: no auto-discovery; must point explicitly to config file
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship.toml"
export STARSHIP_CACHE="$XDG_CACHE_HOME/starship"

# -----------------------------------------------------------------------------
# Less Configuration
# -----------------------------------------------------------------------------

# -R (color passthrough) -F (auto-quit) -X (no termcap clear)
# -i (case-insensitive search) -M (verbose prompt) + custom colors
export LESS='-R -F -X -i -M --use-color -Dd+r$Du+b'

# -----------------------------------------------------------------------------
# Homebrew
# -----------------------------------------------------------------------------

# Pure environment variables; needed even in non-interactive shells.
# HOMEBREW_PREFIX may already be set by Homebrew's shellenv; check before
# overriding.
if [[ -z "$HOMEBREW_PREFIX" ]]; then
  # arm64 (Apple Silicon): /opt/homebrew
  # x86_64 (Intel): /usr/local
  for _brew_prefix in /opt/homebrew /usr/local; do
    if [[ -x "$_brew_prefix/bin/brew" ]]; then
      export HOMEBREW_PREFIX="$_brew_prefix"
      break
    fi
  done
  unset _brew_prefix
fi
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_AUTO_UPDATE=1

# -----------------------------------------------------------------------------
# LS_COLORS
# -----------------------------------------------------------------------------

# macOS BSD ls uses LSCOLORS (different format); LS_COLORS is the GNU/XDG
# format used by zsh completion (list-colors in 30-completion.zsh) and eza.
# gdircolors is provided by coreutils (brew install coreutils, in Brewfile).
if command -v gdircolors &>/dev/null; then
  _gdircolors_cache="$XDG_CACHE_HOME/zsh/gdircolors.zsh"
  if [[ ! -f "$_gdircolors_cache" ]] || \
     [[ "$(command -v gdircolors)" -nt "$_gdircolors_cache" ]]; then
    mkdir -p "${_gdircolors_cache:h}"
    gdircolors -b >! "$_gdircolors_cache"
  fi
  source "$_gdircolors_cache"
  unset _gdircolors_cache
else
  # Minimal fallback: dir=bold blue, symlink=bold cyan, exec=bold green
  export LS_COLORS='di=1;34:ln=1;36:ex=1;32:fi=0:mi=0;31'
fi

# -----------------------------------------------------------------------------
# FZF Environment
# -----------------------------------------------------------------------------

# Must be set before fzf-tab loads in 40-plugins.zsh.
export FZF_DEFAULT_OPTS="
  --color=bg:#292a30,bg+:#1f2024
  --color=fg:#dfdfe0,fg+:#dfdfe0
  --color=hl:#ff8170,hl+:#ff8170
  --color=info:#ffa14f,prompt:#ff8170
  --color=pointer:#ff8170,marker:#78c2b3
  --color=spinner:#78c2b3,header:#6bdfff
  --color=border:#3a3a3f,label:#dfdfe0
  --color=selected-bg:#2d4a6e
  --layout=reverse
  --border=rounded
  --height=50%
  --info=inline
  --preview-window=right:50%:wrap
"
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
# --type d: directories only, for Alt+C cd widget
export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'
