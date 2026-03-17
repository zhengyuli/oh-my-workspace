# 00-env.zsh
# Time-stamp: <2026-03-17 20:36:14 Tuesday by zhengyu.li>
# =============================================================================
# Core Environment Variables
#
# Loaded by: .zprofile (login), .zshrc (interactive)
# Load order: 00 (first conf.d module)
#
# Prerequisites:
#   - XDG_* variables must be set in ~/.zshenv (loaded before this file)
#
# Responsibilities:
#   1. Set editor and pager preferences (EDITOR, VISUAL, PAGER, LESS)
#   2. Configure locale settings (LANG)
#   3. Set Zsh history file location (HISTFILE)
#   4. Define XDG-compliant paths for developer tools
#
# Do NOT add: PATH changes, aliases, functions, tool initialization
#             → PATH changes in 05-path.zsh (centralized path management)
#             → Aliases in 20-aliases.zsh (interactive only)
#             → Functions in functions/ directory (autoloaded)
#             → Tool initialization in 70-tools.zsh (lazy-loaded)
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
# Use LANG only; avoid LC_ALL as it overrides all LC_* settings indiscriminately.
# Set specific LC_* variables if needed (e.g., LC_TIME for date format).
export LANG=en_US.UTF-8

# -----------------------------------------------------------------------------
# Zsh History
# -----------------------------------------------------------------------------
# History is STATE (persists, cannot be regenerated) → XDG_STATE_HOME.
# Do NOT use XDG_CACHE_HOME: cache directories are cleared by OS maintenance
# tools and "free disk space" scripts, which would silently destroy history.
export HISTFILE="$XDG_STATE_HOME/zsh/history"

# Ensure required zsh directories exist on first run (idempotent).
#   XDG_STATE_HOME/zsh  — history (state: persistent, not regeneratable)
#   XDG_CACHE_HOME/zsh  — zcompdump, completion caches (regeneratable)
# These are distinct XDG categories; do NOT consolidate them.
if [[ ! -d "$XDG_STATE_HOME/zsh" ]]; then
  mkdir -p "$XDG_STATE_HOME/zsh"
fi
if [[ ! -d "$XDG_CACHE_HOME/zsh" ]]; then
  mkdir -p "$XDG_CACHE_HOME/zsh"
fi

# -----------------------------------------------------------------------------
# XDG Redirections -- Build and Development Tools
# -----------------------------------------------------------------------------
# Redirect tool data from ~/.toolname to XDG directories.
# See: https://wiki.archlinux.org/title/XDG_Base_Directory

# Go
export GOPATH="${GOPATH:-$XDG_DATA_HOME/go}"

# Bun (JS/TS runtime) -- https://github.com/oven-sh/bun/issues/1678
export BUN_INSTALL="${BUN_INSTALL:-$XDG_DATA_HOME/bun}"

# Rust (cargo/rustup)
export CARGO_HOME="${CARGO_HOME:-$XDG_DATA_HOME/cargo}"
export RUSTUP_HOME="${RUSTUP_HOME:-$XDG_DATA_HOME/rustup}"

# -----------------------------------------------------------------------------
# XDG Redirections -- Editor Tools
# -----------------------------------------------------------------------------

# Vim -- no native XDG support, must redirect via VIMINIT
export VIMINIT="set nocp | source ${XDG_CONFIG_HOME}/vim/vimrc"

# -----------------------------------------------------------------------------
# XDG Redirections -- CLI Tools
# -----------------------------------------------------------------------------

# ripgrep -- supports XDG_CONFIG_HOME but needs explicit path for config file
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/rc"

# carapace -- bridge to native shell completions when no carapace spec exists
# Needs to be set before carapace initializes (70-tools.zsh)
export CARAPACE_BRIDGES='zsh,fish,bash'

# starship -- config requires explicit path (no auto-discovery); cache follows
# XDG_CACHE_HOME natively
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship.toml"

# -----------------------------------------------------------------------------
# Less -- Modern Configuration
# -----------------------------------------------------------------------------
# Use less with mouse support, line numbers, and colors
export LESS='-R -F -X -i -M --use-color -Dd+r$Du+b'

# -----------------------------------------------------------------------------
# Homebrew (macOS)
# -----------------------------------------------------------------------------
# Pure environment variables; needed even in non-interactive shells.
# HOMEBREW_PREFIX may already be set by Homebrew's shellenv; use ${:-} to
# preserve existing value.
if [[ -z "$HOMEBREW_PREFIX" ]]; then
  # arm64 (Apple Silicon): /opt/homebrew
  # x86_64 (Intel):        /usr/local
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
# LS_COLORS -- Colored ls and completion output
# -----------------------------------------------------------------------------
# macOS BSD ls uses LSCOLORS (different format); LS_COLORS is the GNU/XDG format
# used by zsh completion (list-colors in 30-completion.zsh) and eza.
# gdircolors is provided by coreutils (brew install coreutils, always in Brewfile).
if command -v gdircolors &>/dev/null; then
  eval "$(gdircolors -b)"
else
  # Minimal fallback: directory=bold blue, symlink=bold cyan, executable=bold green
  export LS_COLORS='di=1;34:ln=1;36:ex=1;32:fi=0:mi=0;31'
fi

# -----------------------------------------------------------------------------
# fzf -- Fuzzy Finder Environment
# -----------------------------------------------------------------------------
# Must be set before fzf-tab loads in 40-plugins.zsh.
# Doom One color theme for visual consistency.
#
# Note: FZF_ALT_C_COMMAND uses --type d (directories only) for Alt+C cd widget.
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
export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'
