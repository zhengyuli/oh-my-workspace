# 00-env.zsh
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
# Do NOT add:
#   - PATH changes → 05-path.zsh (centralized path management)
#   - Aliases → 20-aliases.zsh (interactive only)
#   - Functions → functions/ directory (autoloaded)
#   - Tool initialization → 70-tools.zsh (lazy-loaded)
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
# History file stored in XDG cache (runtime data, not configuration).
export HISTFILE="$XDG_CACHE_HOME/zsh/history"

# Ensure cache directory exists on first run (idempotent).
# This directory is shared by multiple modules: history, zcompdump,
# completion-cache, uv/carapace completion caches (see 30-completion.zsh,
# 70-tools.zsh). Do not derive the path from HISTFILE alone.
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
export BUN_INSTALL="$XDG_DATA_HOME/bun"

# Rust (cargo/rustup)
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

# -----------------------------------------------------------------------------
# XDG Redirections -- Editor Tools
# -----------------------------------------------------------------------------

# Vim -- no native XDG support; must redirect via VIMINIT
export VIMINIT="set nocp | source ${XDG_CONFIG_HOME}/vim/vimrc"

# -----------------------------------------------------------------------------
# XDG Redirections -- CLI Tools
# -----------------------------------------------------------------------------

# ripgrep -- supports XDG_CONFIG_HOME but needs explicit path for config file
# See: https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#configuration-file
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/rc"

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
# fzf -- Fuzzy Finder Environment
# -----------------------------------------------------------------------------
# Must be set before fzf-tab loads (via atload in 40-plugins.zsh).
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
