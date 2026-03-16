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
#   4. Define XDG-compliant paths for developer tools (pyenv, cargo, go, etc.)
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
export LESS='-R --use-color -Dd+r$Du+b'

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
if [[ ! -d "$XDG_CACHE_HOME/zsh" ]]; then
  mkdir -p "$XDG_CACHE_HOME/zsh"
fi

# -----------------------------------------------------------------------------
# Developer Tools -- XDG-Compliant Paths
# -----------------------------------------------------------------------------
# These variables redirect tool data from ~/.toolname to XDG directories.
# See: https://wiki.archlinux.org/title/XDG_Base_Directory

# Cargo / Rust
export CARGO_HOME="${CARGO_HOME:-$XDG_DATA_HOME/cargo}"

# Go
export GOPATH="${GOPATH:-$XDG_DATA_HOME/go}"

# -----------------------------------------------------------------------------
# Interactive Tool XDG Redirections
# -----------------------------------------------------------------------------
# These tools are primarily used in interactive sessions.

# Vim -- no native XDG support; must redirect via VIMINIT
export VIMINIT="set nocp | source ${XDG_CONFIG_HOME}/vim/vimrc"

# Emacs -- native XDG support since 29+
# Explicit redirect ensures older versions and batch scripts find config
export EMACS_INIT_DIRECTORY="$XDG_CONFIG_HOME/emacs"

# Bun (JS/TS runtime) -- https://github.com/oven-sh/bun/issues/1678
# Note: Bun does not fully support XDG yet. Setting BUN_INSTALL redirects the
# installation directory, cache still uses ~/.bun/install/cache.
# Track progress: https://github.com/oven-sh/bun/issues/1678
export BUN_INSTALL="$XDG_DATA_HOME/bun"

# ripgrep -- supports XDG_CONFIG_HOME but needs explicit path for config file
# See: https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#configuration-file
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/rc"

# -----------------------------------------------------------------------------
# Homebrew (macOS)
# -----------------------------------------------------------------------------
# Silence Homebrew analytics (privacy)
export HOMEBREW_NO_ANALYTICS=1

# Auto-update behavior
export HOMEBREW_NO_AUTO_UPDATE=1

# -----------------------------------------------------------------------------
# Less -- Modern Configuration
# -----------------------------------------------------------------------------
# Use less with mouse support, line numbers, and colors
export LESS='-R -F -X -i -M --use-color -Dd+r$Du+b'

# -----------------------------------------------------------------------------
# FZF -- Default Options (Doom One Theme)
# -----------------------------------------------------------------------------
export FZF_DEFAULT_OPTS="
  --color=bg:#282c34,bg+:#21242b
  --color=fg:#bbc2cf,fg+:#bbc2cf
  --color=hl:#51afef,hl+:#46d9ff
  --color=info:#ecbe7b,prompt:#c678dd
  --color=pointer:#51afef,marker:#98be65
  --color=spinner:#46d9ff,header:#5699af
  --color=border:#3b3f46,label:#bbc2cf
  --color=selected-bg:#2257a0
  --layout=reverse
  --border=rounded
  --height=50%
  --info=inline
  --bind=tab:accept
  --preview-window=right:50%:wrap
"

# Use fd for faster file finding
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
