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

# pyenv -- Python version manager
export PYENV_ROOT="${PYENV_ROOT:-$XDG_DATA_HOME/pyenv}"

# Cargo / Rust
export CARGO_HOME="${CARGO_HOME:-$XDG_DATA_HOME/cargo}"

# Go
export GOPATH="${GOPATH:-$XDG_DATA_HOME/go}"

# Bun -- JavaScript runtime
export BUN_INSTALL="${BUN_INSTALL:-$XDG_DATA_HOME/bun}"

# npm -- global packages
export NPM_CONFIG_PREFIX="${NPM_CONFIG_PREFIX:-$XDG_DATA_HOME/npm}"

# fnm -- Fast Node Manager
export FNM_DIR="${FNM_DIR:-$XDG_DATA_HOME/fnm}"

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
# FZF -- Default Options
# -----------------------------------------------------------------------------
export FZF_DEFAULT_OPTS="
  --height 60%
  --layout=reverse
  --border=rounded
  --preview-window=right:50%:wrap
"

# Use fd for faster file finding
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
