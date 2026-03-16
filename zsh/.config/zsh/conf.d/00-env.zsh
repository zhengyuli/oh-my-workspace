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
# Note: Bun does not fully support XDG yet. Setting BUN_INSTALL redirects the
# installation directory, cache still uses ~/.bun/install/cache.
export BUN_INSTALL="$XDG_DATA_HOME/bun"

# -----------------------------------------------------------------------------
# XDG Redirections -- Editor Tools
# -----------------------------------------------------------------------------

# Vim -- no native XDG support; must redirect via VIMINIT
export VIMINIT="set nocp | source ${XDG_CONFIG_HOME}/vim/vimrc"

# Emacs -- native XDG support since 29+
# Explicit redirect ensures older versions and batch scripts find config
export EMACS_INIT_DIRECTORY="$XDG_CONFIG_HOME/emacs"

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
