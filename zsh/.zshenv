# ~/.zshenv
# =============================================================================
# Bootstrap -- the only Zsh-related file in $HOME
#
# Loaded by: ALL shell types (interactive, script, cron, SSH)
# Load order: First (before .zprofile, .zshrc)
#
# Responsibilities:
#   1. Set XDG base directories (prerequisite for all other config)
#   2. Set ZDOTDIR (redirect Zsh to XDG config directory)
#   3. Redirect tools that do not natively support XDG
#
# Do NOT add: PATH, aliases, functions, plugins, external commands
#             → Put these in .zprofile (login) or .zshrc (interactive)
# =============================================================================

# -----------------------------------------------------------------------------
# XDG Base Directory Specification
# -----------------------------------------------------------------------------
# Must be set first; all other configurations depend on these paths.
# See: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# -----------------------------------------------------------------------------
# Zsh Configuration Directory
# -----------------------------------------------------------------------------
# Redirect Zsh to XDG-compliant config directory.
# After this, Zsh loads .zprofile and .zshrc from $ZDOTDIR instead of $HOME.
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# -----------------------------------------------------------------------------
# Editor XDG Redirections
# -----------------------------------------------------------------------------
# Tools without native XDG support need explicit redirection here so they work
# in ALL shell contexts (including non-interactive scripts).

# Vim -- no native XDG support; must redirect via VIMINIT
export VIMINIT="set nocp | source ${XDG_CONFIG_HOME}/vim/vimrc"

# Emacs -- native XDG support since 29+
# Explicit redirect ensures older versions and batch scripts find config
export EMACS_INIT_DIRECTORY="$XDG_CONFIG_HOME/emacs"
