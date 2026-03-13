# history.zsh -*- mode: zsh; -*-
# Time-stamp: <2026-03-13 18:42:20 Friday by zhengyu.li>
#
# History configuration

# ==============================================================================
# History File Location
# ==============================================================================

# Use XDG state directory
HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/zsh/history"

# Ensure directory exists with error handling (wrapped in anonymous function for proper local scope)
() {
    local hist_dir="${HISTFILE:h}"
    if [[ ! -d "$hist_dir" ]]; then
        if ! mkdir -p "$hist_dir" 2>/dev/null; then
            echo "Warning: Failed to create history directory: $hist_dir" >&2
            # Fallback to default location
            HISTFILE="$HOME/.zsh_history"
        fi
    fi
}

# ==============================================================================
# History Size
# ==============================================================================

HISTSIZE=100000                 # In-memory history size
SAVEHIST=100000                 # File history size

# ==============================================================================
# History Options
# ==============================================================================

setopt BANG_HIST                # Treat '!' specially in expansion
setopt EXTENDED_HISTORY         # Write timestamp to history file
setopt SHARE_HISTORY            # Share history across sessions (includes incremental append)
setopt HIST_EXPIRE_DUPS_FIRST   # Delete duplicates when full
setopt HIST_IGNORE_DUPS         # Don't record duplicates
setopt HIST_IGNORE_ALL_DUPS     # Delete old duplicates
setopt HIST_FIND_NO_DUPS        # Don't show duplicates in search
setopt HIST_IGNORE_SPACE        # Don't record space-prefixed commands
setopt HIST_SAVE_NO_DUPS        # Don't save duplicates
setopt HIST_REDUCE_BLANKS       # Remove extra blanks
