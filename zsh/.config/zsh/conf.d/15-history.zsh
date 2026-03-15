# 15-history.zsh
# =============================================================================
# History Configuration
#
# Loaded by: .zshrc (interactive shells only)
# Load order: After 10-options.zsh, before 20-aliases.zsh
#
# Prerequisite: HISTFILE is set in 00-env.zsh ($XDG_CACHE_HOME/zsh/history)
#
# Responsibilities:
#   1. Set history size limits (HISTSIZE, SAVEHIST)
#   2. Configure history behavior options
#
# Do NOT add: HISTFILE path (set in 00-env.zsh)
# =============================================================================

# -----------------------------------------------------------------------------
# History Size
# -----------------------------------------------------------------------------
# HISTSIZE: maximum entries kept in memory during session
# SAVEHIST: maximum entries written to HISTFILE on exit
# Common practice: 10000-50000 (100000 is excessive)
HISTSIZE=50000
SAVEHIST=50000

# -----------------------------------------------------------------------------
# History Options
# -----------------------------------------------------------------------------
# See: https://zsh.sourceforge.io/Doc/Release/Options.html#History

# --- Recording ---
setopt EXTENDED_HISTORY    # Record timestamp and duration: `: start:elapsed;command`
setopt HIST_IGNORE_SPACE   # Commands starting with space are not recorded
setopt HIST_REDUCE_BLANKS  # Strip superfluous whitespace before saving

# --- Deduplication ---
setopt HIST_EXPIRE_DUPS_FIRST  # Delete oldest duplicates when HISTSIZE is reached
setopt HIST_IGNORE_ALL_DUPS    # Delete old duplicate when new one is added
setopt HIST_SAVE_NO_DUPS       # Don't write duplicates to history file
setopt HIST_FIND_NO_DUPS       # Skip duplicates when searching with arrow keys

# --- Sharing ---
# SHARE_HISTORY implies immediate append, so INC_APPEND_HISTORY is redundant
setopt SHARE_HISTORY       # Share history across all running shells in real time

# --- Safety ---
setopt HIST_VERIFY  # Show expanded history substitution before executing
