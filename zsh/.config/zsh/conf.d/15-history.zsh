# 15-history.zsh
# Time-stamp: <2026-03-17 00:00:00 Monday by zhengyu.li>
# =============================================================================
# History Configuration
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 15 (after 10-options.zsh, before 20-aliases.zsh)
#
# Prerequisites:
#   - HISTFILE is set in 00-env.zsh ($XDG_STATE_HOME/zsh/history)
#
# Responsibilities:
#   1. Set history size limits (HISTSIZE, SAVEHIST)
#   2. Configure history behavior options
#
# Do NOT add: HISTFILE path
#             → Put this in 00-env.zsh (environment variables)
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
# Record timestamp and duration: `: start:elapsed;command`
setopt EXTENDED_HISTORY
# Commands starting with space are not recorded
setopt HIST_IGNORE_SPACE
# Strip superfluous whitespace before saving
setopt HIST_REDUCE_BLANKS

# --- Deduplication ---
# Delete oldest duplicates when HISTSIZE is reached
setopt HIST_EXPIRE_DUPS_FIRST
# Delete old duplicate when new one is added
setopt HIST_IGNORE_ALL_DUPS
# Don't write duplicates to history file
setopt HIST_SAVE_NO_DUPS
# Skip duplicates when searching with arrow keys
setopt HIST_FIND_NO_DUPS

# --- Sharing ---
# SHARE_HISTORY implies immediate append, so INC_APPEND_HISTORY is redundant
# Share history across all running shells in real time
setopt SHARE_HISTORY

# --- Safety ---
# Show expanded history substitution before executing
setopt HIST_VERIFY
