# 15-history.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-17 00:00:00 Tuesday by zhengyu.li>
# =============================================================================
# History Configuration - Size limits and behavior options
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Copyright (C) 2026 zhengyu li
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 15 (after 10-options.zsh, before 20-aliases.zsh)
#
# Prerequisites:
#   - HISTFILE is set in 00-env.zsh ($XDG_STATE_HOME/zsh/history)
#
# Do NOT add: HISTFILE path
#             → Put this in 00-env.zsh (environment variables)
# =============================================================================

# -----------------------------------------------------------------------------
# History Size
# -----------------------------------------------------------------------------

# HISTSIZE: maximum entries kept in memory during session
# SAVEHIST: maximum entries written to HISTFILE on exit
readonly HIST_SAVE_MAX=50000
# HISTSIZE must exceed SAVEHIST so HIST_EXPIRE_DUPS_FIRST has buffer
# room to keep duplicates in memory until trimming, instead of removing
# them immediately (which conflicts with SHARE_HISTORY).
readonly HIST_MEMORY_MAX=60000
HISTSIZE=$HIST_MEMORY_MAX
SAVEHIST=$HIST_SAVE_MAX

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
# Ignore consecutive duplicates only (not all-time).
# HIST_IGNORE_ALL_DUPS conflicts with SHARE_HISTORY: immediate in-memory
# dedup + real-time file sync creates a feedback loop that progressively
# loses history across sessions.
setopt HIST_IGNORE_DUPS
# Don't write duplicates to history file
setopt HIST_SAVE_NO_DUPS
# Skip duplicates when searching with arrow keys
setopt HIST_FIND_NO_DUPS

# --- Sharing ---
# SHARE_HISTORY implies immediate append, so INC_APPEND_HISTORY is redundant
# Share history across all running shells in real time
setopt SHARE_HISTORY
# Use fcntl() for file locking (faster, safer on NFS, zero downside)
setopt HIST_FCNTL_LOCK

# --- Safety ---
# Show expanded history substitution before executing
setopt HIST_VERIFY
