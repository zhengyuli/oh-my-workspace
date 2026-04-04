# 10-options.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-17 00:00:00 Tuesday by zhengyu.li>
# =============================================================================
# Shell Options - Configure zsh behavior and defaults
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Copyright (C) 2026 zhengyu li
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 10 (after 05-path.zsh, before 15-history.zsh)
#
# Do NOT add: history options, aliases, key bindings
#             → History options in 15-history.zsh
#             → Aliases in 20-aliases.zsh
#             → Key bindings in 60-keybinds.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Directory Navigation
# -----------------------------------------------------------------------------

# Type a directory name to cd into it
setopt AUTO_CD
# Cd automatically pushes old dir onto the stack
setopt AUTO_PUSHD
# Do not store duplicates in the directory stack
setopt PUSHD_IGNORE_DUPS
# Suppress output of pushd / popd
setopt PUSHD_SILENT
# Allow cd to use variable names as directories
setopt CDABLE_VARS

# -----------------------------------------------------------------------------
# Completion
# -----------------------------------------------------------------------------

# Move cursor to end after completion
setopt ALWAYS_TO_END
# Automatically list choices on ambiguous completion
setopt AUTO_LIST
# Add trailing slash when completing directories
setopt AUTO_PARAM_SLASH
# Complete from cursor position within a word
setopt COMPLETE_IN_WORD
# Disable Ctrl-S / Ctrl-Q flow control
unsetopt FLOW_CONTROL
# Enable menu on repeated Tab press (fallback when fzf-tab is disabled)
setopt AUTO_MENU

# -----------------------------------------------------------------------------
# Globbing / Expansion
# -----------------------------------------------------------------------------

# Enable extended glob operators: ^, ~, #
setopt EXTENDED_GLOB
# Silently remove patterns with no matches
setopt NULL_GLOB
# Sort glob results numerically
setopt NUMERIC_GLOB_SORT

# -----------------------------------------------------------------------------
# Input / Output
# -----------------------------------------------------------------------------

# Allow # comments in interactive shell
setopt INTERACTIVE_COMMENTS
# Allow '' inside single-quoted strings
setopt RC_QUOTES
# Handle Unicode combining characters correctly
setopt COMBINING_CHARS
# No beep on error
unsetopt BEEP
# Prompt for confirmation before rm * (zsh default)
unsetopt RM_STAR_SILENT

# -----------------------------------------------------------------------------
# Job Control
# -----------------------------------------------------------------------------

# Resume a stopped job by typing its name
setopt AUTO_RESUME
# List jobs in long format (PID, state, working directory)
setopt LONG_LIST_JOBS
# Report background job status immediately
setopt NOTIFY
# Do not run background jobs at lower priority
unsetopt BG_NICE
# Do not warn about running jobs on exit
unsetopt CHECK_JOBS
# Do not send HUP to background jobs on shell exit
unsetopt HUP
