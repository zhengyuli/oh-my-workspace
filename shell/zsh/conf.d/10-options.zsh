# 10-options.zsh -*- mode: sh; -*-
# Time-stamp: <2026-04-06 20:22:11 Monday by zhengyu.li>
#
# =============================================================================
# Shell Options - Configure zsh behavior and defaults
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: options, setopt, zsh, behavior
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   2026-04-06 20:22 zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   Loaded by: .zshrc (interactive shells only)
#   Load order: 10 (after 05-path.zsh, before 15-history.zsh)
#
#   Do NOT add: history options, aliases, key bindings
#               → History options in 15-history.zsh
#               → Aliases in 20-aliases.zsh
#               → Key bindings in 60-keybinds.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Directory Navigation
# -----------------------------------------------------------------------------

# Type a bare directory name to cd into it
setopt AUTO_CD
# cd automatically pushes old directory onto dirstack
setopt AUTO_PUSHD
# Skip duplicate entries when pushing to dirstack
setopt PUSHD_IGNORE_DUPS
# Suppress "~1 ~2" output from pushd / popd
setopt PUSHD_SILENT
# Expand named directories (parameters set to dir paths) in cd
setopt CDABLE_VARS

# -----------------------------------------------------------------------------
# Completion
# -----------------------------------------------------------------------------

# Move cursor to end of word after completing mid-word
setopt ALWAYS_TO_END
# List choices on ambiguous completion (show menu immediately)
setopt AUTO_LIST
# Append trailing slash when completing directory names
setopt AUTO_PARAM_SLASH
# Allow completion from within a word, not just at the end
setopt COMPLETE_IN_WORD
# Disable terminal flow control (Ctrl-S / Ctrl-Q) to free those keys
unsetopt FLOW_CONTROL
# Show completion menu on second consecutive Tab press
setopt AUTO_MENU

# -----------------------------------------------------------------------------
# Globbing / Expansion
# -----------------------------------------------------------------------------

# Enable extended glob operators: ^, ~, # (negation, except, repetition)
setopt EXTENDED_GLOB
# Include dotfiles in glob results without explicit dot
setopt GLOB_DOTS
# No error when a glob pattern matches nothing (expands to empty)
setopt NULL_GLOB
# Sort numeric filenames numerically (file2 before file10)
setopt NUMERIC_GLOB_SORT

# -----------------------------------------------------------------------------
# Input / Output
# -----------------------------------------------------------------------------

# Allow comments (#) in interactive shell commands
setopt INTERACTIVE_COMMENTS
# Use '' inside single-quoted strings to represent a literal quote
setopt RC_QUOTES
# Handle Unicode combining characters correctly in line editor
setopt COMBINING_CHARS
# Disable terminal beep on errors and ambiguous completions
unsetopt BEEP
# Prevent > from overwriting existing files (use >| to force)
unsetopt CLOBBER
# Prompt for confirmation before rm with glob (e.g., rm *)
unsetopt RM_STAR_SILENT

# -----------------------------------------------------------------------------
# Job Control
# -----------------------------------------------------------------------------

# Resume matching suspended job when typing its command name
setopt AUTO_RESUME
# Display PID and status in long format for background jobs
setopt LONG_LIST_JOBS
# Report background job status immediately (don't wait for prompt)
setopt NOTIFY
# Don't lower priority of background jobs with nice
unsetopt BG_NICE
# Don't warn about running jobs on shell exit
unsetopt CHECK_JOBS
# Don't send HUP signal to running jobs on shell exit
unsetopt HUP
