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

# type a directory name to cd into it
setopt AUTO_CD
# push directory onto stack automatically
setopt AUTO_PUSHD
# don't push duplicate directories onto stack
setopt PUSHD_IGNORE_DUPS
# suppress output of pushd / popd
setopt PUSHD_SILENT
# cd to variable values by directory name
setopt CDABLE_VARS

# -----------------------------------------------------------------------------
# Completion
# -----------------------------------------------------------------------------

# move cursor to end of completed word
setopt ALWAYS_TO_END
# list choices on ambiguous completion
setopt AUTO_LIST
# append trailing slash to completed directories
setopt AUTO_PARAM_SLASH
# complete from both cursor ends inward
setopt COMPLETE_IN_WORD
# free Ctrl-S/Ctrl-Q for use in editor and completion widgets
unsetopt FLOW_CONTROL
# Fallback when fzf-tab is disabled (normally fzf-tab owns Tab UI)
setopt AUTO_MENU

# -----------------------------------------------------------------------------
# Globbing / Expansion
# -----------------------------------------------------------------------------

# enable extended globbing (#, ~, ^ operators)
setopt EXTENDED_GLOB
# no error when glob matches nothing (empty expansion)
setopt NULL_GLOB
# sort numeric filenames by numeric value
setopt NUMERIC_GLOB_SORT

# -----------------------------------------------------------------------------
# Input / Output
# -----------------------------------------------------------------------------

# allow comments in interactive shell
setopt INTERACTIVE_COMMENTS
# treat '' inside double quotes as literal single quote
setopt RC_QUOTES
# handle Unicode combining characters correctly
setopt COMBINING_CHARS
# disable terminal bell
unsetopt BEEP
# confirm before deleting all files with rm *
unsetopt RM_STAR_SILENT

# -----------------------------------------------------------------------------
# Job Control
# -----------------------------------------------------------------------------

# resume suspended jobs by typing their name
setopt AUTO_RESUME
# display jobs in long format by default
setopt LONG_LIST_JOBS
# report job status changes immediately
setopt NOTIFY
# run background jobs at normal priority
unsetopt BG_NICE
# don't warn about running jobs on shell exit
unsetopt CHECK_JOBS
# don't kill background jobs when shell exits
unsetopt HUP
