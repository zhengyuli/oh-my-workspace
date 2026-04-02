# 10-options.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-17 00:00:00 Tuesday by zhengyu.li>
# =============================================================================
# Shell Options (setopt / unsetopt)
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 10 (after 05-path.zsh, before 15-history.zsh)
#
# Prerequisites:
#   - None (standalone configuration)
#
# Responsibilities:
#   1. Configure directory navigation options (AUTO_CD, PUSHD_*)
#   2. Configure completion options (AUTO_MENU, COMPLETE_IN_WORD, etc.)
#   3. Configure globbing options (EXTENDED_GLOB, NULL_GLOB, etc.)
#   4. Configure input/output options (INTERACTIVE_COMMENTS, BEEP, etc.)
#   5. Configure job control options (AUTO_RESUME, NOTIFY, etc.)
#
# Do NOT add: history options, aliases, key bindings
#             → History options in 15-history.zsh (dedicated history config)
#             → Aliases in 20-aliases.zsh
#             → Key bindings in 60-keybinds.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Directory Navigation
# -----------------------------------------------------------------------------
# type a directory name to cd into it
setopt AUTO_CD
# cd automatically pushes old dir onto the stack
setopt AUTO_PUSHD
# do not store duplicates in the directory stack
setopt PUSHD_IGNORE_DUPS
# suppress output of pushd / popd
setopt PUSHD_SILENT
# allow cd to use variable names as directories
setopt CDABLE_VARS

# -----------------------------------------------------------------------------
# Completion
# -----------------------------------------------------------------------------
# move cursor to end after completion
setopt ALWAYS_TO_END
# automatically list choices on ambiguous completion
setopt AUTO_LIST
# add trailing slash when completing directories
setopt AUTO_PARAM_SLASH
# complete from both ends of a word
setopt COMPLETE_IN_WORD
# disable Ctrl-S / Ctrl-Q flow control
unsetopt FLOW_CONTROL
# enable menu on repeated Tab press (fallback when fzf-tab is disabled)
setopt AUTO_MENU

# -----------------------------------------------------------------------------
# Globbing / Expansion
# -----------------------------------------------------------------------------
# enable extended glob operators: ^, ~, #
setopt EXTENDED_GLOB
# silently remove patterns with no matches
setopt NULL_GLOB
# sort glob results numerically
setopt NUMERIC_GLOB_SORT

# -----------------------------------------------------------------------------
# Input / Output
# -----------------------------------------------------------------------------
# allow # comments in interactive shell
setopt INTERACTIVE_COMMENTS
# allow '' inside single-quoted strings
setopt RC_QUOTES
# handle Unicode combining characters correctly
setopt COMBINING_CHARS
# no beep on error
unsetopt BEEP
# require confirmation before rm *
unsetopt RM_STAR_SILENT

# -----------------------------------------------------------------------------
# Job Control
# -----------------------------------------------------------------------------
# resume a stopped job by typing its name
setopt AUTO_RESUME
# display PID when listing jobs
setopt LONG_LIST_JOBS
# report background job status immediately
setopt NOTIFY
# do not run background jobs at lower priority
unsetopt BG_NICE
# do not warn about running jobs on exit
unsetopt CHECK_JOBS
# do not send HUP to background jobs on shell exit
unsetopt HUP
