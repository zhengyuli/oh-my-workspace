# 10-options.zsh -*- mode: sh; -*-
# Time-stamp: <2026-04-06 20:22:11 Monday by zhengyu.li>
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

setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT
setopt CDABLE_VARS

# -----------------------------------------------------------------------------
# Completion
# -----------------------------------------------------------------------------

setopt ALWAYS_TO_END
setopt AUTO_LIST
setopt AUTO_PARAM_SLASH
setopt COMPLETE_IN_WORD
unsetopt FLOW_CONTROL
setopt AUTO_MENU

# -----------------------------------------------------------------------------
# Globbing / Expansion
# -----------------------------------------------------------------------------

setopt EXTENDED_GLOB
setopt GLOB_DOTS
setopt NULL_GLOB
setopt NUMERIC_GLOB_SORT

# -----------------------------------------------------------------------------
# Input / Output
# -----------------------------------------------------------------------------

setopt INTERACTIVE_COMMENTS
setopt RC_QUOTES
setopt COMBINING_CHARS
unsetopt BEEP
unsetopt CLOBBER
unsetopt RM_STAR_SILENT

# -----------------------------------------------------------------------------
# Job Control
# -----------------------------------------------------------------------------

setopt AUTO_RESUME
setopt LONG_LIST_JOBS
setopt NOTIFY
unsetopt BG_NICE
unsetopt CHECK_JOBS
unsetopt HUP
