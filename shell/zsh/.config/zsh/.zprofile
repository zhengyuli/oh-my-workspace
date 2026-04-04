# .zprofile -*- mode: sh; -*-
# Time-stamp: <2026-03-20 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Login Shell Initialization - Source non-interactive conf.d fragments
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Copyright (C) 2026 zhengyu li
#
# Loaded by: Login shells only (first terminal open, SSH login)
# Load order: Second (after .zshenv, before .zshrc)
#
# Do NOT add: plugins, completion, prompt, aliases, keybindings
#             → Put these in .zshrc (interactive only)
# =============================================================================

# -----------------------------------------------------------------------------
# Core Configuration Fragments
# -----------------------------------------------------------------------------

# These modules are safe for non-interactive contexts and should be available
# to all login shells (including scripts run via 'ssh host command').

# Environment variables (EDITOR, PAGER, XDG paths, Homebrew settings, etc.)
source "$ZDOTDIR/conf.d/00-env.zsh"

# PATH / FPATH / manpath management
source "$ZDOTDIR/conf.d/05-path.zsh"
