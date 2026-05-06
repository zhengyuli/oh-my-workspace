# .zshrc -*- mode: sh; -*-
# Time-stamp: <2026-03-29 21:26:35 Sunday by zhengyu.li>
# =============================================================================
# Interactive Shell Orchestrator - Guard and source all conf.d modules
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Copyright (C) 2026 zhengyu li
#
# Loaded by: Interactive shells only (new terminal tab, zsh invocation)
# Load order: After .zshenv and .zprofile
#
# This file is a pure loader -- all configuration lives in conf.d/
#
# Do NOT add: environment variables, PATH, direct configuration
#             → Put env vars in 00-env.zsh
#             → Put PATH in 05-path.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Interactive Guard
# -----------------------------------------------------------------------------

# Exit immediately if not running interactively.
# Prevents configuration from leaking into scripts or non-interactive shells.
if [[ $- != *i* ]]; then
  return
fi

# -----------------------------------------------------------------------------
# Module Loader
# -----------------------------------------------------------------------------

# Load all conf.d fragments in lexicographic (numeric prefix) order.
# (N) glob qualifier: silently skip if no files match (null glob).
for _conf in "$ZDOTDIR"/conf.d/*.zsh(N); do
  if [[ "$_conf" == *.example.zsh ]]; then
    continue
  fi
  source "$_conf"
done
unset _conf

