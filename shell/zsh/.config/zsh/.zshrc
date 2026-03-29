# .zshrc -*- mode: sh; -*-
# Time-stamp: <2026-03-18 00:00:00 Wednesday by zhengyu.li>
# =============================================================================
# Interactive Shell Orchestrator
#
# Loaded by: Interactive shells only (new terminal tab, zsh invocation)
# Load order: After .zshenv and .zprofile
#
# Responsibilities:
#   1. Guard against non-interactive execution
#   2. Source all conf.d fragments in numeric order
#
# This file is a pure loader -- all configuration lives in conf.d/
# One concern per file: options, aliases, completion, plugins, prompt, etc.
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
for _conf in "$ZDOTDIR"/conf.d/*.zsh(N~*.example.zsh); do
  source "$_conf"
done
unset _conf

