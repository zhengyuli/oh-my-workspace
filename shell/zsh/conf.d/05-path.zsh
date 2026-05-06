# 05-path.zsh -*- mode: sh; -*-
# Time-stamp: <2026-04-04 13:09:09 Saturday by zhengyu.li>
#
# =============================================================================
# PATH / FPATH / MANPATH / INFOPATH Management - Deduplicated search paths
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Keywords: path, fpath, manpath, infopath, deduplication
#
# Copyright (C) 2026 zhengyu li
#
# History:
#   2026-04-04 13:09 zhengyu li <lizhengyu419@outlook.com> created.
#
# Commentary:
#   Loaded by: .zprofile (login) and .zshrc (interactive)
#   Load order: 05 (after 00-env.zsh sets tool paths)
#
#   Prerequisites:
#     - 00-env.zsh must set: CARGO_HOME, GOPATH, BUN_INSTALL, XDG_* variables
#
#   Do NOT add: tool initialization, aliases, environment variables
#               → Tool initialization in 70-tools.zsh
#               → Aliases in 20-aliases.zsh
#               → Environment variables in 00-env.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Deduplication
# -----------------------------------------------------------------------------

typeset -gU path fpath manpath infopath cdpath

# -----------------------------------------------------------------------------
# PATH
# -----------------------------------------------------------------------------

# Glob qualifier (N-/): N=nullglob (no error if missing), -/=<only dirs>
path=(
  # --- Priority 1: User Local Binaries (Highest) ---
  # uv tool install, pip install --user, etc.
  "$HOME/.local/bin"(N-/)

  # --- Priority 2: Development Tool Binaries ---
  "$CARGO_HOME/bin"(N-/)
  "$GOPATH/bin"(N-/)
  "$BUN_INSTALL/bin"(N-/)

  # --- Priority 3: Package Manager Binaries ---
  # Homebrew (Apple Silicon)
  /opt/homebrew/bin(N-/)
  # Homebrew (Intel Mac / Linux)
  /usr/local/bin(N-/)

  # --- Priority 4: System Sbin (Admin Commands) ---
  # Lower priority - rarely needed in daily development
  /opt/homebrew/sbin(N-/)
  /usr/local/sbin(N-/)

  # --- Priority 5: Existing System PATH (Lowest - Fallback) ---
  $path
)

# -----------------------------------------------------------------------------
# FPATH
# -----------------------------------------------------------------------------

fpath=(
  "$ZDOTDIR/completions"(N-/)

  "$ZDOTDIR/functions"(N-/)

  "$HOMEBREW_PREFIX/share/zsh/site-functions"(N-/)
  "$HOMEBREW_PREFIX/share/zsh-completions"(N-/)

  $fpath
)

# -----------------------------------------------------------------------------
# MANPATH
# -----------------------------------------------------------------------------

manpath=(
  "$HOMEBREW_PREFIX/share/man"(N-/)

  /usr/local/share/man(N-/)
  /usr/share/man(N-/)

  $manpath
)

# -----------------------------------------------------------------------------
# INFOPATH
# -----------------------------------------------------------------------------

infopath=(
  "$HOMEBREW_PREFIX/share/info"(N-/)

  /usr/local/share/info(N-/)
  /usr/share/info(N-/)

  $infopath
)

# -----------------------------------------------------------------------------
# Autoload Custom Functions
# -----------------------------------------------------------------------------

# Must come after fpath is set above.
# (N:t) -- N=nullglob silently skip if no matches, :t=filename only.
# Guard required: autoload -Uz with no args lists all functions to stdout.
_func_files=($ZDOTDIR/functions/*(N:t))
if (( ${#_func_files} )); then
  autoload -Uz "${_func_files[@]}"
fi
unset _func_files
