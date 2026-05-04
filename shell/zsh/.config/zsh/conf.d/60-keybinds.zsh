# 60-keybinds.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-28 14:45:08 Saturday by zhengyu.li>
# =============================================================================
# Key Bindings - Emacs keymap, history, word movement, and widgets
#
# Author: zhengyu li <lizhengyu419@outlook.com>
# Copyright (C) 2026 zhengyu li
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 60 (after 50-prompt.zsh, before 70-tools.zsh)
#
# Do NOT add: aliases, plugin key bindings, tool initialization
#             → Aliases in 20-aliases.zsh
#             → Plugin key bindings in 40-plugins.zsh (plugin-specific)
#             → Tool initialization in 70-tools.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Emacs Keymap
# -----------------------------------------------------------------------------

# Emacs keymap (replace with 'bindkey -v' for vi mode)
bindkey -e

# Reduce Esc-sequence detection delay from 400ms (default 40) to 100ms.
# Affects all multi-key sequences starting with Esc (e.g., \e\e sudo toggle).
KEYTIMEOUT=10

# Vi mode (uncomment to enable; comment out 'bindkey -e' above)
# bindkey -v
# KEYTIMEOUT=1   # vi mode: reduce mode-switch delay to 10ms

# -----------------------------------------------------------------------------
# History Search
# -----------------------------------------------------------------------------

# Prefix-based history search (built-in widgets)
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward

# Note: history-substring-search keybindings (arrow keys) are set in
# 40-plugins.zsh via Zinit's atload hook to ensure widgets exist before binding

# -----------------------------------------------------------------------------
# Word Movement
# -----------------------------------------------------------------------------

bindkey -- '^[[1;5C' forward-word
bindkey -- '^[[1;5D' backward-word
bindkey '^[f' forward-word
bindkey '^[b' backward-word
bindkey '^[d' kill-word
# Ghostty sends ^H for Ctrl+Backspace; most other terminals send ^H for
# plain Backspace, so only bind in Ghostty to avoid breaking Backspace.
if [[ "$TERM_PROGRAM" == "ghostty" ]]; then
  bindkey '^H' backward-kill-word
fi
bindkey '^[[3;5~' kill-word

# -----------------------------------------------------------------------------
# Line Editing
# -----------------------------------------------------------------------------

bindkey '^A' beginning-of-line
bindkey '^E' end-of-line
bindkey '^K' kill-line
bindkey '^U' backward-kill-line
bindkey '^Y' yank
bindkey '^[[3~' delete-char
bindkey '^[^?' backward-kill-word

# -----------------------------------------------------------------------------
# Completion
# -----------------------------------------------------------------------------

# Tab (^I) is owned by fzf-tab — do NOT rebind ^I here.
# Shift-Tab events go to fzf's popup, not zsh.

# -----------------------------------------------------------------------------
# Miscellaneous
# -----------------------------------------------------------------------------

bindkey '^L' clear-screen

# -----------------------------------------------------------------------------
# Sudo Toggle
# -----------------------------------------------------------------------------

# Press Esc twice to add or remove 'sudo' at the start of the line
sudo-command-line() {
  local prefix="sudo "

  if [[ -z "$BUFFER" ]]; then
    zle up-history
    return
  fi

  if [[ "$BUFFER" == ${prefix}* ]]; then
    BUFFER="${BUFFER#${prefix}}"

    if (( CURSOR >= ${#prefix} )); then
      (( CURSOR -= ${#prefix} ))
    else
      CURSOR=0
    fi
  else
    BUFFER="${prefix}${BUFFER}"
    (( CURSOR += ${#prefix} ))
  fi

  zle redisplay
}
zle -N sudo-command-line
bindkey '\e\e' sudo-command-line
