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

# emacs keymap (replace with 'bindkey -v' for vi mode)
bindkey -e

# Vi mode (uncomment to enable; comment out 'bindkey -e' above)
# bindkey -v
# export KEYTIMEOUT=1   # reduce mode-switch delay to 0.1s

# -----------------------------------------------------------------------------
# History Search
# -----------------------------------------------------------------------------

# Ctrl-P / Ctrl-N: prefix-based history search (built-in widgets)
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward

# Note: history-substring-search keybindings (arrow keys) are set in
# 40-plugins.zsh via Zinit's atload hook to ensure widgets exist before binding

# -----------------------------------------------------------------------------
# Word Movement
# -----------------------------------------------------------------------------

# Ctrl-Right
bindkey -- '^[[1;5C' forward-word
# Ctrl-Left
bindkey -- '^[[1;5D' backward-word
# Alt-f
bindkey '^[f' forward-word
# Alt-b
bindkey '^[b' backward-word
# Alt-d   delete word forward
bindkey '^[d' kill-word
# Ctrl-Backspace  delete word backward
# Ghostty sends ^H for Ctrl+Backspace; most other terminals send ^H for
# plain Backspace, so only bind in Ghostty to avoid breaking Backspace.
if [[ "$TERM_PROGRAM" == "ghostty" ]]; then
  bindkey '^H' backward-kill-word
fi
# Ctrl-Delete     delete word forward
bindkey '^[[3;5~' kill-word

# -----------------------------------------------------------------------------
# Line Editing
# -----------------------------------------------------------------------------

# Ctrl-A  jump to start of line
bindkey '^A' beginning-of-line
# Ctrl-E  jump to end of line
bindkey '^E' end-of-line
# Ctrl-K  delete from cursor to end
bindkey '^K' kill-line
# Ctrl-U  delete from cursor to start
bindkey '^U' backward-kill-line
# Ctrl-Y  paste (yank killed text)
bindkey '^Y' yank

# -----------------------------------------------------------------------------
# Editing
# -----------------------------------------------------------------------------

# Delete key
bindkey '^[[3~' delete-char
# Alt-Backspace
bindkey '^[^?' backward-kill-word

# -----------------------------------------------------------------------------
# Completion
# -----------------------------------------------------------------------------

# Tab (^I) is owned by fzf-tab — do NOT rebind ^I here.
# Shift-Tab events go to fzf's popup, not zsh.

# -----------------------------------------------------------------------------
# Miscellaneous
# -----------------------------------------------------------------------------

# Ctrl-L  clear screen
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
