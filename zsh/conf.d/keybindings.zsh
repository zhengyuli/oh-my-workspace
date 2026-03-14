# ==============================================================================
# File: conf.d/keybindings.zsh
# Role: Key bindings configuration for Zsh line editing
#
# Load context : Sourced by .zshrc AFTER plugins are loaded
# Dependencies : zsh-history-substring-search plugin (optional)
# Side effects : Defines key bindings in global namespace
#
# NOTE: This file should be loaded AFTER plugins are sourced, because some
# bindings depend on widgets provided by plugins (e.g., history-substring-search).
# ==============================================================================

# ── Bind Mode ─────────────────────────────────────────────────────

# Use emacs-style key bindings (default)
bindkey -e

# ── Edit Command Line ─────────────────────────────────────────────────────

# Edit command line in $EDITOR with Ctrl+X Ctrl+E
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# ── Word Movement ─────────────────────────────────────────────────────

# Alt+Left/Right to move by word
bindkey '^[b' backward-word
bindkey '^[f' forward-word

# Ctrl+Left/Right for word movement
bindkey '^[[1;5D' backward-word
bindkey '^[[1;5C' forward-word

# ── Line Editing ─────────────────────────────────────────────────────

# Home/End keys
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '^[[1~' beginning-of-line
bindkey '^[[4~' end-of-line

# Delete keys
bindkey '^[[3~' delete-char
bindkey '^[[3;5~' kill-word

# Backspace
bindkey '^?' backward-delete-char
bindkey '^H' backward-delete-char

# Ctrl+U to clear line
bindkey '^U' backward-kill-line

# Ctrl+W to delete word before cursor
bindkey '^W' backward-kill-word

# ── Undo/Redo ─────────────────────────────────────────────────────

bindkey '^_' undo
bindkey '^[_' redo

# ── Miscellaneous ─────────────────────────────────────────────────────

# Ctrl+Z to toggle between foreground and background
_fancy_ctrl_z() {
  if [[ $#BUFFER -eq 0 ]]; then
    BUFFER="fg"
    zle accept-line -w
  else
    zle push-input -w
    zle clear-screen -w
  fi
}
zle -N _fancy_ctrl_z
bindkey '^Z' _fancy_ctrl_z

# ── Sudo Edit ─────────────────────────────────────────────────────

# Ctrl+X S to prepend sudo
_insert_sudo() {
  if [[ "$BUFFER" != sudo\ * ]]; then
    BUFFER="sudo $BUFFER"
    CURSOR=$((CURSOR + 5))
  fi
}
zle -N _insert_sudo
bindkey '^Xs' _insert_sudo

# ── Plugin-Dependent Bindings ─────────────────────────────────────────────

# These bindings require plugins to be loaded first.
# They check if the widget exists before binding.

# zsh-history-substring-search - Arrow key history search
if (( ${+widgets[history-substring-search-up]} )); then
  bindkey '^[[A' history-substring-search-up
  bindkey '^[[B' history-substring-search-down
  bindkey -M vicmd 'k' history-substring-search-up
  bindkey -M vicmd 'j' history-substring-search-down
fi
