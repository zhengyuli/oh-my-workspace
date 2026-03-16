# 60-keybinds.zsh
# =============================================================================
# Key Bindings
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 60 (after plugins, before tools)
#
# Prerequisites:
#   - None (standalone configuration)
#
# Responsibilities:
#   1. Configure editing mode (Emacs or Vi)
#   2. Configure history navigation key bindings
#   3. Configure word/line movement key bindings
#   4. Configure custom ZLE widgets (sudo toggle, cd parent, etc.)
#
# Do NOT add:
#   - Aliases → 20-aliases.zsh
#   - Plugin key bindings → 40-plugins.zsh (plugin-specific)
#   - Tool initialization → 70-tools.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Keymap -- Emacs style (default)
# -----------------------------------------------------------------------------
# Works well in terminals even if you use Vim for editing.
# To use vi mode: replace 'bindkey -e' with 'bindkey -v'
bindkey -e    # emacs keymap

# Vi mode (uncomment to enable; comment out 'bindkey -e' above)
# bindkey -v
# export KEYTIMEOUT=1   # reduce mode-switch delay to 0.1s

# -----------------------------------------------------------------------------
# History search
# -----------------------------------------------------------------------------
# Ctrl-P / Ctrl-N: prefix-based history search (built-in widgets)
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward

# Note: history-substring-search keybindings (arrow keys) are set in
# 40-plugins.zsh via Zinit's atload hook to ensure widgets exist before binding

# -----------------------------------------------------------------------------
# Word movement
# -----------------------------------------------------------------------------
bindkey -- '^[[1;5C' forward-word        # Ctrl-Right
bindkey -- '^[[1;5D' backward-word       # Ctrl-Left
bindkey '^[f'     forward-word        # Alt-f
bindkey '^[b'     backward-word       # Alt-b
bindkey '^[d'     kill-word           # Alt-d   delete word forward
bindkey '^H'      backward-kill-word  # Ctrl-Backspace  delete word backward
bindkey '^[[3;5~' kill-word           # Ctrl-Delete     delete word forward

# -----------------------------------------------------------------------------
# Line editing
# -----------------------------------------------------------------------------
bindkey '^A' beginning-of-line     # Ctrl-A  jump to start of line
bindkey '^E' end-of-line           # Ctrl-E  jump to end of line
bindkey '^K' kill-line             # Ctrl-K  delete from cursor to end
bindkey '^U' backward-kill-line    # Ctrl-U  delete from cursor to start
bindkey '^Y' yank                  # Ctrl-Y  paste (yank killed text)

# -----------------------------------------------------------------------------
# Editing
# -----------------------------------------------------------------------------
bindkey '^[[3~' delete-char        # Delete key
bindkey '^[^?' backward-kill-word  # Alt-Backspace

# Edit current command line in $EDITOR (Ctrl-X Ctrl-E)
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# -----------------------------------------------------------------------------
# Completion
# -----------------------------------------------------------------------------
bindkey '^I'   complete-word          # Tab       trigger completion
bindkey '^[[Z' reverse-menu-complete  # Shift-Tab cycle completion backwards

# -----------------------------------------------------------------------------
# Miscellaneous
# -----------------------------------------------------------------------------
bindkey '^L' clear-screen             # Ctrl-L  clear screen
bindkey ' '  magic-space              # Space   expand history substitution (!! etc.)

# -----------------------------------------------------------------------------
# Custom widget: toggle sudo prefix (Esc Esc)
# -----------------------------------------------------------------------------
# Press Esc twice to add or remove 'sudo' at the start of the line
sudo-command-line() {
  [[ -z $BUFFER ]] && zle up-history
  if [[ $BUFFER == sudo\ * ]]; then
    LBUFFER="${LBUFFER#sudo }"
  elif [[ $BUFFER == $'\x1b[200~'* ]]; then
    LBUFFER="sudo ${LBUFFER:8}"
  else
    LBUFFER="sudo $LBUFFER"
  fi
}
zle -N sudo-command-line
bindkey '\e\e' sudo-command-line

# -----------------------------------------------------------------------------
# Custom widget: jump to parent directory (Alt-,)
# -----------------------------------------------------------------------------
# Goes up one level without discarding the current buffer
# Note: Alt-. is reserved for insert-last-word (cycle through last arguments)
cdParent() { pushd .. > /dev/null; zle reset-prompt }
zle -N cdParent
bindkey '^[,' cdParent

# -----------------------------------------------------------------------------
# Custom widget: clear screen and scrollback buffer (Ctrl-/)
# -----------------------------------------------------------------------------
clear-screen-and-scrollback() {
  echoti civis
  printf '\n%.0s' {1..$LINES}
  zle clear-screen
  echoti cnorm
}
zle -N clear-screen-and-scrollback
bindkey '^_' clear-screen-and-scrollback
