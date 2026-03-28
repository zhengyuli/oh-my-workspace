# 60-keybinds.zsh
# Time-stamp: <2026-03-28 14:40:43 Saturday by zhengyuli>
# =============================================================================
# Key Bindings
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 60 (after 50-prompt.zsh, before 70-tools.zsh)
#
# Prerequisites:
#   - None (standalone configuration)
#
# Responsibilities:
#   1. Configure editing mode (Emacs or Vi)
#   2. Configure history navigation key bindings
#   3. Configure word/line movement key bindings
#   4. Configure custom ZLE widgets (sudo toggle etc.)
#
# Do NOT add: aliases, plugin key bindings, tool initialization
#             → Aliases in 20-aliases.zsh
#             → Plugin key bindings in 40-plugins.zsh (plugin-specific)
#             → Tool initialization in 70-tools.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Keymap -- Emacs style (default)
# -----------------------------------------------------------------------------
# Works well in terminals even if you use Vim for editing.
# To use vi mode: replace 'bindkey -e' with 'bindkey -v'
# emacs keymap
bindkey -e

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
bindkey '^H' backward-kill-word
# Ctrl-Delete     delete word forward
bindkey '^[[3;5~' kill-word

# -----------------------------------------------------------------------------
# Line editing
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

# Edit current command line in $EDITOR (Ctrl-X Ctrl-E)
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# -----------------------------------------------------------------------------
# Completion
# -----------------------------------------------------------------------------
# Tab (^I) is owned by fzf-tab (40-plugins.zsh) - do NOT rebind ^I here.
# fzf-tab registers fzf-tab-complete synchronously at plugin load; any
# subsequent bindkey '^I' would silently break fzf-tab's UI.
# Shift-Tab key events inside fzf's popup go to fzf, not zsh's
# reverse-menu-complete; no zsh binding needed.

# -----------------------------------------------------------------------------
# Miscellaneous
# -----------------------------------------------------------------------------
# Ctrl-L  clear screen
bindkey '^L' clear-screen

# -----------------------------------------------------------------------------
# Custom widget: toggle sudo prefix (Esc Esc)
# -----------------------------------------------------------------------------
# Press Esc twice to add or remove 'sudo' at the start of the line
sudo-command-line() {
  if [[ -z "$BUFFER" ]]; then
    zle up-history
  fi
  if [[ "$BUFFER" == sudo\ * ]]; then
    LBUFFER="${LBUFFER#sudo }"
  else
    LBUFFER="sudo $LBUFFER"
  fi
}
zle -N sudo-command-line
bindkey '\e\e' sudo-command-line
