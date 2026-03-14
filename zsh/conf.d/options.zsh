# ==============================================================================
# File: conf.d/options.zsh
# Role: Shell options, history configuration, and basic settings
#
# Load context : Sourced first by .zshrc (no dependencies)
# Dependencies : XDG_STATE_HOME (from zshenv)
# Side effects : Creates ${XDG_STATE_HOME}/zsh/ if needed for history
# ==============================================================================

# ── History Configuration ─────────────────────────────────────────────────────

HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/zsh/history"
HISTSIZE=100000
SAVEHIST=100000

# Ensure directory exists (wrapped in anonymous function for local scope)
() {
  local hist_dir="${HISTFILE:h}"
  [[ -d "$hist_dir" ]] || mkdir -p "$hist_dir" 2>/dev/null || HISTFILE="$HOME/.zsh_history"
}

# ── History Options ───────────────────────────────────────────────────────────

setopt BANG_HIST                # Treat '!' specially in expansion
setopt EXTENDED_HISTORY         # Write timestamp to history file
setopt SHARE_HISTORY            # Share history across sessions
setopt HIST_EXPIRE_DUPS_FIRST   # Delete duplicates when full
setopt HIST_IGNORE_DUPS         # Don't record duplicates
setopt HIST_IGNORE_ALL_DUPS     # Delete old duplicates
setopt HIST_FIND_NO_DUPS        # Don't show duplicates in search
setopt HIST_IGNORE_SPACE        # Don't record space-prefixed commands
setopt HIST_SAVE_NO_DUPS        # Don't save duplicates
setopt HIST_REDUCE_BLANKS       # Remove extra blanks

# ── Directory Stack ───────────────────────────────────────────────────────────

setopt AUTO_PUSHD               # Push old directory onto stack
setopt PUSHD_IGNORE_DUPS        # Don't push duplicates
setopt PUSHD_MINUS              # Swap +/- meanings for cd

# ── Globbing ─────────────────────────────────────────────────────────────────

setopt EXTENDED_GLOB            # Enable extended glob patterns
setopt GLOB_DOTS                # Glob patterns match dotfiles
setopt NOMATCH                  # Error if no match found

# ── I/O ───────────────────────────────────────────────────────────────────────

setopt CORRECT                  # Spelling correction for commands
setopt INTERACTIVE_COMMENTS     # Allow comments in interactive shell

# ── Jobs ─────────────────────────────────────────────────────────────────────

setopt AUTO_RESUME              # Resume disowned jobs
setopt NOTIFY                   # Report job status immediately
setopt NO_BG_NICE               # Don't nice background jobs
setopt NO_HUP                   # Don't kill background jobs on exit

# ── Other ─────────────────────────────────────────────────────────────────────

setopt NO_BEEP                  # Never beep
setopt MULTIOS                  # Allow multiple redirections
