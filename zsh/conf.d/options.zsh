# options.zsh -*- mode: zsh; -*-
# Time-stamp: <2026-03-11 16:00:00 Tuesday by zhengyu.li>
#
# Shell options and basic settings

# ==============================================================================
# Directory Stack
# ==============================================================================

setopt AUTO_PUSHD           # Push old directory onto stack
setopt PUSHD_IGNORE_DUPS    # Don't push duplicates
setopt PUSHD_MINUS          # Swap +/- meanings for cd

# ==============================================================================
# Globbing
# ==============================================================================

setopt EXTENDED_GLOB        # Enable extended glob patterns
setopt GLOB_DOTS            # Glob patterns match dotfiles
setopt NOMATCH              # Error if no match found

# ==============================================================================
# I/O
# ==============================================================================

setopt CORRECT              # Spelling correction for commands
setopt INTERACTIVE_COMMENTS # Allow comments in interactive shell

# ==============================================================================
# Jobs
# ==============================================================================

setopt AUTO_RESUME          # Resume disowned jobs
setopt NOTIFY               # Report job status immediately
setopt NO_BG_NICE           # Don't nice background jobs
setopt NO_HUP               # Don't kill background jobs on exit

# ==============================================================================
# Other
# ==============================================================================

setopt NO_BEEP              # Never beep
setopt MULTIOS              # Allow multiple redirections
