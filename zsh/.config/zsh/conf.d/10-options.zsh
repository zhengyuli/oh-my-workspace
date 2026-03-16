# 10-options.zsh
# =============================================================================
# Shell Options (setopt / unsetopt)
#
# Loaded by: .zshrc (interactive shells only)
# Load order: 10 (after environment, before history)
#
# Prerequisites:
#   - None (standalone configuration)
#
# Responsibilities:
#   1. Configure directory navigation options (AUTO_CD, PUSHD_*)
#   2. Configure completion options (AUTO_MENU, COMPLETE_IN_WORD, etc.)
#   3. Configure globbing options (EXTENDED_GLOB, NULL_GLOB, etc.)
#   4. Configure input/output options (INTERACTIVE_COMMENTS, BEEP, etc.)
#   5. Configure job control options (AUTO_RESUME, NOTIFY, etc.)
#
# Do NOT add:
#   - History options → 15-history.zsh (dedicated history config)
#   - Aliases → 20-aliases.zsh
#   - Key bindings → 60-keybinds.zsh
# =============================================================================

# -----------------------------------------------------------------------------
# Directory Navigation
# -----------------------------------------------------------------------------
setopt AUTO_CD              # type a directory name to cd into it
setopt AUTO_PUSHD           # cd automatically pushes old dir onto the stack
setopt PUSHD_IGNORE_DUPS    # do not store duplicates in the directory stack
setopt PUSHD_SILENT         # suppress output of pushd / popd
setopt CDABLE_VARS          # allow cd to use variable names as directories

# -----------------------------------------------------------------------------
# Completion
# -----------------------------------------------------------------------------
setopt ALWAYS_TO_END        # move cursor to end after completion
setopt AUTO_LIST            # automatically list choices on ambiguous completion
setopt AUTO_PARAM_SLASH     # add trailing slash when completing directories
setopt COMPLETE_IN_WORD     # complete from both ends of a word
unsetopt FLOW_CONTROL       # disable Ctrl-S / Ctrl-Q flow control

# -----------------------------------------------------------------------------
# Globbing / Expansion
# -----------------------------------------------------------------------------
setopt EXTENDED_GLOB        # enable extended glob operators: ^, ~, #
setopt NULL_GLOB            # silently remove patterns with no matches
setopt NUMERIC_GLOB_SORT    # sort glob results numerically

# -----------------------------------------------------------------------------
# Input / Output
# -----------------------------------------------------------------------------
setopt INTERACTIVE_COMMENTS  # allow # comments in interactive shell
setopt RC_QUOTES             # allow '' inside single-quoted strings
setopt COMBINING_CHARS       # handle Unicode combining characters correctly
unsetopt BEEP                # no beep on error
unsetopt RM_STAR_SILENT      # require confirmation before rm *

# -----------------------------------------------------------------------------
# Job Control
# -----------------------------------------------------------------------------
setopt AUTO_RESUME          # resume a stopped job by typing its name
setopt LONG_LIST_JOBS       # display PID when listing jobs
setopt NOTIFY               # report background job status immediately
unsetopt BG_NICE            # do not run background jobs at lower priority
unsetopt CHECK_JOBS         # do not warn about running jobs on exit
unsetopt HUP                # do not send HUP to background jobs on shell exit
