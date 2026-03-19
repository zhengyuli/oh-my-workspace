# 40-plugins.zsh
# Time-stamp: <2026-03-19 22:07:04 Thursday by zhengyu.li>
# =============================================================================
# Zinit Plugin Management
#
# Loaded by: Interactive shells (.zshrc)
# Load order: 40 (after 30-completion.zsh, before 50-prompt.zsh)
#
# Responsibilities:
#   1. Configure plugin behavior variables (autosuggestions, history-substring-search)
#   2. Bootstrap and configure Zinit plugin manager
#   3. Load core plugins (syntax highlighting, autosuggestions, etc.)
#   4. Load utility plugins (autopair)
#   5. Configure plugin-specific settings
#
# Do NOT add: Tool initialization (pyenv, fnm, direnv, etc.)
#             → Put these in 70-tools.zsh (separate concern)
# =============================================================================

# -----------------------------------------------------------------------------
# Zinit bootstrap
# Data directory follows XDG: $XDG_DATA_HOME/zinit/
# -----------------------------------------------------------------------------
# Idempotency guard - prevent double loading
if (( ${+ZINIT_INITIALIZED} )); then
  return 0
fi

ZINIT_HOME="$XDG_DATA_HOME/zinit/zinit.git"

if [[ ! -f "$ZINIT_HOME/zinit.zsh" ]]; then
  print -P "%F{cyan}Installing Zinit...%f"
  if mkdir -p "$(dirname "$ZINIT_HOME")"; then
    if ! git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"; then
      print -P "%F{red}Failed to clone Zinit%f"
      return 1
    fi
  else
    print -P "%F{red}Failed to create Zinit directory%f"
    return 1
  fi
fi

# Guard against missing file even after clone (e.g. disk full)
if [[ ! -f "$ZINIT_HOME/zinit.zsh" ]]; then
  return 1
fi
source "$ZINIT_HOME/zinit.zsh"
ZINIT_INITIALIZED=1

# -----------------------------------------------------------------------------
# Plugin loading order (matches actual execution order)
# -----------------------------------------------------------------------------
#
# Execution timeline:
#   During startup (sync):
#     1. fzf-tab        - must own ^I before any turbo plugin can override it
#   After prompt (turbo, by wait suffix letter):
#     2. zsh-completions  [0a] - register completions early for zicdreplay
#     3. history-substring-search, autosuggestions, autopair  [0b]
#     4. fast-syntax-highlighting  [0c, LAST] - wraps all ZLE widgets;
#                                               must run after others register theirs
#
# Why fzf-tab is sync (no wait):
#   fzf-tab binds ^I (Tab) to fzf-tab-complete. If loaded with turbo (wait),
#   ^I would be unbound until after the first prompt, causing broken Tab on
#   initial input. Sync load guarantees it owns ^I from the first prompt.
#
# Why FSH is last with wait"0c":
#   FSH wraps all currently-registered ZLE widgets at load time. Loading it
#   before autosuggestions or history-substring-search means those plugins'
#   widgets are registered unwrapped. Using wait"0c" ensures FSH runs after
#   all wait"0b" plugins have registered their widgets.
#
# Why zicompinit in FSH's atinit:
#   zinit intercepts compdef calls during startup and records them. After all
#   wait"0a/0b" plugins load (including zsh-completions which adds many compdef
#   calls), FSH's atinit runs zicompinit -C (fast re-init using cache) and
#   zicdreplay to replay the recorded compdef calls into the live system.
# -----------------------------------------------------------------------------

# --- Sync: fzf-tab (must be first and synchronous) ---
# Load before turbo plugins to ensure ^I ownership is established immediately.
# fzf's completion.zsh is intentionally NOT loaded (see 70-tools.zsh comment).
zinit light Aloxaf/fzf-tab

# --- Turbo 0a: completions (early, before zicdreplay in FSH) ---
# blockf: tells zinit not to add plugin to fpath (avoids triggering compinit
# rebuild); completions are still available via zinit's own path management.
zinit ice wait"0a" lucid blockf
zinit light zsh-users/zsh-completions

# --- Turbo 0b: interactive enhancement plugins ---

# History substring search
# Config vars are read at highlight render time (not at plugin load), so timing
# is not critical. Set here for co-location with the plugin declaration -
# mirrors the autosuggestions pattern and keeps future changes in one place.
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=green,fg=black,bold'
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=white,bold'
# HISTORY_SUBSTRING_SEARCH_FUZZY=1
# disabled: perf cost on large histories
# Keybindings via atload: widgets must exist before bindkey can reference them
zinit ice wait"0b" lucid atload'
  bindkey "^[[A" history-substring-search-up
  bindkey "^[[B" history-substring-search-down
  bindkey "^[OA" history-substring-search-up
  bindkey "^[OB" history-substring-search-down
'
zinit light zsh-users/zsh-history-substring-search

# Autosuggestions from history
# Configuration variables must be set BEFORE this ice/light pair so they are
# available when _zsh_autosuggest_start reads them (even in turbo, the vars
# are already in scope because they are set synchronously above).
# The ! prefix on atload tells zinit to redraw the prompt after activation,
# so the first suggestion appears without requiring a keypress.
# history first, then completion
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
# no suggestion for long buffers
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=40
# Doom One comment color
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#7f8c98'
zinit ice wait"0b" lucid atload"!_zsh_autosuggest_start"
zinit light zsh-users/zsh-autosuggestions

# autopair -- auto-close brackets and quotes
zinit ice wait"0b" lucid
zinit light hlissner/zsh-autopair

# --- Turbo 0c: fast-syntax-highlighting (MUST be last) ---
# atinit runs before the plugin code:
#   ZINIT[COMPINIT_OPTS]=-C  → use cached dump (fast, no security check)
#   ZINIT[ZCOMPDUMP_PATH]    → set XDG-compliant zcompdump path (zicompinit ignores -d flag)
#   zicompinit               → re-run compinit to pick up any new completions
#   zicdreplay               → replay compdef calls captured from turbo plugins
zinit ice wait"0c" lucid atinit"ZINIT[COMPINIT_OPTS]=-C; ZINIT[ZCOMPDUMP_PATH]=\"\${XDG_CACHE_HOME}/zsh/zcompdump\"; zicompinit; zicdreplay"
zinit light zdharma-continuum/fast-syntax-highlighting

# -----------------------------------------------------------------------------
# Plugin configuration
# -----------------------------------------------------------------------------

# fzf-tab -- disable default completion menu in favor of fzf popup
# Note: do NOT use escape sequences (%F{red}...%f) in 'descriptions format'
# (fzf-tab limitation - they appear as literal text, not ANSI color codes).
zstyle ':completion:*' menu no
zstyle ':completion:*:descriptions' format '[%d]'

# Git branch sorting
zstyle ':completion:*:git-checkout:*' sort false

# --- fzf-tab Preview Configurations ---
# NOTE: Variable quoting here is intentional:
#   - Double quotes (command style): $USER expands at source time ✓
#   - Single quotes (fzf-preview style): $word/$group expand at completion time by fzf-tab ✓
# Do NOT add extra quoting - it would break fzf-tab's internal substitution.

# Global preview window size - right panel, 55% width, line-wrap enabled
# Per-command fzf-flags below override this where needed (e.g. kill: down:3)
zstyle ':fzf-tab:*' fzf-flags --preview-window=right:55%:wrap

# cd: preview directory
zstyle ':fzf-tab:complete:cd:*' fzf-preview \
  'eza -1 --color=always --icons "$realpath"'

# Files: preview content - scoped to argument completion only to avoid
# triggering on command names, users, hostnames, etc.
zstyle ':fzf-tab:complete:*:argument*' fzf-preview \
  'bat --color=always --style=plain --line-range=:50 "$realpath" 2>/dev/null \
   || eza -1 --color=always --icons "$realpath" 2>/dev/null'

# Environment variables: preview value
zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset):*' \
  fzf-preview 'echo ${(P)word}'

# kill: preview process info
# Use BSD-compatible -p flag (macOS ps does not support --pid)
zstyle ':completion:*:*:*:*:processes' \
  command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  '[[ $group == "[process ID]" ]] && ps -p $word -o comm= 2>/dev/null'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' \
  fzf-flags --preview-window=down:3:wrap

# brew: preview package info
zstyle ':fzf-tab:complete:brew-(install|uninstall|search|info):*' fzf-preview \
  'brew info $word 2>/dev/null'

# man: preview man page
zstyle ':fzf-tab:complete:(\\|*/|)man:*' fzf-preview \
  'man $word 2>/dev/null | head -50'

# git: preview diff/log
zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview \
  'git diff --color=always $word'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview \
  'git log --oneline --color=always $word'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
  'git log --oneline --color=always $word 2>/dev/null \
   || git show --color=always $word 2>/dev/null'

# Switch groups with , and .
zstyle ':fzf-tab:*' switch-group ',' '.'
