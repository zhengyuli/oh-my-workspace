# 70-tools.zsh
# =============================================================================
# Lazy-Loaded Development Tools
#
# Loaded by: Interactive shells (.zshrc)
# Load order: After 60-keybinds.zsh, before 90-platform.zsh
#
# Responsibilities:
#   1. Lazy-load version managers (pyenv, fnm) to improve shell startup
#   2. Initialize shell integrations for tools (fzf, direnv)
#   3. Configure development tooling that requires shell hooks
#
# Do NOT add: Environment variables, PATH changes, aliases
#             → Environment variables: 00-env.zsh
#             → PATH modifications: 05-path.zsh
#             → Aliases: 20-aliases.zsh
#
# Performance Note:
#   Lazy loading reduces shell startup by 300-500ms per tool. These version
#   managers are only initialized when first called, not on every shell.
# =============================================================================

# -----------------------------------------------------------------------------
# pyenv -- Python Version Manager
# -----------------------------------------------------------------------------
# Lazy load to avoid ~200ms startup penalty. On first call, initialize pyenv
# and forward arguments to the real command.
#
# Prerequisites: brew install pyenv
# Config: $PYENV_ROOT (set in 00-env.zsh)
# Usage: pyenv local 3.12.0
# -----------------------------------------------------------------------------
if [[ -x "${PYENV_ROOT:-$HOME/.pyenv}/bin/pyenv" ]] || command -v pyenv &>/dev/null; then
  pyenv() {
    unset -f pyenv
    eval "$(command pyenv init -)"
    pyenv "$@"
  }
fi

# -----------------------------------------------------------------------------
# fnm -- Fast Node Manager
# -----------------------------------------------------------------------------
# Rust-based Node version manager, 40x faster than nvm. Lazy load for
# instant shell startup while keeping .nvmrc auto-switching capability.
#
# Prerequisites: brew install fnm
# Config: $FNM_DIR (set in 00-env.zsh)
# Usage: fnm install 20 && fnm use 20
# -----------------------------------------------------------------------------
if command -v fnm &>/dev/null; then
  fnm() {
    unset -f fnm
    eval "$(command fnm env --use-on-cd --shell zsh)"
    fnm "$@"
  }
fi

# -----------------------------------------------------------------------------
# fzf -- Fuzzy Finder
# -----------------------------------------------------------------------------
# Load key bindings (Ctrl+R, Ctrl+T) and completion triggers (** + Tab).
# Not lazy-loaded because key bindings must be available immediately.
#
# Prerequisites: brew install fzf
# Key bindings: Ctrl+R (history), Ctrl+T (files), Alt+C (cd)
# Completion: **<Tab> triggers fzf completion
# -----------------------------------------------------------------------------
if command -v fzf &>/dev/null; then
  # Try Homebrew path first, fallback to system path (Linux)
  _fzf_prefix="$(brew --prefix fzf 2>/dev/null)" || _fzf_prefix="/usr/share/fzf"

  if [[ -f "$_fzf_prefix/shell/key-bindings.zsh" ]]; then
    source "$_fzf_prefix/shell/key-bindings.zsh"
  fi
  if [[ -f "$_fzf_prefix/shell/completion.zsh" ]]; then
    source "$_fzf_prefix/shell/completion.zsh"
  fi

  unset _fzf_prefix
fi

# -----------------------------------------------------------------------------
# direnv -- Per-Directory Environment
# -----------------------------------------------------------------------------
# Automatically load/unload environment variables when entering/leaving
# directories with .envrc files. Hook is lightweight (<5ms), no lazy load.
#
# Prerequisites: brew install direnv
# Usage: echo 'export API_KEY=xxx' > .envrc && direnv allow
# Documentation: https://direnv.net/
# -----------------------------------------------------------------------------
if command -v direnv &>/dev/null; then
  eval "$(direnv hook zsh)"
fi
