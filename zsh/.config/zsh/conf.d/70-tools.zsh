# 70-tools.zsh
# =============================================================================
# Tool Shell Integrations
#
# Loaded by: Interactive shells (.zshrc)
# Load order: After 60-keybinds.zsh, before 90-platform.zsh
#
# Responsibilities:
#   1. Initialize shell integrations for tools (fzf, direnv)
#   2. Configure tool completions (bun, uv)
#   3. Set up development tooling that requires shell hooks
#
# Do NOT add: Environment variables, PATH changes, aliases
#             → Environment variables: 00-env.zsh
#             → PATH modifications: 05-path.zsh
#             → Aliases: 20-aliases.zsh
#
# Note: bun and uv don't require lazy loading - they're fast native binaries.
# =============================================================================

# -----------------------------------------------------------------------------
# bun -- JavaScript/TypeScript Runtime
# -----------------------------------------------------------------------------
# Bun is a fast native binary, no lazy loading needed.
# Completions are installed to $BUN_INSTALL/_bun
#
# Prerequisites: brew install oven-sh/bun/bun
# Config: $BUN_INSTALL (set in 00-env.zsh)
# Usage: bun install, bun run dev
# -----------------------------------------------------------------------------
if [[ -f "$BUN_INSTALL/_bun" ]]; then
  source "$BUN_INSTALL/_bun"
fi

# -----------------------------------------------------------------------------
# uv -- Python Package Manager
# -----------------------------------------------------------------------------
# uv is a fast native binary, no lazy loading needed.
# Completions are generated dynamically.
#
# Prerequisites: brew install uv
# Config: $UV_* variables (set in 00-env.zsh)
# Usage: uv add requests, uv venv
# -----------------------------------------------------------------------------
if command -v uv &>/dev/null; then
  eval "$(uv generate-shell-completion zsh)"
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

# -----------------------------------------------------------------------------
# zoxide -- Smart cd Command
# -----------------------------------------------------------------------------
# Fast directory jumping with frecency algorithm. Replaces z, autojump.
#
# Prerequisites: brew install zoxide
# Usage: z <dir> (jump to directory), zi (interactive selection)
# Documentation: https://github.com/ajeetdsouza/zoxide
# -----------------------------------------------------------------------------
if command -v zoxide &>/dev/null; then
  eval "$(zoxide init zsh --cmd z)"
fi
