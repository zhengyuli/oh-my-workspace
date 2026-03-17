# 05-path.zsh
# =============================================================================
# PATH / FPATH / MANPATH / INFOPATH Management
#
# Loaded by: .zprofile (login) and .zshrc (interactive)
# Load order: 05 (after 00-env.zsh sets tool paths)
#
# Responsibilities:
#   1. Configure executable search path (PATH)
#   2. Configure function search path (FPATH) for completions/autoload
#   3. Configure manual page search path (MANPATH)
#   4. Configure info page search path (INFOPATH)
#   5. Ensure idempotency via typeset -gU (unique, global)
#
# Prerequisites:
#   - 00-env.zsh must set: CARGO_HOME, GOPATH, BUN_INSTALL, XDG_* variables
#
# Do NOT add: tool initialization, aliases, environment variables
#             → Tool initialization in 70-tools.zsh
#             → Aliases in 20-aliases.zsh
#             → Environment variables in 00-env.zsh
#
# PATH Priority (highest to lowest):
#   1. User-local binaries (~/.local/bin) - explicit user choice, uv tool install
#   2. Development tool binaries - user-installed dev tools (cargo, go, bun)
#   3. Package manager binaries - Homebrew, system packages
#   4. System sbin - admin commands (lower priority)
#   5. Existing system PATH - fallback
# =============================================================================

# -----------------------------------------------------------------------------
# Deduplication (must come first for idempotency)
# -----------------------------------------------------------------------------
# -g: global scope (affects all contexts)
# -U: unique (remove duplicates, keep first occurrence)
typeset -gU path fpath manpath infopath cdpath

# -----------------------------------------------------------------------------
# PATH -- executable search path
# Highest priority first; $path preserves existing system entries
# -----------------------------------------------------------------------------
# Glob qualifier (N-/): N=nullglob (no error if missing), -/=<only dirs>
path=(
  # === Priority 1: User-local binaries (HIGHEST) ===
  # User's explicit local installations, must override everything
  # uv tool install, pip install --user, etc.
  "$HOME/.local/bin"(N-/)

  # === Priority 2: Development tool binaries ===
  # User-installed language/tool binaries
  "$CARGO_HOME/bin"(N-/)         # Rust crates (cargo install)
  "$GOPATH/bin"(N-/)             # Go packages (go install)
  "$BUN_INSTALL/bin"(N-/)        # Bun packages (bun install -g)

  # === Priority 3: Package manager binaries ===
  # Homebrew (Apple Silicon - M1/M2/M3)
  /opt/homebrew/bin(N-/)
  # Homebrew (Intel Mac / Linux)
  /usr/local/bin(N-/)

  # === Priority 4: System sbin (admin commands) ===
  # Lower priority - rarely needed in daily development
  /opt/homebrew/sbin(N-/)
  /usr/local/sbin(N-/)

  # === Priority 5: Existing system PATH (LOWEST - fallback) ===
  $path
)

# -----------------------------------------------------------------------------
# FPATH -- function/completion search path
# Must be set before compinit (30-completion.zsh)
# -----------------------------------------------------------------------------
fpath=(
  # Custom completion scripts (highest priority)
  "$ZDOTDIR/completions"(N-/)

  # Autoloaded custom functions
  "$ZDOTDIR/functions"(N-/)

  # Homebrew completions (HOMEBREW_PREFIX from 00-env.zsh; Apple Silicon + Intel)
  "$HOMEBREW_PREFIX/share/zsh/site-functions"(N-/)
  "$HOMEBREW_PREFIX/share/zsh-completions"(N-/)

  # Preserve existing fpath (fallback)
  $fpath
)

# -----------------------------------------------------------------------------
# MANPATH -- manual page search path
# -----------------------------------------------------------------------------
manpath=(
  # Homebrew man pages (HOMEBREW_PREFIX from 00-env.zsh; Apple Silicon + Intel)
  "$HOMEBREW_PREFIX/share/man"(N-/)

  # System man pages
  /usr/local/share/man(N-/)
  /usr/share/man(N-/)

  # Preserve existing manpath
  $manpath
)

# -----------------------------------------------------------------------------
# INFOPATH -- info page search path (GNU info system)
# -----------------------------------------------------------------------------
infopath=(
  # Homebrew info pages (HOMEBREW_PREFIX from 00-env.zsh; Apple Silicon + Intel)
  "$HOMEBREW_PREFIX/share/info"(N-/)

  # System info pages
  /usr/local/share/info(N-/)
  /usr/share/info(N-/)

  # Preserve existing infopath
  $infopath
)

# -----------------------------------------------------------------------------
# Autoload custom functions
# Must come after fpath is set above
# (N:t) -- (N)=nullglob silently skip if no matches, (:t)=filename only
# Guard required: 'autoload -Uz' with no arguments lists all functions to stdout
# -----------------------------------------------------------------------------
_func_files=($ZDOTDIR/functions/*(N:t))
if (( ${#_func_files} )); then
  autoload -Uz "${_func_files[@]}"
fi
unset _func_files
