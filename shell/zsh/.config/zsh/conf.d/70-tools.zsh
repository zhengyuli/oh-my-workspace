# 70-tools.zsh -*- mode: sh; -*-
# Time-stamp: <2026-03-20 00:00:00 Friday by zhengyu.li>
# =============================================================================
# Tool Shell Integrations
#
# Loaded by: Interactive shells (.zshrc)
# Load order: 70 (after 60-keybinds.zsh, before 99-local.zsh)
#
# Prerequisites:
#   - 00-env.zsh (XDG paths, tool environment variables)
#   - 30-completion.zsh (compinit for tool completions)
#   - 40-plugins.zsh (Zinit for carapace)
#
# Responsibilities:
#   1. Set tool-specific environment variables (Homebrew, etc.)
#   2. Initialize shell integrations for tools (fzf, direnv)
#   3. Configure tool completions (bun, uv, carapace)
#   4. Set up development tooling that requires shell hooks
#
# Do NOT add: Core environment variables, PATH changes, aliases
#             → Core environment: 00-env.zsh
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
if [[ -f "$BUN_INSTALL/_bun" ]]; then
  source "$BUN_INSTALL/_bun"
fi

# -----------------------------------------------------------------------------
# uv -- Python Package Manager
# -----------------------------------------------------------------------------
# uv is a fast native binary, no lazy loading needed.
# Completions are generated dynamically and cache to file with mtime check.
#
# Prerequisites: brew install uv
# Config: uv natively supports XDG directories (no UV_* overrides needed)
# Usage: uv add requests, uv venv
if command -v uv &>/dev/null; then
  _uv_comp="$XDG_CACHE_HOME/zsh/uv-completion.zsh"

  if [[ ! -f "$_uv_comp" ]] || [[ "$(command -v uv)" -nt "$_uv_comp" ]]; then
    mkdir -p "${_uv_comp:h}"
    uv generate-shell-completion zsh >! "$_uv_comp"
  fi
  source "$_uv_comp"
  unset _uv_comp
fi

# -----------------------------------------------------------------------------
# carapace -- Universal Shell Completion
# -----------------------------------------------------------------------------
# Provides fzf-compatible completions for 1600+ CLI tools automatically.
# Replaces the need to manually write _<cmd> completion functions.
#
# Prerequisites: brew install carapace
# Bridges: falls back to native zsh/bash/fish completions when available
# Usage: any supported command + Tab (e.g. claude --<Tab>, gh <Tab>)
# Supported commands: carapace --list
# Documentation: https://carapace.sh
#
# Note: Must be sourced AFTER compinit (handled by 30-completion.zsh) and
#       AFTER fzf-tab (handled by 40-plugins.zsh turbo wait). Loading here
#       in 70-tools.zsh satisfies both constraints.
if command -v carapace &>/dev/null; then
  _carapace_comp="$XDG_CACHE_HOME/zsh/carapace-completion.zsh"
  if [[ ! -f "$_carapace_comp" ]] || \
     [[ "$(command -v carapace)" -nt "$_carapace_comp" ]]; then
    mkdir -p "${_carapace_comp:h}"
    carapace _carapace zsh >! "$_carapace_comp"
  fi
  source "$_carapace_comp"
  unset _carapace_comp
fi

# -----------------------------------------------------------------------------
# fzf -- Fuzzy Finder
# -----------------------------------------------------------------------------
# Load key bindings only. Must load AFTER fzf-tab (40-plugins.zsh).
#
# Prerequisites: brew install fzf
# Key bindings: Ctrl+R (history), Ctrl+T (files), Alt+C (cd)
#
# Note: fzf ships two shell integration files:
#   key-bindings.zsh - Ctrl-R / Ctrl-T / Alt-C widgets ← load this
#   completion.zsh - **<Tab> trigger + rebinds ^I ← do NOT load
#
# completion.zsh is intentionally skipped. fzf-tab (40-plugins.zsh) is a
# superset: it replaces the entire Tab completion UI with fzf, making
# fzf's own completion.zsh redundant. Loading it would rebind ^I to
# fzf-completion and require a fragile bind/rebind workaround.
#
# Note: HOMEBREW_PREFIX is defined in 00-env.zsh
if command -v fzf &>/dev/null; then
  _fzf_prefix="$HOMEBREW_PREFIX/opt/fzf"

  if [[ -f "$_fzf_prefix/shell/key-bindings.zsh" ]]; then
    source "$_fzf_prefix/shell/key-bindings.zsh"
  fi

  unset _fzf_prefix
fi

# -----------------------------------------------------------------------------
# zoxide -- Smart cd Command
# -----------------------------------------------------------------------------
# Fast directory jumping with frecency algorithm. Replaces z, autojump.
#
# Prerequisites: brew install zoxide
# Usage: z <dir> (jump to directory), zi (interactive selection)
# Documentation: https://github.com/ajeetdsouza/zoxide
if command -v zoxide &>/dev/null; then
  eval "$(zoxide init zsh --cmd z)"
  # zoxide generates zi() for interactive fzf directory selection.
  # zinit defined 'alias zi=zinit' earlier; in zsh, alias expansion precedes
  # function lookup, so the alias silently shadows the function, making zi
  # unreachable. Remove the alias so zi works as the interactive directory
  # picker. Use 'zinit' directly for zinit commands.
  unalias zi 2>/dev/null
fi

# -----------------------------------------------------------------------------
# git -- Redirect --global writes to config.local
# -----------------------------------------------------------------------------
# ~/.config/git/config is the shared config (version-controlled).
# ~/.config/git/config.local holds machine-specific overrides (not tracked).
# config includes config.local via [include], so reads work transparently.
# This wrapper redirects writes so git config --global never touches config.
git() {
  if [[ "$1" == config && "$2" == --global ]]; then
    local -a args=("${@:3}")
    local mode=  # 'r' = read, 'w' = write
    for a in "${args[@]}"; do
      case "$a" in
        --get|--get-all|--get-regexp|--get-urlmatch|--list|--show-origin|--show-scope)
          mode=r; break ;;
        --add|--unset|--unset-all|--rename-section|--remove-section|\
          --replace-all|--edit|-e|--comment)
          mode=w; break ;;
      esac
    done
    if [[ -z "$mode" ]]; then
      # No explicit flag: key value = write, bare key = read
      local -a pos=()
      local skip=0
      for a in "${args[@]}"; do
        if (( skip )); then skip=0; continue; fi
        case "$a" in
          --default|--type|--value-type|--fixed-value|--pattern|--expires-after)
            skip=1 ;;
          --*) ;;
          *) pos+=("$a") ;;
        esac
      done
      (( ${#pos[@]} >= 2 )) && mode=w || mode=r
    fi
    if [[ "$mode" == w ]]; then
      command git config -f "$XDG_CONFIG_HOME/git/config.local" "${args[@]}"
    else
      command git config --global --includes "${args[@]}"
    fi
  else
    command git "$@"
  fi
}
