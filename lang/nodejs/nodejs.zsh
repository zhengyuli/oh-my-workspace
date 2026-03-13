# nodejs.zsh -*- mode: zsh; -*-
# Time-stamp: <2026-03-13 15:27:01 Friday by zhengyu.li>
#
# Node.js configuration via fnm

# ==============================================================================
# fnm Initialization
# ==============================================================================

if command -v fnm &>/dev/null; then
    # Check if fnm was already initialized by zprofile (login shell)
    # FNM_MULTISHELL_PATH is set by fnm env, so its presence indicates prior initialization
    if [[ -z "$FNM_MULTISHELL_PATH" ]]; then
        # Not initialized yet (non-login shell scenario): full initialization
        eval "$(fnm env --use-on-cd --version-file-strategy=recursive --shell zsh)"
    else
        # Already initialized by zprofile
        # Only register the chpwd hook for --use-on-cd functionality
        # Avoid calling fnm env again to prevent accumulating multishell paths
        autoload -U add-zsh-hook
        _fnm_autoload_hook() {
            fnm use --silent-if-unchanged 2>/dev/null
        }
        add-zsh-hook -D chpwd _fnm_autoload_hook 2>/dev/null
        add-zsh-hook chpwd _fnm_autoload_hook
    fi

    # ==============================================================================
    # npm Completion
    # ==============================================================================

    # Cache npm completion
    NPM_COMPLETION_CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/npm-completion"

    _npm_completion_cache() {
        local npm_path
        npm_path="$(command -v npm)" || return 1

        if [[ ! -f "$NPM_COMPLETION_CACHE" || \
              "$NPM_COMPLETION_CACHE" -ot "$npm_path" ]]; then
            # Ensure cache directory exists
            local cache_dir="${NPM_COMPLETION_CACHE:h}"
            [[ -d "$cache_dir" ]] || mkdir -p "$cache_dir" 2>/dev/null || return 1

            # Use temp file for atomic write (avoid race condition)
            local tmp_cache="${NPM_COMPLETION_CACHE}.$$.tmp"
            if npm completion > "$tmp_cache" 2>/dev/null && [[ -s "$tmp_cache" ]]; then
                mv "$tmp_cache" "$NPM_COMPLETION_CACHE"
            else
                rm -f "$tmp_cache"
                return 1
            fi
        fi
        source "$NPM_COMPLETION_CACHE"
    }

    # Try to load npm completion
    if command -v npm &>/dev/null; then
        _npm_completion_cache 2>/dev/null
    fi
fi
