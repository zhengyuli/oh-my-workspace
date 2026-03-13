# python.zsh -*- mode: zsh; -*-
# Time-stamp: <2026-03-13 15:35:23 Friday by zhengyu.li>
#
# Python configuration via pyenv and virtualenvwrapper

# ==============================================================================
# pyenv Configuration
# ==============================================================================

if command -v pyenv &>/dev/null; then
    # PYENV_ROOT is set in zshenv (XDG-compliant path)

    # Initialize pyenv
    eval "$(pyenv init -)"

    # Initialize pyenv-virtualenv (if installed)
    if command -v pyenv-virtualenv-init &>/dev/null; then
        eval "$(pyenv virtualenv-init -)"
    fi
fi

# ==============================================================================
# virtualenvwrapper Configuration (Lazy Loading)
# ==============================================================================

# virtualenvwrapper settings (XDG-compliant with fallback)
export WORKON_HOME="${WORKON_HOME:-${XDG_DATA_HOME:-$HOME/.local/share}/virtualenvs}"
export PROJECT_HOME="${PROJECT_HOME:-$HOME/Projects}"

# Ensure WORKON_HOME exists
[[ -d "$WORKON_HOME" ]] || mkdir -p "$WORKON_HOME"

# Lazy loading for virtualenvwrapper
# Only source virtualenvwrapper.sh when needed (improves startup time)
_lazy_virtualenvwrapper() {
    # Only unfunction if the function actually exists to avoid warnings
    local _fn
    for _fn in workon mkvirtualenv rmvirtualenv lsvirtualenv cdvirtualenv \
               cdsitepackages lssitepackages toggleglobalsitepackages \
               cpvirtualenv setvirtualenvproject mkproject cdproject \
               mktmpenv wipeenv allvirtualenv; do
        (( ${+functions[$_fn]} )) && unfunction $_fn
    done
    unset _fn

    # Find virtualenvwrapper.sh
    local venvwrapper_sh=""
    if [[ -f "/usr/local/bin/virtualenvwrapper.sh" ]]; then
        venvwrapper_sh="/usr/local/bin/virtualenvwrapper.sh"
    elif [[ -f "/opt/homebrew/bin/virtualenvwrapper.sh" ]]; then
        venvwrapper_sh="/opt/homebrew/bin/virtualenvwrapper.sh"
    elif [[ -n "$(command -v virtualenvwrapper.sh)" ]]; then
        venvwrapper_sh="$(command -v virtualenvwrapper.sh)"
    fi

    if [[ -n "$venvwrapper_sh" ]]; then
        source "$venvwrapper_sh"
    else
        echo "Error: virtualenvwrapper.sh not found"
        return 1
    fi
}

# Create wrapper functions for lazy loading
# Using 'command' to call the real function after loading virtualenvwrapper
workon() {
    _lazy_virtualenvwrapper || return 1
    command workon "$@"
}

mkvirtualenv() {
    _lazy_virtualenvwrapper || return 1
    command mkvirtualenv "$@"
}

rmvirtualenv() {
    _lazy_virtualenvwrapper || return 1
    command rmvirtualenv "$@"
}

lsvirtualenv() {
    _lazy_virtualenvwrapper || return 1
    command lsvirtualenv "$@"
}

cdvirtualenv() {
    _lazy_virtualenvwrapper || return 1
    command cdvirtualenv "$@"
}

cdsitepackages() {
    _lazy_virtualenvwrapper || return 1
    command cdsitepackages "$@"
}

# ==============================================================================
# Helper Functions
# ==============================================================================

# Show Python environment info
py-info() {
    echo "Python: $(python --version 2>&1)"
    if command -v pyenv &>/dev/null; then
        echo "pyenv: $(pyenv --version)"
        echo "Current: $(pyenv version)"
        echo "Versions:"
        pyenv versions | head -10
    fi
    echo ""
    echo "Virtualenvs:"
    ls "$WORKON_HOME" 2>/dev/null | head -10 || echo "  (none)"
}

# Create a new virtualenv with current Python
mkvenv() {
    local name="${1:-$(basename "$PWD")}"
    mkvirtualenv "$name" || return 1
    # setvirtualenvproject may not exist in all virtualenvwrapper versions
    if typeset -f setvirtualenvproject >/dev/null 2>&1; then
        setvirtualenvproject "$WORKON_HOME/$name" "$PWD"
    fi
}

# Quick virtualenv activation by partial name
vex() {
    if [[ -z "$1" ]]; then
        echo "Usage: vex <partial_name>"
        echo "Available virtualenvs:"
        lsvirtualenv -b
        return 1
    fi

    local venv=$(lsvirtualenv -b | grep -i "$1" | head -1)
    if [[ -n "$venv" ]]; then
        workon "$venv"
    else
        echo "No virtualenv found matching '$1'"
        return 1
    fi
}
