# ==============================================================================
# File: conf.d/tools.zsh
# Role: External tool initialization (Homebrew, Node.js, Python)
#
# Load context : Sourced after functions.zsh (needs logging helpers)
# Dependencies : functions.zsh (for _zsh_warn, _zsh_error)
# Side effects : Initializes fnm, pyenv, adds to fpath
# ==============================================================================

# ── Homebrew ─────────────────────────────────────────────────────────────────

if [[ "$(uname)" == "Darwin" ]]; then
  # Add Homebrew completion to fpath
  if [[ -d "/opt/homebrew/share/zsh/site-functions" ]]; then
    fpath=("/opt/homebrew/share/zsh/site-functions" $fpath)
  elif [[ -d "/usr/local/share/zsh/site-functions" ]]; then
    fpath=("/usr/local/share/zsh/site-functions" $fpath)
  fi
fi

# Helper: Show dependencies of a brew package
brdeps() {
  (( $# == 1 )) || { _zsh_error "Usage: brdeps <package>"; return 1 }
  brew deps --tree "$1"
}

# Helper: Show leaf packages
brleaves() { brew leaves }

# Helper: Show package info
brinfo() {
  (( $# == 1 )) || { _zsh_error "Usage: brinfo <package>"; return 1 }
  brew info "$1"
}

# Helper: Update and upgrade
brupdate() {
  brew update && brew upgrade && brew cleanup
}

# Helper: List packages with descriptions
brlist() {
  brew list | while read formula; do
    echo -n "$formula: "
    brew desc "$formula" 2>/dev/null || echo "no description"
  done
}

# Helper: Search packages
brsearch() {
  (( $# == 1 )) || { _zsh_error "Usage: brsearch <pattern>"; return 1 }
  brew search "$1"
}

# ── Node.js (fnm) ─────────────────────────────────────────────────────────────

if command -v fnm &>/dev/null; then
  # Check if fnm was already initialized by zprofile (login shell)
  # FNM_MULTISHELL_PATH is set by fnm env, so its presence indicates prior init
  if [[ -z "$FNM_MULTISHELL_PATH" ]]; then
    # Not initialized yet (non-login shell): full initialization
    eval "$(fnm env --use-on-cd --version-file-strategy=recursive --shell zsh)"
  else
    # Already initialized by zprofile
    # Only register the chpwd hook for --use-on-cd functionality
    autoload -U add-zsh-hook
    _fnm_autoload_hook() { fnm use --silent-if-unchanged 2>/dev/null }
    add-zsh-hook -d chpwd _fnm_autoload_hook 2>/dev/null
    add-zsh-hook chpwd _fnm_autoload_hook
  fi

  # npm completion caching
  _npm_completion_cache_path="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/npm-completion"

  _npm_completion() {
    local npm_path
    npm_path="$(command -v npm)" || return 1

    if [[ ! -f "${_npm_completion_cache_path}" || \
          "${_npm_completion_cache_path}" -ot "${npm_path}" ]]; then
      local cache_dir="${_npm_completion_cache_path:h}"
      [[ -d "$cache_dir" ]] || mkdir -p "$cache_dir" 2>/dev/null || return 1

      local tmp="${_npm_completion_cache_path}.$$.tmp"
      if npm completion > "$tmp" 2>/dev/null && [[ -s "$tmp" ]]; then
        mv "$tmp" "${_npm_completion_cache_path}"
      else
        rm -f "$tmp"
        return 1
      fi
    fi
    source "${_npm_completion_cache_path}"
  }

  command -v npm &>/dev/null && _npm_completion 2>/dev/null
fi

# ── Python (pyenv) ────────────────────────────────────────────────────────────

if command -v pyenv &>/dev/null; then
  eval "$(pyenv init -)"

  if command -v pyenv-virtualenv-init &>/dev/null; then
    eval "$(pyenv virtualenv-init -)"
  fi
fi

# virtualenvwrapper settings
export WORKON_HOME="${WORKON_HOME:-${XDG_DATA_HOME:-$HOME/.local/share}/virtualenvs}"
export PROJECT_HOME="${PROJECT_HOME:-$HOME/Projects}"
[[ -d "$WORKON_HOME" ]] || mkdir -p "$WORKON_HOME"

# Lazy loading for virtualenvwrapper
_lazy_virtualenvwrapper() {
  local _fn
  for _fn in workon mkvirtualenv rmvirtualenv lsvirtualenv cdvirtualenv \
             cdsitepackages lssitepackages toggleglobalsitepackages \
             cpvirtualenv setvirtualenvproject mkproject cdproject \
             mktmpenv wipeenv allvirtualenv; do
    (( ${+functions[$_fn]} )) && unfunction $_fn
  done
  unset _fn

  local venvwrapper_sh=""
  if [[ -f "/opt/homebrew/bin/virtualenvwrapper.sh" ]]; then
    venvwrapper_sh="/opt/homebrew/bin/virtualenvwrapper.sh"
  elif [[ -f "/usr/local/bin/virtualenvwrapper.sh" ]]; then
    venvwrapper_sh="/usr/local/bin/virtualenvwrapper.sh"
  elif [[ -n "$(command -v virtualenvwrapper.sh)" ]]; then
    venvwrapper_sh="$(command -v virtualenvwrapper.sh)"
  fi

  if [[ -n "$venvwrapper_sh" ]]; then
    source "$venvwrapper_sh"
  else
    _zsh_error "virtualenvwrapper.sh not found"
    return 1
  fi
}

# Stub functions for lazy loading
workon()      { _lazy_virtualenvwrapper || return 1; workon "$@" }
mkvirtualenv(){ _lazy_virtualenvwrapper || return 1; mkvirtualenv "$@" }
rmvirtualenv(){ _lazy_virtualenvwrapper || return 1; rmvirtualenv "$@" }
lsvirtualenv(){ _lazy_virtualenvwrapper || return 1; lsvirtualenv "$@" }
cdvirtualenv(){ _lazy_virtualenvwrapper || return 1; cdvirtualenv "$@" }
cdsitepackages(){ _lazy_virtualenvwrapper || return 1; cdsitepackages "$@" }

# Helper: Show Python environment info
py-info() {
  echo "Python: $(python --version 2>&1)"
  if command -v pyenv &>/dev/null; then
    echo "pyenv: $(pyenv --version)"
    echo "Current: $(pyenv version)"
    echo "Versions:"; pyenv versions | head -10
  fi
  echo ""; echo "Virtualenvs:"
  ls "$WORKON_HOME" 2>/dev/null | head -10 || echo "  (none)"
}

# Helper: Create venv with project association
mkvenv() {
  local name="${1:-$(basename "$PWD")}"
  mkvirtualenv "$name" || return 1
  typeset -f setvirtualenvproject >/dev/null 2>&1 && \
    setvirtualenvproject "$WORKON_HOME/$name" "$PWD"
}

# Helper: Quick virtualenv activation by partial name
vex() {
  (( $# == 1 )) || { _zsh_error "Usage: vex <partial_name>"; return 1 }
  local venv=$(lsvirtualenv -b | grep -i "$1" | head -1)
  if [[ -n "$venv" ]]; then
    workon "$venv"
  else
    _zsh_error "No virtualenv found matching '$1'"
    return 1
  fi
}
