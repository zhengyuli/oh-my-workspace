# ==============================================================================
# File: conf.d/functions.zsh
# Role: User utility functions and logging helpers for the interactive session
#
# Load context : Sourced by .zshrc after options.zsh
# Dependencies : None
# Side effects : Defines the following in the global namespace:
#                 _zsh_error  _zsh_warn  _zsh_info  _zsh_debug
#                 has  mkcd  path_contains  backup
# ==============================================================================

# ── Logging Helpers ───────────────────────────────────────────────────────────
: "${ZSH_LOG_LEVEL:=2}"  # 0 silent | 1 error | 2 warn | 3 info | 4 debug

_zsh_error() { print -u2 -- "[ERROR] $(date '+%T') zsh: $*"; }
_zsh_warn()  { (( ZSH_LOG_LEVEL >= 2 )) && print -u2 -- "[WARN]  $(date '+%T') zsh: $*"; return 0; }
_zsh_info()  { (( ZSH_LOG_LEVEL >= 3 )) && print -u2 -- "[INFO]  $(date '+%T') zsh: $*"; return 0; }
_zsh_debug() { (( ZSH_LOG_LEVEL >= 4 )) && print -u2 -- "[DEBUG] $(date '+%T') zsh: $*"; return 0; }

# ── Command Helpers ───────────────────────────────────────────────────────────

# True if $1 exists anywhere in $PATH
has() { command -v "$1" &>/dev/null }

# Create directory and cd into it
mkcd() {
  (( $# == 1 )) || { _zsh_error "Usage: mkcd <directory>"; return 1 }
  [[ -n "$1" ]]  || { _zsh_error "mkcd: argument must not be empty"; return 1 }
  mkdir -p -- "$1" && cd -- "$1"
}

# Check if directory is in PATH
path_contains() {
  (( $# == 1 )) || { _zsh_error "Usage: path_contains <directory>"; return 1 }
  local dir
  for dir in "${path[@]}"; do
    [[ "$dir" == "$1" ]] && return 0
  done
  return 1
}

# ── Backup Functions ───────────────────────────────────────────────────────────

# Create timestamped backup
backup() {
  (( $# >= 1 )) || { _zsh_error "Usage: backup <file> [suffix]"; return 1 }
  local file="$1"
  local suffix="${2:-bak}"
  [[ -f "$file" ]] || { _zsh_error "backup: '$file' not found"; return 1 }
  cp -a "$file" "${file}.${suffix}.$(date +%Y%m%d%H%M%S)"
}

# ── File Functions ─────────────────────────────────────────────────────────────

# Extract archive (smart extractor)
extract() {
  (( $# == 1 )) || { _zsh_error "Usage: extract <archive>"; return 1 }
  local file="$1"
  [[ -f "$file" ]] || { _zsh_error "extract: '$file' not found"; return 1 }

  case "$file" in
    *.tar.bz2)   tar xjf "$file"     ;;
    *.tar.gz)    tar xzf "$file"     ;;
    *.tar.xz)    tar xJf "$file"     ;;
    *.bz2)       bunzip2 "$file"     ;;
    *.rar)       unrar x "$file"     ;;
    *.gz)        gunzip "$file"      ;;
    *.tar)       tar xf "$file"      ;;
    *.tbz2)      tar xjf "$file"     ;;
    *.tgz)       tar xzf "$file"     ;;
    *.zip)       unzip "$file"       ;;
    *.Z)         uncompress "$file"  ;;
    *.7z)        7z x "$file"        ;;
    *.xz)        unxz "$file"        ;;
    *.exe)       cabextract "$file"  ;;
    *)           _zsh_error "extract: unknown format '$file'"; return 1 ;;
  esac
}

# ── Directory Functions ───────────────────────────────────────────────────────

# Create directory and cd into it
mkcd() {
  (( $# == 1 )) || { _zsh_error "Usage: mkcd <directory>"; return 1 }
  [[ -n "$1" ]] || { _zsh_error "mkcd: argument must not be empty"; return 1 }
  mkdir -p -- "$1" && cd -- "$1"
}

# ── Development Functions ─────────────────────────────────────────────────────

# JSON pretty print
json() {
  if [[ -z "$1" ]]; then
    python3 -m json.tool
  else
    python3 -m json.tool "$1"
  fi
}

# ── macOS Specific ────────────────────────────────────────────────────────────

if [[ "$(uname)" == "Darwin" ]]; then
  # Open man page in Preview
  pman() {
    (( $# == 1 )) || { _zsh_error "Usage: pman <command>"; return 1 }
    man -t "$1" | open -f -a Preview
  }

  # Quick Look a file
  ql() {
    (( $# == 1 )) || { _zsh_error "Usage: ql <file>"; return 1 }
    qlmanage -p "$1" >&/dev/null &
  }
fi

# ── Miscellaneous ─────────────────────────────────────────────────────────────

# Calculator
calc() {
  local pattern='^[0-9()+*/. ^-]+$'
  if [[ ! "$*" =~ $pattern ]]; then
    _zsh_error "Invalid arithmetic expression"
    return 1
  fi
  echo "$*" | bc -l
}

# Timer
timer() {
  local seconds="${1:-60}"
  echo "Timer set for $seconds seconds..."
  sleep "$seconds"
  printf '\aTimer done!\n'
}

# Colorized man pages (only if not already defined)
if (( ! ${+functions[man]} )); then
  man() {
    LESS_TERMCAP_md=$'\e[01;31m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[45;93m' \
    LESS_TERMCAP_se=$'\e[0m' \
    command man "$@"
  }
fi
