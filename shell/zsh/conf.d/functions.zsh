# functions.zsh -*- mode: zsh; -*-
# Time-stamp: <2026-03-13 15:55:46 Friday by zhengyu.li>
#
# Utility functions

# ==============================================================================
# File Operations
# ==============================================================================

# Create a backup of a file
backup() {
    if [[ -z "$1" ]]; then
        echo "Usage: backup <file>" >&2
        return 1
    fi
    if [[ ! -e "$1" ]]; then
        echo "Error: '$1' does not exist" >&2
        return 1
    fi
    cp "$1" "$1.backup-$(date +%Y%m%d_%H%M%S)"
}

# Extract various archive formats
extract() {
    if [[ -z "$1" ]]; then
        echo "Usage: extract <archive>"
        return 1
    fi

    if [[ ! -f "$1" ]]; then
        echo "Error: '$1' is not a valid file"
        return 1
    fi

    case "$1" in
        *.tar.bz2)   tar xjf "$1"     ;;
        *.tar.gz)    tar xzf "$1"     ;;
        *.tar.xz)    tar xJf "$1"     ;;
        *.tar.zst)   tar --zstd -xf "$1" ;;
        *.bz2)       bunzip2 "$1"     ;;
        *.rar)       unrar x "$1"     ;;
        *.gz)        gunzip "$1"      ;;
        *.tar)       tar xf "$1"      ;;
        *.tbz2)      tar xjf "$1"     ;;
        *.tgz)       tar xzf "$1"     ;;
        *.zip)       unzip "$1"       ;;
        *.Z)         uncompress "$1"  ;;
        *.7z)        7z x "$1"        ;;
        *.dmg)       hdiutil attach "$1" ;;
        *)           echo "Error: Unknown archive format '$1'" ;;
    esac
}

# Find files by name
ff() {
    if [[ -z "$1" ]]; then
        echo "Usage: ff <pattern>"
        return 1
    fi
    find . -type f -iname "*$1*" 2>/dev/null
}

# Find directories by name (use fdir to avoid conflict with fd file finder)
fdir() {
    if [[ -z "$1" ]]; then
        echo "Usage: fdir <pattern>"
        return 1
    fi
    find . -type d -iname "*$1*" 2>/dev/null
}

# ==============================================================================
# Process Management
# ==============================================================================

# Find process by name
psg() {
    if [[ -z "$1" ]]; then
        echo "Usage: psg <pattern>"
        return 1
    fi
    ps aux | grep -i "$1" | grep -v grep
}

# Kill process by name
killp() {
    if [[ -z "$1" ]]; then
        echo "Usage: killp <pattern>"
        return 1
    fi
    local pids=($(psg "$1" | awk '{print $2}'))
    if [[ ${#pids[@]} -gt 0 ]]; then
        echo "Killing: ${pids[*]}"
        kill "${pids[@]}"
    else
        echo "No processes found matching '$1'"
    fi
}

# ==============================================================================
# Network
# ==============================================================================

# Check if a port is in use
port() {
    if [[ -z "$1" ]]; then
        echo "Usage: port <port_number>"
        return 1
    fi
    lsof -i :"$1"
}

# ==============================================================================
# Development
# ==============================================================================

# Generate a random string
randstr() {
    local length=${1:-16}
    LC_ALL=C tr -dc 'A-Za-z0-9' </dev/urandom | head -c "$length"
    echo
}

# JSON pretty print
jsonpp() {
    if [[ -z "$1" ]]; then
        python3 -m json.tool
    else
        python3 -m json.tool "$1"
    fi
}

# ==============================================================================
# macOS Specific
# ==============================================================================

if [[ "$(uname)" == "Darwin" ]]; then
    # Open man page in Preview
    pman() {
        man -t "$1" | open -f -a Preview
    }

    # Quick Look a file
    ql() {
        qlmanage -p "$1" >&/dev/null &
    }
fi

# ==============================================================================
# Miscellaneous
# ==============================================================================

# Calculator
calc() {
    if [[ ! "$*" =~ ^[0-9+\-*/\(\)\.\^[:space:]]+$ ]]; then
        echo "Error: Invalid arithmetic expression" >&2
        return 1
    fi
    echo "$*" | bc -l
}

# Timer
timer() {
    local seconds=${1:-60}
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
