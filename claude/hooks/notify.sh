#!/bin/bash
# notify.sh
# =============================================================================
# Claude Code Hook - Desktop Notifications
#
# Location: ~/.claude/hooks/notify.sh
# Triggered by: Claude Code hook system (stop, notification, clear events)
# Dependencies: terminal-notifier, jq
#
# References:
#   1. Claude Code Hooks: https://docs.anthropic.com/en/docs/claude-code/hooks
#   2. terminal-notifier: https://github.com/julienXX/terminal-notifier
# =============================================================================

set -euo pipefail

# --- Configuration ---
PROJECT="$(basename "${PWD:-unknown-directory}")"
EVENT_TYPE="${1:-stop}"

# --- Dependency Validation ---
check_dependencies() {
    local missing=()

    if ! command -v terminal-notifier > /dev/null 2>&1; then
        missing+=("terminal-notifier")
    fi

    if ! command -v jq > /dev/null 2>&1; then
        missing+=("jq")
    fi

    if (( ${#missing[@]} > 0 )); then
        printf 'Missing dependencies: %s\n' "${missing[*]}" >&2
        return 1
    fi
    return 0
}

# --- Input Processing ---
read_input() {
    local input

    if ! IFS= read -r input; then
        printf 'Warning: Failed to read stdin input\n' >&2
        return 1
    fi

    if [[ -z "$input" ]]; then
        printf 'Warning: Empty stdin input\n' >&2
        return 1
    fi

    printf '%s\n' "$input"
}

extract_message() {
    local input="$1"
    local message
    message=$(printf '%s\n' "$input" | jq -r '.message // empty' 2>/dev/null)
    printf '%s\n' "${message:-No message provided}"
}

# --- Terminal Control Functions ---
set_tab_title() {
    printf '\033]0;%s\007' "$1" > /dev/tty
}

ring_bell() {
    printf '\a' > /dev/tty
}

# --- Notification Functions ---
send_notification() {
    local title="$1"
    local body="$2"
    local sound="${3:-Glass}"

    if ! terminal-notifier \
        -title "$title" \
        -message "$body" \
        -sound "$sound" \
        -activate com.mitchellh.ghostty \
        > /dev/null 2>&1; then
        printf 'Warning: terminal-notifier failed\n' >&2
        return 1
    fi
}

# --- Main Logic ---
validate_event_type() {
    local event="$1"
    case "$event" in
        stop|notification|clear)
            return 0
            ;;
        *)
            printf 'Invalid event type: %s\n' "$event" >&2
            return 1
            ;;
    esac
}

main() {
    if ! check_dependencies; then
        exit 1
    fi

    if ! validate_event_type "$EVENT_TYPE"; then
        exit 1
    fi

    local input
    input=$(read_input)

    if [[ -z "$input" ]]; then
        exit 1
    fi

    local message
    message=$(extract_message "$input")

    case "$EVENT_TYPE" in
        stop)
            ring_bell
            set_tab_title "⏸ $PROJECT"
            send_notification "Claude Code" "${PROJECT}: Task complete, awaiting input" "Glass"
            ;;
        notification)
            ring_bell
            set_tab_title "❓ $PROJECT"
            send_notification "Claude Code — Input Required" "${PROJECT}: ${message}" "Ping"
            ;;
        clear)
            set_tab_title "$PROJECT"
            ;;
    esac
}

main "$@"
