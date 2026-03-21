#!/bin/bash
# notify.sh
# =============================================================================
# Claude Code Hook - Desktop Notifications
#
# Location: ~/.claude/hooks/notify.sh
# Triggered by: Claude Code hook system (stop, notification, clear events)
# Dependencies: terminal-notifier (brew install terminal-notifier)
#
# References:
#   1. Claude Code Hooks: https://docs.anthropic.com/en/docs/claude-code/hooks
# =============================================================================
set -euo pipefail

# --- Configuration ---
PROJECT="$(basename "${PWD:-unknown-directory}")"
EVENT_TYPE="${1:-stop}"

# --- Input Processing ---
read_input() {
    # Claude Code sends a single-line JSON payload; read one line and return.
    # Do NOT use cat here — it waits for EOF and will block the hook indefinitely.
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
    # python3 is available on macOS; more robust than sed for JSON parsing
    message=$(printf '%s\n' "$input" \
        | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('message',''))" \
        2>/dev/null || true)
    printf '%s\n' "${message:-No message provided}"
}

# --- Terminal Control Functions ---
set_tab_title() {
    # Use -w /dev/tty instead of -t 0: stdin is a pipe in hook context so
    # -t 0 is always false; /dev/tty refers to the controlling terminal
    # independently of what stdin is
    [[ -w /dev/tty ]] || return 0
    printf '\033]0;%s\007' "$1" > /dev/tty
}

ring_bell() {
    [[ -w /dev/tty ]] || return 0
    printf '\a' > /dev/tty
}

# --- Notification Functions ---
detect_terminal_bundle_id() {
    # Each terminal injects $TERM_PROGRAM into the shell environment;
    # Claude Code inherits it from the shell that launched it
    case "${TERM_PROGRAM:-}" in
        ghostty)      printf 'com.mitchellh.ghostty' ;;
        iTerm.app)    printf 'com.googlecode.iterm2' ;;
        WarpTerminal) printf 'dev.warp.Warp-Stable' ;;
        *)            printf 'com.apple.Terminal' ;;
    esac
}

send_notification() {
    local title="$1"
    local body="$2"
    local bundle_id
    bundle_id=$(detect_terminal_bundle_id)
    # -sender:   controls which app icon appears on the notification
    # -activate: controls which app is focused when the notification is clicked
    terminal-notifier \
        -title "$title" \
        -message "$body" \
        -sender "$bundle_id" \
        2>/dev/null || true
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
    if ! validate_event_type "$EVENT_TYPE"; then
        exit 1
    fi

    # clear event needs no stdin; handle early to avoid spurious read failures
    if [[ "$EVENT_TYPE" == "clear" ]]; then
        set_tab_title "$PROJECT"
        return 0
    fi

    local input
    if ! input=$(read_input); then
        exit 0
    fi

    local message
    message=$(extract_message "$input")

    case "$EVENT_TYPE" in
        stop)
            ring_bell
            set_tab_title "⏸ $PROJECT"
            send_notification "Claude Code" "${PROJECT}: Task complete, awaiting input"
            ;;
        notification)
            ring_bell
            set_tab_title "❓ $PROJECT"
            send_notification "Claude Code — Input Required" "${PROJECT}: ${message}"
            ;;
    esac
}

main "$@"
