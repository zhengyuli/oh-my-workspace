#!/bin/bash
# notify.sh
# =============================================================================
# Claude Code Hook - Desktop Notifications
#
# Location: ~/.claude/hooks/notify.sh
# Triggered by: Claude Code hook system
#   Stop, Notification, UserPromptSubmit, SessionStart, SessionEnd, PreToolUse
# Dependencies: terminal-notifier (brew install terminal-notifier)
#
# Tab title flow:
#   SessionStart / UserPromptSubmit → project
#   PreToolUse → ⚙ project
#   Stop → ⏸ project
#   Notification → ❓ project
#   SessionEnd → project
#
# References:
#   https://docs.anthropic.com/en/docs/claude-code/hooks
# =============================================================================
set -euo pipefail

# Sanitize project name: strip characters that break notification
# title rendering
PROJECT="$(basename "${PWD:-unknown-directory}" | tr -d '"'\''`')"
EVENT_TYPE="${1:-stop}"

# Max length for user-supplied strings passed to terminal-notifier
readonly MAX_MSG_LENGTH=200

# Parse both message and notification_type in a single python3 call.
# Outputs two lines: <notification_type>\n<message>
parse_input "$input")
  printf '%s\n' "$parsed" | sed -n '2p')
   printf '%s\n' "$message"[:${MAX_MSG_LENGTH}])
" 2>/dev/null
    if [[ $? -ne 0 ]]; then
            printf '\nNo message provided\n' >&2
        fi
    fi
}# Parse both message and notification_type in a single python3 call
# Outputs two lines: <notification_type>\n<message>
parse_input() {
    local input="$1"
    printf '%s\n' "$input" | python3 -c "
import sys, json
d = json.load(sys.stdin)
print(d.get('notification_type', ''))
msg = d.get('message', 'No message provided') or 'No message provided'
print(msg[:${MAX_MSG_LENGTH}])
" 2>/dev/null
    if [[ $? -ne 0 ]]; then
        printf '\nNo message provided\n'
    fi
}set_tab_title() {
    # -w /dev/tty: stdin is a pipe in hook context, so -t 0 is always false.
    # /dev/tty addresses the controlling terminal regardless of stdin.
    # if statement prevents set -e exit on terminal write failure.
    if [[ -w /dev/tty ]]; then
        if printf '\033]0;%s\007' "$1" > /dev/tty 2>/dev/null; then
            :  # success
        fi
    fi
}

ring_bell() {
    # if statement prevents set -e exit on terminal write failure.
    if [[ -w /dev/tty ]]; then
        if printf '\a' > /dev/tty 2>/dev/null; then
            :  # success
        fi
    fi
}

send_notification() {
    local title="$1"
    local body="$2"
    terminal-notifier \
        -title "$title" \
        -message "$body" \
        2>/dev/null & disown
}

validate_event_type() {
    case "$1" in
        stop|notification|prompt|sessionstart|sessionend|pretooluse) return 0 ;;
        *)
            printf 'Invalid event type: %s\n' "$1" >&2
            return 1
            ;;
    esac
}

main() {
    if ! validate_event_type "$EVENT_TYPE"; then
        exit 1
    fi

    # Events that need no stdin — handle before any read_input call
    case "$EVENT_TYPE" in
        sessionstart|prompt)
            set_tab_title "$PROJECT"
            return 0
            ;;
        pretooluse)
            set_tab_title "⚙ $PROJECT"
            return 0
            ;;
        stop)
            ring_bell
            set_tab_title "⏸ $PROJECT"
            send_notification "Claude Code" "${PROJECT}: Task complete, awaiting input"
            return 0
            ;;
        sessionend)
            set_tab_title "$PROJECT"
            send_notification "Claude Code" "${PROJECT}: Session ended"
            return 0
            ;;
    esac

    # Events that require stdin (notification only)
    local input
    input=$(read_input)
    if [[ $? -ne 0 ]]; then
        exit 0
    fi

    local parsed notification_type message
    parsed=$(parse_input "$input")
    notification_type=$(printf '%s\n' "$parsed" | sed -n '1p')
    message=$(printf '%s\n' "$parsed" | sed -n '2p')

    # notification event
    ring_bell
    set_tab_title "❓ $PROJECT"
    case "$notification_type" in
        permission_prompt)
            send_notification "Claude Code — Permission Required" \
                              "${PROJECT}: Approval needed to proceed"
            ;;
        idle_prompt|*)
            send_notification "Claude Code — Input Required" "${PROJECT}: ${message}"
            ;;
    esac
}

main "$@"
