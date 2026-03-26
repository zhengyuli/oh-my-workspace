#!/usr/bin/env bash
# =============================================================================
# Stop Hook - Session End Verification
#
# Location: .claude/hooks/stop.sh
# Usage:    Automatically triggered at session end
#
# Verifies:
# - No uncommitted configuration changes
# - Last commit follows Conventional Commits format
# =============================================================================

set -euo pipefail

# Check for uncommitted changes in configuration files
_check_uncommitted_changes() {
    local -r changes=$(git status --porcelain 2>/dev/null | grep -E '^\?\?|^M|^A' || true)

    if [[ -n "$changes" ]]; then
        printf '[hook] ⚠ Uncommitted changes detected:\n' >&2
        printf '%s\n' "$changes" >&2
        printf '\nConsider committing these changes before ending the session.\n' >&2
    fi
}

# Check if last commit follows Conventional Commits format
_check_commit_format() {
    local -r last_msg=$(git log -1 --pretty=format:"%s" 2>/dev/null || true)

    if [[ -z "$last_msg" ]]; then
        return 0
    fi

    # Conventional Commits pattern: type(scope): description
    local -r pattern='^(feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert)(\([a-z0-9-]+\))?:\s.+'

    if [[ ! "$last_msg" =~ $pattern ]]; then
        printf '[hook] ⚠ Last commit does not follow Conventional Commits format:\n' >&2
        printf '  %s\n\n' "$last_msg" >&2
        printf 'Expected format: type(scope): description\n' >&2
        printf 'Example: feat(zsh): add git aliases for status and log\n' >&2
    fi
}

# Main verification
printf '[hook] Running session end verification...\n\n'

_check_uncommitted_changes
_check_commit_format

printf '\n[hook] Session end verification complete.\n'
