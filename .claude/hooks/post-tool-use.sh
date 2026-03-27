#!/usr/bin/env bash
# =============================================================================
# PostToolUse Hook - Syntax Validation
#
# Location: .claude/hooks/post-tool-use.sh
# Usage:    Automatically triggered after editing files
#
# Validates syntax for shell, elisp, and python files after editing.
# =============================================================================

set -euo pipefail

# Read tool invocation context from stdin (Claude Code hook protocol).
# PostToolUse delivers JSON: {"tool_name": "...", "tool_input": {"file_path": "..."}, ...}
_input=$(cat)
FILE_PATH=$(printf '%s\n' "$_input" \
  | python3 -c \
    "import sys,json; d=json.load(sys.stdin); \
     print(d.get('tool_input',{}).get('file_path',''))" \
  2>/dev/null || true)

# Skip if no file path in payload
if [[ -z "$FILE_PATH" ]]; then
    exit 0
fi

# Extract file extension
EXT="${FILE_PATH##*.}"

# Validate syntax based on file type
_validate_syntax() {
    local -r file="$1"
    local -r ext="$2"

    case "$ext" in
        sh|bash)
            if command -v bash >/dev/null 2>&1; then
                if ! bash -n "$file" 2>&1; then
                    printf '[hook] Syntax error in %s\n' "$file" >&2
                    return 1
                fi
                printf '[hook] ✓ Shell syntax valid: %s\n' "$file"
            fi
            ;;

        zsh)
            if command -v zsh >/dev/null 2>&1; then
                if ! zsh -n "$file" 2>&1; then
                    printf '[hook] Syntax error in %s\n' "$file" >&2
                    return 1
                fi
                printf '[hook] ✓ Zsh syntax valid: %s\n' "$file"
            fi
            ;;

        el)
            if command -v emacs >/dev/null 2>&1; then
                if ! emacs --batch -f batch-byte-compile "$file" 2>&1 | grep -v "Wrote\|Compiling"; then
                    printf '[hook] ✓ Emacs Lisp syntax valid: %s\n' "$file"
                    # Clean up the .elc file created during validation
                    rm -f "${file}c"
                fi
            fi
            ;;

        py)
            if command -v python3 >/dev/null 2>&1; then
                if ! python3 -m py_compile "$file" 2>&1; then
                    printf '[hook] Syntax error in %s\n' "$file" >&2
                    return 1
                fi
                printf '[hook] ✓ Python syntax valid: %s\n' "$file"
            fi
            ;;

        *)
            # No validation for this file type
            ;;
    esac

    return 0
}

# Run validation
_validate_syntax "$FILE_PATH" "$EXT"
