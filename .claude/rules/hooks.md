# Claude Code Hooks Integration

Hook-based automation for configuration management workflows.

## Hook Types

### PreToolUse
**When:** Before tool execution
**Use:** Parameter validation, input sanitization, auto-formatting prep

### PostToolUse
**When:** After tool execution
**Use:** Auto-formatting, syntax validation, verification

### Stop
**When:** Session end
**Use:** Final verification, cleanup, commit verification

## Configuration Examples

Configure hooks in `.claude/settings.json` (project-level) or
`~/.claude/settings.json` (user-level).

### Environment Variables Available in Hook Scripts

| Variable | Description |
|----------|-------------|
| `CLAUDE_FILE_PATH` | Absolute path of the file being operated on |
| `CLAUDE_TOOL_NAME` | Name of the tool being invoked (e.g. `Write`, `Edit`) |
| `CLAUDE_TOOL_INPUT` | JSON-encoded tool input parameters |

### Example: PostToolUse Syntax Validation

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": ".claude/hooks/validate-syntax.sh"
          }
        ]
      }
    ]
  }
}
```

Hook script (`.claude/hooks/validate-syntax.sh`):

```bash
#!/usr/bin/env bash
# validate-syntax.sh --- PostToolUse syntax validation hook
#
# Location: .claude/hooks/validate-syntax.sh
# Usage:    Invoked automatically by Claude Code after Write/Edit

set -euo pipefail

file="${CLAUDE_FILE_PATH:-}"
[[ -z "$file" || ! -f "$file" ]] && exit 0

case "$file" in
  *.sh)  bash -n "$file" ;;
  *.zsh) zsh -n "$file" ;;
  *.el)  emacs --batch -f batch-byte-compile "$file" 2>/dev/null ;;
  *.py)  python3 -m py_compile "$file" ;;
esac
```

### Example: Stop Hook - Uncommitted File Reminder

```json
{
  "hooks": {
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": ".claude/hooks/session-end-check.sh"
          }
        ]
      }
    ]
  }
}
```

Hook script (`.claude/hooks/session-end-check.sh`):

```bash
#!/usr/bin/env bash
# session-end-check.sh --- Stop hook for session-end verification

set -euo pipefail

# Warn about uncommitted config changes
if git status --porcelain | grep -qE '\.(sh|zsh|el|py)$'; then
  printf 'Reminder: uncommitted config changes detected\n' >&2
fi

# Verify last commit follows Conventional Commits
if ! git log -1 --pretty=%B 2>/dev/null \
    | grep -qE '^(feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert)(\(.+\))?: .+'; then
  printf 'Warning: last commit message may not follow Conventional Commits\n' >&2
fi
```

### Hook Script Conventions

- Store hook scripts under `.claude/hooks/` (committed to repo)
- Make scripts executable: `chmod 755 .claude/hooks/*.sh`
- Hook scripts must exit `0` to allow the operation to proceed;
  non-zero exit **blocks** the tool call (PreToolUse) or is ignored
  (PostToolUse/Stop)
- Keep hooks fast — slow hooks degrade the interactive experience

### Troubleshooting Hooks Not Triggering

1. Verify `settings.json` is valid JSON (`python3 -m json.tool settings.json`)
2. Check the `matcher` regex matches the tool name exactly
3. Confirm hook script is executable (`ls -la .claude/hooks/`)
4. Test the script manually: `CLAUDE_FILE_PATH=path/to/file .claude/hooks/validate-syntax.sh`

## TodoWrite Best Practices

Use TodoWrite for tracking complex configuration changes spanning multiple files.

### When to Use
- Multi-file updates (3+ files)
- Complex installs with dependencies
- Breaking changes to configs
- Large-scale refactoring

### Benefits
- Reveals out-of-order steps
- Identifies missing dependencies
- Ensures correct granularity
- Provides rollback roadmap

## Auto-Formatting Hooks

### Syntax Validation by Language
- **Shell (Bash):** `bash -n "$CLAUDE_FILE_PATH"`
- **Shell (Zsh):** `zsh -n "$CLAUDE_FILE_PATH"`
- **Emacs Lisp:** `emacs --batch -f batch-byte-compile "$CLAUDE_FILE_PATH"`
- **Python:** `python -m py_compile "$CLAUDE_FILE_PATH"`

### File Size Checks
Warn when files exceed size limits (see `coding-style.md` for authoritative limits):
- Shell scripts: warn at 500 lines
- Python modules: warn at 600 lines
- Emacs Lisp: warn at 500 lines

### Header Verification
Ensure all config files have standard headers (see `ai-generation.md`).

## Pre-Commit Hooks

### Syntax Checking
Validate all modified configs before commit (see `lang/shell.md`, `lang/elisp.md`).

### Secrets Detection
Check for hardcoded secrets (see `security.md`):
```bash
if git diff --cached | grep -E '(password|secret|api_key|token).*=.*["\']'; then
  echo "Error: Potential secrets detected"
  exit 1
fi
```

### Integration with git-workflow.md
1. Pre-commit hooks validate syntax and check for secrets
2. Follow Conventional Commits format during commit (see `git-workflow.md`)
3. PostToolUse hooks verify commit was created correctly

## Session End Verification

### Final Checks
- Verify all modified configs tested
- Check for uncommitted critical files
- Ensure README updated if needed
- Verify commit messages follow Conventional Commits (see `git-workflow.md`)

### Example Verification Script
```bash
# Check for uncommitted config changes
git status --porcelain | grep -E '\.(sh|zsh|el|py)$' &&
  echo "Reminder: Test and commit modified configs"

# Verify commit message format
git log -1 --pretty=%B | grep -qE '^(feat|fix|docs|refactor)(\(.+\))?: .+' ||
  echo "Warning: Non-standard commit message"
```

## Permission Management

### Auto-Accept Guidelines

**Safe to auto-accept:**
- Read operations (Read, Glob, Grep)
- Syntax validation commands
- Git status/log commands
- Backup operations (cp, mv with .bak)

**Require confirmation:**
- Write operations (Edit, Write)
- Delete operations (rm, unlink)
- System modifications (install, uninstall)
- Network operations

## Hook Integration

### Multi-Level Validation
1. **Claude Code hooks:** PreToolUse/PostToolUse for immediate feedback
2. **Git hooks:** Pre-commit for repository-level validation
3. **User hooks:** Stop for final session checks

### Interaction with Existing Rules
- Language-specific rules in `lang/` extend (not replace) these hooks
- Specific overrides general: `lang/python.md` takes precedence for Python files
- Safety first: stricter validation wins in conflicts

### Common Issues
- **Hook not triggering:** Verify configuration syntax and matcher patterns
- **TodoWrite not working:** Check task status and dependencies
- **Syntax false positives:** Use language-appropriate validators

## Quick Reference

| Hook Type | When | Use |
|-----------|------|-----|
| PreToolUse | Before tool | Validation, prep |
| PostToolUse | After tool | Syntax check |
| Stop | Session end | Final verification |

| Operation | Auto-Accept | Confirm |
|-----------|-------------|---------|
| Read/Glob/Grep | ✓ | |
| Write/Edit | | ✓ |
| Git status/log | ✓ | |
| Delete/System ops | | ✓ |

## See Also

- `coding-style.md` - Code formatting standards
- `git-workflow.md` - Commit message conventions
- `lang/shell.md` - Shell-specific validation
- `lang/elisp.md` - Emacs Lisp validation
- `security.md` - Secrets detection rules
