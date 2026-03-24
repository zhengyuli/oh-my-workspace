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
Warn when files exceed 800 lines (see `patterns.md` for size limits).

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
