---
version: "1.0.0"
last-updated: "2026-03-27"
maintainer: "zhengyu.li"
---
# Hooks Integration Principles

This project uses hooks for automated validation. Hooks complement rules by
providing **enforcement** rather than just guidance.

## Hook Philosophy

**Rules tell Claude what to think about; hooks force Claude what to do.**

| Aspect | Rules (this directory) | Hook Scripts |
|--------|------------------------|--------------|
| Purpose | Behavioral guidance | Technical enforcement |
| Loading | Into Claude context | Registered in settings.json |
| Execution | LLM understands, may ignore | Deterministic, cannot bypass |
| Use case | Tell Claude how to think | Force Claude to do something |

## Active Hooks

### Claude Code Hooks (PostToolUse, Stop)

**Purpose:** Validate changes during Claude sessions.

**What Claude should know:**
- Hooks run automatically after edits (PostToolUse) and at session end (Stop)
- Hooks catch syntax errors and configuration issues
- Claude should not duplicate hook checks in responses (trust the hooks)
- If hooks fail, Claude should help diagnose and fix the root cause

**When Claude should suggest adding hooks:**
- Recurring syntax errors in specific file types
- Common mistakes that could be caught automatically
- Project-specific validation needs (e.g., .elc detection for Emacs)

### Git Hooks (Pre-Commit)

**Purpose:** Enforce standards before code enters the repository.

**What Claude should know:**
- Git hooks are **mandatory** - they cannot be bypassed
- Pre-commit blocks .elc files (see `lang/elisp.md` for rationale)
- Claude should proactively clean .elc files before committing Emacs changes

**When Claude should suggest git hooks:**
- Files that should never be committed (.elc, secrets, build artifacts)
- Project-specific commit requirements

## Hook Design Principles

When Claude designs or suggests hooks:

1. **Single responsibility** - Each hook does one thing well
2. **Fast execution** - Hooks run on every operation, must be quick
3. **Clear output** - Use `[hook]` prefix, explain what failed and why
4. **Non-blocking vs blocking** - Use exit codes appropriately:
   - Exit 0: Success (allow operation)
   - Exit 1: Error (block operation)
   - Non-zero exit with warning: Log issue but allow operation

## Configuration Requirements

Claude must ensure:

- **Claude Code hooks**: Registered in `.claude/settings.json` using `$CLAUDE_PROJECT_DIR`
- **Git hooks**: Manually created in `.git/hooks/` directory

## Proactive Actions

Claude should proactively perform these actions without waiting for hooks:

### Before Committing Emacs Changes

```bash
# Remove stale .elc files from repository
find editor/emacs -name '*.elc' -delete

# Remove stale .elc files from stow target
find ~/.config/emacs/ -name '*.elc' -delete
```

See `lang/elisp.md` for detailed .elc cleanup procedure.

## See Also

- `git-workflow.md` - Commit message conventions
- `lang/elisp.md` - Emacs Lisp byte compilation and .elc cleanup
- `testing.md` - Testing workflows with hooks
- `security.md` - Secrets detection rules
