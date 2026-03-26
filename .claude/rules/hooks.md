# Claude Code Hooks Integration

This project uses Claude Code hooks for automated validation and verification.

## Active Hooks

### PostToolUse: Syntax Validation

After editing source files, syntax is automatically validated:

| File Type | Command |
|-----------|---------|
| `.sh` | `bash -n` |
| `.zsh` | `zsh -n` |
| `.el` | `emacs --batch -f batch-byte-compile` |
| `.py` | `python3 -m py_compile` |

### Stop: Session End Check

At session end, verifies:
- No uncommitted config changes
- Last commit follows Conventional Commits

## Pre-Commit Hooks

### Stale .elc Detection

Block commits with staged `.elc` files. Warn when `emacs/` changes are
committed while stale `.elc` files exist in `~/.config/emacs/`.

```bash
# .git/hooks/pre-commit (excerpt)

# Block staged .elc files
if git diff --cached --name-only | grep -q '\.elc$'; then
  printf 'error: staged .elc files detected\n' >&2
  exit 1
fi

# Warn on stale .elc in stow target
if git diff --cached --name-only | grep -q '^emacs/.*\.el$'; then
  stale=$(find "${HOME}/.config/emacs" -name '*.elc' 2>/dev/null)
  if [[ -n "$stale" ]]; then
    printf 'warning: stale .elc files in ~/.config/emacs/\n' >&2
  fi
fi
```

See `lang/elisp.md` for cleanup procedure.

### Secrets Detection

Check for hardcoded secrets before commit (see `security.md`).

## Hook Script Location

Scripts are stored in `.claude/hooks/` and committed to the repo.

## See Also

- `git-workflow.md` - Commit message conventions
- `security.md` - Secrets detection rules
- `lang/elisp.md` - Emacs Lisp byte compilation
