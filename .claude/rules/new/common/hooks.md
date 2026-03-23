# Hooks System

## Hook Types

| Hook Type | Purpose | Example |
|-----------|---------|---------|
| Pre-commit | Validation before commit | Linting, syntax checks |
| Post-commit | Actions after commit | Notifications |
| Pre-push | Validation before push | Test runs |

## Pre-Commit Verification

Before any commit:

```bash
# 1. Syntax check
zsh -n script.zsh

# 2. Check for cache files
git status --porcelain | grep -E '\.(zcompdump|swp|swo|bak|elc|pyc)$'

# 3. Verify symlinks
find -L ~/.config -type l -xtype l 2>/dev/null
```

## Shell Hooks

Zsh hook functions:
- `chpwd` - Run on directory change
- `preexec` - Run before command execution
- `precmd` - Run before prompt display

## Hook Best Practices

- Use named functions for debugging
- Check if hook already registered
- Keep hooks fast (run on every operation)
- Provide clear output on failure
