---
version: "1.0.0"
last-updated: "2026-03-27"
maintainer: "zhengyu.li"
---
# Claude Code Hooks Integration

This project uses two types of hooks for automated validation and verification:
1. **Claude Code hooks** - Integrated with Claude Code sessions
2. **Git hooks** - Integrated with git operations

## Hook Types Overview

| Type | Location | Trigger | Configuration |
|------|----------|---------|---------------|
| Claude Code hooks | `.claude/hooks/` | Claude Code tool events | `settings.json` or `settings.local.json` |
| Git hooks | `.git/hooks/` | Git operations (commit, push, etc.) | Automatic (git built-in) |

## Claude Code Hooks

### Available Hooks

#### PostToolUse: Syntax Validation

After editing source files, syntax is automatically validated:

| File Type | Command |
|-----------|---------|
| `.sh` | `bash -n` |
| `.zsh` | `zsh -n` |
| `.el` | `emacs --batch -f batch-byte-compile` |
| `.py` | `python3 -m py_compile` |

**Script:** `.claude/hooks/post-tool-use.sh`

#### Stop: Session End Check

At session end, verifies:
- No uncommitted config changes
- Last commit follows Conventional Commits

**Script:** `.claude/hooks/stop.sh`

### Configuration

Claude Code hooks must be explicitly registered in `settings.json` or `settings.local.json`. **There is no auto-discovery mechanism** - hook scripts in `.claude/hooks/` will not run unless registered.

**Project-level configuration** (`.claude/settings.json`):
```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": ".claude/hooks/post-tool-use.sh"
          }
        ]
      }
    ],
    "Stop": [
      {
        "matcher": ".*",
        "hooks": [
          {
            "type": "command",
            "command": ".claude/hooks/stop.sh"
          }
        ]
      }
    ]
  }
}
```

**Note:** Use relative paths from project root (`.claude/hooks/...`) instead of environment variables like `${CLAUDE_PROJECT_ROOT}`.

**Global configuration** (`~/.claude/settings.json`):
```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": "/path/to/oh-my-workspace/.claude/hooks/post-tool-use.sh"
          }
        ]
      }
    ]
  }
}
```

**Configuration Priority:**
- Project-level `.claude/settings.json` overrides global `~/.claude/settings.json`
- `.claude/settings.local.json` is for personal settings (typically in `.gitignore`)

### Matcher Patterns

The `matcher` field uses regex to match tool names:

| Pattern | Matches |
|---------|---------|
| `Edit` | Edit tool only |
| `Edit\|Write` | Edit or Write tools |
| `.*` | All tools |

### Hook Script Requirements

Hook scripts must:
- Be executable (`chmod +x script.sh`)
- Exit with code 0 on success
- Exit with non-zero code to block the operation
- Accept tool arguments via command line or environment variables

## Git Hooks

### Pre-Commit: .elc Detection and Validation

Automatically runs before each commit to:

1. **Block staged .elc files** - Prevents committing compiled Emacs files
2. **Warn on stale .elc** - Alerts when emacs/ changes are committed with stale .elc in stow target

**Script:** `.git/hooks/pre-commit` (not in version control, created during setup)

**Implementation:**
```bash
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

## Hook Installation

### Claude Code Hooks

**Important:** Hook scripts must be explicitly registered in `settings.json` to run. There is no auto-discovery.

1. **Verify hook scripts exist and are executable:**
   ```bash
   ls -la .claude/hooks/
   chmod +x .claude/hooks/*.sh
   ```

2. **Create or update `.claude/settings.json`:**
   ```bash
   # Create settings.json with hook configuration
   cat > .claude/settings.json << 'EOF'
   {
     "hooks": {
       "PostToolUse": [
         {
           "matcher": "Edit|Write",
           "hooks": [
             {
               "type": "command",
               "command": ".claude/hooks/post-tool-use.sh"
             }
           ]
         }
       ],
       "Stop": [
         {
           "matcher": ".*",
           "hooks": [
             {
               "type": "command",
               "command": ".claude/hooks/stop.sh"
             }
           ]
         }
       ]
     }
   }
   EOF
   ```

3. **Note:** `.claude/settings.json` may be in `.gitignore` if it contains project-specific settings. For team-shared hooks, consider committing a template or documenting the required configuration.

### Git Hooks

Git hooks are automatically installed during repository setup:

1. **Created by setup script:**
   ```bash
   ./setup.sh install --all
   ```

2. **Manual installation (if needed):**
   ```bash
   # Copy hook template
   cp .git/hooks/pre-commit.sample .git/hooks/pre-commit

   # Make executable
   chmod +x .git/hooks/pre-commit
   ```

## Troubleshooting

### Claude Code Hooks Not Running

**Symptom:** Hooks don't execute when using Claude Code.

**Solutions:**
1. Verify configuration in `settings.json` or `settings.local.json`
2. Check hook script is executable: `ls -la .claude/hooks/`
3. Verify matcher pattern matches the tool name
4. Check hook script path is correct (use `${CLAUDE_PROJECT_ROOT}` for project-relative paths)

### Git Hooks Not Running

**Symptom:** Pre-commit checks don't run.

**Solutions:**
1. Verify hook exists: `ls -la .git/hooks/pre-commit`
2. Make executable: `chmod +x .git/hooks/pre-commit`
3. Test manually: `.git/hooks/pre-commit`

### Hook Script Errors

**Symptom:** Hook script fails with errors.

**Solutions:**
1. Check script syntax: `bash -n script.sh`
2. Run script manually with test arguments
3. Check script has proper shebang: `#!/usr/bin/env bash`
4. Verify script uses `set -euo pipefail` for error handling

## Best Practices

### Hook Script Guidelines

1. **Keep scripts focused** - Each hook should do one thing well
2. **Provide clear output** - Use printf with [hook] prefix for messages
3. **Handle errors gracefully** - Use proper exit codes
4. **Document dependencies** - Comment required tools in script header
5. **Test before deploying** - Run hooks manually to verify behavior

### Configuration Guidelines

1. **Use project-level settings** - Prefer `.claude/settings.local.json` for project-specific hooks
2. **Use global settings sparingly** - Only for hooks that apply across all projects
3. **Document hook behavior** - Update this file when adding new hooks
4. **Version control hook scripts** - Store in `.claude/hooks/` and commit to repo

## See Also

- `git-workflow.md` - Commit message conventions
- `security.md` - Secrets detection rules
- `lang/elisp.md` - Emacs Lisp byte compilation
- `testing.md` - Testing workflows with hooks
