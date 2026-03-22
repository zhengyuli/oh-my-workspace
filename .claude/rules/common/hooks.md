---
paths:
  - "**/*"
---

# Hooks System

> This file establishes repository-wide hook conventions.

## Hook Types

| Hook Type | Purpose | Example |
|-----------|---------|---------|
| Pre-commit | Validation before commit | Linting, syntax checks |
| Post-commit | Actions after commit | Notifications, deployment |
| Pre-push | Validation before push | Test runs |
| Post-merge | Cleanup after merge | Dependency updates |

## Pre-Commit Hooks

### Standard Checks

Before any commit, run applicable verification:

```bash
# Shell scripts
zsh -n script.zsh
shellcheck script.sh

# Emacs Lisp
emacs --batch -l package-lint.el -f package-lint-batch-and-exit *.el

# Configuration validation
/pre-commit-check
```

### Hook Implementation

```bash
#!/bin/bash
# .git/hooks/pre-commit

set -e

# Get staged files
staged=$(git diff --cached --name-only --diff-filter=ACM)

# Check shell scripts
shell_files=$(echo "$staged" | grep -E '\.(sh|zsh)$')
if [[ -n "$shell_files" ]]; then
    echo "$shell_files" | xargs shellcheck
fi

# Check elisp files
elisp_files=$(echo "$staged" | grep -E '\.el$')
if [[ -n "$elisp_files" ]]; then
    for f in $elisp_files; do
        emacs --batch -l package-lint.el -f package-lint-batch-and-exit "$f"
    done
fi
```

## Tool-Specific Hooks

### Shell

- `zsh -n` for syntax validation
- `shellcheck` for static analysis
- `shfmt` for formatting (optional)

See [../shell/hooks.md](../shell/hooks.md) for shell-specific hooks.

### Emacs Lisp

- `package-lint` for package compliance
- `byte-compile-file` for compilation check
- `ert-run-tests-batch` for test execution

See [../elisp/hooks.md](../elisp/hooks.md) for Emacs-specific hooks.

## Hook Best Practices

### Keep Hooks Fast

Hooks run on every commit. Keep them efficient:

- Use `--batch` mode for Emacs
- Skip expensive checks for small changes
- Cache results when possible

### Fail Fast

Exit immediately on first error:

```bash
set -e  # Exit on error
set -o pipefail  # Pipe errors
```

### Provide Clear Output

Tell users what's being checked and why it failed:

```bash
printf 'Checking %s...\n' "$file"
if ! check "$file"; then
    printf 'FAIL: %s failed check\n' "$file" >&2
    exit 1
fi
printf 'OK\n'
```

## Git Hook Management

### Hook Installation

```bash
# Copy hooks from repo
cp -r githooks/* .git/hooks/
chmod +x .git/hooks/*

# Or use core.hooksPath
git config core.hooksPath .githooks
```

### Hook Templates

Store hook templates in the repository for team consistency:

```
githooks/
├── pre-commit
├── pre-push
└── commit-msg
```

## Related Files

- For shell hooks, see [../shell/hooks.md](../shell/hooks.md)
- For elisp hooks, see [../elisp/hooks.md](../elisp/hooks.md)
- For testing, see [testing.md](./testing.md)
