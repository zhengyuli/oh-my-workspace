---
paths:
  - "**/*"
---

# Git Workflow

> This file extends repository-wide git workflow standards.

## Commit Message Format

All commits follow conventional commits format:

```
<type>: <description>

<optional body>
```

### Types

| Type | Usage |
|------|-------|
| `feat` | New feature |
| `fix` | Bug fix |
| `refactor` | Code restructuring without behavior change |
| `docs` | Documentation changes |
| `test` | Adding or modifying tests |
| `chore` | Maintenance tasks, dependency updates |
| `perf` | Performance improvements |
| `ci` | CI/CD configuration changes |

### Examples

```bash
# CORRECT - conventional commit format
git commit -m "feat: add starship prompt configuration"
git commit -m "fix: correct XDG path for ripgrep config"
git commit -m "refactor: consolidate zsh conf.d files"

# WRONG - vague or inconsistent format
git commit -m "update stuff"
git commit -m "Fixed the thing"
```

## Commit Practices

### Atomic Commits

- Each commit should represent a single logical change
- Avoid mixing unrelated changes in one commit
- Split large refactors into logical steps

### Commit Message Quality

```bash
# CORRECT - descriptive and explains why
git commit -m "feat(zsh): add idempotent PATH management

Use typeset -U to deduplicate path entries, allowing safe
re-sourcing of conf.d files without path bloat."

# WRONG - terse, doesn't explain context
git commit -m "add typeset"
```

## Pull Request Workflow

### Creating PRs

1. Analyze full commit history (not just latest commit)
2. Use `git diff [base-branch]...HEAD` to see all changes
3. Draft comprehensive PR summary
4. Include test plan with verification steps
5. Push with `-u` flag if new branch

### PR Description Template

```markdown
## Summary
- <bullet point 1>
- <bullet point 2>

## Test plan
- [ ] <verification step 1>
- [ ] <verification step 2>
```

## Pre-Commit Verification

Before any commit, verify:

- [ ] No cache files in staging area (see [xdg-paths.md](./xdg-paths.md))
- [ ] Configuration headers are compliant
- [ ] No alignment spaces violations
- [ ] All comments in English

Use `/pre-commit-check` command for automated verification.

## Branch Naming

| Pattern | Example |
|---------|---------|
| Feature | `feat/starship-config` |
| Bugfix | `fix/zsh-path-dedup` |
| Refactor | `refactor/consolidate-hooks` |
| Personal | `zhengyu/experimental-setup` |
