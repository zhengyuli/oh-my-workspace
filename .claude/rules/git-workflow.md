---
version: "1.0.0"
last-updated: "2026-03-26"
maintainer: "zhengyu.li"
---
# Git Workflow

Git conventions based on industry best practices.

## Commit Message Format

Follow [Conventional Commits](https://www.conventionalcommits.org/) v1.0.0.

### Format

```
type(scope): description

[optional body]

[optional footer(s)]
```

### Types

| Type | Description |
|------|-------------|
| `feat` | New feature |
| `fix` | Bug fix |
| `docs` | Documentation changes |
| `style` | Code style changes (formatting, semicolons) |
| `refactor` | Code refactoring without feature changes |
| `perf` | Performance improvements |
| `test` | Adding or modifying tests |
| `build` | Build system changes |
| `ci` | CI/CD changes |
| `chore` | Maintenance tasks |
| `revert` | Revert a previous commit |

### Examples

```bash
feat(zsh): add git aliases for status and log
fix(emacs): resolve startup error with missing package
docs: update README with new package structure
refactor(setup): simplify stow conflict resolution
```

### Rules

- **Subject line:** 50 characters max
- **Imperative mood:** "add feature" not "added feature"
- **No period:** Don't end with `.`
- **Body (optional):** Wrap at 72 characters, explain what and why
- **Issues:** Reference with `#issue-number`

## Branch Naming

Follow [GitHub Flow](https://docs.github.com/en/get-started/quickstart/github-flow).

### Format

```
type-description
type/description
```

### Examples

```
add-tmux-config
fix-emacs-startup
update-readme
refactor/stow-logic
```

### Rules

- Use lowercase
- Separate words with hyphens
- Keep it short and descriptive
- Delete branches after merge

## Tags

Use annotated tags for releases:

```bash
# Create annotated tag
git tag -a v1.2.0 -m "feat: add ghostty terminal config"

# List tags
git tag -l

# Push tags
git push origin --tags
```

Tag naming: `vMAJOR.MINOR.PATCH` following
[Semantic Versioning](https://semver.org/).

## Merge Strategy

- **Feature branches → main:** squash merge to keep history clean
- **Hotfix branches → main:** fast-forward merge to preserve fix commit
- **Never force-push to main**

```bash
# Squash merge (preferred for features)
git merge --squash feature/add-tmux-config
git commit -m "feat(tmux): add tmux configuration"

# Fast-forward merge (hotfixes)
git merge --ff-only hotfix/fix-zsh-startup
```

## Hotfix Workflow

For urgent fixes that must bypass the normal branch flow:

```bash
# 1. Branch from main (not a feature branch)
git checkout -b hotfix/fix-zsh-path main

# 2. Commit with fix type
git commit -m "fix(zsh): correct PATH ordering for homebrew"

# 3. Merge back to main immediately
git checkout main
git merge --ff-only hotfix/fix-zsh-path

# 4. Tag if it warrants a release
git tag -a v1.0.1 -m "fix(zsh): correct PATH ordering"

# 5. Delete hotfix branch
git branch -d hotfix/fix-zsh-path
```

## Pull Requests

- **One logical change** per PR
- **Descriptive title** matching commit format
- **Reference issues** in description
- **Small PRs** - easier to review

## Dotfiles-Specific Conventions

- Group related config changes together
- Test stow operations before committing
- Update documentation when adding new packages
- Verify symlinks work after changes
- Run `./setup.sh clean` before committing to remove stale compiled files
  (`.elc`, `.zwc`, `.pyc`, editor swap/backup files, `.DS_Store`)

## Secrets & .gitignore

Ensure sensitive file patterns are always in `.gitignore`:

```
.env
*.key
*.pem
*_secret
credentials
config.local
*.local.toml
*.local.yml
*.local.yaml
```

Scan before pushing:

```bash
git log -p | grep -E "(password|token|key)" | head -20
```

## Incident Response

If secrets are accidentally committed:

1. **Rotate immediately** — Generate new credentials before anything else
2. **Remove from history** — Use
   [`git filter-repo`](https://github.com/newren/git-filter-repo)
   (preferred over deprecated `git filter-branch`) or BFG Repo-Cleaner
3. **Force push** — Only after coordinating with all collaborators
4. **Audit** — Check logs for unauthorized access