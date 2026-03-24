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