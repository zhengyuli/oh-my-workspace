# Standards

Universal baseline for the oh-my-workspace repository.

## Formatting

- 2-space indentation (never tabs)
- Never align values with spaces
- Prefer separate-line comments over inline

## File Organization

- Many small files > few large files; organize by feature/domain
- Every file must include a standard header:
  - File location path
  - Usage / invocation instructions
  - Dependencies
  - Non-obvious design choices with reference URLs

## Comments

- Explain WHY, not WHAT
- Keep comments up-to-date with code changes
- Remove commented-out code before committing

## Principles

- Immutability: create new configurations, never modify in-place
- XDG paths: use `$HOME` / `$XDG_*` variables, never hardcode
- Safe defaults: provide fallback values for optional variables
- No dead code or commented-out code
- DRY / KISS / YAGNI / Single Responsibility