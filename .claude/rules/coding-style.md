# Coding Style

Universal coding standards for the oh-my-workspace repository.

## Line Length

Maximum 80 characters for all files.

Rationale: Improves readability in side-by-side diffs and terminal windows.

## File Headers

Every configuration file must include a standard header with:
- File location path
- Usage / invocation instructions
- Dependencies (packages, tools, modules this file relies on)
- Non-obvious design choices with reference URLs where helpful

## Function Documentation

Every function must have:
- Purpose description
- Parameters
- Return values / exit codes

## Comments

- Explain **why**, not **what**
- Use comments for non-obvious logic
- Keep comments up-to-date with code changes
- Remove commented-out code before committing

## Naming Conventions

Use meaningful, descriptive names. Avoid single-letter or cryptic
abbreviations.

## Formatting Rules

- Never align values with spaces
- Prefer separate-line comments over inline
- 2-space indentation for applicable languages

## Code Quality Principles

- **No dead code** - Remove unused functions and variables
- **Single responsibility** - Each function does one thing well
- **DRY** - Don't repeat yourself; extract common logic
- **YAGNI** - Don't add features you don't need yet
- **KISS** - Keep it simple, avoid over-engineering

## Language-Specific Extensions

Language-specific rules are conditionally loaded via `globs` frontmatter.
They extend these universal rules with language-specific conventions.

| Rule file        | Globs                                                                                             |
|------------------|---------------------------------------------------------------------------------------------------|
| `lang/bash.md`   | `**/*.sh`, `**/*.bash`                                                                            |
| `lang/zsh.md`    | `**/*.zsh`, `**/.zsh*`, `**/zshrc`, `**/zprofile`, `**/zshenv`, `**/zlogin`                      |
| `lang/elisp.md`  | `**/*.el`                                                                                         |
| `lang/config.md` | `**/config`, `**/*.conf`, `**/*.cfg`, `**/rc`, `**/.gitconfig`, `**/git/config`                  |
| `lang/toml.md`   | `**/*.toml`                                                                                       |
| `lang/yaml.md`   | `**/*.yml`, `**/*.yaml`                                                                           |

### Unified Structure

All lang rules follow a consistent section template:
1. **File Header** — Location, usage, dependencies
2. **Delimiter Hierarchy** — Consistent with this file
3. **Code Patterns** — Language-specific syntax and conventions
4. **Anti-Patterns** — Explicit "Don't" list with corrections
5. **Security** — Language-specific security concerns
6. **References** — Links to official documentation
7. **Validation** — Tool-specific commands

See `lang/_template.md` for the complete template structure.
