# AI Generation Constraints

Rules for AI-assisted development (Claude-specific).

## Documentation Requirements

### Comments

AI must add comments explaining:
- Complex logic or algorithms
- Non-obvious design decisions
- Business rules and constraints
- Workarounds and their reasons

```bash
# Good - explains WHY
# Use printf instead of echo for portability (POSIX compliance)
printf '%s\n' "$message"

# Bad - explains WHAT
# Print the message
printf '%s\n' "$message"
```

### Function Documentation

Every function must have:
- Purpose description
- Parameter documentation
- Return value/exit code documentation

```bash
# Install packages using GNU Stow.
#
# Creates symlinks in $HOME for each package directory.
# Handles conflicts by removing existing files (with confirmation).
#
# @param $1 - Space-separated list of package names
# @return 0 on success, 1 on failure
_install_packages() {
  ...
}
```

## Code Quality

### Follow Project Rules

AI must follow:
- `coding-style.md` - Formatting and documentation
- `git-workflow.md` - Commit message format
- `security.md` - No hardcoded secrets

### Naming Conventions

Use meaningful names:

```bash
# Bad - cryptic
x=$(pwd)

# Good - descriptive
workspace_dir=$(pwd)
```

### No Dead Code

- Remove unused functions
- Delete commented-out code
- Clean up debugging statements

## Safety Checks

### Secrets

AI must not generate code that:
- Hardcodes API keys or tokens
- Embeds passwords in scripts
- Exposes sensitive configuration

### Input Validation

AI must validate:
- User-provided parameters
- Environment variables
- File paths before operations

```bash
# Check before overwriting
if [[ -f "$target" ]]; then
  printf 'warning: %s exists, skipping\n' "$target"
  return 1
fi
```

### File Operations

Safe file operations:

```bash
# Check directory exists
[[ -d "$dir" ]] || mkdir -p "$dir"

# Verify before symlink
[[ -L "$link" ]] || ln -s "$target" "$link"

# Backup before modify
cp "$file" "${file}.bak"
```

## Dotfiles-Specific Rules

### Stow Operations

Before suggesting changes:
1. Test with `stow -n -v` (dry-run)
2. Verify no conflicts
3. Check symlink paths are correct

### Package Structure

- Respect existing category organization
- Follow `category/package` naming
- Update `PKG_ALL` array in `setup.sh`

### Documentation Updates

When adding packages:
- Update README if it exists
- Add inline comments explaining purpose
- Document any dependencies

## Quality Checklist

Before completing any task, verify:

- [ ] All functions documented
- [ ] Complex logic explained
- [ ] No hardcoded secrets
- [ ] Input validation present
- [ ] Safe file operations
- [ ] Follows project conventions
- [ ] Commit message follows Conventional Commits