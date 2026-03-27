---
version: "1.0.0"
last-updated: "2026-03-26"
maintainer: "zhengyu.li"
---
# Security

Security best practices for dotfiles management.

## Secrets Management

### Never Commit Secrets

- API keys
- Access tokens
- Passwords
- Private keys
- Database credentials

### Use Environment Variables

Store secrets in environment variables, not files:

```bash
# Bad - hardcoded secret
API_KEY="sk-1234567890"

# Good - environment variable
API_KEY="${API_KEY:-}"
```

### .gitignore Patterns

Ensure these patterns are in `.gitignore`:

```
.env
*.key
*.pem
*_secret
credentials
config.local
```

## Input Validation

### Validate User Input

Always validate external input:

```bash
# Validate package name
_validate_package() {
  local -r pkg="$1"
  if [[ -z "$pkg" ]]; then
    printf 'error: package name required\n' >&2
    return 1
  fi
}
```

### Sanitize Variables

Quote variables to prevent word splitting and glob expansion:

```bash
# Bad - unquoted
rm -rf $dir

# Good - quoted
rm -rf "$dir"
```

### Avoid eval

Never use `eval` on untrusted input:

```bash
# Dangerous
eval "$user_input"

# Safe alternative
case "$user_input" in
  option1) do_something ;;
  option2) do_other ;;
  *) printf 'invalid option\n' >&2 ;;
esac
```

## File Permissions

### Recommended Permissions

| File Type | Permission | Octal |
|-----------|------------|-------|
| Scripts | rwxr-xr-x | 755 |
| Config files | rw-r--r-- | 644 |
| SSH keys | rw------- | 600 |
| Secrets | rw------- | 600 |

### Check Permissions

```bash
# Verify script is executable
[[ -x "$script" ]] || chmod 755 "$script"

# Verify secret file permissions (cross-platform)
# find returns output only if permissions match; empty = wrong perms
if [[ -z "$(find "$secret_file" -maxdepth 0 -perm 0600 2>/dev/null)" ]]; then
  printf 'error: %s must be 600\n' "$secret_file" >&2
  chmod 600 "$secret_file"
fi
```

> **Note:** Avoid `stat -f` (macOS-only) and `stat -c` (Linux-only).
> Use `find -perm` for portable permission checks across both platforms.

## Dotfiles-Specific Security

### Review Before Stowing

Check configs for:
- Hardcoded paths (use `$HOME` or `$XDG_*`)
- Internal network addresses
- Machine-specific settings
- Username references

### Shell History

Be careful with:
- Commands containing secrets
- API calls with tokens
- Database connection strings

Prefix sensitive commands with a space to exclude them from history.
See `lang/zsh.md` (`HIST_IGNORE_SPACE`) and `lang/bash.md`
(`HISTCONTROL`) for per-shell configuration.

### Safe File Operations

Always guard file operations to prevent data loss:

```bash
# Check directory exists before using it
[[ -d "$dir" ]] || mkdir -p "$dir"

# Verify before creating symlink (don't overwrite existing)
[[ -L "$link" ]] || ln -s "$target" "$link"

# Backup before modifying in-place
cp "$file" "${file}.bak"

# Check before overwriting a target file
if [[ -f "$target" ]]; then
  printf 'warning: %s exists, skipping\n' "$target" >&2
  return 1
fi
```

## Incident Response

If secrets are accidentally committed:

1. **Rotate immediately** - Generate new credentials
2. **Remove from history** - Use
   [`git filter-repo`](https://github.com/newren/git-filter-repo)
   (preferred over deprecated `git filter-branch`) or BFG Repo-Cleaner
3. **Force push** - Only if necessary and coordinated with team
4. **Audit** - Check for unauthorized access