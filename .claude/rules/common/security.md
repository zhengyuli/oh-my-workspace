---
paths:
  - "**/*"
---

# Security Guidelines

> This file establishes repository-wide security practices for dotfiles.

## Secrets Management

### Never Commit Secrets

**CRITICAL**: Never commit sensitive information to the repository.

| Secret Type | Examples | Handling |
|-------------|----------|----------|
| API keys | `GITHUB_TOKEN`, `OPENAI_API_KEY` | Environment variables |
| Passwords | Database passwords, SSH passphrases | `.gitignore` + env vars |
| Private keys | SSH keys, GPG keys | `.gitignore`, separate secure storage |
| Certificates | TLS certs, client certs | `.gitignore` |

### Detection

Before committing, scan for potential secrets:

```bash
# Check staged files for potential secrets
git diff --cached | grep -iE '(password|secret|token|api_key|private_key)'

# Use gitleaks if available
gitleaks protect --staged
```

### Machine-Specific Configuration

Use local config files that are gitignored:

```bash
# Git local config (already in .gitignore)
git config --local user.name "Your Name"

# Zsh local config
# Put in ~/.config/zsh/.zshrc.local (sourced by .zshrc)
```

## Input Validation

### Shell Scripts

Always validate and sanitize inputs:

```bash
# CORRECT - validate input
validate_input() {
    local input="$1"
    if [[ -z "$input" ]]; then
        printf '%s\n' "Error: input required" >&2
        return 1
    fi
    if [[ ! "$input" =~ ^[a-zA-Z0-9_-]+$ ]]; then
        printf '%s\n' "Error: invalid characters in input" >&2
        return 1
    fi
}

# WRONG - untrusted input
eval "$user_input"  # NEVER do this
```

### Configuration Files

- Validate paths exist before operations
- Check file permissions before reading sensitive data
- Use absolute paths for security-critical operations

## Injection Prevention

### Shell Command Injection

```bash
# CORRECT - use arrays to avoid word splitting
local -a args=("--option" "$value")
command "${args[@]}"

# WRONG - vulnerable to injection
command "--option $value"
```

### Path Traversal

```bash
# CORRECT - validate path doesn't escape intended directory
local target="/expected/directory/$user_path"
local resolved
resolved=$(cd "$(dirname "$target")" && pwd)/$(basename "$target")
if [[ ! "$resolved" =~ ^/expected/directory/ ]]; then
    printf '%s\n' "Error: path traversal attempt" >&2
    return 1
fi
```

## File Permissions

### Sensitive Files

Set appropriate permissions for sensitive files:

```bash
# SSH keys - owner read/write only
chmod 600 ~/.ssh/id_*

# SSH directory - owner access only
chmod 700 ~/.ssh

# Config files with secrets
chmod 600 ~/.config/git/config.local
```

### Executable Scripts

```bash
# Scripts should be executable only by owner when possible
chmod 700 ~/.local/bin/sensitive-script.sh

# General scripts
chmod 755 ~/.local/bin/general-script.sh
```

## Eval Safety

### Avoid Eval When Possible

```bash
# CORRECT - use indirect expansion
local var_name="MY_VAR"
print "${(P)var_name}"

# CORRECT - use associative arrays
typeset -A config
config[key]="value"
print "${config[key]}"

# DANGEROUS - eval with untrusted input
eval "echo $user_input"  # NEVER do this
```

### Safe Eval Pattern

When eval is absolutely necessary:

```bash
# CORRECT - allowlist validated tools
local -a allowed_tools=(rbenv pyenv nodenv)
local tool="$1"

if (( ${allowed_tools[(I)$tool]} )); then
    eval "$($tool init -)"
else
    printf '%s\n' "Error: unknown tool: $tool" >&2
    return 1
fi
```

## Security Checklist

Before committing:

- [ ] No hardcoded secrets in any files
- [ ] `.gitignore` covers sensitive file patterns
- [ ] No `eval` with untrusted input
- [ ] File permissions are appropriate
- [ ] Machine-specific configs use local files
