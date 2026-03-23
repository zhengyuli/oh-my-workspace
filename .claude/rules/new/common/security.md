# Security Guidelines

## Mandatory Security Checks

Before ANY commit:
- [ ] No hardcoded secrets (API keys, passwords, tokens)
- [ ] All user inputs validated
- [ ] Error messages don't leak sensitive data

## Secret Management

- NEVER hardcode secrets in source code
- ALWAYS use environment variables or secret files
- Load secrets from gitignored files:

```bash
# Load from local config
if [[ -f "$XDG_CONFIG_HOME/secrets.env" ]]; then
    source "$XDG_CONFIG_HOME/secrets.env"
fi
```

## Machine-Specific Configuration

Use local config files that are gitignored:
- Git: `config.local` (via `includeIf`)
- Shell: `.zshrc.local` (sourced at end)
- Any tool with local override support

## Input Validation

Always validate inputs:
- Check for empty/missing values
- Validate format with regex patterns
- Check file paths exist before operations
- Use `--` to end option parsing

## File Permissions

Set appropriate permissions:
- SSH keys: `chmod 600`
- SSH directory: `chmod 700`
- Config files with secrets: `chmod 600`
