# Testing Requirements

## Testing Philosophy

Workspace testing focuses on:
1. **Configuration validation** - configs load without errors
2. **Syntax verification** - no syntax errors in scripts
3. **Integration testing** - tools work together correctly

## Syntax Validation

```bash
# Shell syntax check
zsh -n script.zsh
bash -n script.sh

# Emacs load test
emacs --batch --eval '(progn (load-file "init.el") (message "OK"))'
```

## Configuration Loading Test

```bash
# Test shell configuration loads
zsh -c 'source ~/.zshenv && echo "OK"'

# Test full environment
zsh -c 'source ~/.zshenv && source ~/.config/zsh/.zshrc && echo "OK"'
```

## Stow Package Testing

```bash
# Dry run before actual stow
stow -n package

# Verify symlinks are valid
find -L ~/.config -type l -xtype l 2>/dev/null
```

## Pre-Commit Checklist

Before committing:
- [ ] Shell scripts pass syntax check (`zsh -n`)
- [ ] Configuration loads without errors
- [ ] No broken symlinks in target directories
- [ ] Cache files not included in commit
