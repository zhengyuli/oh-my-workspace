---
paths:
  - "**/*"
---

# Testing Requirements

> This file establishes testing standards for workspace validation scripts.

## Testing Philosophy

Workspace testing focuses on:

1. **Configuration validation** - configs load without errors
2. **Syntax verification** - no syntax errors in scripts
3. **Integration testing** - tools work together correctly
4. **Regression prevention** - changes don't break existing setups

## Shell Script Testing

### Syntax Validation

```bash
# Check syntax without execution
bash -n script.sh
zsh -n script.zsh
```

### Load Testing

```bash
# Test zsh configuration loads
zsh -c 'source ~/.zshenv && echo "OK"'

# Test specific file
zsh -c 'source ~/.config/zsh/conf.d/00-env.zsh && echo "OK"'
```

### Shellcheck

Use shellcheck for static analysis:

```bash
# Check single file
shellcheck script.sh

# Check with zsh support (if available)
shellcheck --shell=bash script.sh
```

### Common Test Patterns

```bash
# Test function output
test_function_output() {
    local result
    result=$(my_function "input")
    if [[ "$result" != "expected" ]]; then
        printf 'FAIL: expected "%s", got "%s"\n' "expected" "$result"
        return 1
    fi
    printf 'PASS\n'
}

# Test file existence
test_config_exists() {
    if [[ ! -f "$HOME/.config/git/config" ]]; then
        printf 'FAIL: git config not found\n'
        return 1
    fi
    printf 'PASS\n'
}

# Test symlink validity
test_symlink_valid() {
    local link="$HOME/.zshenv"
    if [[ -L "$link" && -e "$link" ]]; then
        printf 'PASS\n'
    else
        printf 'FAIL: %s is not a valid symlink\n' "$link"
        return 1
    fi
}
```

## Emacs Configuration Testing

### Batch Load Test

```bash
# Test init.el loads without errors
emacs --batch --eval '(progn
  (load-file "emacs/.config/emacs/init.el")
  (message "OK"))'
```

### Byte Compilation Check

```bash
# Check for byte-compile errors (without writing .elc files)
emacs --batch --eval '(progn
  (setq byte-compile-error-on-warn t)
  (byte-compile-file "module.el"))'
```

### Package-Lint

For elisp files, use package-lint:

```bash
# Run package-lint on elisp files
emacs --batch -l package-lint.el -f package-lint-batch-and-exit *.el
```

## Integration Testing

### Stow Package Testing

```bash
# Verify stow symlinks are created correctly
test_stow_package() {
    local pkg="$1"
    stow -n "$pkg"  # dry run
    if stow "$pkg"; then
        # Verify symlinks exist and are valid
        find ~/.config -type l -xtype l 2>/dev/null | head -20
    fi
}

# Check for broken symlinks
find -L ~/.config -type l -xtype l 2>/dev/null | head -20
```

### Full Environment Test

```bash
# Test complete zsh environment
zsh -c 'source ~/.zshenv && source ~/.config/zsh/.zshrc && echo "Environment OK"'

# Test tool availability
zsh -c 'source ~/.zshenv && which starship && which rg && which fd'
```

## Validation Commands

### Pre-Commit Validation

Run before any commit:

```bash
# Check for cache files
git status --porcelain | grep -E '\.(zcompdump|swp|swo|bak|elc|pyc)$|cache/|undo/'

# Verify symlinks
ls -la ~/.zshenv ~/.config/zsh/.zshrc ~/.config/git/config

# Test shell loads
zsh -c 'source ~/.zshenv && echo "OK"'

# Test emacs loads
emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") (message "OK"))'
```

Use `/pre-commit-check` for automated pre-commit validation.

### Configuration Validation

Use `/validate-config` for comprehensive validation:

- File header compliance
- Comment formatting
- Alignment space detection
- Syntax checks per tool

## Test Checklist

### Before Committing Changes

- [ ] Shell scripts pass syntax check (`zsh -n`)
- [ ] Zsh configuration loads without errors
- [ ] Emacs configuration loads without errors
- [ ] No broken symlinks in ~/.config
- [ ] Cache files not included in commit
- [ ] Stow symlinks point to correct locations

### After Stow Operations

- [ ] All expected symlinks created
- [ ] No broken symlinks
- [ ] Shell starts without errors
- [ ] Tools find their configurations

## Testing Tools

| Tool | Purpose | Usage |
|------|---------|-------|
| `bash -n` / `zsh -n` | Syntax check | `zsh -n script.zsh` |
| `shellcheck` | Static analysis | `shellcheck script.sh` |
| `emacs --batch` | Emacs load test | See patterns above |
| `find -L ... -xtype l` | Broken symlinks | See patterns above |
| `stow -n` | Dry-run stow | `stow -n package` |
