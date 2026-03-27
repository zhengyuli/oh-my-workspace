---
version: "1.0.0"
last-updated: "2026-03-27"
maintainer: "zhengyu.li"
---
# Testing Standards

Universal testing principles for oh-my-workspace configuration management.

## Testing Philosophy

**Test before committing, verify after stowing.**

Configuration changes can have far-reaching effects. Always validate changes
before committing them to version control and verify they work correctly after
stowing to the target location.

### Core Principles

1. **Syntax validation first** - Ensure files parse correctly
2. **Functional testing second** - Verify behavior works as expected
3. **Integration testing last** - Confirm changes work in context
4. **Automate what you can** - Use hooks for repetitive checks

## Testing by File Type

### Shell Scripts (.sh, .bash, .zsh)

**Syntax Validation:**
```bash
# Check for syntax errors without execution
bash -n script.sh
zsh -n script.zsh
```

**Functional Testing:**
```bash
# Test in isolation
./script.sh --help

# Test with dry-run if available
./setup.sh install --dry-run zsh

# Test in new shell session
zsh -c 'source ~/.zshrc && echo "Success"'
```

**Integration Testing:**
```bash
# Open new terminal and verify
# 1. Shell starts without errors
# 2. Aliases are available
# 3. Functions work correctly
# 4. Path is set correctly
```

See `lang/bash.md` and `lang/zsh.md` for language-specific testing features.

### Emacs Lisp (.el)

**Syntax Validation:**
```bash
# Byte compile to check syntax
emacs --batch -f batch-byte-compile init.el

# Or validate without creating .elc
emacs --batch --eval "(progn (find-file \"init.el\") (check-parens) (emacs-lisp-mode))"
```

**Unit Testing:**
```elisp
;; Use ERT (Emacs Lisp Regression Testing)
(ert-deftest my-test ()
  "Test description."
  (should (eq 1 1)))
```

Run tests:
```bash
# Run all tests
emacs --batch -l init.el -l omw-test.el -f ert-run-tests-batch-and-exit
```

**Integration Testing:**
```bash
# 1. Clean stale .elc files (CRITICAL - see lang/elisp.md)
find ~/.config/emacs/ -name '*.elc' -delete

# 2. Restart Emacs
emacs &

# 3. Verify in *Messages* buffer
# 4. Test key functionality
```

**Critical:** Emacs loads `.elc` files in preference to `.el` files. Always
remove `.elc` files before testing changes. See `lang/elisp.md` for details.

See `lang/elisp.md` for comprehensive ERT testing guidance.

### Python (.py)

**Syntax Validation:**
```bash
# Check for syntax errors
python3 -m py_compile script.py
```

**Type Checking:**
```bash
# Static type analysis (if using type hints)
mypy script.py
```

**Unit Testing:**
```bash
# Run tests with pytest
pytest tests/

# Run with coverage
pytest --cov=module tests/

# Run specific test
pytest tests/test_module.py::test_function
```

**Integration Testing:**
```bash
# Test in virtual environment
uv run python script.py

# Verify imports work
python3 -c "import module; print('OK')"
```

See `lang/python.md` for comprehensive Python testing standards.

## Testing Workflows

### Before Committing

**1. Syntax Validation (Automated)**
```bash
# PostToolUse hook runs automatically
# Or manually validate:
bash -n script.sh
emacs --batch -f batch-byte-compile init.el
python3 -m py_compile script.py
```

**2. Functional Testing (Manual)**
```bash
# Test the specific functionality you changed
./setup.sh install --dry-run zsh
emacs --batch -l init.el
pytest tests/test_changed.py
```

**3. Integration Testing (If Applicable)**
```bash
# Test in real environment
# Open new terminal, restart Emacs, etc.
```

**4. Pre-Commit Checks (Automated)**
```bash
# .git/hooks/pre-commit runs automatically
# Blocks .elc files, warns on potential issues
```

### After Stowing

**1. Verify Symlinks**
```bash
# Check symlinks were created correctly
ls -la ~/.zshrc ~/.config/zsh/
ls -la ~/.config/emacs/init.el
```

**2. Test in Real Environment**
```bash
# Shell: Open new terminal
# Emacs: Restart and check *Messages*
# Python: Import in new session
```

**3. Verify Functionality**
```bash
# Test actual usage
git status
ll  # if alias defined
emacs --version
```

### Rollback Testing

Always have a rollback plan before making changes.

**For Shell:**
```bash
# Backup before changing
cp ~/.zshrc ~/.zshrc.bak

# If broken, restore
cp ~/.zshrc.bak ~/.zshrc
source ~/.zshrc
```

**For Emacs:**
```bash
# Backup before changing
cp -r ~/.config/emacs/lisp ~/.config/emacs/lisp.bak

# If broken, restore
rm -rf ~/.config/emacs/lisp
mv ~/.config/emacs/lisp.bak ~/.config/emacs/lisp
```

**For Stow:**
```bash
# Unstow if broken
./setup.sh uninstall zsh

# Restore from backup or git
git checkout HEAD~1 -- shell/zsh/
./setup.sh install zsh
```

## Automated Testing

### Hooks

Claude Code hooks automate syntax validation:

- **PostToolUse**: Validates syntax after editing
- **Stop**: Checks for uncommitted changes and commit format
- **Pre-commit**: Blocks .elc files, warns on issues

See `hooks.md` for hook implementation details.

### Continuous Integration

For critical changes, consider testing in CI:

```yaml
# Example GitHub Actions workflow
name: Test
on: [push, pull_request]
jobs:
  test:
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v3
      - name: Validate shell syntax
        run: find . -name '*.sh' -exec bash -n {} \;
      - name: Validate elisp syntax
        run: find editor/emacs -name '*.el' -exec emacs --batch -f batch-byte-compile {} \;
      - name: Test setup script
        run: ./setup.sh install --dry-run --all
```

## Testing Checklist

Before completing any configuration change:

- [ ] Syntax validation passed (automated via hooks)
- [ ] Functional testing completed
- [ ] Integration testing done (if applicable)
- [ ] Symlinks verified (after stowing)
- [ ] Real environment tested
- [ ] Backup created before changes
- [ ] Rollback plan documented
- [ ] Commit message follows Conventional Commits

## Common Testing Issues

**"My change has no effect"**

For Emacs: Stale `.elc` files are being loaded instead of `.el` files.
```bash
find ~/.config/emacs/ -name '*.elc' -delete
```

For Shell: Old session has cached values.
```bash
# Open new terminal, or:
source ~/.zshrc
```

**"Tests pass but functionality broken"**

Integration issue: Test environment differs from real environment.
- Test in actual environment (new terminal, restarted Emacs)
- Check for environment variable dependencies
- Verify path resolution in stowed location

**"Hook doesn't run"**

Hook script not executable or wrong location.
```bash
# Verify hook is executable
ls -la .claude/hooks/post-tool-use.sh
ls -la .git/hooks/pre-commit

# Make executable if needed
chmod +x .claude/hooks/post-tool-use.sh
chmod +x .git/hooks/pre-commit
```

## See Also

- `hooks.md` - Automated testing hooks
- `dev-workflow.md` - Testing in development workflow
- `lang/bash.md` - Bash testing features
- `lang/zsh.md` - Zsh testing features
- `lang/elisp.md` - ERT testing framework
- `lang/python.md` - pytest and type checking
