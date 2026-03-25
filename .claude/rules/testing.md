# Testing Strategy

Unified testing approach for oh-my-workspace configuration code.

## Overview

Configuration code is tested at three levels:

1. **Syntax** — Does the file parse without errors?
2. **Unit** — Does each function behave correctly in isolation?
3. **Integration** — Do installed symlinks and loaded configs work
   end-to-end?

## Syntax Validation

Run before every commit (also wired into PostToolUse hooks — see
`hooks.md`):

```bash
# Shell (Bash)
bash -n script.sh

# Shell (Zsh)
zsh -n script.zsh

# Emacs Lisp — byte-compile catches more than syntax alone
emacs --batch -f batch-byte-compile init.el

# Python
python3 -m py_compile module.py
```

## Shell Script Testing

Use [bats-core](https://github.com/bats-core/bats-core) for unit and
integration tests of shell scripts:

```bash
# tests/install.bats
setup() {
  # Create an isolated home directory for each test
  export HOME
  HOME="$(mktemp -d)"
}

teardown() {
  rm -rf "$HOME"
}

@test "stow creates expected symlink" {
  stow -v -t "$HOME" -d "$BATS_TEST_DIRNAME/.." shell/zsh
  [ -L "$HOME/.zshrc" ]
}

@test "setup.sh install exits 0" {
  run bash setup.sh install shell/zsh
  [ "$status" -eq 0 ]
}
```

Run tests:

```bash
bats tests/
```

## Python Testing

Use pytest (see `lang/python.md` for full details):

```bash
# Run all tests
uv run pytest

# Run with coverage report
uv run pytest --cov=dotfiles --cov-report=term-missing
```

## Emacs Lisp Testing

Use ERT (see `lang/elisp.md` for full details):

```bash
emacs --batch -l omw-shell.el -l tests/omw-shell-test.el \
  -f ert-run-tests-batch-and-exit
```

## Integration Testing

After stowing a package, verify the live environment:

```bash
# 1. Confirm symlinks exist and point to the right source
ls -la "$HOME/.zshrc"
readlink -f "$HOME/.zshrc"

# 2. Source the config in a subshell — catches runtime errors
zsh -c 'source ~/.zshrc && echo OK'

# 3. For Emacs configs, start in batch mode to catch load errors
emacs --batch -l ~/.config/emacs/init.el --eval '(message "OK")'
```

## CI Checklist

Before merging any change, confirm:

- [ ] All syntax checks pass (`bash -n`, `zsh -n`, byte-compile, `py_compile`)
- [ ] Unit tests pass (bats / pytest / ERT as applicable)
- [ ] Integration test: config loads without errors in a clean environment
- [ ] No secrets in diff (`git diff --cached | grep -iE 'password|secret|token'`)
- [ ] Commit message follows Conventional Commits (see `git-workflow.md`)

## See Also

- `hooks.md` — Automated syntax validation via PostToolUse hooks
- `lang/shell.md` — Shell strict mode and error handling
- `lang/elisp.md` — Byte compilation and ERT test patterns
- `lang/python.md` — pytest conventions
- `development-workflow.md` — Verification phase checklist
