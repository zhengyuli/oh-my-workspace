---
paths:
  - "**/*.sh"
  - "**/*.zsh"
  - "**/conf.d/*"
---

# Shell Testing

> This file extends [../common/testing.md](../common/testing.md) with shell-specific testing approaches.

## Syntax Validation

### Basic Syntax Check

```bash
# Check bash syntax
bash -n script.sh

# Check zsh syntax
zsh -n script.zsh
```

### Shellcheck

Use shellcheck for comprehensive static analysis:

```bash
# Check single file
shellcheck script.sh

# Check all shell scripts
shellcheck **/*.sh

# With specific shell
shellcheck --shell=bash script.sh
shellcheck --shell=sh script.sh
```

### Common Shellcheck Issues

| Code | Issue | Fix |
|------|-------|-----|
| SC2086 | Double quote variables | `"$var"` |
| SC2181 | Check exit code directly | `if cmd; then` |
| SC2164 | Use `cd -P` or check | `cd "$dir" || exit` |
| SC2016 | Single quotes prevent expansion | Use double quotes |
| SC2120 | Function references arguments but takes none | Add parameters |

## Load Testing

### Test Configuration Loading

```bash
# Test zsh configuration loads
zsh -c 'source ~/.zshenv && echo "OK"'

# Test specific file
zsh -c 'source ~/.config/zsh/conf.d/00-env.zsh && echo "OK"'

# Test full zshrc
zsh -c 'source ~/.zshenv && source ~/.config/zsh/.zshrc && echo "OK"'
```

### Test Function Availability

```bash
# Test function is defined
zsh -c 'source ~/.zshenv && type my_function && echo "OK"'

# Test command exists
zsh -c 'source ~/.zshenv && which starship && echo "OK"'
```

## BATS (Bash Automated Testing System)

### Basic BATS Test

```bash
#!/usr/bin/env bats

# test_script.bats

setup() {
    # Setup before each test
    source script.sh
}

teardown() {
    # Cleanup after each test
    :
}

@test "function returns expected value" {
    result=$(my_function "input")
    [ "$result" = "expected" ]
}

@test "function handles empty input" {
    run my_function ""
    [ "$status" -eq 1 ]
    [ "$output" = "Error: input required" ]
}

@test "function validates input format" {
    run my_function "invalid!"
    [ "$status" -eq 1 ]
}
```

### BATS Assertions

```bash
# Exit status
[ "$status" -eq 0 ]

# Output equality
[ "$output" = "expected output" ]

# Output contains
[[ "$output" == *"partial match"* ]]

# Multiple lines
[ "${lines[0]}" = "first line" ]
[ "${lines[1]}" = "second line" ]
```

### Running BATS Tests

```bash
# Run single test file
bats test_script.bats

# Run all tests in directory
bats tests/

# Run with verbose output
bats --tap test_script.bats
```

## Shell Unit Testing Patterns

### Simple Test Functions

```bash
#!/usr/bin/env zsh

# Simple test framework
tests_passed=0
tests_failed=0

test_function_output() {
    local result
    result=$(my_function "input")
    if [[ "$result" == "expected" ]]; then
        printf 'PASS: my_function\n'
        (( tests_passed++ ))
    else
        printf 'FAIL: my_function - expected "%s", got "%s"\n' "expected" "$result"
        (( tests_failed++ ))
    fi
}

test_file_exists() {
    if [[ -f "$HOME/.config/git/config" ]]; then
        printf 'PASS: git config exists\n'
        (( tests_passed++ ))
    else
        printf 'FAIL: git config not found\n'
        (( tests_failed++ ))
    fi
}

test_symlink_valid() {
    local link="$HOME/.zshenv"
    if [[ -L "$link" && -e "$link" ]]; then
        printf 'PASS: symlink valid\n'
        (( tests_passed++ ))
    else
        printf 'FAIL: %s is not a valid symlink\n' "$link"
        (( tests_failed++ ))
    fi
}

# Run tests
test_function_output
test_file_exists
test_symlink_valid

# Summary
printf '\nResults: %d passed, %d failed\n' "$tests_passed" "$tests_failed"
[[ $tests_failed -eq 0 ]]
```

## Integration Testing

### Stow Package Testing

```bash
# Test stow symlinks are created correctly
test_stow_package() {
    local pkg="$1"

    # Dry run first
    if ! stow -n "$pkg" 2>/dev/null; then
        printf 'FAIL: stow dry-run failed for %s\n' "$pkg"
        return 1
    fi

    # Actual stow
    if stow "$pkg"; then
        # Verify symlinks exist and are valid
        local broken
        broken=$(find -L ~/.config -type l -xtype l 2>/dev/null | head -1)
        if [[ -n "$broken" ]]; then
            printf 'FAIL: broken symlink found: %s\n' "$broken"
            return 1
        fi
        printf 'PASS: %s stowed successfully\n' "$pkg"
        return 0
    else
        printf 'FAIL: stow failed for %s\n' "$pkg"
        return 1
    fi
}
```

### Full Environment Test

```bash
# Test complete zsh environment
test_zsh_environment() {
    if zsh -c 'source ~/.zshenv && source ~/.config/zsh/.zshrc && echo "OK"' 2>/dev/null | grep -q "OK"; then
        printf 'PASS: zsh environment loads\n'
        return 0
    else
        printf 'FAIL: zsh environment failed to load\n'
        return 1
    fi
}

# Test tool availability
test_tools_available() {
    local -a tools=(starship rg fd)
    local tool

    for tool in "${tools[@]}"; do
        if ! command -v "$tool" >/dev/null 2>&1; then
            printf 'FAIL: tool not found: %s\n' "$tool"
            return 1
        fi
    done
    printf 'PASS: all tools available\n'
    return 0
}
```

## Pre-Commit Testing

### Automated Pre-Commit Script

```bash
#!/usr/bin/env zsh
# pre-commit-tests.sh

set -e

printf 'Running pre-commit tests...\n\n'

# 1. Syntax check all shell files
printf '1. Syntax checking shell files...\n'
for f in **/*.zsh **/*.sh; do
    if [[ -f "$f" ]]; then
        zsh -n "$f" && printf '  ✓ %s\n' "$f"
    fi
done

# 2. Shellcheck
printf '\n2. Running shellcheck...\n'
if command -v shellcheck >/dev/null 2>&1; then
    for f in **/*.sh **/*.zsh; do
        if [[ -f "$f" ]]; then
            shellcheck "$f" && printf '  ✓ %s\n' "$f"
        fi
    done
else
    printf '  shellcheck not installed, skipping\n'
fi

# 3. Test configuration loading
printf '\n3. Testing configuration loading...\n'
zsh -c 'source ~/.zshenv && echo OK' && printf '  ✓ zshenv\n'
zsh -c 'source ~/.zshenv && source ~/.config/zsh/.zshrc && echo OK' && printf '  ✓ zshrc\n'

# 4. Check for broken symlinks
printf '\n4. Checking for broken symlinks...\n'
broken=$(find -L ~/.config -type l -xtype l 2>/dev/null | head -1)
if [[ -z "$broken" ]]; then
    printf '  ✓ No broken symlinks\n'
else
    printf '  ✗ Broken symlink: %s\n' "$broken"
    exit 1
fi

printf '\n✓ All pre-commit tests passed\n'
```

## Test Checklist

### Before Committing Shell Changes

- [ ] Syntax check passes (`zsh -n`)
- [ ] Shellcheck passes (no warnings)
- [ ] Configuration loads without errors
- [ ] Functions are testable
- [ ] Error cases are handled
- [ ] No hardcoded secrets

### After Stow Operations

- [ ] All expected symlinks created
- [ ] No broken symlinks in `~/.config`
- [ ] Shell starts without errors
- [ ] Tools find their configurations
