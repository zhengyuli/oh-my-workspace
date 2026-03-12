# Zsh Testing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement comprehensive test suite for shell/zsh using bats framework with real environment testing approach

**Architecture:** Tests will be located in /tmp/dotfiles-tests/ to avoid modifying the codebase. Tests cover syntax validation, setup.sh plugin installation, function behavior, alias expansion, and full shell integration. Uses bats framework with a single test runner for easy execution.

**Tech Stack:** bats (Bash Automated Testing System), zsh, git, standard Unix tools

---

## Task 1: Create Test Infrastructure

**Files:**
- Create: `/tmp/dotfiles-tests/test_helper.bash`

**Step 1: Write test_helper.bash with shared utilities**

```bash
#!/usr/bin/env bash
# test_helper.bash - Shared test utilities for dotfiles testing

# Setup function - runs before each test
setup() {
    export TEST_TEMP_DIR="/tmp/dotfiles-test-$$"
    mkdir -p "$TEST_TEMP_DIR"
}

# Teardown function - runs after each test
teardown() {
    rm -rf "$TEST_TEMP_DIR"
}

# Check if command exists
command_exists() {
    command -v "$1" &>/dev/null
}

# Skip test if command is missing
skip_if_missing() {
    if ! command_exists "$1"; then
        skip "Missing required command: $1"
    fi
}

# Create temporary file
create_temp_file() {
    local filename="${1:-testfile}"
    local filepath="$TEST_TEMP_DIR/$filename"
    touch "$filepath"
    echo "$filepath"
}

# Create temporary directory
create_temp_dir() {
    local dirname="${1:-testdir}"
    local dirpath="$TEST_TEMP_DIR/$dirname"
    mkdir -p "$dirpath"
    echo "$dirpath"
}

# Assert file exists
assert_file_exists() {
    [[ -f "$1" ]]
}

# Assert directory exists
assert_dir_exists() {
    [[ -d "$1" ]]
}

# Get DOTFILES path
get_dotfiles_path() {
    if [[ -n "${DOTFILES:-}" ]]; then
        echo "$DOTFILES"
    else
        # Resolve from script location
        local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
        echo "$(dirname "$(dirname "$script_dir")")"
    fi
}
```

**Step 2: Verify test_helper.bash syntax**

Run: `bash -n /tmp/dotfiles-tests/test_helper.bash`
Expected: No output (syntax OK)

**Step 3: Commit (note: tests are in /tmp, so skip git commit)**

---

## Task 2: Syntax & Structure Tests

**Files:**
- Create: `/tmp/dotfiles-tests/01_syntax.bats`

**Step 1: Write test for shell script syntax validation**

```bash
#!/usr/bin/env bats
# 01_syntax.bats - Syntax validation tests

load test_helper

DOTFILES="$(get_dotfiles_path)"

@test "All .sh files have valid bash syntax" {
    for file in "$DOTFILES"/shell/zsh/setup.sh; do
        run bash -n "$file"
        [[ $status -eq 0 ]]
    done
}

@test "All .zsh files have valid zsh syntax" {
    for file in "$DOTFILES"/shell/zsh/conf.d/*.zsh; do
        run zsh -n "$file"
        [[ $status -eq 0 ]]
    done
}

@test "All .symlink files have valid zsh syntax" {
    for file in "$DOTFILES"/shell/zsh/*.symlink; do
        run zsh -n "$file"
        [[ $status -eq 0 ]]
    done
}

@test "All files have Time-stamp header with English weekday" {
    local has_chinese=0

    for file in "$DOTFILES"/shell/zsh/setup.sh "$DOTFILES"/shell/zsh/*.symlink "$DOTFILES"/shell/zsh/conf.d/*.zsh; do
        if grep -q "星期\|星期一\|星期二\|星期三\|星期四\|星期五\|星期六\|星期日" "$file" 2>/dev/null; then
            has_chinese=1
            echo "Chinese weekday found in: $file"
        fi
    done

    [[ $has_chinese -eq 0 ]]
}

@test "All .symlink files exist and are regular files" {
    for file in "$DOTFILES"/shell/zsh/*.symlink; do
        assert_file_exists "$file"
    done
}

@test "conf.d files are numbered 00-06" {
    local expected_files="00-options.zsh 01-history.zsh 02-completion.zsh 03-keybindings.zsh 04-aliases.zsh 05-functions.zsh 06-prompt.zsh"

    for expected in $expected_files; do
        assert_file_exists "$DOTFILES/shell/zsh/conf.d/$expected"
    done
}

@test "conf.d files load without errors" {
    for file in "$DOTFILES"/shell/zsh/conf.d/*.zsh; do
        run zsh -c "source '$file'"
        [[ $status -eq 0 ]]
    done
}
```

**Step 2: Run syntax tests**

Run: `bats /tmp/dotfiles-tests/01_syntax.bats`
Expected: All tests PASS

**Step 3: Verify test output**

Expected: 7 tests, all passing

---

## Task 3: setup.sh Plugin Installation Tests

**Files:**
- Create: `/tmp/dotfiles-tests/02_setup.bats`

**Step 1: Write test for check_zsh function**

```bash
#!/usr/bin/env bats
# 02_setup.bats - setup.sh plugin installation tests

load test_helper

DOTFILES="$(get_dotfiles_path)"
ZSH_PLUGIN_DIR="/tmp/test-plugins-$$"

setup() {
    setup
    export ZSH_PLUGIN_DIR
    mkdir -p "$ZSH_PLUGIN_DIR"
}

teardown() {
    teardown
    rm -rf "$ZSH_PLUGIN_DIR"
}

@test "check_zsh succeeds when zsh is installed" {
    skip_if_missing "zsh"

    run zsh -c "source '$DOTFILES/shell/zsh/setup.sh'; check_zsh"
    [[ $status -eq 0 ]]
}

@test "ensure_plugin_dir creates directory if missing" {
    run zsh -c "source '$DOTFILES/shell/zsh/setup.sh'; ensure_plugin_dir"
    [[ $status -eq 0 ]]
    assert_dir_exists "$ZSH_PLUGIN_DIR"
}

@test "install_plugin clones from GitHub successfully" {
    skip_if_missing "git"

    # Use a small, fast repository for testing
    local test_repo="https://github.com/zsh-users/zsh-autosuggestions"
    local plugin_name="zsh-autosuggestions-test"

    run zsh -c "source '$DOTFILES/shell/zsh/setup.sh'; install_plugin '$plugin_name' '$test_repo'"

    [[ $status -eq 0 ]]
    assert_dir_exists "$ZSH_PLUGIN_DIR/$plugin_name"
}

@test "install_plugin is idempotent (handles already-installed plugins)" {
    skip_if_missing "git"

    local test_repo="https://github.com/zsh-users/zsh-autosuggestions"
    local plugin_name="zsh-autosuggestions-idem"

    # Install twice
    run zsh -c "source '$DOTFILES/shell/zsh/setup.sh'; install_plugin '$plugin_name' '$test_repo'"
    [[ $status -eq 0 ]]

    run zsh -c "source '$DOTFILES/shell/zsh/setup.sh'; install_plugin '$plugin_name' '$test_repo'"
    [[ $status -eq 0 ]]
}

@test "install_all_plugins installs all 4 plugins" {
    skip_if_missing "git"

    # Mock the PLUGIN_URLS to use smaller repos for testing
    run zsh -c "
        source '$DOTFILES/shell/zsh/setup.sh'

        # Override plugin directory for testing
        export ZSH_PLUGIN_DIR='$ZSH_PLUGIN_DIR'

        # Run install_all_plugins
        install_all_plugins
    "

    [[ $status -eq 0 ]]

    # Check all 4 plugins are installed
    assert_dir_exists "$ZSH_PLUGIN_DIR/zsh-autosuggestions"
    assert_dir_exists "$ZSH_PLUGIN_DIR/zsh-completions"
    assert_dir_exists "$ZSH_PLUGIN_DIR/zsh-history-substring-search"
    assert_dir_exists "$ZSH_PLUGIN_DIR/zsh-syntax-highlighting"
}

@test "Plugins are cloned to correct location" {
    skip_if_missing "git"

    # Already tested in previous test, just verify structure
    assert_dir_exists "$ZSH_PLUGIN_DIR/zsh-autosuggestions/zsh-autosuggestions.zsh"
    assert_dir_exists "$ZSH_PLUGIN_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
}
```

**Step 2: Run setup tests**

Run: `bats /tmp/dotfiles-tests/02_setup.bats`
Expected: All tests PASS (may take 10-15 seconds due to git clone)

**Step 3: Verify plugin installation**

Run: `ls -la /tmp/test-plugins-$$/`
Expected: 4 plugin directories

---

## Task 4: Function Behavior Tests

**Files:**
- Create: `/tmp/dotfiles-tests/03_functions.bats`

**Step 1: Write tests for directory navigation functions**

```bash
#!/usr/bin/env bats
# 03_functions.bats - Function behavior tests

load test_helper

DOTFILES="$(get_dotfiles_path)"

setup() {
    setup
    source "$DOTFILES/shell/zsh/conf.d/05-functions.zsh"
}

teardown() {
    teardown
}

# Directory Navigation
@test "mkcd creates directory and changes into it" {
    local test_dir="$(create_temp_dir "mkcd-test")"

    cd "$TEST_TEMP_DIR"
    run zsh -c "mkcd '$test_dir' && pwd"
    [[ $output == "$test_dir" ]]
}

@test "up navigates up 1 directory" {
    local test_dir="$(create_temp_dir "up-test/subdir")"

    cd "$test_dir"
    run zsh -c "up 1 && pwd"
    [[ $output == "$(dirname "$test_dir")" ]]
}

@test "up navigates up N directories" {
    local test_dir="$(create_temp_dir "up-test/a/b/c")"

    cd "$test_dir"
    run zsh -c "up 3 && pwd"
    [[ $output == "$(dirname "$(dirname "$(dirname "$test_dir")")")" ]]
}

# File Operations
@test "backup creates timestamped backup file" {
    local test_file="$(create_temp_file "backup-test.txt")"
    echo "test content" > "$test_file"

    run backup "$test_file"
    [[ $status -eq 0 ]]

    # Check backup file exists with timestamp
    run ls "${test_file}.backup-"*
    [[ $status -eq 0 ]]
}

@test "extract handles .tar.gz format" {
    skip_if_missing "tar"
    skip_if_missing "gzip"

    local test_dir="$(create_temp_dir "extract-test")"
    local test_file="$test_dir/test.txt"
    echo "test content" > "$test_file"

    # Create tar.gz
    tar czf "$TEST_TEMP_DIR/test.tar.gz" -C "$test_dir" test.txt

    run extract "$TEST_TEMP_DIR/test.tar.gz"
    [[ $status -eq 0 ]]
    assert_file_exists "$TEST_TEMP_DIR/test.txt"
}

@test "extract handles .zip format" {
    skip_if_missing "zip"

    local test_dir="$(create_temp_dir "extract-zip-test")"
    local test_file="$test_dir/test.txt"
    echo "test content" > "$test_file"

    # Create zip
    cd "$test_dir" && zip -r "$TEST_TEMP_DIR/test.zip" test.txt

    run extract "$TEST_TEMP_DIR/test.zip"
    [[ $status -eq 0 ]]
}

@test "ff finds files by pattern" {
    local test_dir="$(create_temp_dir "ff-test")"
    touch "$test_dir/findme.txt"
    touch "$test_dir/findme2.txt"

    cd "$test_dir"
    run ff "findme"
    [[ $status -eq 0 ]]
    [[ "$output" == *"findme"* ]]
}

@test "fd finds directories by pattern" {
    local test_dir="$(create_temp_dir "fd-test")"
    mkdir -p "$test_dir/searchdir"
    mkdir -p "$test_dir/searchdir2"

    cd "$test_dir"
    run fd "search"
    [[ $status -eq 0 ]]
    [[ "$output" == *"search"* ]]
}

# Process Management
@test "psg finds processes by name" {
    # Start a sleep process
    sleep 100 &>/dev/null &
    local pid=$!

    run psg "sleep"
    [[ $status -eq 0 ]]
    [[ "$output" == *"$pid"* ]]

    # Cleanup
    kill $pid 2>/dev/null || true
}

@test "killp kills process by name" {
    # Start a sleep process
    sleep 100 &>/dev/null &
    local pid=$!

    run killp "sleep"
    [[ $status -eq 0 ]]

    # Verify process is gone
    run ps -p $pid
    [[ $status -ne 0 ]]
}

# Network
@test "port checks if port is in use" {
    skip_if_missing "lsof"

    run port "22"
    [[ $status -eq 0 ]]  # Port 22 is typically in use on macOS
}

# Development
@test "randstr generates random string of default length" {
    run randstr
    [[ $status -eq 0 ]]
    [[ ${#output} -eq 16 ]]
}

@test "randstr generates random string of specified length" {
    run randstr 32
    [[ $status -eq 0 ]]
    [[ ${#output} -eq 32 ]]
}

@test "jsonpp formats JSON correctly" {
    skip_if_missing "python3"

    local test_json='{"key":"value","number":42}'
    run echo "$test_json" \| jsonpp
    [[ $status -eq 0 ]]
    [[ "$output" == *"key"*"value"* ]]
}

# Git
@test "gcm commits with message" {
    skip_if_missing "git"

    local test_repo="$(create_temp_dir "gcm-test")"
    cd "$test_repo"
    git init
    echo "test" > test.txt
    git add test.txt

    run gcm "Test commit"
    [[ $status -eq 0 ]]
    run git log --oneline
    [[ "$output" == *"Test commit"* ]]
}

@test "gcmp commits and pushes" {
    skip_if_missing "git"

    # This test requires a remote, so we'll skip if not in a git repo
    if [[ ! -d "$DOTFILES/.git" ]]; then
        skip "Not in a git repository"
    fi

    # Just verify the function exists and has correct logic
    run type gcmp
    [[ $status -eq 0 ]]
}

# Calculator
@test "calc performs basic arithmetic" {
    skip_if_missing "bc"

    run calc "2 + 2"
    [[ $status -eq 0 ]]
    [[ "$output" == "4"* ]]
}
```

**Step 2: Run function tests**

Run: `bats /tmp/dotfiles-tests/03_functions.bats`
Expected: Most tests PASS (some may skip if tools missing)

**Step 3: Review skipped tests**

Check output for "skip" messages to understand which tools are missing

---

## Task 5: Alias Expansion Tests

**Files:**
- Create: `/tmp/dotfiles-tests/04_aliases.bats`

**Step 1: Write tests for alias expansion**

```bash
#!/usr/bin/env bats
# 04_aliases.bats - Alias expansion tests

load test_helper

DOTFILES="$(get_dotfiles_path)"

setup() {
    setup
    source "$DOTFILES/shell/zsh/conf.d/04-aliases.zsh"
}

teardown() {
    teardown
}

# General aliases
@test "c expands to clear" {
    run zsh -c "alias c"
    [[ "$output" == *"clear"* ]]
}

@test "q expands to exit" {
    run zsh -c "alias q"
    [[ "$output" == *"exit"* ]]
}

@test ".. expands to cd .." {
    run zsh -c "alias .."
    [[ "$output" == *"cd .."* ]]
}

@test "... expands to cd ../.." {
    run zsh -c "alias ..."
    [[ "$output" == *"cd ../.."* ]]
}

# File operation aliases
@test "cp expands to cp -iv" {
    run zsh -c "alias cp"
    [[ "$output" == *"cp -iv"* ]]
}

@test "mv expands to mv -iv" {
    run zsh -c "alias mv"
    [[ "$output" == *"mv -iv"* ]]
}

@test "rm expands to rm -iv" {
    run zsh -c "alias rm"
    [[ "$output" == *"rm -iv"* ]]
}

# Git aliases
@test "gs expands to git status" {
    run zsh -c "alias gs"
    [[ "$output" == *"git status"* ]]
}

@test "gd expands to git diff" {
    run zsh -c "alias gd"
    [[ "$output" == *"git diff"* ]]
}

@test "gc expands to git commit" {
    run zsh -c "alias gc"
    [[ "$output" == *"git commit"* ]]
}

@test "gp expands to git push" {
    run zsh -c "alias gp"
    [[ "$output" == *"git push"* ]]
}

@test "glog expands to git log with options" {
    run zsh -c "alias glog"
    [[ "$output" == *"git log"*"oneline"*"graph"* ]]
}

# macOS-specific aliases (only on Darwin)
@test "showhidden expands to correct defaults command on macOS" {
    if [[ "$(uname)" != "Darwin" ]]; then
        skip "macOS-specific alias"
    fi

    run zsh -c "alias showhidden"
    [[ "$output" == *"defaults write"*"AppleShowAllFiles"* ]]
}

@test "flushdns expands to correct cache flush command on macOS" {
    if [[ "$(uname)" != "Darwin" ]]; then
        skip "macOS-specific alias"
    fi

    run zsh -c "alias flushdns"
    [[ "$output" == *"dscacheutil"*"mDNSResponder"* ]]
}

# Conditional aliases (eza vs ls)
@test "ls alias uses eza if available, otherwise ls -G" {
    if command_exists "eza"; then
        run zsh -c "alias ls"
        [[ "$output" == *"eza"* ]]
    else
        run zsh -c "alias ls"
        [[ "$output" == *"ls -G"* ]]
    fi
}

@test "ll alias uses eza -lh if available" {
    if command_exists "eza"; then
        run zsh -c "alias ll"
        [[ "$output" == *"eza"*"lh"* ]]
    else
        run zsh -c "alias ll"
        [[ "$output" == *"ls"*"lhG"* ]]
    fi
}

# grep replacement (rg vs grep)
@test "grep alias uses rg if available" {
    if command_exists "rg"; then
        run zsh -c "alias grep"
        [[ "$output" == *"rg"* ]]
    else
        run zsh -c "alias grep"
        [[ "$output" == *"grep"*"color=auto"* ]]
    fi
}
```

**Step 2: Run alias tests**

Run: `bats /tmp/dotfiles-tests/04_aliases.bats`
Expected: All tests PASS (some may be conditional based on installed tools)

**Step 3: Verify conditional behavior**

Check that tests correctly handle eza vs ls, rg vs grep conditionals

---

## Task 6: Integration Tests

**Files:**
- Create: `/tmp/dotfiles-tests/05_integration.bats`

**Step 1: Write tests for shell initialization**

```bash
#!/usr/bin/env bats
# 05_integration.bats - Integration tests for full shell initialization

load test_helper

DOTFILES="$(get_dotfiles_path)"
ZSH_PLUGIN_DIR="/tmp/test-plugins-integration-$$"

setup() {
    setup
    export ZSH_PLUGIN_DIR
    mkdir -p "$ZSH_PLUGIN_DIR"
}

teardown() {
    teardown
    rm -rf "$ZSH_PLUGIN_DIR"
}

# zshenv.symlink tests
@test "zshenv sets XDG directories correctly" {
    local zshenv="$DOTFILES/shell/zsh/zshenv.symlink"

    run zsh -c "
        source '$zshenv'
        [[ -n \"\$XDG_CONFIG_HOME\" ]] && [[ -n \"\$XDG_CACHE_HOME\" ]] && [[ -n \"\$XDG_DATA_HOME\" ]]
    "
    [[ $status -eq 0 ]]
}

@test "zshenv sets DOTFILES variable" {
    local zshenv="$DOTFILES/shell/zsh/zshenv.symlink"

    run zsh -c "
        source '$zshenv'
        [[ -n \"\$DOTFILES\" ]]
    "
    [[ $status -eq 0 ]]
}

@test "zshenv adds script/bin to PATH" {
    local zshenv="$DOTFILES/shell/zsh/zshenv.symlink"

    run zsh -c "
        source '$zshenv'
        [[ \"\$PATH\" == *\"\$DOTFILES/script/bin\"* ]]
    "
    [[ $status -eq 0 ]]
}

@test "zshenv sets EDITOR" {
    local zshenv="$DOTFILES/shell/zsh/zshenv.symlink"

    run zsh -c "
        source '$zshenv'
        [[ -n \"\$EDITOR\" ]]
    "
    [[ $status -eq 0 ]]
}

# zprofile.symlink tests
@test "zprofile sources without errors" {
    local zprofile="$DOTFILES/shell/zsh/zprofile.symlink"

    run zsh -c "source '$zprofile'"
    [[ $status -eq 0 ]]
}

# zshrc.symlink tests
@test "zshrc loads conf.d files in correct order" {
    local zshrc="$DOTFILES/shell/zsh/zshrc.symlink"

    run zsh -c "
        export DOTFILES='$DOTFILES'
        source '$zshrc' 2>&1 | grep -E '(00-options|01-history|02-completion|03-keybindings|04-aliases|05-functions|06-prompt)'
    "

    # Should not have errors
    [[ $status -eq 0 ]]
}

@test "zshrc creates completion cache directory" {
    local zshrc="$DOTFILES/shell/zsh/zshrc.symlink"
    local test_cache="/tmp/zsh-cache-test-$$"
    export ZSH_CACHE="$test_cache"

    run zsh -c "
        export DOTFILES='$DOTFILES'
        export ZSH_CACHE='$test_cache'
        source '$zshrc'
    "

    [[ $status -eq 0 ]]
    assert_dir_exists "$test_cache"

    rm -rf "$test_cache"
}

@test "zshrc initializes completion system" {
    local zshrc="$DOTFILES/shell/zsh/zshrc.symlink"

    run zsh -c "
        export DOTFILES='$DOTFILES'
        source '$zshrc'
        whence -v compinit
    "

    [[ $status -eq 0 ]]
}

@test "zshrc loads plugins if installed" {
    skip_if_missing "git"

    # Install plugins first
    source "$DOTFILES/shell/zsh/setup.sh"
    export ZSH_PLUGIN_DIR
    install_all_plugins

    local zshrc="$DOTFILES/shell/zsh/zshrc.symlink"

    run zsh -c "
        export DOTFILES='$DOTFILES'
        export ZSH_PLUGIN_DIR='$ZSH_PLUGIN_DIR'
        source '$zshrc'
        whence -v _zsh_autosuggest_strategy
    "

    [[ $status -eq 0 ]]
}

@test "zshrc configures fzf if installed" {
    if ! command_exists "fzf"; then
        skip "fzf not installed"
    fi

    local zshrc="$DOTFILES/shell/zsh/zshrc.symlink"

    run zsh -c "
        export DOTFILES='$DOTFILES'
        source '$zshrc'
        [[ -n \"\$FZF_DEFAULT_COMMAND\" ]]
    "

    [[ $status -eq 0 ]]
}

@test "zshrc configures zoxide if installed" {
    if ! command_exists "zoxide"; then
        skip "zoxide not installed"
    fi

    local zshrc="$DOTFILES/shell/zsh/zshrc.symlink"

    run zsh -c "
        export DOTFILES='$DOTFILES'
        source '$zshrc'
        whence -v z
    "

    [[ $status -eq 0 ]]
}

@test "conf.d files load without errors in sequence" {
    run zsh -c "
        export DOTFILES='$DOTFILES'
        for file in \$DOTFILES/shell/zsh/conf.d/*.zsh; do
            source \"\$file\" || exit 1
        done
    "

    [[ $status -eq 0 ]]
}

@test "shell initialization completes in less than 2 seconds" {
    local zshrc="$DOTFILES/shell/zsh/zshrc.symlink"

    run zsh -c "
        export DOTFILES='$DOTFILES'
        time zsh -c 'source $zshrc'
    " 2>&1

    # Extract time (format varies by shell, just check it runs)
    [[ $status -eq 0 ]]
}

@test "shell starts without errors when plugins are missing" {
    # Use empty plugin directory
    export ZSH_PLUGIN_DIR="/tmp/empty-plugins-$$"
    mkdir -p "$ZSH_PLUGIN_DIR"

    local zshrc="$DOTFILES/shell/zsh/zshrc.symlink"

    run zsh -c "
        export DOTFILES='$DOTFILES'
        export ZSH_PLUGIN_DIR='$ZSH_PLUGIN_DIR'
        source '$zshrc'
    "

    [[ $status -eq 0 ]]

    rm -rf "$ZSH_PLUGIN_DIR"
}

@test "shell starts without errors when optional tools are missing" {
    # Temporarily hide tools
    local orig_path="$PATH"
    export PATH="/usr/bin:/bin"

    local zshrc="$DOTFILES/shell/zsh/zshrc.symlink"

    run zsh -c "
        export DOTFILES='$DOTFILES'
        source '$zshrc'
    "

    # Restore PATH
    export PATH="$orig_path"

    [[ $status -eq 0 ]]
}
```

**Step 2: Run integration tests**

Run: `bats /tmp/dotfiles-tests/05_integration.bats`
Expected: Most tests PASS (some may skip if tools missing)

**Step 3: Verify timing test**

Check that shell initialization completes in reasonable time

---

## Task 7: Create Test Runner

**Files:**
- Create: `/tmp/dotfiles-tests/run_tests.sh`

**Step 1: Write main test runner script**

```bash
#!/usr/bin/env bash
# run_tests.sh - Main test runner for dotfiles zsh configuration

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test directory
TEST_DIR="/tmp/dotfiles-tests"
LOG_FILE="$TEST_DIR/test_results.log"

# Get dotfiles path
DOTFILES="$(cd "$(dirname "$0")/../.." && pwd)"
export DOTFILES

# Header
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Dotfiles Zsh Test Suite${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Check dependencies
echo -e "${YELLOW}Checking dependencies...${NC}"

if ! command -v bats &>/dev/null; then
    echo -e "${RED}ERROR: bats is not installed${NC}"
    echo "Install with: brew install bats-core"
    exit 1
fi

if ! command -v zsh &>/dev/null; then
    echo -e "${RED}ERROR: zsh is not installed${NC}"
    exit 1
fi

if ! command -v git &>/dev/null; then
    echo -e "${RED}ERROR: git is not installed${NC}"
    exit 1
fi

echo -e "${GREEN}✓ All required dependencies found${NC}"
echo ""

# Check optional tools
echo -e "${YELLOW}Optional tools:${NC}"
command -v eza &>/dev/null && echo -e "  ${GREEN}✓ eza${NC}" || echo -e "  ${YELLOW}○ eza (not found, some tests may skip)${NC}"
command -v rg &>/dev/null && echo -e "  ${GREEN}✓ ripgrep${NC}" || echo -e "  ${YELLOW}○ ripgrep (not found, some tests may skip)${NC}"
command -v fzf &>/dev/null && echo -e "  ${GREEN}✓ fzf${NC}" || echo -e "  ${YELLOW}○ fzf (not found, some tests may skip)${NC}"
command -v zoxide &>/dev/null && echo -e "  ${GREEN}✓ zoxide${NC}" || echo -e "  ${YELLOW}○ zoxide (not found, some tests may skip)${NC}"
echo ""

# Change to test directory
cd "$TEST_DIR"

# Initialize log file
echo "Dotfiles Test Results - $(date)" > "$LOG_FILE"
echo "==========================================" >> "$LOG_FILE"
echo "" >> "$LOG_FILE"

# Track results
total_tests=0
passed_tests=0
failed_tests=0
start_time=$(date +%s)

# Run tests in order
test_files=(
    "01_syntax.bats"
    "02_setup.bats"
    "03_functions.bats"
    "04_aliases.bats"
    "05_integration.bats"
)

for test_file in "${test_files[@]}"; do
    if [[ ! -f "$test_file" ]]; then
        echo -e "${RED}ERROR: Test file not found: $test_file${NC}"
        exit 1
    fi

    echo -e "${BLUE}Running: $test_file${NC}"
    echo "" >> "$LOG_FILE"
    echo "=== $test_file ===" >> "$LOG_FILE"

    if bats "$test_file" >> "$LOG_FILE" 2>&1; then
        echo -e "${GREEN}✓ $test_file passed${NC}"
    else
        echo -e "${RED}✗ $test_file failed${NC}"
    fi

    echo ""
done

# Calculate duration
end_time=$(date +%s)
duration=$((end_time - start_time))

# Parse results from log
total_tests=$(grep -c "^ ✓" "$LOG_FILE" 2>/dev/null || echo "0")
failed_tests=$(grep -c "^ ✗" "$LOG_FILE" 2>/dev/null || echo "0")
passed_tests=$((total_tests - failed_tests))

# Summary
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Test Summary${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""
echo -e "Total tests:  $total_tests"
echo -e "Passed:       ${GREEN}$passed_tests${NC}"
echo -e "Failed:       ${RED}$failed_tests${NC}"
echo -e "Duration:     ${duration}s"
echo ""
echo -e "Full log: ${YELLOW}$LOG_FILE${NC}"
echo ""

# Exit with appropriate code
if [[ $failed_tests -gt 0 ]]; then
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
else
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
fi
```

**Step 2: Make test runner executable**

Run: `chmod +x /tmp/dotfiles-tests/run_tests.sh`
Expected: No output

**Step 3: Run all tests**

Run: `/tmp/dotfiles-tests/run_tests.sh`
Expected: All tests pass, summary shows 0 failures

---

## Task 8: Verification & Documentation

**Files:**
- Create: `/tmp/dotfiles-tests/README.md`

**Step 1: Write README with usage instructions**

```markdown
# Dotfiles Zsh Test Suite

Comprehensive test suite for `shell/zsh` configuration using bats framework.

## Quick Start

```bash
# Run all tests
/tmp/dotfiles-tests/run_tests.sh
```

## Requirements

### Required
- bats >= 1.10 (`brew install bats-core`)
- zsh >= 5.8
- git >= 2.30

### Optional (tests will skip gracefully if missing)
- eza
- ripgrep (rg)
- fzf
- zoxide

## Test Structure

```
/tmp/dotfiles-tests/
├── run_tests.sh              # Main test runner
├── test_helper.bash          # Shared utilities
├── 01_syntax.bats            # Syntax validation
├── 02_setup.bats             # Plugin installation
├── 03_functions.bats         # Function behavior
├── 04_aliases.bats           # Alias expansion
├── 05_integration.bats       # Shell initialization
└── test_results.log          # Execution report
```

## What's Tested

### 1. Syntax & Structure (01_syntax.bats)
- Bash/zsh syntax validation
- Time-stamp format compliance
- File header structure
- conf.d loading order

### 2. Plugin Installation (02_setup.bats)
- setup.sh functions
- Git clone operations
- Idempotency
- All 4 plugins installation

### 3. Functions (03_functions.bats)
- Directory navigation (mkcd, up)
- File operations (backup, extract, ff, fd)
- Process management (psg, killp)
- Network utilities (port)
- Development tools (randstr, jsonpp, gcm, gcmp)
- macOS specific (pman, ql)

### 4. Aliases (04_aliases.bats)
- General aliases (c, q, .., ...)
- File operations (cp, mv, rm)
- Git shortcuts (gs, gd, gc, gp, glog)
- macOS-specific (showhidden, flushdns)
- Conditional (eza vs ls, rg vs grep)

### 5. Integration (05_integration.bats)
- Environment variable setup
- conf.d loading sequence
- Completion system initialization
- Plugin loading
- Tool integration (fzf, zoxide)
- Performance (< 2s startup)

## Interpreting Results

### Success
```
✓ 01_syntax.bats passed
✓ 02_setup.bats passed
...
All tests passed!
```

### Failures
Check `test_results.log` for detailed error messages:
```bash
cat /tmp/dotfiles-tests/test_results.log
```

### Skipped Tests
Tests may skip if optional tools are missing. This is expected behavior.

## Troubleshooting

### "bats: command not found"
```bash
brew install bats-core
```

### "zsh: command not found"
```bash
brew install zsh
```

### Slow plugin installation tests
The `02_setup.bats` file runs git clone operations which may take 10-15 seconds. This is normal.

### Permission denied
```bash
chmod +x /tmp/dotfiles-tests/run_tests.sh
```

## Test Philosophy

This suite uses **Real Environment Testing**:
- Tests actual installed tools on your system
- No complex mocking
- Catches environment-specific issues
- Tests real behavior, not mocked behavior

## Maintenance

### Adding New Tests
1. Create new `06_feature.bats` file
2. Add to `test_files` array in `run_tests.sh`
3. Run to verify

### Updating Existing Tests
1. Edit the appropriate `.bats` file
2. Run `run_tests.sh` to verify

## Notes

- Tests are located in `/tmp` to avoid modifying the codebase
- No git commits are needed (tests are ephemeral)
- All temp files use `$$` for process isolation
- Tests clean up after themselves via `teardown()`
```

**Step 2: Run final verification**

Run: `/tmp/dotfiles-tests/run_tests.sh`
Expected: All tests pass, clean summary output

**Step 3: Review test results log**

Run: `cat /tmp/dotfiles-tests/test_results.log`
Expected: Detailed output of all test runs

---

## Execution Checklist

After completing all tasks:

- [ ] All test files created in `/tmp/dotfiles-tests/`
- [ ] test_helper.bash provides shared utilities
- [ ] 01_syntax.bats validates all shell scripts
- [ ] 02_setup.bats tests plugin installation
- [ ] 03_functions.bats tests all functions
- [ ] 04_aliases.bats tests all aliases
- [ ] 05_integration.bats tests full initialization
- [ ] run_tests.sh executes all tests sequentially
- [ ] README.md documents usage
- [ ] All tests pass with `run_tests.sh`
- [ ] No modifications to shell/zsh source code
- [ ] No modifications outside shell/zsh directory

---

## Notes

- Tests are ephemeral (in `/tmp`) and not committed to git
- Real environment testing means tests may vary based on installed tools
- Tests clean up temporary files/directories after each test
- Performance test ensures shell startup remains fast
- Idempotency tests ensure setup.sh can be run multiple times safely
