# Test Design: shell/zsh Comprehensive Testing

**Date:** 2026-03-13
**Author:** Claude Code
**Status:** Approved

## Overview

**Testing Approach:** Real Environment Testing - validates zsh configuration against actual installed tools on macOS system.

**Test Framework:** bats (Bash Automated Testing System)

**Test Location:** `/tmp/dotfiles-tests/`

**Test Runner:** Single executable script that runs all tests sequentially

## Architecture

```
/tmp/dotfiles-tests/
├── run_tests.sh              # Main test runner (single entry point)
├── test_helper.bash          # Shared test utilities and setup/teardown
├── 01_syntax.bats            # Syntax validation for all files
├── 02_setup.bats             # setup.sh plugin installation tests
├── 03_functions.bats         # Function behavior tests
├── 04_aliases.bats           # Alias expansion tests
├── 05_integration.bats       # Full shell initialization tests
└── test_results.log          # Test execution report (generated)
```

## Test Categories

### 1. Syntax & Structure Tests (`01_syntax.bats`)

**Purpose:** Validate all shell scripts have valid syntax and follow conventions

**Tests:**
- All `.sh` files pass `bash -n` syntax check
- All `.zsh` and `.symlink` files pass `zsh -n` syntax check
- All files have correct Time-stamp format (English weekday)
- All files have correct file header structure
- `*.symlink` files exist and are regular files
- `conf.d/*.zsh` files are numbered 00-06 and loadable

### 2. setup.sh Tests (`02_setup.bats`)

**Purpose:** Test plugin installation with real git operations

**Setup:**
- Creates temporary plugin directory in `/tmp/test-plugins-$$/`
- Uses real `git clone` operations
- Cleanup after each test

**Tests:**
- `check_zsh()` function works
- `ensure_plugin_dir()` creates directory if missing
- `install_plugin()` clones from GitHub successfully
- `install_plugin()` handles already-installed plugins (idempotency)
- `install_all_plugins()` installs all 4 plugins in correct order
- Plugins are cloned to correct location
- Error handling when git URL is invalid

### 3. Function Tests (`03_functions.bats`)

**Purpose:** Test utility functions work with real commands

**Tests by Function:**

**Directory Navigation:**
- `mkcd()` creates directory and changes into it
- `up()` navigates up N directories correctly

**File Operations:**
- `backup()` creates timestamped backup file
- `extract()` handles .tar.gz, .zip, .tar.bz2 formats
- `ff()` finds files by pattern
- `fd()` finds directories by pattern

**Process Management:**
- `psg()` finds processes by name
- `killp()` kills processes

**Network:**
- `port()` checks if port is in use

**Development:**
- `randstr()` generates random string
- `serve()` starts Python HTTP server
- `jsonpp()` formats JSON correctly

**Git:**
- `gcm()` commits with message
- `gcmp()` commits and pushes
- `gclone()` clones and changes directory

**macOS Specific:**
- `pman()` opens man page in Preview
- `ql()` triggers Quick Look

### 4. Alias Tests (`04_aliases.bats`)

**Purpose:** Verify aliases expand to correct commands

**Tests:**

**General:**
- `c` expands to `clear`
- `q` expands to `exit`
- `..` expands to `cd ..`
- `...` expands to `cd ../..`

**ls Replacements:**
- `ls` expands to `eza` or `ls -G` (conditional)
- `ll` expands to `eza -lh` or `ls -lhG`
- `la` expands to `eza -lha` or `ls -lhAG`

**grep Replacements:**
- `grep` expands to `rg` or `grep --color=auto`
- `fgrep` expands correctly

**File Operations:**
- `cp` expands to `cp -iv`
- `mv` expands to `mv -iv`
- `rm` expands to `rm -iv`

**Git:**
- `gs` expands to `git status`
- `gd` expands to `git diff`
- `glog` expands to `git log --oneline --graph --decorate`

**macOS:**
- `showhidden` expands to correct defaults command
- `flushdns` expands to correct cache flush command

### 5. Integration Tests (`05_integration.bats`)

**Purpose:** Test complete shell initialization process

**Tests:**

**zshenv.symlink:**
- Sets XDG directories correctly
- Sets DOTFILES variable
- Adds script/bin to PATH
- Sets EDITOR

**zprofile.symlink:**
- Adds Homebrew paths
- Adds pyenv paths
- Sources without errors

**zshrc.symlink:**
- Loads conf.d files in correct order (00-06)
- Creates completion cache directory
- Initializes completion system
- Loads plugins if installed
- Configures fzf if installed
- Configures zoxide if installed
- Sources local config files if they exist

**conf.d Loading Order:**
- 00-options.zsh sets shell options
- 01-history.zsh configures history
- 02-completion.zsh initializes completion
- 03-keybindings.zsh binds keys
- 04-aliases.zsh defines aliases
- 05-functions.zsh defines functions
- 06-prompt.zsh sets prompt

**Plugin Integration:**
- zsh-autosuggestions loads without errors
- zsh-syntax-highlighting loads without errors
- zsh-history-substring-search loads without errors
- zsh-completions loads without errors

**Error Handling:**
- Shell starts without errors when plugins are missing
- Shell starts without errors when optional tools are missing

**Performance:**
- Shell initialization completes in < 2 seconds

## Test Runner (`run_tests.sh`)

**Purpose:** Single entry point that runs all tests in sequence

**Features:**
1. Environment Setup (sets DOTFILES, creates test directory)
2. Dependency Check (bats, zsh, git, optional tools)
3. Test Execution (runs tests in order 01-05)
4. Reporting (summary, execution time, failures)
5. Cleanup (removes temp dirs, preserves log)

## Success Criteria

**Test Pass Conditions:**
1. All syntax tests pass
2. setup.sh successfully clones all 4 plugins
3. All functions work with real commands
4. All aliases expand correctly
5. Shell initializes without errors in < 2 seconds

## Limitations

**What This Tests:**
- Syntax correctness
- Function behavior with real commands
- Plugin installation process
- Shell initialization sequence

**What This Doesn't Test:**
- Visual appearance of prompt
- Interactive behavior
- Performance under heavy load
- Cross-platform compatibility

## System Requirements

- macOS (tested on Darwin)
- zsh >= 5.8
- git >= 2.30
- bats >= 1.10
- Optional: eza, ripgrep, fzf, zoxide

## Estimated Execution Time

| Test Suite | Time |
|------------|------|
| 01_syntax.bats | 1-2s |
| 02_setup.bats | 10-15s |
| 03_functions.bats | 5-10s |
| 04_aliases.bats | 1-2s |
| 05_integration.bats | 5-10s |
| **Total** | **25-40s** |
