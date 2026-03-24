# Rules Enhancement Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add three new rule files and enhance two existing files to establish strong consistency constraints for configuration code in oh-my-workspace

**Architecture:** Additive approach - create new files alongside existing rules and enhance existing files with backward-compatible additions. No restructuring of directory layout.

**Tech Stack:** Markdown documentation, Claude Code rules system

---

## File Structure

**New files:**
- `.claude/rules/patterns.md` (~150 lines) - Immutability principle and design patterns
- `.claude/rules/development-workflow.md` (~120 lines) - Research-first workflow for configs
- `.claude/rules/hooks.md` (~100 lines) - Claude Code hooks integration

**Enhanced files:**
- `.claude/rules/coding-style.md` (+40 lines) - Add immutability and file organization sections
- `.claude/rules/ai-generation.md` (+60 lines) - Add configuration code standards

**Total changes:** ~470 lines across 5 files

---

## Task 1: Create patterns.md

**Files:**
- Create: `.claude/rules/patterns.md`

**Purpose:** Define design patterns and principles for maintainable configuration code

- [x] **Step 1: Create file header and immutability section**

Create `.claude/rules/patterns.md`:

```markdown
# Common Patterns

Design patterns and principles for oh-my-workspace configuration code.

## Immutability Principle

**ALWAYS create new configurations, NEVER modify existing ones in-place:**

```bash
# WRONG: Modifies file in-place
sed -i 's/old/new/' ~/.zshrc

# CORRECT: Creates new version with backup
cp ~/.zshrc ~/.zshrc.bak
sed 's/old/new/' ~/.zshrc.bak > ~/.zshrc
```

**For Emacs Lisp:**

```elisp
;; WRONG: Modifies existing list
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; CORRECT: Creates new list with addition
(setq auto-mode-alist
      (cons '("\\.py\\'" . python-mode) auto-mode-alist))
```

**Rationale:**
- Enables easy rollback if changes break
- Prevents unintended side effects
- Makes debugging easier
- Maintains history of changes
```

- [x] **Step 2: Add file organization section**

Append to `.claude/rules/patterns.md`:

```markdown
## File Organization

**MANY SMALL FILES > FEW LARGE FILES:**

- **Target size:** 200-400 lines per file
- **Maximum size:** 800 lines (split if approaching)
- **Organization:** By feature/domain, not by type
- **Cohesion:** High within files, low between files

**When to split configuration files:**

- Emacs: Separate major modes into individual files (e.g., `init-python.el`, `init-git.el`)
- Zsh: Split aliases, functions, and options into separate files
- Shell scripts: Extract utilities from main scripts

**Example - Modular Emacs Config:**

```
~/.config/emacs/
├── init.el          # Main entry (< 100 lines)
├── init-base.el     # Base configuration (< 300 lines)
├── init-editor.el   # Editor settings (< 200 lines)
├── init-shell.el    # Shell integration (< 150 lines)
├── init-git.el      # Git configuration (< 200 lines)
└── init-python.el   # Python setup (< 150 lines)
```

**Benefits:**
- Easier to find and modify specific functionality
- Reduces merge conflicts
- Enables selective loading
- Improves readability
```

- [x] **Step 3: Add safe defaults section**

Append to `.claude/rules/patterns.md`:

```markdown
## Safe Defaults

**Always provide fallback values:**

```bash
# Good - uses default if not set
EDITOR="${EDITOR:-emacs}"
PAGER="${PAGER:-less}"

# Bad - fails if not set
export EDITOR
```

**For environment variables:**

```bash
# Provide sensible defaults
export PATH="${HOME}/bin:${PATH:-/usr/local/bin:/usr/bin:/bin}"
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
```
```

- [x] **Step 4: Add modular configuration section**

Append to `.claude/rules/patterns.md`:

```markdown
## Modular Emacs Configuration

**Structure Emacs configs for maintainability:**

```elisp
;;; init.el -*- lexical-binding: t; -*-
;; Main entry point - load modules

;; Core modules (required)
(require 'init-base)
(require 'init-packages)
(require 'init-keybindings)

;; Feature modules (optional based on use)
(require 'init-shell)
(require 'init-git)
(require 'init-project)

;; Local overrides (not in git)
(require 'init-local nil t)
```

**Each module file should:**
- Be < 400 lines
- Have single responsibility (e.g., `init-git.el` only configures git-related packages)
- Be documented with commentary section
- Handle missing dependencies gracefully

**Module template:**

```elisp
;;; init-<feature>.el --- Brief description -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Your Name

;;; Commentary:

;; Full description of what this module configures.
;; Dependencies: init-base.el, package-X

;;; Code:

;; Configuration here

(provide 'init-<feature>)
;;; init-<feature>.el ends here
```
```

- [x] **Step 5: Add design patterns section**

Append to `.claude/rules/patterns.md`:

```markdown
## Design Patterns

### Repository Pattern for Configs

Encapsulate config access behind consistent interface:

```bash
# Define standard operations
config_load() { /* load config */ }
config_save() { /* save config */ }
config_validate() { /* check syntax */ }
config_backup() { /* create backup */ }
```

**Benefits:**
- Enables easy swapping of config locations
- Simplifies testing with mock configs
- Centralizes error handling

### Validation Pattern

**Always validate configs before use:**

```bash
_validate_config() {
  local -r config_file="$1"

  # Check file exists
  [[ -f "$config_file" ]] || return 1

  # Check readable
  [[ -r "$config_file" ]] || return 1

  # Validate syntax (shell)
  bash -n "$config_file" 2>/dev/null || return 1

  return 0
}

# Usage
if _validate_config "$config"; then
  source "$config"
else
  printf 'error: invalid config %s\n' "$config" >&2
  return 1
fi
```
```

- [x] **Step 6: Add anti-patterns section**

Append to `.claude/rules/patterns.md`:

```markdown
## Anti-Patterns to Avoid

### Deep Nesting

**Limit nesting to 4 levels maximum:**

```bash
# WRONG - 5 levels deep
if condition1; then
  if condition2; then
    if condition3; then
      if condition4; then
        if condition5; then
          # Too deep!
        fi
      fi
    fi
  fi
fi

# CORRECT - Refactor to reduce nesting
_validate_and_execute() {
  condition1 || return 1
  condition2 || return 1
  condition3 || return 1
  condition4 || return 1
  # Execute logic here
}
```

### Magic Numbers

**No hardcoded values - use constants or config:**

```bash
# WRONG
sleep 30
timeout 300

# CORRECT
readonly CONNECT_TIMEOUT=30
readonly MAX_RETRIES=3
readonly RETRY_DELAY=5

sleep "$RETRY_DELAY"
timeout "$CONNECT_TIMEOUT"
```

### Hardcoded Paths

**Use variables instead of hardcoded paths:**

```bash
# WRONG
source /home/user/.config/zsh/aliases.zsh

# CORRECT
source "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/aliases.zsh"
```

### Large Monolithic Configs

**Don't create one giant config file:**

```bash
# WRONG - 2000 line .zshrc
# Everything in one file

# CORRECT - Modular structure
source "$ZDOTDIR/aliases.zsh"
source "$ZDOTDIR/functions.zsh"
source "$ZDOTDIR/options.zsh"
source "$ZDOTDIR/completion.zsh"
```
```

- [x] **Step 7: Verify file completeness**

Check that `.claude/rules/patterns.md`:
- Has all 6 main sections
- Follows markdown formatting conventions
- Examples use correct syntax highlighting
- Total length is ~150 lines

- [x] **Step 8: Commit patterns.md**

```bash
git add .claude/rules/patterns.md
git commit -m "feat(rules): add patterns.md with immutability and design patterns"
```

Expected: Commit created successfully

---

## Task 2: development-workflow.md ✅ COMPLETE

**Files:**
- Create: `.claude/rules/development-workflow.md`

**Purpose:** Define research-first workflow for configuration development

- [x] **Step 1: Create file header and research section**

Create `.claude/rules/development-workflow.md`:

```markdown
# Development Workflow

Process for developing and modifying configuration files in oh-my-workspace.

> This file extends [git-workflow.md](./git-workflow.md) with the full feature development process that happens before git operations.

## Research First (MANDATORY)

**Before implementing any configuration change:**

### 1. Check Existing Implementations

```bash
# Search within oh-my-workspace
rg "similar-config" --type sh
rg "alias ll" --type zsh

# Check dotfiles repositories
gh search repos "dotfiles zsh aliases"
gh search code "export EDITOR" --language shell
```

### 2. Review Documentation

```bash
# Use Context7 for package docs
# Or read primary vendor documentation
# Verify version compatibility
```

### 3. Understand Dependencies

Ask yourself:
- What other configs will be affected?
- What tools depend on this config?
- Are there version requirements?

**Example:**
```markdown
Adding ripgrep config:
- Affects: .zshrc (aliases), .bashrc (aliases)
- Depends on: ripgrep installed
- Conflicts with: grep aliases
```
```

- [ ] **Step 2: Add planning phase section**

Append to `.claude/rules/development-workflow.md`:

```markdown
## Planning Phase

**Before making changes:**

### 1. Document the Change

Create brief plan answering:
- **What:** Which config(s) are you modifying?
- **Why:** What problem does this solve?
- **Impact:** What else will be affected?

### 2. Create Implementation Plan

For complex changes, list:
1. Files to modify
2. Order of operations
3. Backup strategy
4. Rollback plan

### 3. Use TodoWrite for Complex Changes

**When to use TodoWrite:**
- Multi-file configuration updates
- Complex package installation
- Breaking changes affecting multiple configs
- Refactoring across files

**Example:**

```markdown
Tasks:
- [ ] Research existing zsh aliases in project
- [ ] Create new alias file: tool/zsh/.config/zsh/aliases.zsh
- [ ] Add alias sourcing to main .zshrc
- [ ] Update README with new aliases
- [ ] Test in new shell session
```

**TodoWrite reveals:**
- Out of order steps
- Missing dependencies
- Extra unnecessary changes
- Wrong granularity
- Misinterpreted requirements
```

- [ ] **Step 3: Add implementation phase section**

Append to `.claude/rules/development-workflow.md`:

```markdown
## Implementation Phase

**For configuration files:**

### 1. Backup Before Modifying

**Follow immutability principle:**

```bash
# Create backup
cp ~/.zshrc ~/.zshrc.bak

# Or use version control
git add .zshrc
git commit -m "backup: before adding aliases"
```

### 2. Make Changes Incrementally

- One logical change at a time
- Test after each change
- Document as you go

**Example workflow:**
```bash
# Step 1: Add new alias section
# Test: open new terminal, verify aliases work

# Step 2: Update README
# Test: verify README is accurate

# Step 3: Commit
git add -A
git commit -m "feat(zsh): add ripgrep aliases"
```

### 3. Follow Immutability

**Create new versions instead of modifying in-place:**

```bash
# Instead of: sed -i 's/old/new/' file
# Do this:
cp file file.bak
sed 's/old/new/' file.bak > file
```

**Use symlinks to switch between versions:**

```bash
# Maintain versioned configs
~/.config/zsh/
├── zshrc.v1
├── zshrc.v2
└── current -> zshrc.v2  # symlink
```
```

- [ ] **Step 4: Add verification phase section**

Append to `.claude/rules/development-workflow.md`:

```markdown
## Verification Phase

**After making changes:**

### 1. Syntax Validation

```bash
# Shell scripts
bash -n script.sh

# Zsh scripts
zsh -n script.zsh

# Emacs Lisp
emacs --batch -l '(load "init.el")'

# Python
python -m py_compile script.py
```

### 2. Functional Testing

**Open new environment to test:**

```bash
# For shell configs
zsh  # Open new shell
# Test aliases, functions, options

# For Emacs configs
emacs  # Restart Emacs
# Test keybindings, modes, packages
```

**Check symlinks are correct:**

```bash
# Verify stow symlinks
ls -la ~/.config/zsh
ls -la ~/.config/emacs
```

### 3. Documentation Check

Ensure:
- [ ] README updated if needed
- [ ] Inline comments explain non-obvious choices
- [ ] File header is present and accurate
- [ ] CLAUDE.md updated if behavior changes
```

- [ ] **Step 5: Add rollback strategy section**

Append to `.claude/rules/development-workflow.md`:

```markdown
## Rollback Strategy

**If changes cause issues:**

### 1. Git Rollback

```bash
# Check what changed
git diff .zshrc

# Restore specific file
git checkout .zshrc

# Or clean all uncommitted changes
git clean -fd
```

### 2. Backup Restoration

```bash
# Restore from backup
cp ~/.zshrc.bak ~/.zshrc

# Or restore from stow
cd /path/to/oh-my-workspace
./setup.sh uninstall zsh
./setup.sh install zsh
```

### 3. Symlink Switch

**If using versioned configs:**

```bash
# Switch to previous version
ln -sf ~/.config/zsh/zshrc.v1 ~/.config/zsh/current

# Restart shell to apply
exec zsh
```
```

- [ ] **Step 6: Add config-specific workflows section**

Append to `.claude/rules/development-workflow.md`:

```markdown
## Config-Specific Workflows

### Adding New Package

**Example: Adding ripgrep configuration**

1. **Research:**
   ```bash
   # Check for existing ripgrep configs
   rg "ripgrep" --type sh
   # Check dotfiles repos
   gh search code "rg --" --language shell
   ```

2. **Plan:**
   ```markdown
   Files: tool/rg/.config/rg/config
   Dependencies: ripgrep installed
   Conflicts: grep aliases in zsh
   ```

3. **Create:**
   ```bash
   mkdir -p tool/rg/.config/rg
   # Add config file with header
   ```

4. **Configure:**
   ```bash
   # Add to setup.sh PKG_ALL array
   PKG_ALL=(
     ...
     tool/rg
   )
   ```

5. **Test:**
   ```bash
   stow -n -v -d tool/rg  # Dry-run
   stow -v -d tool/rg      # Install
   rg --version           # Verify
   ```

6. **Document:**
   - Update README.md
   - Add inline comments
   - Document dependencies

### Modifying Existing Config

1. **Research:** Understand current implementation
2. **Backup:** Create backup or git commit
3. **Plan:** Document changes and impact
4. **Implement:** Make incremental changes
5. **Verify:** Test in clean environment
6. **Commit:** Follow [git-workflow.md](./git-workflow.md) conventions

### Merging Upstream Changes

1. **Research:** Understand what changed upstream
2. **Plan:** Identify conflicts with local customizations
3. **Merge:** Carefully integrate changes
4. **Test:** Verify functionality preserved
5. **Document:** Update comments explaining local customizations
```

- [ ] **Step 7: Verify file completeness**

Check that `.claude/rules/development-workflow.md`:
- Has all main sections
- Follows markdown formatting conventions
- Examples are practical and clear
- Total length is ~120 lines

- [ ] **Step 8: Commit development-workflow.md**

```bash
git add .claude/rules/development-workflow.md
git commit -m "feat(rules): add development-workflow.md with research-first approach"
```

Expected: Commit created successfully

---

## Task 3: Create hooks.md

**Files:**
- Create: `.claude/rules/hooks.md`

**Purpose:** Define Claude Code hooks integration for configuration management

- [ ] **Step 1: Create file header and hook types section**

Create `.claude/rules/hooks.md`:

```markdown
# Hooks System

Integration with Claude Code hooks for configuration management.

## Hook Types

**Available hooks for oh-my-workspace:**

- **PreToolUse**: Before tool execution (validation, parameter modification)
- **PostToolUse**: After tool execution (auto-format, verification)
- **Stop**: When session ends (final verification)
```

- [ ] **Step 2: Add TodoWrite best practices section**

Append to `.claude/rules/hooks.md`:

```markdown
## TodoWrite Best Practices

**Use TodoWrite tool to track configuration changes:**

### When to Use

- Multi-file configuration updates
- Complex package installation
- Breaking changes that affect multiple configs
- Refactoring shell aliases across files

### Benefits

TodoWrite reveals:
- Out of order steps
- Missing dependencies
- Extra unnecessary changes
- Wrong granularity
- Misinterpreted requirements

### Example Usage

**Adding new shell configuration:**

```markdown
Tasks:
- [ ] Research existing zsh aliases in project
- [ ] Create new alias file: tool/zsh/.config/zsh/aliases.zsh
- [ ] Add alias sourcing to main .zshrc
- [ ] Update README with new aliases
- [ ] Test in new shell session
```

**Installing new tool:**

```markdown
Tasks:
- [ ] Check for existing configs in oh-my-workspace
- [ ] Research dotfiles repos for reference implementations
- [ ] Create package directory structure
- [ ] Add configuration files with headers
- [ ] Update setup.sh PKG_ALL array
- [ ] Test stow dry-run
- [ ] Install and verify
- [ ] Update README and docs
```
```

- [ ] **Step 3: Add auto-formatting hooks section**

Append to `.claude/rules/hooks.md`:

```markdown
## Auto-Formatting Hooks

**PostToolUse hook for configuration files:**

After editing configuration files, automatically:

### 1. Validate Syntax

```bash
# Shell scripts
bash -n "$FILE" 2>/dev/null || echo "Syntax error in $FILE"

# Emacs Lisp
emacs --batch -l "(load \"$FILE\")" 2>&1 || echo "Error in $FILE"

# Python
python -m py_compile "$FILE" 2>&1 || echo "Error in $FILE"
```

### 2. Check File Size

```bash
lines=$(wc -l < "$FILE")
if (( lines > 800 )); then
  echo "Warning: $FILE is getting large ($lines lines). Consider splitting."
fi
```

### 3. Verify Header Presence

```bash
# Check for file header
if ! grep -q "^# .*-.*-\*- mode:" "$FILE"; then
  echo "Warning: $FILE missing standard header"
fi
```
```

- [ ] **Step 4: Add pre-commit hooks section**

Append to `.claude/rules/hooks.md`:

```markdown
## Pre-Commit Hooks

**Before committing configuration changes:**

### 1. Verify All Configs

```bash
# Run syntax checks on all modified configs
git diff --cached --name-only | grep -E '\.(sh|zsh|el)$' | while read file; do
  bash -n "$file" 2>/dev/null
done
```

### 2. Check for Secrets

```bash
# Verify no hardcoded secrets
git diff --cached | grep -E '(password|token|api_key|secret)' && {
  echo "Error: Potential secrets detected in commit"
  exit 1
}
```

### 3. Validate File Headers

```bash
# Ensure all new files have headers
git diff --cached --name-only --diff-filter=A | while read file; do
  if ! head -5 "$file" | grep -q "^# "; then
    echo "Error: $file missing file header"
    exit 1
  fi
done
```
```

- [ ] **Step 5: Add session end verification section**

Append to `.claude/rules/hooks.md`:

```markdown
## Session End Verification

**Stop hook for final checks:**

When session ends:

### 1. Verify Testing

Check all modified configs have been tested:
- [ ] Shell configs tested in new terminal
- [ ] Emacs configs tested with restart
- [ ] Symlinks verified

### 2. Check Uncommitted Changes

```bash
# Check for uncommitted critical files
git status --short | grep -E '\.(zshrc|bashrc|init\.el)$' && {
  echo "Warning: Uncommitted changes to critical configs"
}
```

### 3. Ensure Documentation Updated

If behavior changed, verify:
- [ ] README updated if needed
- [ ] Inline comments added
- [ ] CLAUDE.md updated if behavior changed
```

- [ ] **Step 6: Add permission management section**

Append to `.claude/rules/hooks.md`:

```markdown
## Permission Management

**Auto-accept for configuration changes:**

### Guidelines

Use with caution:
- **Enable for:** Well-defined, repetitive config updates
- **Disable for:** Exploratory config changes, major refactoring
- **Never use:** dangerously-skip-permissions flag

### Recommended Configuration

Configure `allowedTools` in `~/.claude.json` instead of auto-accept:

```json
{
  "allowedTools": {
    "Read": ["^/Users/zhengyu\\.li/.*"],
    "Edit": ["^/Users/zhengyu\\.li/oh-my-workspace/.*"],
    "Write": ["^/Users/zhengyu\\.li/oh-my-workspace/.*"],
    "Bash": [
      "^stow ",
      "^bash -n ",
      "^zsh -n ",
      "^git (status|diff|add|commit) "
    ]
  }
}
```

### Safety Rules

1. **Never skip permission prompts** unless change is well-understood
2. **Review each permission request** for safety
3. **Use TodoWrite** to track complex changes instead of auto-accept
```

- [ ] **Step 7: Verify file completeness**

Check that `.claude/rules/hooks.md`:
- Has all main sections
- Follows markdown formatting conventions
- Examples are practical
- Total length is ~100 lines

- [ ] **Step 8: Commit hooks.md**

```bash
git add .claude/rules/hooks.md
git commit -m "feat(rules): add hooks.md with TodoWrite best practices"
```

Expected: Commit created successfully

---

## Task 4: Enhance coding-style.md

**Files:**
- Modify: `.claude/rules/coding-style.md`

**Purpose:** Add immutability principle and file organization standards

- [ ] **Step 1: Read current coding-style.md**

```bash
cat .claude/rules/coding-style.md
```

Expected: File content displayed

- [ ] **Step 2: Add immutability principle after "Universal Rules" section**

Insert after the "## Language-Specific Extensions" section header:

```markdown

## Immutability Principle

**ALWAYS create new configurations, NEVER modify existing ones in-place:**

```bash
# WRONG: Direct modification
sed -i 's/old/new/' ~/.zshrc

# CORRECT: Create new version
cp ~/.zshrc ~/.zshrc.bak
sed 's/old/new/' ~/.zshrc.bak > ~/.zshrc
```

**For Emacs Lisp:**

```elisp
;; WRONG: Modifies existing list
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; CORRECT: Creates new list with addition
(setq auto-mode-alist
      (cons '("\\.py\\'" . python-mode) auto-mode-alist))
```

**Rationale:**
- Enables easy rollback if changes break
- Prevents unintended side effects
- Makes debugging easier
- Maintains history of changes
```

- [ ] **Step 3: Add file organization section**

Insert after immutability section:

```markdown

## File Organization

**MANY SMALL FILES > FEW LARGE FILES:**

- **Target size:** 200-400 lines per file
- **Maximum size:** 800 lines (split if approaching)
- **Organization:** By feature/domain, not by type
- **Cohesion:** High within files, low between files

**When to split configuration files:**

- Emacs: Separate major modes into individual files (e.g., `init-python.el`, `init-git.el`)
- Zsh: Split aliases, functions, and options into separate files
- Shell scripts: Extract utilities from main scripts

**Example - Modular Emacs Config:**

```
~/.config/emacs/
├── init.el          # Main entry (< 100 lines)
├── init-base.el     # Base configuration (< 300 lines)
├── init-editor.el   # Editor settings (< 200 lines)
├── init-shell.el    # Shell integration (< 150 lines)
├── init-git.el      # Git configuration (< 200 lines)
└── init-python.el   # Python setup (< 150 lines)
```

**Benefits:**
- Easier to find and modify specific functionality
- Reduces merge conflicts
- Enables selective loading
- Improves readability
```

- [ ] **Step 4: Enhance code quality checklist**

Replace the "### Code Quality" section (lines 65-70) with:

```markdown
### Code Quality

- **No dead code** - Remove unused functions and variables
- **Single responsibility** - Each function does one thing well
- **DRY** - Don't repeat yourself; extract common logic
- **YAGNI** - Don't add features you don't need yet
- **Immutability** - Create new versions, never modify in-place
- **File size** - Keep files focused (< 800 lines)

## Code Quality Checklist

```

Then replace the existing checklist with:

```markdown
## Code Quality Checklist

**Before marking work complete:**

- [ ] Code is readable and well-named
- [ ] Functions are small (<50 lines)
- [ ] Files are focused (<800 lines)
- [ ] No deep nesting (>4 levels)
- [ ] Proper error handling
- [ ] No hardcoded values (use constants or config)
- [ ] No mutation (immutable patterns used)
- [ ] File size is reasonable (split if >800 lines)
```

- [ ] **Step 5: Verify changes**

Check that `.claude/rules/coding-style.md`:
- Has immutability section after "Universal Rules"
- Has file organization section
- Enhanced code quality checklist
- Total additions ~40 lines

- [ ] **Step 6: Commit coding-style.md enhancement**

```bash
git add .claude/rules/coding-style.md
git commit -m "feat(rules): enhance coding-style.md with immutability and file organization"
```

Expected: Commit created successfully

---

## Task 5: Enhance ai-generation.md

**Files:**
- Modify: `.claude/rules/ai-generation.md`

**Purpose:** Add configuration code standards and stronger constraints

- [ ] **Step 1: Read current ai-generation.md**

```bash
cat .claude/rules/ai-generation.md
```

Expected: File content displayed

- [ ] **Step 2: Add configuration code standards section after "Dotfiles-Specific Rules"**

Insert after the "## Quality Checklist" section:

```markdown

## Configuration Code Standards

### File Headers (MANDATORY)

**Every configuration file MUST include:**

**For Shell Scripts:**

```bash
# filename.ext -*- mode: sh; -*-
# =============================================================================
# Brief Description
#
# Location: $WORKSPACE_DIR/path/to/file
# Usage:    How to use this file
# =============================================================================
```

**For Emacs Lisp:**

```elisp
;;; filename.el --- Brief description -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Your Name

;; Author: Your Name
;; URL: https://github.com/user/repo

;;; Commentary:

;; Full description of what this file configures.

;;; Code:
```

**AI MUST ensure all generated config files include these headers.**

### Configuration Comments

**Comments in configuration code MUST:**

1. **Explain WHY, not WHAT:**
   ```bash
   # Good - explains WHY
   # Use full path for compatibility with stow (avoid symlink issues)
   export PATH="/usr/local/bin:$PATH"

   # Bad - explains WHAT
   # Export PATH
   export PATH="/usr/local/bin:$PATH"
   ```

2. **Document dependencies:**
   ```bash
   # Requires: git, fzf, ripgrep
   # Conflicts with: oh-my-zsh (if installed)
   alias gs='git status'
   ```

3. **Explain non-obvious choices:**
   ```bash
   # Use printf for POSIX compliance (echo -n not portable)
   # See: https://unix.stackexchange.com/q/65803
   printf '%s' "$var"
   ```

### Configuration Code Style

**AI MUST follow these rules when generating configuration code:**

1. **Modular over monolithic:**
   - Prefer multiple small config files
   - Avoid one giant .zshrc or init.el

2. **Explicit over implicit:**
   - Always use `source` instead of `.` for clarity
   - Use full paths instead of relative
   - Explicitly declare dependencies

3. **Version control friendly:**
   - Avoid machine-specific paths in committed files
   - Use `$HOME` or `$XDG_CONFIG_HOME` instead of hardcoded paths
   - Keep local overrides in separate files (e.g., `.zshrc.local`)

4. **Immutable patterns:**
   - Create backup before modifying
   - Generate new versions instead of in-place edits
   - Use symlinks to switch between versions
```

- [ ] **Step 3: Enhance quality checklist**

Replace the existing "## Quality Checklist" section (lines 133-143) with:

```markdown
## Quality Checklist for Config Code

**Before completing any configuration change:**

- [ ] All functions documented
- [ ] Complex logic explained
- [ ] No hardcoded secrets
- [ ] Input validation present
- [ ] Safe file operations
- [ ] Follows project conventions
- [ ] Commit message follows Conventional Commits
- [ ] Standard file header present
- [ ] Comments explain WHY, not WHAT
- [ ] Dependencies documented
- [ ] Non-obvious choices explained
- [ ] File size under 800 lines
- [ ] Modular structure (split if complex)
- [ ] No hardcoded machine-specific paths
- [ ] Backup created before modification
```

- [ ] **Step 4: Verify changes**

Check that `.claude/rules/ai-generation.md`:
- Has configuration code standards section after "Dotfiles-Specific Rules"
- Enhanced quality checklist with config-specific items
- Total additions ~60 lines

- [ ] **Step 5: Commit ai-generation.md enhancement**

```bash
git add .claude/rules/ai-generation.md
git commit -m "feat(rules): enhance ai-generation.md with config code standards"
```

Expected: Commit created successfully

---

## Task 6: Final Verification

**Files:**
- Verify all changes

**Purpose:** Ensure all files are complete and consistent

- [ ] **Step 1: Verify all new files exist**

```bash
ls -la .claude/rules/{patterns,development-workflow,hooks}.md
```

Expected: All three files listed

- [ ] **Step 2: Verify enhanced files have changes**

```bash
git diff HEAD~5 .claude/rules/coding-style.md
git diff HEAD~5 .claude/rules/ai-generation.md
```

Expected: Diffs show additions

- [ ] **Step 3: Check total line count**

```bash
wc -l .claude/rules/{patterns,development-workflow,hooks}.md
wc -l .claude/rules/coding-style.md .claude/rules/ai-generation.md
```

Expected: ~470 total new lines

- [ ] **Step 4: Verify all commits**

```bash
git log --oneline -5
```

Expected: 5 commits visible

- [ ] **Step 5: Test rules are loaded**

Start a new Claude Code session and verify rules are loaded by checking if Claude follows the new immutability principle.

- [ ] **Step 6: Create summary commit (if needed)**

If any files were missed:

```bash
git add -A
git commit -m "feat(rules): complete rules enhancement with patterns, workflow, and hooks"
```

Expected: Either "nothing to commit" or commit created

---

## Success Criteria

After completing all tasks:

1. ✅ `.claude/rules/patterns.md` exists with ~150 lines
2. ✅ `.claude/rules/development-workflow.md` exists with ~120 lines
3. ✅ `.claude/rules/hooks.md` exists with ~100 lines
4. ✅ `.claude/rules/coding-style.md` enhanced with +40 lines
5. ✅ `.claude/rules/ai-generation.md` enhanced with +60 lines
6. ✅ All files follow project markdown conventions
7. ✅ All commits follow Conventional Commits format
8. ✅ No conflicts with existing rules
9. ✅ Total additions ~470 lines
10. ✅ Rules are loaded in Claude Code session

## Estimated Time

- Task 1: patterns.md - 30 minutes
- Task 2: development-workflow.md - 25 minutes
- Task 3: hooks.md - 20 minutes
- Task 4: coding-style.md enhancement - 15 minutes
- Task 5: ai-generation.md enhancement - 20 minutes
- Task 6: Final verification - 10 minutes

**Total: ~2 hours** (including review and iteration)
