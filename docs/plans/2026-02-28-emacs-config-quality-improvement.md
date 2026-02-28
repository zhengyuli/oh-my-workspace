# Emacs Configuration Quality Improvement Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix code bugs, enhance documentation, unify time-stamps, and improve code comments for the Emacs configuration.

**Architecture:** Batch fix approach - modify 8 files total: 2 bug fixes, 1 documentation update, 2 time-stamp updates, 3 comment enhancements. All changes are independent and can be verified with `emacs --debug-init`.

**Tech Stack:** Emacs Lisp (Elisp), Markdown documentation, time-stamp auto-update

---

## Task 1: Fix before-save-hook Duplication in init-prog.el

**Files:**
- Modify: `emacs/lisp/init-prog.el:237-240`

**Step 1: Read the file to locate the duplicate hook**

Run: `head -n 250 emacs/lisp/init-prog.el | tail -n 20`

Expected output:
```elisp
;; ==================================================================================
;; Before save hooks (mode-specific)
;; Time-stamp: globally enabled (only affects files with Time-stamp marker)
(add-hook 'before-save-hook #'time-stamp)  ; <-- This line (239) to be removed

;; Programming mode specific before-save hook
(defun prog-before-save-hook ()
```

**Step 2: Remove the duplicate line 239**

Run: `sed -i '' '239d' emacs/lisp/init-prog.el`

Verify: `head -n 245 emacs/lisp/init-prog.el | tail -n 15`

Expected: Line 239 should now be the blank line after `;; Time-stamp:` comment, and `(add-hook 'before-save-hook #'time-stamp)` should be gone.

**Step 3: Verify the fix is correct**

Run: `grep -n "before-save-hook" emacs/lisp/init-prog.el`

Expected: Only 2 occurrences
- Line ~252: inside `prog-mode-hook` lambda (correct)
- Line ~243: inside `prog-before-save-hook` function (correct)

**Step 4: Test Emacs loads without errors**

Run: `emacs --batch --eval "(progn (require 'init-prog) (message \"init-prog loaded successfully\"))" 2>&1 | tail -n 5`

Expected: `init-prog loaded successfully` (no errors)

**Step 5: Commit**

```bash
git add emacs/lisp/init-prog.el
git commit -m "fix(init-prog): remove duplicate before-save-hook for time-stamp

The time-stamp hook is already set globally in init-base.el:247.
The local buffer hook in prog-mode-hook inherits the global setting.
Using (nil t) parameters makes the hook buffer-local.

This fixes redundant execution of time-stamp on every save."
```

---

## Task 2: Fix Spacing in init.el

**Files:**
- Modify: `emacs/init.el:97`

**Step 1: Read the file to locate the spacing issue**

Run: `sed -n '95,100p' emacs/init.el`

Expected output:
```elisp
;; ==================================================================================
;; Core modules (foundation - must load first)
(require' init-funcs)  ; <-- Missing space after require
(require 'init-packages)
```

**Step 2: Fix the spacing**

Run: `sed -i '' 's/(require'\'' init-funcs)/(require '\''init-funcs)/' emacs/init.el`

Verify: `sed -n '95,100p' emacs/init.el`

Expected: `(require 'init-funcs)` with proper spacing

**Step 3: Verify syntax with Emacs batch mode**

Run: `emacs --batch --eval "(progn (load-file \"emacs/init.el\") (message \"init.el syntax OK\"))" 2>&1 | tail -n 5`

Expected: `init.el syntax OK` (no errors about invalid read syntax)

**Step 4: Commit**

```bash
git add emacs/init.el
git commit -m "style(init): fix spacing in require statement

Change (require' init-funcs) to (require 'init-funcs)
for consistent formatting with other require statements."
```

---

## Task 3: Update CLAUDE.md with External Dependencies Section

**Files:**
- Modify: `CLAUDE.md`

**Step 1: Find the insertion point**

Run: `grep -n "## Validation" CLAUDE.md`

Expected: Line number where `## Validation` section starts (insert new section before this)

**Step 2: Read current CLAUDE.md to understand structure**

Run: `cat CLAUDE.md`

**Step 3: Create the External Dependencies section**

Create a temporary file with new content, then insert it before `## Validation`:

```bash
cat > /tmp/external-deps.md << 'EOF'
## External Dependencies

### Verify Dependencies
```bash
# Run in Emacs
M-x config-dependency-validate
```

### Core Tools (P0 - Required)
| Tool | Purpose | Install |
|------|---------|---------|
| git | Version control | brew install git |
| ripgrep | Code search | brew install ripgrep |
| the_silver_searcher | Fallback search | brew install the_silver_searcher |
| fd | Fast file find | brew install fd |
| coreutils | macOS GNU ls | brew install coreutils |

### LSP Servers (P1 - Development)
| Language | Server | Install |
|----------|--------|---------|
| Python | pylsp | pip install python-lsp-server[all] |
| Go | gopls | go install golang.org/x/tools/gopls@latest |
| C/C++ | clangd | Xcode Command Line Tools |
| YAML | yaml-language-server | npm install -g yaml-language-server |
| Bash | bash-language-server | npm install -g bash-language-server |
| Dockerfile | docker-langserver | npm install -g dockerfile-language-server-nodejs |
| CMake | cmake-language-server | pip install cmake-language-server |

### Auxiliary Tools (P2 - Optional)
| Tool | Purpose | Install |
|------|---------|---------|
| aspell | Spell checking | brew install aspell |
| hunspell | Spell checking alternative | brew install hunspell |
| pandoc | Document conversion | brew install pandoc |
| marksman | Markdown LSP | brew install marksman |
| libvterm | Emacs vterm dependency | brew install libvterm |

EOF
```

**Step 4: Insert the new section before `## Validation`**

Run (adjust line number based on Step 1):
```bash
# Find the line number of ## Validation
VALIDATION_LINE=$(grep -n "^## Validation" CLAUDE.md | cut -d: -f1)

# Insert new content before ## Validation
sed -i '' "${VALIDATION_LINE}r /tmp/external-deps.md" CLAUDE.md

# Add a blank line after inserted content for spacing
sed -i '' "$((VALIDATION_LINE))a\\
" CLAUDE.md
```

**Step 5: Verify the content was added**

Run: `grep -A 5 "## External Dependencies" CLAUDE.md`

Expected: Should see the new section header and table content

**Step 6: Commit**

```bash
git add CLAUDE.md
git commit -m "docs(CLAUDE.md): add External Dependencies section

- Add dependency verification command
- Categorize tools by priority (P0/P1/P2)
- Include installation instructions for all tools
- Cover LSP servers for Python, Go, C/C++, YAML, Bash, Dockerfile, CMake"
```

---

## Task 4: Update CLAUDE.md with Troubleshooting Section

**Files:**
- Modify: `CLAUDE.md`

**Step 1: Find the end of CLAUDE.md**

Run: `wc -l CLAUDE.md`

Expected: Total line count

**Step 2: Append Troubleshooting section**

```bash
cat >> CLAUDE.md << 'EOF'

## Troubleshooting

### Slow Emacs Startup
1. Check for byte-compile cache issues
   ```bash
   # Remove old byte-compiled files
   rm -rf ~/.emacs.d/eln-cache/
   ```
2. Run dependency validation
   ```bash
   M-x config-dependency-validate
   ```
3. Review startup logs
   ```bash
   # View *Messages* buffer
   C-h e
   ```

### Package Installation Fails
1. Check network and proxy settings
   ```elisp
   M-x show-http-proxy  ; Check current proxy
   M-x set-http-proxy   ; Set proxy if needed
   ```
2. Refresh package contents
   ```bash
   M-x package-refresh-contents
   ```
3. Verify use-package is installed
   ```bash
   M-x package-install RET use-package
   ```

### LSP Not Working
1. Confirm LSP server is installed
   ```bash
   M-x config-dependency-validate
   ```
2. Check eglot status
   ```bash
   M-x eglot  ; Start LSP manually
   ```
3. Review LSP events log
   ```bash
   C-h b  ; Switch to *eglot-events* buffer
   ```

### centaur-tabs Not Showing in Some Buffers
1. Check if buffer is explicitly hidden
   ```bash
   C-h v centaur-tabs-hide-predicates RET
   ```
2. Force refresh tabs
   ```bash
   M-x centaur-tabs-local-mode
   ```
EOF
```

**Step 3: Verify content was added**

Run: `tail -n 50 CLAUDE.md | head -n 10`

Expected: Should see `## Troubleshooting` section

**Step 4: Commit**

```bash
git add CLAUDE.md
git commit -m "docs(CLAUDE.md): add Troubleshooting section

- Slow Emacs startup diagnostics
- Package installation failure recovery
- LSP troubleshooting steps
- centaur-tabs display issues"
```

---

## Task 5: Update CLAUDE.md with Quick Start Section

**Files:**
- Modify: `CLAUDE.md`

**Step 1: Find the Project Overview section**

Run: `grep -n "^# CLAUDE.md" CLAUDE.md | head -1`

**Step 2: Insert Quick Start after Project Overview**

```bash
# Create Quick Start content
cat > /tmp/quick-start.md << 'EOF'

## Quick Start

### First-time Installation
```bash
cd ~/oh-my-workspace/emacs
./setup.sh
```

### Verify Installation
```bash
# Start Emacs
emacs

# Inside Emacs, verify dependencies
M-x config-dependency-validate
```

### Common Commands

| Command | Function |
|---------|----------|
| `M-x config-dependency-validate` | Verify external dependencies |
| `M-x cleanup-config-timers` | Clean up configuration timers |
| `M-x describe-variable` | Show variable documentation |
| `C-c p p` | Switch project (Projectile) |
| `C-c p f` | Find file in project |
| `C-c p b` | Switch project buffer |
| `C-x b` | Switch buffer (Vertico) |
| `C-s` | Search in buffer (Consult) |

EOF

# Find Project Overview section end (after CLAUDE.md content)
# Insert after the current directory description
OVERVIEW_END=$(grep -n "^# currentDate" CLAUDE.md | cut -d: -f1)

# Insert Quick Start before currentDate
sed -i '' "${OVERVIEW_END}r /tmp/quick-start.md" CLAUDE.md
```

**Step 3: Verify content was added**

Run: `grep -A 3 "## Quick Start" CLAUDE.md`

Expected: Should see Quick Start section with installation and commands

**Step 4: Commit**

```bash
git add CLAUDE.md
git commit -m "docs(CLAUDE.md): add Quick Start section

- First-time installation instructions
- Verification steps
- Common commands reference table"
```

---

## Task 6: Update Time-stamp in init-fonts.el

**Files:**
- Modify: `emacs/lisp/init-fonts.el:2`

**Step 1: Check current time-stamp**

Run: `head -n 3 emacs/lisp/init-fonts.el`

Expected output:
```elisp
;;; init-fonts.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-21 12:00:00 Saturday by zhengyuli>
```

**Step 2: Update time-stamp to 2026-02-28**

```bash
# Get current date in time-stamp format
TIMESTAMP=$(date +"%Y-%02m-%02d %02H:%02M:%02S %A by zhengyuli")

# Update the time-stamp line
sed -i '' "2s|;; Time-stamp: <.*>|;; Time-stamp: <$TIMESTAMP>|" emacs/lisp/init-fonts.el
```

**Step 3: Verify the update**

Run: `head -n 3 emacs/lisp/init-fonts.el`

Expected: `;; Time-stamp: <2026-02-28 HH:MM:SS Saturday by zhengyuli>` (date should be 2026-02-28)

**Step 4: Commit**

```bash
git add emacs/lisp/init-fonts.el
git commit -m "chore(init-fonts): update time-stamp to 2026-02-28"
```

---

## Task 7: Update Time-stamp in init-funcs.el

**Files:**
- Modify: `emacs/lisp/init-funcs.el:2`

**Step 1: Check current time-stamp**

Run: `head -n 3 emacs/lisp/init-funcs.el`

Expected output:
```elisp
;;; init-funcs.el -*- lexical-binding: t; -*-
;; Time-stamp: <2026-02-27 19:00:00 Thursday by zhengyuli>
```

**Step 2: Update time-stamp to 2026-02-28**

```bash
# Get current date in time-stamp format
TIMESTAMP=$(date +"%Y-%02m-%02d %02H:%02M:%02S %A by zhengyuli")

# Update the time-stamp line
sed -i '' "2s|;; Time-stamp: <.*>|;; Time-stamp: <$TIMESTAMP>|" emacs/lisp/init-funcs.el
```

**Step 3: Verify the update**

Run: `head -n 3 emacs/lisp/init-funcs.el`

Expected: `;; Time-stamp: <2026-02-28 HH:MM:SS Saturday by zhengyuli>` (date should be 2026-02-28)

**Step 4: Commit**

```bash
git add emacs/lisp/init-funcs.el
git commit -m "chore(init-funcs): update time-stamp to 2026-02-28"
```

---

## Task 8: Add centaur-tabs Documentation to init-ui.el

**Files:**
- Modify: `emacs/lisp/init-ui.el` (around line 117)

**Step 1: Find the centaur-tabs use-package block**

Run: `grep -n ";; Tabs - centaur-tabs" emacs/lisp/init-ui.el`

Expected: Line number ~117

**Step 2: Read the context around the block**

Run: `sed -n '115,125p' emacs/lisp/init-ui.el`

**Step 3: Insert documentation before the use-package block**

```bash
# Find the line number of ;; Tabs - centaur-tabs
TABS_LINE=$(grep -n ";; Tabs - centaur-tabs" emacs/lisp/init-ui.el | cut -d: -f1)

# Insert documentation before the existing comment
sed -i '' "$((TABS_LINE - 1))i\\
;; ==================================================================================\\
;; Tabs - centaur-tabs\\
;;\\
;; Buffer grouping logic (centaur-tabs-buffer-groups):\\
;;   - Magit modes (magit-status-mode, magit-log-mode, etc.) -> \"Magit\"\\
;;   - Terminal modes (vterm, shell, eshell) -> \"Terminal\"\\
;;   - Programming modes (python, go, c/c++, elisp) -> project-based groups\\
;;   - Special modes (dashboard, org) -> individual groups\\
;;\\
;; Performance optimization:\\
;;   - Uses weak hash table for buffer group caching\\
;;   - Cache invalidation on major mode changes\\
;;\\
;; Note: Do NOT redefine centaur-tabs-buffer-groups directly.\\
;;       Use advice-add to extend behavior.\\
;; ==================================================================================\\
" emacs/lisp/init-ui.el
```

**Step 4: Verify the insertion**

Run: `sed -n '115,140p' emacs/lisp/init-ui.el`

Expected: Should see the new documentation block followed by the original `;; Tabs - centaur-tabs` line

**Step 5: Test Emacs loads without errors**

Run: `emacs --batch --eval "(progn (require 'init-ui) (message \"init-ui loaded successfully\"))" 2>&1 | tail -n 5`

Expected: `init-ui loaded successfully` (no errors)

**Step 6: Commit**

```bash
git add emacs/lisp/init-ui.el
git commit -m "docs(init-ui): add centaur-tabs configuration documentation

- Document buffer grouping logic
- Explain performance optimization (caching)
- Add warning about not redefining centaur-tabs-buffer-groups"
```

---

## Task 9: Add Dirvish Keybindings Documentation to init-dired.el

**Files:**
- Modify: `emacs/lisp/init-dired.el` (around line 118)

**Step 1: Find the keybindings section**

Run: `grep -n ";; Key bindings" emacs/lisp/init-dired.el | grep -i dirvish`

Expected: Line number ~118 inside Dirvish config block

**Step 2: Read the context**

Run: `sed -n '116,145p' emacs/lisp/init-dired.el`

**Step 3: Insert documentation before bind-keys**

```bash
# Find the line number of (with-eval-after-load 'dirvish
DIRVISH_BIND_LINE=$(grep -n "(with-eval-after-load 'dirvish" emacs/lisp/init-dired | cut -d: -f1)

# Insert documentation before the bind-keys
sed -i '' "$((DIRVISH_BIND_LINE + 1))i\\
  ;; Key bindings\\
  ;;\\
  ;; Key layout:\\
  ;;   ? - Help menu (cheatsheet)\\
  ;;   a - Attributes setup menu\\
  ;;   o - Quick access directories\\
  ;;\\
  ;;   f - fd search    |    s - Quick sort      |    l - ls switches menu\\
  ;;   v - View file    |    * - Mark menu       |    y - Copy/paste menu\\
  ;;\\
  ;;   TAB - Expand/collapse subdirectory\\
  ;;   N - Narrow search\\
  ;;   H - Recent access history\\
  ;;\\
  ;; Note: Some keybindings override native dired keys.\\
  ;;       Dirvish overrides dired-mode-map bindings.\\
" emacs/lisp/init-dired.el
```

**Step 4: Verify the insertion**

Run: `sed -n '116,150p' emacs/lisp/init-dired.el`

Expected: Should see the new keybinding documentation

**Step 5: Test Emacs loads without errors**

Run: `emacs --batch --eval "(progn (require 'init-dired) (message \"init-dired loaded successfully\"))" 2>&1 | tail -n 5`

Expected: `init-dired loaded successfully` (no errors)

**Step 6: Commit**

```bash
git add emacs/lisp/init-dired.el
git commit -m "docs(init-dired): add Dirvish keybindings documentation

- Document key layout with visual grouping
- Explain potential conflicts with native dired keys
- Add quick reference for common operations"
```

---

## Task 10: Add Python Package Installation Documentation to init-python.el

**Files:**
- Modify: `emacs/lisp/init-python.el` (after line 41)

**Step 1: Find the python-dev-packages definition**

Run: `grep -n "defvar python-dev-packages" emacs/lisp/init-python.el`

Expected: Line number ~35

**Step 2: Read the context**

Run: `sed -n '33,50p' emacs/lisp/init-python.el`

**Step 3: Insert documentation after the defvar**

```bash
# Find the line number of the closing parenthesis of python-dev-packages
DEV_PACKAGES_END=$(grep -n "defvar python-dev-packages" emacs/lisp/init-python.el | cut -d: -f1)
DEV_PACKAGES_END=$((DEV_PACKAGES_END + 7))

# Insert documentation after the defvar
sed -i '' "${DEV_PACKAGES_END}a\\
;;\\
;; Auto package installation flow:\\
;;   1. Triggered on venv activation (poetry-venv-workon / pyvenv-workon)\\
;;   2. Async fetch installed packages list (cached to avoid duplicate calls)\\
;;   3. Compare against required packages list, find missing\\
;;   4. Validate package names for security (prevent command injection)\\
;;   5. Async install missing packages (progress shown in dedicated buffer)\\
;;\\
;; Cache mechanism:\\
;;   - Auto-invalidates when venv changes\\
;;   - Cache key: VIRTUAL_ENV environment variable\\
;;\\
;; Security measures:\\
;;   - Package name format validation (python-validate-package-name)\\
;;   - Shell argument escaping (shell-quote-argument)\\
" emacs/lisp/init-python.el
```

**Step 4: Verify the insertion**

Run: `sed -n '35,65p' emacs/lisp/init-python.el`

Expected: Should see the new documentation after `python-dev-packages` definition

**Step 5: Test Emacs loads without errors**

Run: `emacs --batch --eval "(progn (require 'init-python) (message \"init-python loaded successfully\"))" 2>&1 | tail -n 5`

Expected: `init-python loaded successfully` (no errors)

**Step 6: Commit**

```bash
git add emacs/lisp/init-python.el
git commit -m "docs(init-python): add async package installation flow documentation

- Document the 5-step auto installation process
- Explain cache mechanism and invalidation
- Highlight security measures (validation, escaping)"
```

---

## Task 11: Final Verification

**Files:**
- Test: All modified files

**Step 1: Verify all files compile**

Run: `emacs --batch --eval "(progn (setq load-path (cons \"emacs/lisp\" load-path)) (mapc #'require '(init-funcs init-packages init-base init-fonts init-ui init-dired init-python init-prog)) (message \"All modules compiled successfully\"))" 2>&1 | tail -n 10`

Expected: `All modules compiled successfully` (no errors)

**Step 2: Verify before-save-hook fix**

Run: `emacs --batch --eval "(progn (require 'init-prog) (message \"before-save-hook has %d entries\" (length before-save-hook)))" 2>&1 | grep "before-save-hook"`

Expected: Should show hook count (time-stamp should be counted once from init-base.el)

**Step 3: Verify CLAUDE.md changes**

Run: `grep -c "## " CLAUDE.md`

Expected: Should show increased section count (Quick Start, External Dependencies, Troubleshooting added)

**Step 4: Verify time-stamps are current**

Run: `grep "Time-stamp: <2026-02-28" emacs/lisp/init-fonts.el emacs/lisp/init-funcs.el`

Expected: Both files should show `2026-02-28` date

**Step 5: Verify comment additions**

Run: `grep -c ";; Performance optimization" emacs/lisp/init-ui.el emacs/lisp/init-dired.el emacs/lisp/init-python.el`

Expected: Should count the new documentation sections

**Step 6: Create summary commit**

```bash
git add -A
git commit -m "chore: finalize Emacs config quality improvements

All tasks completed:
- Fixed before-save-hook duplication in init-prog.el
- Fixed spacing in init.el require statement
- Added External Dependencies section to CLAUDE.md
- Added Troubleshooting section to CLAUDE.md
- Added Quick Start section to CLAUDE.md
- Updated time-stamps in init-fonts.el and init-funcs.el
- Added centaur-tabs documentation to init-ui.el
- Added Dirvish keybindings documentation to init-dired.el
- Added Python package installation flow documentation to init-python.el

Verification: All modules load without errors in batch mode."
```

---

## Testing Checklist

After all tasks complete:

- [ ] Emacs starts without errors: `emacs --debug-init`
- [ ] `M-x config-dependency-validate` runs successfully
- [ ] Time-stamps show 2026-02-28 in modified files
- [ ] `before-save-hook` executes only once per save
- [ ] CLAUDE.md has new sections (Quick Start, External Dependencies, Troubleshooting)
- [ ] New comments are present in init-ui.el, init-dired.el, init-python.el
- [ ] All git commits have descriptive messages

---

## Rollback Plan

If issues occur:

```bash
# Reset to before changes
git log --oneline -5  # Find commit before first change
git reset --hard <commit-hash>

# Or selectively revert
git revert HEAD  # Revert last commit
```

---

## Files Changed Summary

| File | Change | Lines |
|------|--------|-------|
| `emacs/lisp/init-prog.el` | Fix: remove duplicate hook | -1 |
| `emacs/init.el` | Fix: spacing | 0 |
| `CLAUDE.md` | Docs: add 3 sections | +150 |
| `emacs/lisp/init-fonts.el` | Chore: update time-stamp | 0 |
| `emacs/lisp/init-funcs.el` | Chore: update time-stamp | 0 |
| `emacs/lisp/init-ui.el` | Docs: add comments | +20 |
| `emacs/lisp/init-dired.el` | Docs: add comments | +15 |
| `emacs/lisp/init-python.el` | Docs: add comments | +20 |
