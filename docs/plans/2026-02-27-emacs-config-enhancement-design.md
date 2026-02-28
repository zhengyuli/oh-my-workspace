# Emacs Configuration Enhancement Design

**Date:** 2026-02-27
**Status:** Approved
**Scope:** 9 improvements across 3 priority phases

## Overview

This design documents the implementation of 9 enhancements to the Emacs configuration, based on the comprehensive evaluation conducted on 2026-02-27. The improvements address package management reliability, LSP completion, security, keybinding organization, syntax highlighting, code formatting, and debugging capabilities.

## Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Package manager | Keep package.el | Stability, add mirror sources for reliability |
| Keybinding system | Migrate to general.el | Better organization with leader prefixes |
| Sensitive config | auth-source + password-store | Leverage existing pass setup, no HTTP proxy changes |
| Implementation | Incremental commits | Easy review and rollback |

## Phase 1: High Priority (3 commits)

### 1.1 Package Mirror Sources

**File:** `emacs/init.el`

Add China mirror fallbacks for package archives to improve reliability:

```elisp
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
```

**Behavior:** Primary sources tried first, mirrors as automatic fallback.

### 1.2 Eglot + Cape Completion Integration

**Files:** `init-completion.el`, `init-prog.el`

Combine LSP completions with local completion backends:

```elisp
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-super
                               #'eglot-completion-at-point
                               #'cape-file)))))
```

**Behavior:** LSP completions combined with file path completion, no duplicate candidates.

### 1.3 Auth-Source Infrastructure

**Files:** `init-auth.el`, `init.el`

Add helper functions for retrieving secrets from password-store:

```elisp
(defun emacs-get-secret (host &optional user)
  "Retrieve secret from auth-source for HOST and USER."
  (auth-source-pick-first-password
   :host host
   :user (or user user-login-name)))

(defun emacs-get-api-key (service)
  "Retrieve API key for SERVICE from auth-source."
  (emacs-get-secret (format "api.%s.com" service) "apikey"))
```

**Behavior:** HTTP proxy unchanged. Users can store API keys in pass and retrieve programmatically.

## Phase 2: Medium Priority (3 commits)

### 2.1 general.el Migration

**New File:** `lisp/core/init-keybind.el`
**Modified:** `init.el`, `init-funcs.el`, module files

Structure keybindings with leader prefix `C-c`:

| Prefix | Purpose | Examples |
|--------|---------|----------|
| `C-c f` | Files | find, recent, save |
| `C-c b` | Buffers | switch, kill, list |
| `C-c p` | Project | switch, find, search |
| `C-c g` | Git | status, log, blame |
| `C-c s` | Search | grep, rg, consult |
| `C-c t` | Toggle | theme, fullscreen, tabs |
| `C-c e` | Edit | format, comment, multi-cursor |

**Migration strategy:**
- Keep `lazy-set-key` for mode-specific maps
- Global bindings migrate to general.el
- Preserve existing shortcuts (no breaking changes)

### 2.2 Tree-sitter Enhancement

**Files:** `init-prog.el`, language modules

Enable tree-sitter modes for better syntax highlighting:

```elisp
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (go-mode . go-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (sh-mode . bash-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (cmake-mode . cmake-ts-mode))))
```

**Behavior:** Auto-uses tree-sitter modes when grammars available. `treesit-auto` installs grammars on first use.

### 2.3 C/C++ clang-format

**File:** `init-cc.el`

Add auto-formatting for C/C++ files:

```elisp
(use-package clang-format
  :defer t
  :after cc-mode
  :config
  (add-hook 'c-mode-hook
            (lambda () (add-hook 'before-save-hook #'clang-format-buffer nil t)))
  (add-hook 'c++-mode-hook
            (lambda () (add-hook 'before-save-hook #'clang-format-buffer nil t))))
```

**Behavior:** Auto-format on save. Requires `brew install clang-format`.

## Phase 3: Low Priority (3 commits)

### 3.1 Performance Monitoring Toggle

**File:** `init.el`

Add toggle for performance debugging:

```elisp
(defun emacs-toggle-performance-debug ()
  "Toggle performance monitoring for debugging startup."
  (interactive)
  (setq emacs-performance-debug (not emacs-performance-debug))
  ;; Toggle use-package statistics
  )
```

**Behavior:** Default off. `M-x emacs-toggle-performance-debug` enables verbose logging.

### 3.2 repeat-mode Integration

**File:** `init-editing.el`

Enable repeat mode for efficient repeated commands:

```elisp
(when (fboundp 'repeat-mode)
  (repeat-mode 1)
  ;; Define repeat maps for narrowing, case changes, etc.
  )
```

**Behavior:** After `narrow-to-region`, subsequent keys repeat without prefix.

### 3.3 Elpaca Migration Documentation

**New File:** `docs/future-elpaca-migration.md`

Documentation-only commit outlining future Elpaca migration path.

## Testing & Validation

### After Each Commit
```bash
emacs --batch -l ~/.emacs.d/init.el
```

### Validation Checklist
- [ ] 1.1: `M-x package-refresh-contents` succeeds
- [ ] 1.2: Completion includes LSP + file paths in Python file
- [ ] 1.3: `(emacs-get-api-key "test")` returns nil without error
- [ ] 2.1: `C-c f f` opens find-file
- [ ] 2.2: Python file uses `python-ts-mode`
- [ ] 2.3: C file formats on save
- [ ] 3.1: `M-x emacs-toggle-performance-debug` toggles state
- [ ] 3.2: `M-x narrow-to-region` enters repeat mode
- [ ] 3.3: Documentation file exists

### Dependencies
```bash
brew install clang-format
# Tree-sitter grammars auto-installed by treesit-auto
```

## Risk Assessment

| Item | Scope | Risk | Impact |
|------|-------|------|--------|
| 1.1 Package mirrors | Low | Very Low | Reliability |
| 1.2 Eglot + Cape | Medium | Low | Productivity |
| 1.3 Auth-source | Low | Very Low | Security |
| 2.1 general.el | High | Medium | Organization |
| 2.2 Tree-sitter | Medium | Low | UX |
| 2.3 clang-format | Low | Very Low | Code quality |
| 3.1 Performance | Low | Very Low | Debugging |
| 3.2 repeat-mode | Low | Very Low | Efficiency |
| 3.3 Elpaca docs | Very Low | None | Documentation |

## Rollback Strategy

Each commit is atomic. If issues arise:
```bash
git revert HEAD
# or
git checkout HEAD~1 -- emacs/lisp/path/to/file.el
```
