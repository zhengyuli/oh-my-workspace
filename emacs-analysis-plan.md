# emacs-analysis-plan.md
# =============================================================================
# Emacs Configuration Analysis Plan
# =============================================================================

# Emacs Configuration Analysis Plan

## 1. Executive Summary

The Emacs configuration at `emacs/.config/emacs/` is well-engineered with
consistent use-package patterns, proper XDG compliance, a clear module
architecture, and thorough file headers. The overall compliance is approximately
90-92%, not the 100% claimed in `emacs/CLAUDE.md`.

Seventeen issues were identified. The most impactful are a confirmed functional
bug in `centaur-tabs` key binding (both group-navigation keys call the same
command) and a missing guard for `nerd-icons` in terminal mode.

**Overall code quality rating: 9.0/10**

| Dimension | Rating | Notes |
|---|---|---|
| Architecture | Excellent | Modular, consistent, proper separation of concerns |
| Naming convention | 99% | Site-packages are an intentional edge case |
| Documentation | 98% | Minor errors in separator examples in `emacs/CLAUDE.md` |
| Code style | 93% | Inline `setq` comments, missing `:defer t` |
| Functionality | 97% | `centaur-tabs` binding bug, `nerd-icons` guard missing |

---

## 2. Directory Layout Analysis

```
emacs/.config/emacs/lisp/
├── editor/       (7 files) - UI, editing, completion, search, fonts, templates
├── lib/          (2 files) - proxy, utils (shared library modules)
├── lang/         (8 files) - prog, cc, go, python, typescript/javascript, elisp, shell
│   └── config/  (3 files) - cmake, yaml, dockerfile
├── text/         (1 file)  - markdown
└── tool/         (5 files) - ai, git, pass, pdf, term
```

### Strengths

- `lib/` correctly isolates shared utilities from feature modules.
- No directory is overloaded; module count per directory is balanced.
- The `tool/` grouping cleanly separates developer tooling from language support.

### Observations Requiring User Decision

**`lang/config/` subdirectory** - The three files it contains are structurally
identical to files directly in `lang/`. The subdirectory name `config` is
overloaded and could be confused with "Emacs configuration." See Question D.

**`text/` directory** - Currently contains only one file (`omw-markdown.el`).
This is thin but defensible as a placeholder. See Question C.

---

## 3. Issues Found

### 3.1 Critical - Standards Violations (4 issues)

**Issue C-1: `dired-custom-extension.el` missing mandatory `Time-stamp`**
- File: `site-packages/dired-custom-extension-0.0.1/dired-custom-extension.el:2`
- Line 2 is a blank `;;` with no `Time-stamp`. CLAUDE.md mandates this on ALL files.
- Fix: Add `Time-stamp: <2026-03-18 00:00:00 Wednesday by zhengyu.li>` on line 2.

**Issue C-2: `corfu-terminal` use-package missing `:defer t`**
- File: `lisp/editor/omw-completion.el:85`
- Block reads: `:ensure t / :when ... / :after corfu / :config ...`
- Required order: `:ensure :when :defer :after :hook :bind :custom-face :config`
- Fix: Insert `:defer t` between `:when` and `:after corfu`.

**Issue C-3: `claude-code-ide` use-package missing `:defer t`**
- File: `lisp/tool/omw-ai.el:40`
- Block reads: `:vc ... / :bind ... / :config ...` with no explicit `:defer t`.
- Fix: Insert `:defer t` after `:vc (...)`.

**Issue C-4: `dired-custom-extension.el` functions lack `omw/` prefix**
- File: `site-packages/dired-custom-extension-0.0.1/dired-custom-extension.el`
- All 13 functions use `dired-` prefix, not `omw/`. See Question A.

---

### 3.2 High - Functional Issues (3 issues)

**Issue H-1: `centaur-tabs` group navigation — both `M-P` and `M-N` call the same command**
- File: `lisp/editor/omw-appearance.el:83-85`
- Both keys are bound to `centaur-tabs-switch-group`.
- Impact: Group navigation is one-directional regardless of key pressed.
- Fix: Bind `M-P` → `centaur-tabs-backward-group`, `M-N` → `centaur-tabs-forward-group`.

**Issue H-2: `omw/pet-mode-line-indicator` calls `nerd-icons-devicon` without display guard**
- File: `lisp/lang/omw-python.el:60-63`
- In terminal mode, `nerd-icons-devicon` will raise `Symbol's function definition is void`.
- Fix: Wrap with `(when (fboundp 'nerd-icons-devicon) ...)`.

**Issue H-3: `dired-goto-first-line`/`dired-goto-last-line` use obsolete functions**
- File: `site-packages/dired-custom-extension-0.0.1/dired-custom-extension.el:47,53`
- `beginning-of-buffer` and `end-of-buffer` are obsolete; generate byte-compiler warnings.
- Fix: Replace with `(goto-char (point-min))` and `(goto-char (point-max))`.

---

### 3.3 Medium - Architecture and Naming Issues (3 issues)

**Issue M-1: `omw-javascript.el` file name does not match content**
- File: `lisp/lang/omw-javascript.el`
- Only configures TypeScript (`typescript-mode`). No `js-mode` support.
- See Question B.

**Issue M-2: Redundant `(require 'omw-utils)` in `init.el`**
- File: `init.el:252`
- Every lang module that uses `omw-utils` already issues `(require 'omw-utils)`
  inside its own hooks. The explicit require in `init.el` is redundant.
- Fix: Remove `(require 'omw-utils)` from `init.el`.

**Issue M-3: `lang/config/` subdirectory naming ambiguity**
- See Question D.

---

### 3.4 Low - Code Style Issues (7 issues)

**Issue L-1: Inline comments within `setq` blocks**
- Files:
  - `lisp/editor/omw-appearance.el:132` — comment inside `dashboard` setq
  - `lisp/editor/omw-font.el:135-140` — comments inside `textsize` setq
  - `lisp/editor/omw-explorer.el:158-164` — comments inside `dired-omit-files` concat
- `emacs/CLAUDE.md` permits inline comments only inside `let*` binding descriptions.
- Fix: Move each comment to a standalone line above the relevant code block.

**Issue L-2: Misleading docstrings in `omw/cc-mode-setup` and `omw/go-mode-setup`**
- Files: `lisp/lang/omw-cc.el:52`, `lisp/lang/omw-go.el:52`
- Both say "Apply custom settings for X mode." but only call `tools-check-and-prompt`.
- Fix: Change to "Check required X development tools and prompt to install if missing."

**Issue L-3: `emacs/CLAUDE.md` separator examples are 78 chars (should be 79)**
- CLAUDE.md states 79 chars but examples show 75 `=` signs (78 total).
- Code files correctly use 76 `=` signs (79 total). Documentation is wrong.
- Fix: Add one `=` to each separator example in `emacs/CLAUDE.md`.

**Issue L-4: `(when (not noninteractive) ...)` should be `(unless noninteractive ...)`**
- File: `lisp/lib/omw-utils.el:70`
- Fix: Replace with `unless`.

**Issue L-5: `omw/find-available-font` lambda has over-indented body**
- File: `lisp/editor/omw-font.el:74-77`
- `font-list` and `(and font ...)` body are indented one extra space.
- Fix: Correct to standard Emacs Lisp `cl-find-if` / lambda formatting.

**Issue L-6: `prog-mode` single hook uses unnecessary double parentheses**
- File: `lisp/lang/omw-prog.el:150`
- `:hook ((prog-mode . omw/prog-mode-setup))` should be `:hook (prog-mode . omw/prog-mode-setup)`.
- Fix: Remove outer parentheses.

**Issue L-7: `dirvish-attributes` uses `append` on two constant lists**
- File: `lisp/editor/omw-explorer.el:66-67`
- `(append '(...) '(...))` on constants — combine into a single literal list.
- Fix: Replace with `'(vc-state subtree-state nerd-icons git-msg file-modes file-time file-size)`.

---

## 4. Proposed Action Plan

### 4.1 Clear Fixes (19 changes, no user decision required)

| Group | File | Changes |
|---|---|---|
| A | `site-packages/.../dired-custom-extension.el` | Add Time-stamp (C-1); replace obsolete buffer nav calls (H-3) |
| B | `lisp/editor/omw-completion.el` | Add `:defer t` to `corfu-terminal` (C-2) |
| C | `lisp/tool/omw-ai.el` | Add `:defer t` to `claude-code-ide` (C-3) |
| D | `lisp/editor/omw-appearance.el` | Fix `centaur-tabs` group nav bindings (H-1); move inline comment (L-1) |
| E | `lisp/lang/omw-python.el` | Guard `nerd-icons-devicon` call (H-2) |
| F | `lisp/lang/omw-cc.el`, `omw-go.el` | Correct misleading docstrings (L-2) |
| G | `init.el` | Remove redundant `(require 'omw-utils)` (M-2) |
| H | `lisp/editor/omw-font.el` | Fix lambda indentation (L-5); move inline comments (L-1) |
| I | `lisp/editor/omw-explorer.el` | Move inline comments (L-1); flatten `dirvish-attributes` list (L-7) |
| J | `lisp/lib/omw-utils.el` | Replace `when (not ...)` with `unless` (L-4) |
| K | `lisp/lang/omw-prog.el` | Remove double parentheses from `:hook` (L-6) |
| L | `emacs/CLAUDE.md` | Fix separator example widths (L-3) |

### 4.2 Conditional Actions (await user decision)

| Question | Issue | Action if approved |
|---|---|---|
| A | C-4 | Add `omw/` prefix to all 13 functions in `dired-custom-extension.el` |
| B | M-1 | Rename `omw-javascript.el` → `omw-typescript.el` (update provide + require) |
| C | — | Decide whether to merge `text/` into `editor/` or keep as-is |
| D | M-3 | Rename `lang/config/` → `lang/infra/` or flatten into `lang/` |

---

## 5. Questions Requiring User Decision

### Question A: `omw/` prefix in `dired-custom-extension.el`

The `site-packages/` directory holds code maintained as if it were an external
package. All 13 functions use `dired-` prefix, consistent with Emacs ecosystem
convention for dired-ecosystem extensions.

**For keeping `dired-` prefix:** The file is in `site-packages/` to signal
"treat as external package." External packages are not subject to the `omw/`
naming rule. The `dired-` prefix follows established Emacs conventions and
would enable future MELPA publication.

**For adding `omw/` prefix:** `CLAUDE.md` mandates `omw/` for all custom
functions without exception. The code is not actually published externally.

**Decision needed:** Formally document `site-packages/` as exempt, or rename
all 13 functions?

---

### Question B: `omw-javascript.el` naming

The file configures only TypeScript. Its name says "javascript."

- **Option 1 (Recommended):** Rename to `omw-typescript.el`, update
  `(provide 'omw-javascript)` → `(provide 'omw-typescript)` and
  `(require 'omw-javascript)` → `(require 'omw-typescript)` in `init.el`.
- **Option 2:** Add `js-mode` hook registration to make the name accurate
  (TypeScript LSP already handles `.js` files; only a hook is needed).
- **Option 3:** Keep as-is with an explanatory comment.

**Decision needed:** Which option?

---

### Question C: `text/` directory

`lisp/text/` contains only `omw-markdown.el`.

- **Option 1:** Keep as a placeholder for future text-mode modules (org, rst, tex).
- **Option 2:** Move `omw-markdown.el` to `lisp/editor/` and remove the directory.

**Decision needed:** Keep sparse `text/` directory, or merge into `editor/`?

---

### Question D: `lang/config/` subdirectory naming

The name `config` conflicts with the common use of "config" to mean "Emacs
configuration."

- **Option 1:** Keep `lang/config/` as-is (current grouping is clear enough).
- **Option 2:** Rename to `lang/infra/` (clearer terminology for cmake/yaml/dockerfile).
- **Option 3:** Flatten the 3 files directly into `lisp/lang/` (simplest).

**Decision needed:** Which option?

---

## Summary Statistics

| Category | Count | Files Affected |
|---|---|---|
| Critical (standards violations) | 4 | 3 files |
| High (functional/bugs) | 3 | 3 files |
| Medium (architecture) | 3 | 2 files + directory |
| Low (style) | 7 | 8 files |
| **Total** | **17** | **10+ files** |

Clear fixes ready for implementation: 19 changes across 13 files.
Awaiting user decision: 4 questions (A, B, C, D).
