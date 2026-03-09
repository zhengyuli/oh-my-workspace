# use-package Content (Extracted from CLAUDE.md)

## use-package Keyword Order (MANDATORY) (Lines 201-391)

**Keywords MUST appear in this exact order:**

```
1. :ensure or :vc          # Package source
2. :when or :if            # Conditional loading (optional)
3. :defer or :demand       # Loading strategy
4. :after                  # Dependencies (optional)
5. :hook                   # Mode hooks
6. :bind                   # Keybindings
7. :custom-face            # Face customization (optional)
8. :config                 # Configuration
```

### :ensure/:vc Rules

**Rule A: External packages from ELPA/MELPA**
```elisp
(use-package <package>
  :ensure t)  ; ✅ CORRECT
```

**Rule B: Built-in Emacs packages**
```elisp
(use-package <built-in>
  :ensure nil)  ; ✅ CORRECT
```

**Rule C: Packages from Git/VCS**
```elisp
(use-package <package>
  :vc (:url "https://github.com/<user>/<repo>" :rev :newest))  ; ✅ CORRECT
```

### :defer vs :demand Rules

**Rule A: Default behavior - Use :defer t**
```elisp
(use-package <package>
  :ensure t
  :defer t)  ; ✅ CORRECT for 95% of packages
```

**Rule B: Critical packages requiring immediate load**
```elisp
(use-package <critical-package>
  :ensure t
  :demand t  ; ✅ Use for: theme system, completion framework core
  :config
  ...)
```

**When to use :demand t:**
- Theme packages (doom-themes)
- Completion framework core (vertico, orderless)
- Packages that must be available at startup
- Environment tracking packages (pyvenv, poetry) - need to track state immediately

**Rule C: Environment tracking packages**
```elisp
;; ✅ CORRECT: Tracking packages need :demand t to activate immediately
(use-package pyvenv
  :ensure t
  :demand t
  :config
  (pyvenv-tracking-mode 1))

;; ❌ AVOID: :defer t prevents tracking from working correctly
(use-package pyvenv
  :ensure t
  :defer t
  :hook (python-mode . pyvenv-tracking-mode))  ; May miss environment changes
```

**When to use :defer t:**
- All language modes
- All tool integrations
- All enhancement packages

### :hook Rules

**Rule A: Single hook - Simple form**
```elisp
:hook (prog-mode . smartparens-mode)  ; ✅ CORRECT
```

**Rule B: Multiple hooks - List form**
```elisp
:hook ((c-mode . eglot-ensure)       ; ✅ CORRECT
       (c++-mode . eglot-ensure)
       (python-mode . eglot-ensure))
```

**Rule C: Setup function for complex configurations**
```elisp
;; Define setup function when multiple buffer-local settings needed
(defun omw/prog-mode-setup ()
  "Apply custom settings for programming modes."
  (setq-local tab-width 4)
  (display-line-numbers-mode 1)
  (hs-minor-mode 1))

(use-package prog-mode
  :hook (prog-mode . omw/prog-mode-setup))  ; ✅ CORRECT
```

**Setup function naming:** `omw/<mode>-setup`
- Examples: `omw/prog-mode-setup`, `omw/markdown-mode-setup`, `omw/vterm-mode-setup`

**When to use setup functions:**
- 3+ buffer-local settings for the same mode
- Settings that should always be applied together
- Improves maintainability

**Setup function rules:**
```elisp
;; ✅ CORRECT: Use setq-local for variables
(defun omw/prog-mode-setup ()
  (setq-local tab-width 4
              indent-tabs-mode nil)  ; Variable, not a mode
  (display-line-numbers-mode 1))    ; Mode function

;; ❌ AVOID: Treating variable as mode function
(defun omw/prog-mode-setup ()
  (setq-local tab-width 4)
  (indent-tabs-mode -1))  ; WRONG: indent-tabs-mode is a variable!
```

**Prefer built-in functions over custom wrappers:**
```elisp
;; ✅ CORRECT: Use built-in function directly
:hook (pdf-view-mode . pdf-view-fit-height-to-window)

;; ❌ AVOID: Unnecessary wrapper function
(defun omw/pdf-view-mode-setup ()
  (setq-local pdf-view-display-size 'fit-height))
:hook (pdf-view-mode . omw/pdf-view-mode-setup)
```

### :bind Rules

**Rule A: Global keybindings only**
```elisp
:bind (("C-x C-t" . multi-vterm)    ; ✅ CORRECT
       ("C-c g s" . magit-status))
```

**Rule B: Mode-local keybindings only**
```elisp
:bind (:map vterm-mode-map          ; ✅ CORRECT
            ("M-1" . nil)
            ("C-g" . vterm--self-insert))
```

**Rule C: Both global and mode-local**
```elisp
:bind (("C-x j" . dired-jump)       ; ✅ CORRECT
       (:map dired-mode-map
             ("C-c C-c" . dired-do-copy)
             ("C-c C-r" . dired-do-rename)))
```

**Keybinding prefix guidelines:**
- `C-c <letter>` - For mode-specific commands
- `C-c <letter> <letter>` - For sub-modes (e.g., `C-c g s` for git status)
- Avoid overriding Emacs defaults unless documented

### :custom-face Rules

**Rule A: Use `:custom-face` instead of `custom-set-faces`**
```elisp
:custom-face
(centaur-tabs-selected ((t (:inherit fixed-pitch :bold t :foreground "#28cd41"))))  ; ✅ CORRECT
```

**Rule B: Use explicit face attributes to prevent buffer-local remapping issues**
```elisp
;; ✅ CORRECT: Explicit attributes prevent inheritance from buffer-local remapping
:custom-face
(centaur-tabs-display-line ((t (:inherit fixed-pitch :box nil :underline nil))))

;; ❌ AVOID: Inheriting from 'default can pick up buffer-local face remapping
:custom-face
(some-face ((t (:inherit default))))  ; May be affected by markdown-mode scaling
```

**Why use `:inherit fixed-pitch` instead of `:inherit default`:**
- Buffer-local face remapping (e.g., `face-remap-add-relative` in markdown-mode) affects faces that inherit from `default`
- Using `fixed-pitch` or explicit attributes prevents unintended font scaling in UI elements like tab bars

## Best Practices - use-package (Lines 838-848)

### 4. use-package Keyword Order

**Always follow the exact order** to avoid confusion:

```
:ensure → :when → :defer → :after → :hook → :bind → :custom-face → :config
```

**Special cases:**
- `:mode` can come with `:hook` or before it (for mode associations)
- `:demand t` instead of `:defer t` for packages that must activate immediately

## Quick Reference - use-package Order (Line 901)

2. **use-package Order:** `:ensure → :when → :defer → :after → :hook → :bind → :custom-face → :config`
