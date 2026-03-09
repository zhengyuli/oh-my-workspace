# Quick Reference Card Content (Extracted from CLAUDE.md Lines 896-967)

## Quick Reference Card

### Essential Rules

1. **File Header:** Must include Time-stamp, current copyright year (2026), Dependencies
2. **use-package Order:** `:ensure → :when → :defer → :after → :hook → :bind → :custom-face → :config`
3. **Naming Prefix:** Always `omw/` for custom functions/variables
4. **Setup Functions:** Use `setq-local` for variables, mode functions for toggles; prefer built-in functions
5. **Comments:** Minimal, only for non-obvious code
6. **Separators:** `;; ==================================================================================`
7. **Language Modules:** Keep language-related code together, don't artificially limit lines
8. **LSP Configuration:** Centralize all LSP in init-prog.el, never in language modules
9. **Face Customization:** Use `:custom-face` with `fixed-pitch` inheritance to avoid buffer-local remapping
10. **Tracking Packages:** Use `:demand t` for environment tracking (pyvenv, poetry)

### Pre-Commit Checklist

- [ ] Configuration loads: `emacs --batch --eval '(progn (load-file "emacs/init.el") ...)'`
- [ ] No naming violations: All functions use `omw/` prefix
- [ ] No variable violations: All defcustom use `:group 'omw-emacs`
- [ ] File headers complete: Time-stamp, Copyright 2026, Dependencies, Commentary, GPL
- [ ] Docstrings present: All setup functions have documentation
- [ ] use-package order correct: Keywords in standard order

### Common Patterns

```elisp
;; Minimal language module
(use-package <lang>-mode
  :ensure t
  :defer t)

;; Setup function pattern
(defun omw/<mode>-setup ()
  "Apply custom settings for <mode>."
  (setq-local var1 value1)
  (mode-enable))

(use-package <mode>
  :hook (<mode> . omw/<mode>-setup))

;; Custom variable
(defcustom omw/<var> <default>
  "Description."
  :type '<type>
  :group 'omw-emacs)

;; Minor mode
(define-minor-mode omw/<mode>-mode
  "Description."
  :lighter " Indicator"
  :global nil
  (if omw/<mode>-mode
      (add-hook 'hook-name #'omw/function nil t)
    (remove-hook 'hook-name #'omw/function t)))

;; Package with custom faces (use fixed-pitch to avoid buffer-local remapping)
(use-package centaur-tabs
  :ensure t
  :custom-face
  (centaur-tabs-selected ((t (:inherit fixed-pitch :bold t :foreground "#28cd41")))))

;; Environment tracking package (use :demand t for immediate activation)
(use-package pyvenv
  :ensure t
  :demand t
  :config
  (pyvenv-tracking-mode 1))
```

---
