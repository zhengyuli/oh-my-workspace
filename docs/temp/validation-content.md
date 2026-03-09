# Validation Content (Extracted from CLAUDE.md)

## Code Quality and Compliance Validation (Lines 698-760)

### Automated Checks

**Test configuration loads without errors:**
```bash
# Full initialization test
emacs --debug-init

# Batch mode test (faster for CI/CD)
emacs --batch --eval '(progn (load-file "init.el") (message "✅ Configuration loaded successfully"))'
```

**Check naming conventions:**
```bash
# Find functions without omw/ prefix
grep -rn "defun (?!omw/)" lisp --include="*.el" | grep -v "^;;"

# Find defcustom without :group 'omw-emacs
grep -rn "defcustom.*:group" lisp --include="*.el" | grep -v "omw-emacs" | grep -v "^;;"
```

**Check file header completeness:**
```bash
# Verify Time-stamp presence
grep -rn "Time-stamp:" lisp --include="*.el" | wc -l

# Verify current copyright year (2026)
grep -rn "Copyright (C) 2026" lisp --include="*.el" | wc -l

# Verify GPL license text
for file in lisp/**/*.el lisp/*.el; do
  grep -q "This program is free software" "$file" || echo "Missing GPL: $file"
done
```

**Check use-package keyword order:**
```bash
# Find potential violations (visual inspection needed)
grep -A10 "use-package" lisp --include="*.el" | grep -E ":hook|:bind" | head -20
```

### Manual Review Checklist

- [ ] All custom functions use `omw/` prefix
- [ ] All defcustom use `:group 'omw-emacs`
- [ ] All files have Time-stamp, Copyright 2026, Dependencies, Commentary
- [ ] All files have complete GPL license text
- [ ] All setup functions have docstrings
- [ ] use-package keywords follow correct order
- [ ] Section separators (`;; ==================================================================================` ) used correctly
- [ ] Files end with `(provide 'init-xxx)` and `;;; init-xxx.el ends here`

### Compliance Scoring

Based on comprehensive audits (2026-03-09), the configuration achieves **98%+ compliance** with these standards.

**Key metrics:**
- Naming convention violations: 0
- File header violations: 0
- Documentation gaps: 0
- use-package order violations: 0
- Configuration load errors: 0

## Validation and Troubleshooting (Lines 762-779)

**Test Emacs loads without errors:**
```bash
emacs --debug-init
```

**Check for LSP issues:**
1. Verify server installed: `which pylsp gopls clangd`
2. Start LSP manually: `M-x eglot`
3. Check events log: Switch to `*eglot-events*` buffer

**Package installation issues:**
```elisp
M-x package-refresh-contents  ; Refresh package list
```

**Slow startup**: Check `*Messages*` buffer with `C-h e`

## Best Practices - Testing (Lines 863-879)

### 6. Testing Before Committing

**Always run these commands before committing changes:**

```bash
# 1. Syntax check
emacs --batch --eval '(progn (load-file "emacs/init.el") (message "✅ OK"))'

# 2. Debug check
emacs --debug-init

# 3. Quick audit
grep -rn "defun (?!omw/)" emacs/lisp --include="*.el"
grep -rn "defcustom.*:group" emacs/lisp --include="*.el" | grep -v "omw-emacs"
```

**Why**: Catches common issues before they reach the repository.

## Pre-Commit Checklist (Lines 911-918)

### Pre-Commit Checklist

- [ ] Configuration loads: `emacs --batch --eval '(progn (load-file "emacs/init.el") ...)'`
- [ ] No naming violations: All functions use `omw/` prefix
- [ ] No variable violations: All defcustom use `:group 'omw-emacs`
- [ ] File headers complete: Time-stamp, Copyright 2026, Dependencies, Commentary, GPL
- [ ] Docstrings present: All setup functions have documentation
- [ ] use-package order correct: Keywords in standard order
