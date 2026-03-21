---
name: validate-config
description: Validate configuration files for compliance with dotfiles standards
invocable: true
---

# Configuration Validation Workflow

Usage: `/validate-config [--tool=<zsh|emacs|vim|all>]`

## Steps

1. **Check file headers**
   - Time-stamp present and formatted correctly
   - Section separators use correct format (79 chars)
   - Header structure follows standards

2. **Check inline comments**
   - No comments on same line as code
   - Comments on line above code they describe

3. **Check alignment spaces**
   - Single space only between elements
   - No padding spaces for visual alignment

4. **Check comment language**
   - All comments in English

5. **Run syntax checks per tool**
   - Zsh: `zsh -n .config/zsh/.zshrc`
   - Emacs: `emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") (message "OK"))'`
   - Vim: `vim -c 'q'`

6. **Report compliance status**
   - Summary of checks passed/failed
   - List of any violations found

## Tool-Specific Checks

### Zsh
```bash
# Syntax check
zsh -n .config/zsh/.zshrc

# Find unquoted variable expansions
grep -rn '\$[A-Za-z_][A-Za-z0-9_]*[^}]' .config/zsh/conf.d --include="*.zsh" | grep -v '"\${'

# Find [[ ]] && patterns
grep -rn '\[\[.*\]\] &&' .config/zsh/conf.d --include="*.zsh"

# Check for echo usage
grep -rn '\becho\b' .config/zsh/conf.d --include="*.zsh"
```

### Emacs
```bash
# Configuration load check
emacs --batch --eval '(progn (load-file "emacs/.config/emacs/init.el") (message "OK"))'

# Find functions without omw/ prefix
grep -rn "defun (?!omw/)" lisp --include="*.el" | grep -v "^;;"

# Find defcustom without :group 'omw-emacs
grep -rn "defcustom.*:group" lisp --include="*.el" | grep -v "omw-emacs" | grep -v "^;;"
```

### Vim
```bash
# Configuration load check
vim -c 'q'

# Verify XDG paths are used
vim -c 'echo &undodir'
```

## Exit Codes

- 0: All checks passed
- 1: One or more checks failed
