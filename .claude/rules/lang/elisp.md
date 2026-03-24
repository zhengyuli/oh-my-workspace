# Emacs Lisp Conventions

Coding standards for Emacs Lisp in oh-my-workspace.

## References

- [Emacs Lisp Style Guide](https://github.com/bbatsov/emacs-lisp-style-guide) - Community standard
- [Emacs Lisp Manual](https://www.gnu.org/software/emacs/manual/elisp.html) - Official documentation

## Naming Conventions

### Use kebab-case

```elisp
;; Good
(defun omw-buffer-empty-p ()
  ...)

;; Bad
(defun omwBufferEmptyP ()
  ...)
```

### Package Prefix

Prefix all names with package namespace:

```elisp
;; For oh-my-workspace, use omw- prefix
(defvar omw-config-directory nil)
(defun omw-setup-environment () ...)
(defmacro omw-with-temp-buffer (&rest body) ...)
```

### Predicates

End predicate functions with `-p`:

```elisp
(defun omw-buffer-empty-p ()
  "Return t if buffer is empty."
  (zerop (buffer-size)))

(defun omw-file-exists-p (filepath)
  "Return t if FILEPATH exists."
  (file-exists-p filepath))
```

## File Structure

### Commentary Section

Start with commentary:

```elisp
;;; omw-shell.el --- Shell configuration for oh-my-workspace -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zhengyu.li

;; Author: zhengyu.li
;; URL: https://github.com/zhengyu-li/oh-my-workspace

;;; Commentary:

;; This module provides shell integration for oh-my-workspace.
;; It configures shell-mode, term-mode, and related utilities.

;;; Code:
```

### Autoloads

Use `;;;###autoload` for public functions:

```elisp
;;;###autoload
(defun omw-open-terminal ()
  "Open terminal in current directory."
  (interactive)
  ...)
```

### Provide at End

```elisp
(provide 'omw-shell)
;;; omw-shell.el ends here
```

## Documentation

### Docstrings

Every public function needs a docstring:

```elisp
(defun omw-setup-aliases (aliases)
  "Set up shell ALIASES in the current buffer.
ALIASES should be an alist of (NAME . COMMAND) pairs.

Example:
  (omw-setup-aliases '((\"ll\" . \"ls -la\")
                       (\"gs\" . \"git status\")))"
  ...)
```

### First Line

First line should be a complete sentence:

```elisp
;; Good
"Return the absolute path to the workspace directory."

;; Bad
"Returns absolute path"  ;; Not a sentence
"Get workspace dir"       ;; Too terse
```

### Parameters

Document parameters in docstring:

```elisp
(defun omw-find-config (name &optional directory)
  "Find config file with NAME in DIRECTORY or default location.
NAME should be a string without extension.
DIRECTORY defaults to `omw-config-directory'.

Returns the full path to the config file, or nil if not found."
  ...)
```

## Key Conventions

### Use kbd Macro

```elisp
;; Good - readable
(define-key shell-mode-map (kbd "C-c C-s") 'omw-shell-sync)

;; Bad - hard to read
(define-key shell-mode-map "\C-c\C-s" 'omw-shell-sync)
```

### Follow Emacs Conventions

Common prefixes:
- `C-c` - User bindings (C-c C-<letter> for major modes)
- `C-x` - Global bindings
- `C-h` - Help

## Package Management

### use-package

Use `use-package` for dependencies:

```elisp
(use-package shell
  :ensure nil  ; Built-in package
  :config
  (setq explicit-shell-file-name "/usr/bin/env zsh")
  :bind
  (:map shell-mode-map
        ("C-c C-s" . omw-shell-sync)))
```

### Declare Dependencies

```elisp
;; Package-Requires header
;; Package-Requires: ((emacs "27.1") (dash "2.19") (s "1.12"))
```

## Best Practices

### Lexical Binding

Always use lexical binding:

```elisp
;;; filename.el -*- lexical-binding: t; -*-
```

### Avoid Global Variables

Prefer `defvar-local` for buffer-local:

```elisp
(defvar-local omw-shell-buffer-name nil
  "Name of the current shell buffer.")
```

### Use cl-lib

For modern Common Lisp features:

```elisp
(require 'cl-lib)

(cl-defun omw-process-output (&key input timeout)
  "Process INPUT with TIMEOUT."
  ...)
```

### Error Handling

```elisp
(defun omw-safe-load (file)
  "Safely load FILE, returning t on success."
  (condition-case err
      (progn
        (load file)
        t)
    (error
     (message "Failed to load %s: %s" file err)
     nil)))
```