---
paths:
  - "**/*.el"
  - "**/emacs/**"
---

# Emacs Lisp Testing

> This file extends [../common/testing.md](../common/testing.md) with Emacs Lisp specifics.

## ERT (Emacs Lisp Regression Testing)

### Basic Test Definition

```elisp
(require 'ert)

;; Simple assertion test
(ert-deftest omw-test-addition ()
  "Test basic addition."
  (should (= (+ 1 2) 3)))

;; Test with multiple assertions
(ert-deftest omw-test-string-operations ()
  "Test string operations."
  (should (string= "hello" "hello"))
  (should-not (string= "hello" "world"))
  (should-error (substring "hi" 0 10)))
```

### Assertion Types

| Function | Purpose |
|----------|---------|
| `(should FORM)` | Assert FORM is non-nil |
| `(should-not FORM)` | Assert FORM is nil |
| `(should-error FORM)` | Assert FORM signals error |
| `(should-error FORM :type 'error-type)` | Assert specific error type |

### Test Organization

```elisp
;;; omw-module-tests.el --- Tests for omw-module -*- lexical-binding: t; -*-

(require 'ert)
(require 'omw-module)

;; ============================================================================
;; Helper Functions
(defun omw-test-setup ()
  "Set up test environment."
  ...)

(defun omw-test-teardown ()
  "Clean up after tests."
  ...)

;; ============================================================================
;; Unit Tests
(ert-deftest omw-test-function-a ()
  "Test function-a behavior."
  (omw-test-setup)
  (unwind-protect
      (should (omw/function-a "input")))
    (omw-test-teardown)))

;; ============================================================================
;; Integration Tests
(ert-deftest omw-test-integration ()
  "Test module integration."
  ...)

(provide 'omw-module-tests)
```

### Running Tests

```bash
# Run all tests in file
emacs --batch -l ert -l omw-module.el -l omw-module-tests.el -f ert-run-tests-batch-and-exit

# Run specific test
emacs --batch -l ert -l omw-module.el -l omw-module-tests.el \
  --eval '(ert-run-tests-batch-and-exit "omw-test-function-a")'

# Run tests interactively
M-x ert-run-tests-interactively RET "omw-test-" RET
```

## Buttercup (BDD Testing)

### Basic Buttercup Test

```elisp
(require 'buttercup)

(describe "omw/function-a"
  :var ((result))

  (before-each
    (setq result nil))

  (it "returns expected value for valid input"
    (expect (omw/function-a "valid")
            :to-equal 'expected-value))

  (it "handles invalid input gracefully"
    (expect (omw/function-a nil)
            :to-throw)))

(describe "omw-module"
  (describe "when feature is enabled"
    (before-each
      (setq omw-feature-enabled t))

    (it "behaves correctly"
      (expect (omw/do-something) :to-be-truthy)))

  (describe "when feature is disabled"
    (before-each
      (setq omw-feature-enabled nil))

    (it "returns nil"
      (expect (omw/do-something) :to-be-nil))))
```

### Buttercup Expectations

| Matcher | Purpose |
|---------|---------|
| `:to-equal VALUE` | Deep equality |
| `:to-be TRUTHY` | Truthy check |
| `:to-be-truthy` | Non-nil check |
| `:to-be-nil` | Nil check |
| `:to-throw` | Error check |
| `:to-match REGEX` | String match |
| `:to-have-same-items-as LIST` | List equality |

### Running Buttercup Tests

```bash
# Run all buttercup tests
emacs --batch -l buttercup.el -l omw-module.el -l omw-module-tests.el -f buttercup-run

# Run with specific pattern
emacs --batch -l buttercup.el -l omw-module.el -l omw-module-tests.el \
  --eval '(buttercup-run "omw/function")'
```

## Configuration Load Testing

### Batch Load Test

```bash
# Test init.el loads without errors
emacs --batch --eval '(progn
  (load-file "emacs/.config/emacs/init.el")
  (message "OK"))'

# Test specific module
emacs --batch --eval '(progn
  (load-file "emacs/.config/emacs/init.el")
  (require (quote omw-module))
  (message "Module loaded OK"))'
```

### Byte Compilation Test

```bash
# Check for byte-compile errors (without writing .elc files)
emacs --batch --eval '(progn
  (setq byte-compile-error-on-warn t)
  (byte-compile-file "lisp/omw-module.el"))'

# Batch compile all files
emacs --batch --eval '(byte-recompile-directory "lisp" 0 t)'
```

## Test Patterns

### Testing Commands

```elisp
(ert-deftest omw-test-user-command ()
  "Test user command."
  (let ((buffer (generate-new-buffer "*test*")))
    (unwind-protect
        (with-current-buffer buffer
          (omw-user-command)
          (should (string-match-p "expected" (buffer-string))))
      (kill-buffer buffer))))
```

### Testing Hooks

```elisp
(ert-deftest omw-test-hook-registration ()
  "Test hook is registered correctly."
  (should (memq 'omw/prog-mode-setup prog-mode-hook)))

(ert-deftest omw-test-hook-behavior ()
  "Test hook behavior."
  (with-temp-buffer
    (python-mode)
    (run-hooks 'python-mode-hook)
    (should (eq python-indent-offset 4))))
```

### Testing Custom Variables

```elisp
(ert-deftest omw-test-custom-variable ()
  "Test custom variable behavior."
  (let ((omw-feature-enabled t))
    (should (omw/is-feature-enabled)))
  (let ((omw-feature-enabled nil))
    (should-not (omw/is-feature-enabled))))
```

### Testing File Operations

```elisp
(ert-deftest omw-test-file-operations ()
  "Test file operations."
  (let ((temp-file (make-temp-file "omw-test")))
    (unwind-protect
        (progn
          (omw/write-to-file temp-file "content")
          (should (file-exists-p temp-file))
          (should (string= (omw/read-from-file temp-file) "content")))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

## Test Checklist

### Before Committing Emacs Lisp

- [ ] ERT tests exist for core functions
- [ ] Tests pass: `emacs --batch -l ert -l *.el -f ert-run-tests-batch-and-exit`
- [ ] Configuration loads: `emacs --batch --eval '(load-file "init.el")'`
- [ ] Byte compilation succeeds without warnings
- [ ] package-lint passes (if available)

### Test Coverage Goals

| Component | Minimum Coverage |
|-----------|------------------|
| Utility functions | 80% |
| Core features | 70% |
| Configuration | Load test only |
