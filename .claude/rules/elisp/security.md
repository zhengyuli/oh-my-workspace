---
paths:
  - "**/*.el"
  - "**/emacs/**"
---

# Emacs Lisp Security

> This file extends [../common/security.md](../common/security.md) with Emacs Lisp specifics.

## Eval Safety

### Avoid Eval When Possible

```elisp
;; CORRECT - use direct function calls
(funcall handler)

;; CORRECT - use symbols and funcall
(let ((handler 'my-handler))
  (funcall handler))

;; DANGEROUS - eval with untrusted input
(eval (read user-input))  ; NEVER do this
```

### Safe Eval Patterns

When eval is absolutely necessary:

```elisp
;; CORRECT - validate before eval
(defun omw/safe-eval (expr)
  "Safely evaluate EXPR if it passes validation."
  (when (omw/valid-expr-p expr)
    (eval expr)))

;; CORRECT - use allowlist for functions
(defun omw/safe-funcall (fn &rest args)
  "Call FN with ARGS only if FN is in allowed list."
  (let ((allowed '(safe-fn1 safe-fn2 safe-fn3)))
    (when (memq fn allowed)
      (apply fn args))))
```

### Read Safety

```elisp
;; DANGEROUS - read can execute code
(read user-input)  ; Malicious input: "#.(shell-command \"rm -rf ~\")"

;; CORRECT - use read-from-string with caution
(let ((result (read-from-string user-input)))
  (when (omw/valid-data-p (car result))
    (car result)))
```

## File Permissions

### Check Before Operations

```elisp
;; Check file exists before reading
(defun omw/safe-read-file (path)
  "Safely read file at PATH."
  (when (and (file-exists-p path)
             (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

;; Check directory is writable
(defun omw/safe-write-file (path content)
  "Safely write CONTENT to PATH."
  (let ((dir (file-name-directory path)))
    (when (and (file-directory-p dir)
               (file-writable-p dir))
      (with-temp-file path
        (insert content)))))
```

### Sensitive File Handling

```elisp
;; Set restrictive permissions on sensitive files
(defun omw/set-restrictive-permissions (file)
  "Set restrictive permissions on FILE (0600)."
  (when (file-exists-p file)
    (set-file-modes file #o600)))

;; Store secrets in secure location
(defcustom omw-secrets-directory
  (expand-file-name "secrets" user-emacs-directory)
  "Directory for sensitive files."
  :type 'directory
  :group 'omw-emacs)
```

## Secrets Management

### Never Hardcode Secrets

```elisp
;; WRONG - hardcoded secrets
(setq api-key "sk-1234567890abcdef")

;; CORRECT - use environment variables
(setq api-key (getenv "MY_API_KEY"))

;; CORRECT - use auth-source
(setq api-key (auth-source-pick-first-password :host "api.example.com"))
```

### Auth-Source Integration

```elisp
;; Use auth-source for credentials
(defun omw/get-credential (host &optional user)
  "Get credential for HOST and optional USER from auth-source."
  (let ((entry (auth-source-search :host host :user user :max 1)))
    (when entry
      (let ((secret (plist-get (car entry) :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))
```

### GPG Integration

```elisp
;; Use .gpg files for sensitive configuration
;; Emacs will prompt for passphrase automatically
(setq omw/secrets-file (expand-file-name "secrets.el.gpg" user-emacs-directory))

;; Load secrets if available
(when (file-exists-p omw/secrets-file)
  (load omw/secrets-file))
```

## Network Security

### URL Validation

```elisp
;; Validate URLs before fetching
(defun omw/safe-url-fetch (url)
  "Fetch URL after validation."
  (when (omw/valid-url-p url)
    (url-retrieve url #'omw/handle-response)))

(defun omw/valid-url-p (url)
  "Check if URL is valid and uses safe protocol."
  (and (stringp url)
       (string-match-p "\\`https?://" url)
       (not (string-match-p "[\n\r]" url))))
```

### TLS Verification

```elisp
;; Ensure TLS verification is enabled
(setq gnutls-verify-error t
      tls-checktrust t
      tls-program '("gnutls-cli --x509cafile %t -p %p %h"))
```

## Process Security

### Shell Command Safety

```elisp
;; CORRECT - use call-process with explicit args
(call-process "git" nil nil nil "status" "--short")

;; CORRECT - use process-lines for output
(process-lines "git" "status" "--short")

;; DANGEROUS - shell command with untrusted input
(shell-command (format "echo %s" user-input))  ; NEVER do this

;; CORRECT - use format only with trusted/validated input
(defun omw/safe-git-status (dir)
  "Run git status in DIR after validation."
  (when (and (file-directory-p dir)
             (file-in-directory-p dir (expand-file-name "~/projects")))
    (let ((default-directory dir))
      (process-lines "git" "status" "--short"))))
```

### Environment Isolation

```elisp
;; Run with clean environment
(defun omw/isolated-process (program &rest args)
  "Run PROGRAM with ARGS in isolated environment."
  (let ((process-environment '("PATH=/usr/bin:/bin"))
        (exec-path '("/usr/bin" "/bin")))
    (apply #'call-process program nil nil nil args)))
```

## Security Checklist

Before committing Emacs Lisp code:

- [ ] No hardcoded secrets or API keys
- [ ] No `eval` with untrusted input
- [ ] No `read` on untrusted data without validation
- [ ] File operations check permissions first
- [ ] Network requests validate URLs
- [ ] Shell commands use explicit args, not shell strings
- [ ] Sensitive files use GPG encryption or auth-source
