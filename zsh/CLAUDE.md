# ZSH Configuration Coding Standard

> **Scope**: `.zshenv`, `.zprofile`, `.zshrc`, and all module files under `conf.d/`
> **Sources**: Google Shell Style Guide · Oh My Zsh Code Style Guide · ZSH Opinionated
> Best Practices (ChristopherA) · zsh-users/zsh completion-style-guide · Arch Linux Zsh
> Wiki · ZSH User's Guide (zsh.sourceforge.io)
>
> **Key constraint**: These are *shell configuration files*, not standalone scripts.
> They are sourced into a live interactive session. This changes the rules:
> no shebang, no `exit`, no unconditional slow commands at the top level,
> and every function shares the user's global namespace for the lifetime of the session.

---

## Table of Contents

1. [Directory Layout](#1-directory-layout)
2. [Startup File Responsibilities](#2-startup-file-responsibilities)
3. [Loading Order & Execution Context](#3-loading-order--execution-context)
4. [Naming Conventions](#4-naming-conventions)
5. [Variable Rules](#5-variable-rules)
6. [Function Rules](#6-function-rules)
7. [Formatting](#7-formatting)
8. [ZSH-Specific Syntax](#8-zsh-specific-syntax)
9. [Comments & Documentation](#9-comments--documentation)
10. [Security](#10-security)
11. [Performance](#11-performance)
12. [Error Handling](#12-error-handling)
13. [Logging](#13-logging)
14. [Prohibited Patterns](#14-prohibited-patterns)

---

## 1. Directory Layout

```
~/.config/zsh/                    ← $ZDOTDIR
├── .zshenv                       ← Universal env vars (all shell types)
├── .zprofile                     ← Login-shell bootstrap (PATH, brew, etc.)
├── .zshrc                        ← Interactive-shell entry point
└── conf.d/                       ← Modules, sourced explicitly by .zshrc
    ├── options.zsh               ← setopt declarations
    ├── completion.zsh            ← compinit and completion styles
    ├── prompt.zsh                ← Prompt configuration
    ├── functions.zsh             ← User utility functions and logging helpers
    ├── aliases.zsh               ← Aliases
    ├── keybindings.zsh           ← bindkey declarations
    ├── tools.zsh                 ← External tool init (nvm, rbenv, etc.)
    └── local.zsh                 ← Machine-local overrides (gitignored)
```

**Rules:**

- `ZDOTDIR` must be set to `~/.config/zsh` in the system-level `/etc/zshenv`
  or at the very top of `~/.zshenv` before any other file is read.
- The three root files (`.zshenv`, `.zprofile`, `.zshrc`) live directly under
  `$ZDOTDIR`. Every other file lives under `conf.d/`.
- Module load order is controlled by **explicit `source` statements** in
  `.zshrc`, not by filename prefixes or glob expansion order. This makes
  dependencies visible in code rather than implicit in filenames.
- `local.zsh` must be listed in `.gitignore`. It holds machine-specific paths,
  credentials, or overrides that must never be committed.

---

## 2. Startup File Responsibilities

Each file has a **single, strictly scoped responsibility**. Putting the wrong
content in the wrong file causes subtle bugs: variables missing in scripts,
duplicate `PATH` entries, slow SSH connections.

### `.zshenv` — Universal environment

Sourced for **every** zsh invocation: interactive, non-interactive, login,
non-login, scripts, cron jobs, `ssh host cmd`, and editor sub-shells.

| Put here | Never put here |
|----------|----------------|
| `ZDOTDIR` | `PATH` manipulation |
| `XDG_*` base directories | Any command that produces output |
| `EDITOR`, `VISUAL`, `PAGER`, `LANG`, `LC_ALL` | Any command that assumes a TTY |
| Variables needed by scripts and cron jobs | `setopt` that affects script behaviour |
| Tool home dirs (`GOPATH`, `CARGO_HOME`, …) | Plugin loading, aliases, prompt |

### `.zprofile` — Login-shell bootstrap

Sourced **once per login session** (new terminal, SSH login), before `.zshrc`.
Not sourced in subshells.

| Put here | Never put here |
|----------|----------------|
| `PATH` / `path` array construction | Aliases or functions |
| `MANPATH`, `INFOPATH`, `PKG_CONFIG_PATH` | `setopt`, prompts, completions |
| One-time tool bootstrap: `eval "$(brew shellenv)"` | Anything that must run in every subshell |
| Session-wide `export` statements | Interactive UI (output, colour, prompts) |

### `.zshrc` — Interactive-shell entry point

Sourced for every **interactive** shell: new terminal tabs, `zsh` subshells,
`exec zsh`. This file's only job is to source `conf.d/` modules in the correct
order.

| Put here | Never put here |
|----------|----------------|
| Non-interactive guard (`[[ $- == *i* ]] \|\| return 0`) | Direct configuration of any kind |
| Bootstrap logging stubs | `exit` |
| Explicit `source` calls for each `conf.d/` module | Slow top-level commands |

### `conf.d/` modules — Scoped configuration units

Each module owns **exactly one concern**. Modules must be safe to `source` in
isolation — no hidden dependencies on load order beyond what the explicit
source list in `.zshrc` documents.

---

## 3. Loading Order & Execution Context

```
Shell type              Files sourced (in order)
──────────────────────  ──────────────────────────────────────────────────────
Login + interactive     /etc/zshenv → .zshenv → /etc/zprofile → .zprofile
                        → /etc/zshrc → .zshrc (→ conf.d modules) → /etc/zlogin
Login non-interactive   /etc/zshenv → .zshenv → /etc/zprofile → .zprofile
                        → /etc/zlogin
Interactive non-login   /etc/zshenv → .zshenv → /etc/zshrc
(subshell)              → .zshrc (→ conf.d modules)
Script / cron           /etc/zshenv → .zshenv   ← ONLY these two
```

**Practical implications:**

- Code in `.zshenv` runs for cron jobs and editor shells — keep it minimal
  and side-effect free.
- `.zprofile` does **not** run in subshells; never assume its exports are
  present in sub-processes unless they are also in `.zshenv`.
- On **macOS**, Terminal.app opens every window as a login + interactive shell,
  so `.zprofile` runs more often than on Linux. Use `typeset -U path` to
  prevent duplicate `PATH` entries on re-source.
- `PATH` must be set in `.zprofile`, not `.zshenv`, because macOS
  `/etc/zprofile` runs `path_helper` which would overwrite a `PATH` set in
  `.zshenv`.

---

## 4. Naming Conventions

### 4.1 Variables

| Scope | Convention | Example |
|-------|------------|---------|
| Exported env vars (public API) | `UPPER_SNAKE_CASE` | `EDITOR`, `GOPATH` |
| Private zsh-config vars (internal) | `_lower_snake_case` | `_zsh_cache_dir` |
| Function-local vars | `lower_snake_case` with `local` | `local target_dir` |
| Integer locals | `typeset -i name` | `typeset -i retry_count=0` |
| Array locals | `typeset -a name` | `typeset -a plugin_list=()` |
| Associative array locals | `typeset -A name` | `typeset -A tool_dirs=()` |

The `_` prefix on internal config vars is a namespace guard that prevents
collisions with variables set by plugins and third-party tools.

```zsh
# ✅
_zsh_conf_dir="${ZDOTDIR}/conf.d"
export EDITOR="${EDITOR:-nvim}"

# ❌ Internal var uses public naming — risks collision with any tool that
#    exports a variable of the same name
ZSH_CONF_DIR="${ZDOTDIR}/conf.d"
```

### 4.2 Functions

| Role | Convention | Example |
|------|------------|---------|
| User-callable utility | `lower_snake_case` | `mkcd`, `path_contains` |
| Private / internal helper | `_lower_snake_case` | `_zsh_load_module` |
| ZSH hook (names fixed by ZSH) | As required | `precmd`, `chpwd` |
| ZLE widget | `_widget_name`, registered via `zle -N` | `_fzf_history` |
| Lazy-loader stub | Same name as the intercepted command | `nvm`, `node` |

### 4.3 Module files

```
<short-kebab-description>.zsh
```

No numeric prefix. Names describe the single concern the module owns.
Load order is determined solely by the explicit source list in `.zshrc`.

```zsh
# In .zshrc — order is explicit, dependencies are visible in code
_zsh_load "${_zsh_conf}/options.zsh"
_zsh_load "${_zsh_conf}/completion.zsh"   # depends on options being set first
_zsh_load "${_zsh_conf}/prompt.zsh"
_zsh_load "${_zsh_conf}/functions.zsh"
_zsh_load "${_zsh_conf}/aliases.zsh"
_zsh_load "${_zsh_conf}/keybindings.zsh"
_zsh_load "${_zsh_conf}/tools.zsh"
_zsh_load "${_zsh_conf}/local.zsh"        # always last
```

Adding a new module is one line. Moving a module earlier or later is one line.
No filenames need renaming.

### 4.4 Never shadow built-ins or common commands

Do not use these as variable or function names:
`path`, `pwd`, `dir`, `read`, `command`, `type`, `print`, `source`.

---

## 5. Variable Rules

### 5.1 Always use the `${var}` brace form

```zsh
# ✅ Brace form is unambiguous when concatenating
echo "${HOME}/.config/zsh"
source "${ZDOTDIR}/conf.d/${module}.zsh"

# ❌ Breaks silently when the variable name runs into adjacent letters
echo "$HOME_backup"   # ZSH reads this as ${HOME_backup}, not ${HOME}_backup
```

### 5.2 Always quote variable expansions

```zsh
# ✅ Double-quotes prevent word-splitting and glob expansion
[[ -f "${config_file}" ]] && source "${config_file}"

# ✅ Single-quotes when no expansion is needed
alias grep='grep --color=auto'

# ❌ Unquoted — breaks on paths with spaces or glob characters
source ${ZDOTDIR}/conf.d/aliases.zsh
```

### 5.3 Use parameter expansion for defaults

```zsh
# ✅ Concise and idiomatic
EDITOR="${EDITOR:-nvim}"
HISTSIZE="${HISTSIZE:-50000}"
_cache="${XDG_CACHE_HOME:-${HOME}/.cache}/zsh"

# ❌ Verbose equivalent — avoid
if [[ -z "${EDITOR}" ]]; then
  EDITOR="nvim"
fi
```

### 5.4 Manage PATH with the `path` array

ZSH keeps `$path` (array) and `$PATH` (colon string) automatically in sync.
Always manipulate the array; never concatenate `$PATH` with colons.

```zsh
# ✅ In .zprofile — deduplicated, readable, order-controlled
typeset -U path     # -U removes duplicate entries automatically on every write
path=(
  "${HOME}/.local/bin"
  "${GOPATH}/bin"
  "${CARGO_HOME:-${HOME}/.cargo}/bin"
  /usr/local/bin
  $path             # Retain existing system entries
)

# ❌ String concatenation — produces duplicates on re-source
export PATH="${HOME}/.local/bin:${PATH}"
```

### 5.5 Declare types explicitly

```zsh
typeset -i  HISTSIZE=50000           # Integer — prevents accidental string assignment
typeset -r  _ZSH_STD_VERSION="1.0"  # Readonly constant
typeset -gx EDITOR="nvim"           # Global + exported (use in .zshenv/.zprofile)
typeset -U  path                     # Unique-element array (auto-dedup on write)
```

---

## 6. Function Rules

### 6.1 Never use the `function` keyword

```zsh
# ✅ POSIX-compatible form used by all major ZSH style guides
mkcd() {
  mkdir -p -- "$1" && cd -- "$1"
}

# ❌ The `function` keyword is redundant and not POSIX
function mkcd() {
  mkdir -p -- "$1" && cd -- "$1"
}
```

### 6.2 Declare every function-local variable with `local` or `typeset`

Functions in `.zshrc` persist for the entire shell session. Any undeclared
variable leaks into the global namespace permanently, potentially clobbering
plugin or tool state.

```zsh
# ✅ Full local scoping
_zsh_load_module() {
  local module_path="$1"
  local -i start_ms end_ms

  [[ -f "${module_path}" ]] || return 1
  start_ms=${EPOCHREALTIME/./}
  source "${module_path}"
  end_ms=${EPOCHREALTIME/./}

  (( end_ms - start_ms > 100 )) \
    && _zsh_debug "slow module ($(( end_ms - start_ms ))ms): ${module_path:t}"
}

# ❌ `module_path` leaks into the global session after the function returns
_zsh_load_module() {
  module_path="$1"
  source "${module_path}"
}
```

### 6.3 Use `return`, never `exit`

`exit` in a sourced file terminates the **entire shell session** — it closes
the terminal window or kills the SSH connection.

```zsh
# ✅
_require_command() {
  command -v "$1" &>/dev/null && return 0
  _zsh_warn "required command not found: $1"
  return 1
}

# ❌ Closes the terminal
_require_command() {
  command -v "$1" &>/dev/null || exit 1
}
```

### 6.4 Validate arguments with guard clauses

Place all validation at the top of the function. This keeps the happy path
un-nested and makes failure paths explicit.

```zsh
# ✅ Guards at the top; happy path is flat
mkcd() {
  (( $# == 1 )) || { _zsh_error "mkcd: expected 1 argument, got $#"; return 1 }
  [[ -n "$1" ]]  || { _zsh_error "mkcd: argument must not be empty";  return 1 }
  mkdir -p -- "$1" && cd -- "$1"
}

# ❌ Nested conditionals obscure the happy path
mkcd() {
  if (( $# == 1 )); then
    if [[ -n "$1" ]]; then
      mkdir -p -- "$1" && cd -- "$1"
    fi
  fi
}
```

### 6.5 Size and complexity limits

- Function body: **40 lines** maximum. Extract helpers beyond that.
- Nesting depth: **3 levels** maximum. Deeper nesting signals a need to refactor.
- One function, one responsibility.

### 6.6 Return data via stdout, not global variables

```zsh
# ✅ Caller captures with command substitution — composable, no coupling
_git_branch() {
  local branch
  branch=$(git symbolic-ref --short HEAD 2>/dev/null) || return 1
  print -- "${branch}"
}

current=$(_git_branch) || return

# ❌ Writes to a global variable — hidden coupling, impossible to compose
_git_branch() {
  GIT_BRANCH=$(git symbolic-ref --short HEAD 2>/dev/null)
}
```

---

## 7. Formatting

### 7.1 Indentation

**2 spaces.** No tabs. No exceptions.

### 7.2 Line length

**80 characters** maximum. Break longer lines with trailing `\`.
For pipelines, start each continuation line with `|`.

```zsh
# Long option list — one flag per line
git log \
  --oneline \
  --graph \
  --decorate \
  --all

# Long pipeline — pipe character leads each continuation line
find "${XDG_DATA_HOME}/apps" -name '*.desktop' \
  | grep -v '^#' \
  | sort -u \
  | head -20
```

### 7.3 Control flow

`; then` and `; do` stay on the same line as `if` / `for` / `while`.
`else`, `elif`, `fi`, `done` are on their own lines, vertically aligned with
their opening keyword.

```zsh
for module in "${modules[@]}"; do
  if [[ -f "${module}" ]]; then
    source "${module}"
  elif [[ -d "${module}" ]]; then
    _zsh_warn "expected file, got directory: ${module}"
  else
    _zsh_error "module not found: ${module}"
  fi
done
```

### 7.4 `case` statements

Alternatives are indented 2 spaces. `;;` is on its own line.
One-liners may keep `;;` on the same line only when pattern, action, and `;;`
all fit within 80 characters.

```zsh
case "${OSTYPE}" in
  darwin*)
    alias ls='ls -G'
    ;;
  linux*)
    alias ls='ls --color=auto'
    ;;
  *)
    ;;   # No colour support — leave ls as-is
esac
```

### 7.5 Blank lines

- **1 blank line** between top-level declarations (functions, alias groups).
- **2 blank lines** between major sections within a file.
- No trailing whitespace on any line.

### 7.6 Section separator format

Every logical section in every file must use this exact separator style.
Total line length is 80 characters including the leading `# `.

```zsh
# ── Section Title ─────────────────────────────────────────────────────────────
```

Fill character: `─` (U+2500 BOX DRAWINGS LIGHT HORIZONTAL).
Apply the same style identically across every file.

---

## 8. ZSH-Specific Syntax

### 8.1 Conditional tests: always `[[ ]]`

`[[ ]]` prevents word-splitting, supports `=~` (regex) and `==` (glob), and
allows `&&` / `||` without spawning a subshell. Never use `[ ]`, `test`, or
`/usr/bin/[` in ZSH config files.

```zsh
# ✅
[[ -f "${file}" ]] && source "${file}"
[[ "${OSTYPE}" == darwin* ]]                   # Glob — RHS must NOT be quoted
[[ "${ver}" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]    # Regex

# ❌ POSIX test — no glob/regex, word-splitting risk on unquoted vars
[ -f "${file}" ] && source "${file}"
```

### 8.2 Arithmetic: always `(( ))` or `$(( ))`

```zsh
# ✅
(( retry_count++ ))
(( elapsed = end_time - start_time ))
local half=$(( total / 2 ))

# ❌ Obsolete and less readable
let retry_count+=1
elapsed=$(expr ${end_time} - ${start_time})
```

### 8.3 Command substitution: always `$( )`

```zsh
# ✅
local kernel="$(uname -s)"

# ❌ Backticks require escaping when nested
local kernel=`uname -s`
```

### 8.4 Arrays

> **NOTE**: ZSH arrays are **1-indexed**. `${arr[1]}` is the first element.
> This differs from bash (0-indexed). Every non-obvious index access must carry
> an annotation.

```zsh
typeset -a plugins=(git fzf zoxide delta)

echo "${plugins[1]}"      # "git"         — first element; index 1, not 0
echo "${plugins[-1]}"     # "delta"       — last element
echo "${plugins[2,3]}"    # "fzf zoxide"  — slice from index 2 to 3 inclusive
echo "${#plugins[@]}"     # 4             — element count
echo "${plugins[@]}"      # all elements, each separately quoted

for plugin in "${plugins[@]}"; do
  _load_plugin "${plugin}"
done
```

### 8.5 Associative arrays

```zsh
typeset -A tool_dirs=(
  [node]="${NVM_DIR}/versions/node/current/bin"
  [ruby]="${RBENV_ROOT}/shims"
)

# Key-existence check — use (( ${+array[key]} )) not [[ -n ... ]]
# NOTE: [[ -n "${tool_dirs[node]}" ]] is true even for keys explicitly set to ""
(( ${+tool_dirs[node]} )) && path=("${tool_dirs[node]}" $path)
```

### 8.6 ZSH path expansion modifiers

These are ZSH-specific and have no bash equivalent. Always annotate their use.

```zsh
local f="/home/user/docs/readme.md"

# NOTE: ZSH-only path modifiers — no bash equivalent
echo "${f:t}"    # tail (filename):   readme.md
echo "${f:r}"    # root (strip ext):  /home/user/docs/readme
echo "${f:e}"    # extension:         md
echo "${f:h}"    # head (parent dir): /home/user/docs
echo "${f:A}"    # absolute path (resolve symlinks)
```

### 8.7 Regex capture groups

> **NOTE**: ZSH stores capture groups in `$match[]`, **not** `$BASH_REMATCH[]`.
> Indices start at 1.

```zsh
local ver="v2.14.3"

if [[ "${ver}" =~ ^v([0-9]+)\.([0-9]+)\.([0-9]+)$ ]]; then
  # NOTE: ZSH capture groups → $match[1..n], not $BASH_REMATCH[1..n]
  local -i major=${match[1]} minor=${match[2]} patch=${match[3]}
fi
```

### 8.8 `setopt` rules

- All `setopt` / `unsetopt` calls belong exclusively in `conf.d/options.zsh`.
- Every option line **must** carry an inline comment explaining its effect.
- Never set `ERR_EXIT` globally — in a sourced file it closes the terminal on
  any non-zero return code.
- Use `setopt LOCAL_OPTIONS` inside functions to scope option changes locally.

```zsh
# ✅ In conf.d/options.zsh — documented and centralised
setopt HIST_IGNORE_SPACE   # Omit commands prefixed with a space from history
setopt SHARE_HISTORY       # Sync history across all active sessions in real time
setopt AUTO_CD             # Type a directory name alone to cd into it

# ✅ Option change scoped to a single function
_process_pipe() {
  setopt LOCAL_OPTIONS PIPE_FAIL   # Restored automatically on function return
  some_cmd | other_cmd || return 1
}

# ❌ Never in a sourced config file
setopt ERR_EXIT
```

---

## 9. Comments & Documentation

### 9.1 File header (required in every file)

```zsh
# ==============================================================================
# File: conf.d/functions.zsh
# Role: User utility functions and logging helpers for the interactive session
#
# Load context : Sourced by .zshrc after prompt.zsh
# Dependencies : None
# Side effects : Defines the following in the global namespace:
#                  _zsh_error  _zsh_warn  _zsh_info  _zsh_debug
#                  has  mkcd  path_contains
# ==============================================================================
```

### 9.2 Section separator (required between every logical section)

```zsh
# ── Logging helpers ───────────────────────────────────────────────────────────
```

### 9.3 Function documentation (required for non-trivial functions)

```zsh
# Create a directory (and all parents) then immediately cd into it.
#
# Arguments:
#   $1  target  path of the directory to create; must not be empty
#
# Returns:
#   0    success
#   1    missing or empty argument
#   >0   propagates mkdir or cd exit code
#
# Side effects:
#   Changes $PWD for the current shell session.
mkcd() {
  (( $# == 1 )) || { _zsh_error "mkcd: expected 1 argument, got $#"; return 1 }
  [[ -n "$1" ]]  || { _zsh_error "mkcd: argument must not be empty";  return 1 }
  mkdir -p -- "$1" && cd -- "$1"
}
```

One-liners get a single inline comment on the same line:

```zsh
has() { command -v "$1" &>/dev/null }   # True if $1 exists anywhere in $PATH
```

### 9.4 Inline comments: explain *why*, not *what*

```zsh
# ✅ Adds context the code alone cannot convey
setopt HIST_IGNORE_SPACE   # Prefix a command with a space to keep it out of
                           # history — useful when typing credentials inline

# ❌ Restates the obvious — adds zero value
setopt HIST_IGNORE_SPACE   # Set HIST_IGNORE_SPACE
```

### 9.5 ZSH-vs-bash trap annotations (required)

Every place where ZSH behaviour silently differs from bash **must** carry a
`# NOTE:` comment to prevent future editors from "fixing" working code.

```zsh
# NOTE: ZSH arrays are 1-indexed; ${arr[1]} is the first element (not 0)
# NOTE: ZSH regex captures → $match[], not $BASH_REMATCH[]
# NOTE: 'return' inside a sourced file stops sourcing only that file;
#       'exit' terminates the entire shell session
# NOTE: setopt NOUNSET is the ZSH equivalent of bash's set -u
# NOTE: ZSH globs skip dotfiles by default; use setopt GLOB_DOTS or
#       the (#D) glob qualifier to include them
# NOTE: typeset -U path deduplicates $PATH on every write; re-sourcing
#       .zprofile is therefore always safe
```

### 9.6 Annotation tags

Use these consistently so they are greppable across the entire config tree.

| Tag | When to use |
|-----|-------------|
| `# TODO(author, YYYY-MM-DD):` | Planned improvement |
| `# FIXME(author, YYYY-MM-DD):` | Known defect |
| `# NOTE:` | Non-obvious behaviour, ZSH/bash difference, platform quirk |
| `# WARN:` | Dangerous or irreversible operation |
| `# PERF:` | Performance-motivated decision or measured trade-off |
| `# DEPRECATED:` | What replaces this and when it will be removed |

---

## 10. Security

### 10.1 Never `eval` untrusted input

```zsh
# ❌ Executes arbitrary code from an environment variable
eval "${USER_SUPPLIED_VAR}"

# ✅ Explicit allowlist before any dynamic execution
_safe_tool_init() {
  local tool="$1"
  local -a allowed=(rbenv pyenv nodenv)

  (( ${allowed[(I)${tool}]} )) || {
    _zsh_error "security: '${tool}' is not in the allowed list"
    return 1
  }
  has "${tool}" || return 0
  eval "$("${tool}" init - zsh)"
}
```

### 10.2 Validate files before sourcing

```zsh
# ✅ Verify existence, type, and readability before sourcing
_zsh_safe_source() {
  local file="$1"
  [[ -e "${file}" ]] || return 0   # Missing is not an error (optional file)
  [[ -f "${file}" ]] || {
    _zsh_warn "will not source '${file}': not a regular file"
    return 1
  }
  [[ -r "${file}" ]] || {
    _zsh_warn "will not source '${file}': not readable"
    return 1
  }
  source "${file}"
}
```

### 10.3 Use `--` to terminate option parsing

Prevents filenames beginning with `-` from being treated as flags.

```zsh
# ✅
rm    -- "${file}"
echo  -- "${value}"
grep  -- "${pattern}" "${file}"

# ❌ Breaks when $file is "-rf /" or $value starts with "-"
rm "${file}"
```

### 10.4 Never store secrets in tracked files

```zsh
# ❌ Secret committed to version control
export AWS_SECRET_ACCESS_KEY="AKIAIOSFODNN7EXAMPLE"

# ✅ Load from the gitignored local override
_zsh_safe_source "${ZDOTDIR}/conf.d/local.zsh"

# ✅ Or delegate to a secrets CLI at call time
if has op; then
  export GITHUB_TOKEN="$(op item get 'GitHub PAT' --fields credential 2>/dev/null)"
fi
```

### 10.5 Restrict config file permissions

```zsh
chmod 700 "${ZDOTDIR}"
chmod 600 "${ZDOTDIR}/.zshenv"
chmod 600 "${ZDOTDIR}/conf.d/local.zsh"
```

Document these permissions in the repository README and enforce them in any
setup or bootstrap script.

### 10.6 Quote all external data

Any value read from a file, environment variable, or command output is
untrusted until validated. Always quote it.

```zsh
# ✅ IFS= prevents field-splitting; -r prevents backslash interpretation
while IFS= read -r line; do
  process -- "${line}"
done < "${config_file}"

# ❌ Word-splitting and glob expansion applied to every line
while read line; do
  process $line
done < ${config_file}
```

---

## 11. Performance

Target: total startup time under **200 ms**. Measure before optimising.

```zsh
# Add temporarily to the very top of .zshrc to identify slow modules
zmodload zsh/zprof
# ... rest of config ...
zprof    # Print timing report; remove both lines after use
```

### 11.1 Cache `compinit` — rebuild at most once per day

```zsh
# In conf.d/completion.zsh
autoload -Uz compinit

# NOTE: -C skips the security check and reuses the cached .zcompdump.
# The cache is rebuilt only when the dump file's day-of-year differs from today,
# keeping completions fresh without a full rebuild on every shell open.
_zsh_comp_dump="${XDG_CACHE_HOME:-${HOME}/.cache}/zsh/zcompdump-${ZSH_VERSION}"

if [[ -f "${_zsh_comp_dump}" \
   && "$(date +%j)" == "$(date -r "${_zsh_comp_dump}" +%j 2>/dev/null)" ]]; then
  compinit -C -d "${_zsh_comp_dump}"
else
  compinit    -d "${_zsh_comp_dump}"
fi

unset _zsh_comp_dump
```

### 11.2 Lazy-load slow tools

Tools such as `nvm`, `rbenv`, `pyenv` add 100–800 ms each when initialised
eagerly. Use stub functions that self-replace on first call.

```zsh
# Generic lazy-loader — zero startup cost, transparent to the user.
#
# Arguments:
#   $1  init_src   — file to source on first call ("" to skip)
#   $2  init_cmd   — command string to eval on first call ("" to skip)
#   $@  stub_names — names of the stub functions to register
#
# PERF: nvm eager init ≈ 250 ms; this pattern reduces that to ≈ 0 ms.
_lazy_load() {
  local init_src="$1" init_cmd="$2"; shift 2
  local -a names=("$@")
  local init_fn="_lazy_init_${names[1]}"   # NOTE: ZSH arrays are 1-indexed

  eval "
${init_fn}() {
  unfunction ${names[*]} ${init_fn} 2>/dev/null
  [[ -n '${init_src}' && -s '${init_src}' ]] && source '${init_src}'
  [[ -n '${init_cmd}' ]] && eval \"\$(${init_cmd})\"
}
"
  local name
  for name in "${names[@]}"; do
    eval "${name}() { ${init_fn} && ${name} \"\$@\"; }"
  done
}

# Usage — each tool is one line
[[ -s "${NVM_DIR}/nvm.sh" ]] \
  && _lazy_load "${NVM_DIR}/nvm.sh" "" nvm node npm npx yarn
has rbenv \
  && _lazy_load "" "rbenv init - --no-rehash zsh" rbenv ruby gem bundle
```

### 11.3 Prefer built-in parameter expansions over external forks

```zsh
# ✅ ZSH modifiers — no fork, runs in the current process
local name="${filepath:t}"   # equivalent to $(basename "${filepath}")
local dir="${filepath:h}"    # equivalent to $(dirname  "${filepath}")
local ext="${filepath:e}"    # extension only
local upper="${var:u}"       # uppercase
local lower="${var:l}"       # lowercase

# ❌ Each $(...) forks a subshell and execs an external binary
local name="$(basename "${filepath}")"
local dir="$(dirname  "${filepath}")"
```

### 11.4 Guard every `source` with an existence check

```zsh
# ✅ Silent no-op when the file is absent
[[ -f "${file}" ]] && source "${file}"

# ❌ Emits an error and may halt the module loader
source "${file}"
```

### 11.5 Cache expensive subshell results to a file

```zsh
# ✅ Fork once; all subsequent shell opens read from the file (microseconds)
_java_home_cache="${XDG_CACHE_HOME:-${HOME}/.cache}/zsh/java_home"
if [[ ! -f "${_java_home_cache}" ]]; then
  java -XshowSettings:all -version 2>&1 \
    | awk '/java.home/ { print $NF }' \
    > "${_java_home_cache}" 2>/dev/null || true
fi
export JAVA_HOME="$(<"${_java_home_cache}")"
unset _java_home_cache

# ❌ Forks java on every shell open (≈ 100 ms each time)
export JAVA_HOME="$(java -XshowSettings:all -version 2>&1 | awk '/java.home/{ print $NF }')"
```

---

## 12. Error Handling

### 12.1 Never set `ERR_EXIT` globally

`ERR_EXIT` (`set -e`) in a sourced file closes the terminal on any non-zero
return code. Use explicit, localised checks instead.

```zsh
# ❌ NEVER in any sourced config file
setopt ERR_EXIT
set -e

# ✅ Explicit and scoped
source "${module}" || {
  _zsh_error "failed to load: ${module:t}"
  return 1
}
```

### 12.2 Check return values of critical operations

```zsh
# ✅ Every critical path has an explicit failure branch
_load_module() {
  local file="$1"

  [[ -f "${file}" ]] || {
    _zsh_warn "module not found, skipping: ${file:t}"
    return 0    # A missing optional module is not fatal
  }
  source "${file}" || {
    _zsh_error "failed to source: ${file:t} (exit $?)"
    return 1
  }
}
```

### 12.3 Scope `PIPE_FAIL` with `LOCAL_OPTIONS`

Never set `PIPE_FAIL` globally in a sourced file. Use `LOCAL_OPTIONS` so the
option is automatically restored when the function returns.

```zsh
# ✅ PIPE_FAIL active only within this function
_process_entries() {
  setopt LOCAL_OPTIONS PIPE_FAIL   # NOTE: LOCAL_OPTIONS auto-restores on return
  grep -v '^#' "${1}" \
    | sort -u \
    | _ingest || { _zsh_error "pipeline failed for: $1"; return 1 }
}
```

### 12.4 Use guard clauses; avoid deep nesting

```zsh
# ✅ Early returns keep the happy path flat and readable
_configure_tool() {
  local tool="$1"
  [[ -n "${tool}" ]]  || { _zsh_error "tool name required"; return 1 }
  has "${tool}"        || return 0   # Tool absent — skip silently, not an error
  eval "$("${tool}" init - zsh)"
}

# ❌ Deep nesting buries the main logic
_configure_tool() {
  if [[ -n "$1" ]]; then
    if has "$1"; then
      eval "$("$1" init - zsh)"
    fi
  else
    _zsh_error "tool name required"; return 1
  fi
}
```

---

## 13. Logging

### 13.1 Logging helpers

Define the full implementations in `conf.d/functions.zsh`. Place minimal
bootstrap stubs (same signatures, no timestamp) at the top of `.zshrc` so
that module loading errors are captured before `functions.zsh` is sourced.
All output goes to **stderr** (`-u2`) to avoid polluting `$()` captures.

```zsh
# Log level: ZSH_LOG_LEVEL  0 silent | 1 error | 2 warn (default) | 3 info | 4 debug
: "${ZSH_LOG_LEVEL:=2}"

_zsh_error() { print -u2 -- "[ERROR] $(date '+%T') zsh: $*"; }
_zsh_warn()  { (( ZSH_LOG_LEVEL >= 2 )) && print -u2 -- "[WARN]  $(date '+%T') zsh: $*"; return 0; }
_zsh_info()  { (( ZSH_LOG_LEVEL >= 3 )) && print -u2 -- "[INFO]  $(date '+%T') zsh: $*"; return 0; }
_zsh_debug() { (( ZSH_LOG_LEVEL >= 4 )) && print -u2 -- "[DEBUG] $(date '+%T') zsh: $*"; return 0; }
```

### 13.2 Log level convention

| Helper | Active at level | When to use |
|--------|-----------------|-------------|
| `_zsh_error` | Always | Unrecoverable failure that causes incorrect behaviour |
| `_zsh_warn` | ≥ 2 (default) | Recoverable issue; feature degraded (e.g. optional tool absent) |
| `_zsh_info` | ≥ 3 | Normal lifecycle events (module loaded, tool initialised) |
| `_zsh_debug` | ≥ 4 | Timing data, variable dumps — developer mode only |

### 13.3 Adjust log level at runtime without editing files

```zsh
# For the current session only
ZSH_LOG_LEVEL=4 zsh

# Persistently on a specific machine — place in conf.d/local.zsh (not committed)
export ZSH_LOG_LEVEL=3
```

### 13.4 All log output must go to stderr

```zsh
# ✅ stderr — invisible to $() captures, correct for diagnostic output
_zsh_info "loading module: ${module:t}"

# ❌ stdout — corrupts any $() capture that happens to be active
echo "loading module: ${module:t}"
```

---

## 14. Prohibited Patterns

| Pattern | Reason | Replacement |
|---------|--------|-------------|
| `exit` in any sourced file | Terminates the entire shell session | `return` |
| `function foo { }` keyword | Redundant, not POSIX | `foo() { }` |
| Variables without `local` inside functions | Permanent global namespace leak | `local` / `typeset` |
| `[ ]` or `test` | Missing ZSH features; word-splitting risk | `[[ ]]` |
| Backtick `` `cmd` `` | Requires escaping when nested; hard to read | `$(cmd)` |
| `let` or `expr` for arithmetic | Obsolete | `(( ))` / `$(( ))` |
| `eval` on untrusted input | Arbitrary code execution | Allowlist + array expansion |
| `setopt ERR_EXIT` globally | Closes the terminal on any non-zero return | Explicit `\|\| return` checks |
| `setopt PIPE_FAIL` globally | Unexpected failures in sourced files | `setopt LOCAL_OPTIONS PIPE_FAIL` inside functions |
| Numeric prefixes on module filenames | Implicit ordering; brittle on insertion | Explicit `source` list in `.zshrc` |
| Glob-based module loading (`conf.d/*.zsh`) | Load order depends on filename sort | Explicit `source` list in `.zshrc` |
| Top-level `$(slow_cmd)` in `.zprofile` / `.zshrc` | Forks a process on every shell open | Lazy load or file-backed cache |
| `export PATH="...:${PATH}"` | Produces duplicate entries on re-source | `typeset -U path; path=(new $path)` |
| Unquoted variable expansions | Word-splitting and glob expansion on values | `"${var}"` everywhere |
| Secrets in tracked files | Exposed in version control | `conf.d/local.zsh` (gitignored) |
| Commented-out dead code | Misleading; increases maintenance noise | Delete it — git history preserves it |
| `source file` without an existence check | Error on missing optional files | `[[ -f "${f}" ]] && source "${f}"` |
| `echo` / `print` to stdout for logging | Corrupts `$()` captures | `_zsh_warn` / `_zsh_error` helpers |
| `setopt` without an inline comment | Option names are non-obvious | One comment per `setopt` line |
| `setopt` outside `conf.d/options.zsh` | Scattered and hard to audit | Centralise all options in `options.zsh` |

---

*Standard version: 1.0.0*
*Sources: Google Shell Style Guide · Oh My Zsh Code Style Guide ·*
*ZSH Opinionated Best Practices · zsh-users/zsh · Arch Linux Wiki · zsh.sourceforge.io*
