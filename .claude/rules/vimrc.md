---
paths:
  - "editor/vim/**"
---

# Vim Script Configuration Files

This rule targets **Vim only**. Neovim-specific features (`:checkhealth`,
built-in LSP, Lua config) are out of scope.

Standards for Vim configuration files (vimrc, .vim).

References: [Google Vimscript Style Guide](https://google.github.io/styleguide/vimscriptguide.xml),
[vim-galore](https://github.com/mhinz/vim-galore),
[Vim Options](https://vimdoc.sourceforge.net/htmldoc/options.html).

## File Header

```
" filename -*- mode: vim; -*-
" Time-stamp: <2026-03-28 00:00:00 Friday by zhengyu.li>
" =============================================================================
" Title - Brief description
"
" Author: zhengyu li <lizhengyu419@outlook.com>
" Keywords: keyword1, keyword2
"
" Copyright (C) 2026 zhengyu li
"
" History:
"   2026-MM-DD HH:MM zhengyu li <lizhengyu419@outlook.com> created.
"
" Commentary:
"   Detailed description of what this config does.
"
" Location: ~/.config/vim/vimrc
" Loaded via: VIMINIT=source\ $XDG_CONFIG_HOME/vim/vimrc (set in 00-env.zsh)
" XDG: Manually redirected (Vim predates XDG; VIMINIT bypasses default loader)
"      - Data:  $XDG_DATA_HOME/vim/undo  (~/.local/share/vim/undo)
"      - State: $XDG_STATE_HOME/vim/viminfo  (~/.local/state/vim/viminfo)
"
" References:
"   1. Vim Options: https://vimdoc.sourceforge.net/htmldoc/options.html
"   2. XDG in Vim: https://wiki.archlinux.org/title/XDG_Base_Directory#Vim
" =============================================================================
```

## Delimiter Hierarchy

**Level 0** (File Header): `" ============...` (79 chars)
**Level 1** (Primary Section): `" -----------...` (79 chars)
**Level 2** (Subsection): `" --- Title ---`

**Title Case required**: capitalize the first letter of every word in both
Section Title and Subsection Title (e.g., `XDG Paths`, `File Type Overrides`).
Abbreviations follow their established convention: ALL CAPS for standard
abbreviations (e.g., `FZF Preview`, `PDF Tools`, `JSON Mode`), lowercase
for established lowercase names (e.g., `cc Mode`, `sh Mode`, `xref`).

### Blank Lines

Blank lines mark boundaries between delimiter levels and top-level statements.

**Around delimiters** — one blank line before Level 1 opening, one after
Level 1 closing.  Level 2 has no blank line after the delimiter — code follows
immediately.

```vim
" -----------------------------------------------------------------------------
" Display
" -----------------------------------------------------------------------------

" --- Core ---
set number
set relativenumber

" --- Colors ---
set termguicolors
```

**Between top-level statements within the same subsection** — one blank line.
Related statements (e.g., consecutive `set`, `let`) are not separated.

```vim
" --- Navigation ---
nnoremap <silent> <Esc><Esc> :nohlsearch<CR>
vnoremap < <gv
vnoremap > >gv

augroup vimrc
    autocmd!
augroup END

let s:cfg = ($XDG_CONFIG_HOME != '' ? $XDG_CONFIG_HOME : $HOME . '/.config')
```

**Inside function bodies** — one blank line between logical steps.
Single-expression functions have no extra blank lines.

```vim
" Multi-step body
function! s:StatuslineActive() abort
    let l:git = exists('*FugitiveHead') ? '  ' . FugitiveHead() . ' ' : ''
    let l:ft = &filetype

    return l:git . ' ' . l:ft
endfunction

" Single-expression body — no extra blank lines
function! s:StatuslineGit() abort
    return exists('*FugitiveHead') ? '  ' . FugitiveHead() . ' ' : ''
endfunction
```

**Prohibited**: two or more consecutive blank lines anywhere in the file.

## Indentation

2-space indent for Vim script block bodies (`if`, `for`, `while`,
`function`, `augroup`).  Continuation lines (`\`) align under the
opening statement.

```vim
if has('termguicolors')
  set termguicolors
endif

augroup vimrc
  autocmd!
  autocmd BufReadPost * if line("'\"") > 0 | execute 'normal! g`"' | endif
augroup END

let &runtimepath = s:cfg  . '/vim,'
               \ . s:cfg  . '/vim/after,'
               \ . $VIMRUNTIME
```

## Line Length

79 characters maximum. Use line continuation (`\`) aligned under the opening
statement.

Exceptions:

- URLs and file paths that cannot be wrapped
- Option values that cannot be meaningfully split

```vim
" CORRECT — continuation indented to match opening command
let &runtimepath = s:cfg  . '/vim,'
               \ . s:cfg  . '/vim/after,'
               \ . $VIMRUNTIME
```

## Comments

Vim script uses `"` for line comments (not `#`).

Comments explain *why*, not *what*. The code itself should be readable
enough to show what it does.

**When to comment**:
- Non-obvious option values (what does `timeoutlen=4200` mean?)
- Why a specific approach was chosen over alternatives
- Constraints from Vim's limited XDG support

**When NOT to comment**:
- Self-documenting options (`set number`, `set expandtab`)
- Obvious operations

**No end-of-line comments** — place comments on a separate `"` line above
the setting (see [Anti-Patterns > Don't: End-of-Line Comments](#dont-end-of-line-comments)).

```vim
" WRONG — restates the obvious (what the code already says)
" Set expandtab to use spaces
set expandtab

" CORRECT — explains why (project convention)
" Use spaces instead of tabs (project convention)
set expandtab
```

## Code Patterns

### Plugin Management

No plugin manager by design. Vim is a lightweight fallback editor; Doom Emacs
is the primary editor. Optional plugin support degrades gracefully (e.g.,
vim-fugitive branch in statusline shows empty string when absent).

### Options: `set` vs `let`

- `set` for Vim options: `set number`, `set tabstop=4`
- `let &option` for computed values: `let &undodir = s:data . '/vim/undo'`
- `setlocal` for buffer-local settings in autocommands

```vim
" Global option
set number

" Computed path
let &undodir = s:data . '/vim/undo'

" Buffer-local in autocmd
autocmd FileType python setlocal tabstop=4 shiftwidth=4
```

### Variables

Use appropriate scope prefixes. Prefer script-local for internal state.

| Scope | Prefix | Use case |
|-------|--------|----------|
| Script-local | `s:var` | Internal state, paths |
| Global | `g:var` | Cross-script config |
| Buffer-local | `b:var` | Per-buffer state |
| Window-local | `w:var` | Per-window state |
| Vim option | `&option` | Read/write option value |

```vim
let s:cfg = ($XDG_CONFIG_HOME != '' ? $XDG_CONFIG_HOME : $HOME . '/.config')
```

### Key Mappings

Use `noremap` variants to prevent recursive resolution. Add `<silent>` for
non-interactive mappings.

```vim
nnoremap <silent> <Esc><Esc> :nohlsearch<CR>
vnoremap < <gv
vnoremap > >gv
```

### Auto Commands

Always wrap in an augroup with `autocmd!` to prevent duplication on re-source.

```vim
augroup vimrc
    autocmd!

    " Return to last cursor position when reopening a file
    autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"' |
        \ endif
augroup END
```

### Functions

Use `function!` with `abort` keyword. Name script-local functions with `s:`
prefix.

```vim
function! s:StatuslineGit() abort
    return exists('*FugitiveHead') ? '  ' . FugitiveHead() . ' ' : ''
endfunction
```

### Conditional Logic

Use `has('feature')` for capability checks, not version numbers.

```vim
if has('clipboard')
    set clipboard=unnamedplus
endif

if has('termguicolors')
    set termguicolors
endif
```

### Strings

Prefer single quotes for literals; double quotes only when escape sequences are
needed.

```vim
let s:path = '/vim/undo'
let s:greeting = "Hello\nWorld"
```

### Regular Expressions

Prefer `\v` (very magic) mode for readability.

```vim
if &filetype !~# '\v^(diff|xxd)$'
```

### XDG Paths

Vim does not natively support XDG. Use script-local variables with fallbacks
and `mkdir` guard.

```vim
let s:cfg = ($XDG_CONFIG_HOME != '' ? $XDG_CONFIG_HOME : $HOME . '/.config')
let s:data = ($XDG_DATA_HOME != '' ? $XDG_DATA_HOME : $HOME . '/.local/share')
let s:state = ($XDG_STATE_HOME != ''
             \ ? $XDG_STATE_HOME : $HOME . '/.local/state')

for s:dir in [s:state . '/vim', s:data . '/vim/undo']
    if !isdirectory(s:dir) | call mkdir(s:dir, 'p') | endif
endfor
```

### Section Uniqueness

Each section title must be unique within the file at every delimiter level
(Level 1 and Level 2). Group related settings together — do not create
multiple sections of the same name.

```vim
" WRONG — duplicate section at Level 2
" --- Mappings ---
nnoremap j gj
" --- Options ---
set number
" --- Mappings ---              ← same name reused
nnoremap k gk

" CORRECT
" --- Mappings ---
nnoremap j gj
nnoremap k gk
" --- Options ---
set number
```

### Validation at Boundaries

Validate inputs at system boundaries in script-local functions.

```vim
function! s:LoadConfig(name) abort
    if a:name !~# '\v^\w+$'
        echoerr 'Invalid config name: ' . a:name
        return
    endif
    execute 'source' s:cfg . '/vim/' . a:name . '.vim'
endfunction
```

### Nesting Limit

Maximum 3 nesting levels. Use early-return guards to flatten structure.

```vim
" WRONG — 4 levels deep
function! s:Process(file) abort
    if filereadable(a:file)
        if !isdirectory(s:tmp)
            if mkdir(s:tmp, 'p')
                if writefile([], a:file) == 0
                    " process file
                endif
            endif
        endif
    endif
endfunction

" CORRECT — early returns, 2 levels max
function! s:Process(file) abort
    if !filereadable(a:file) | return | endif
    if isdirectory(s:tmp) | return | endif
    call mkdir(s:tmp, 'p')
    call writefile([], a:file)
endfunction
```

### No Magic Numbers

Non-obvious numeric values must be explained with a comment.  Self-explanatory
values (e.g., `tabstop=4`) need no comment.

```vim
" WRONG — what does 4200 mean?
set timeoutlen=4200

" CORRECT — explain non-obvious values
" 4.2 s: long enough for multi-key sequences (e.g., <Esc><Esc>)
set timeoutlen=4200
```

### Highlight Definitions

Group by semantic category. Align columns for readability.

**Exception to no-align rule**: Highlight definitions are exempt from the
"Don't: Align Values" anti-pattern. Column alignment across `guifg`/`guibg`/
`gui` fields enables visual scanning of the color table — the value here
outweighs the diff-noise concern.

This exception applies **only** to `hi` (highlight) definitions — never
use alignment for `set`, `let`, `map`, or any other Vim commands.

```vim
" --- Core ---
hi Normal          guifg=#f8f8f2  guibg=#282a36  gui=NONE
hi CursorLine      guifg=NONE     guibg=#44475a  gui=NONE

" --- Syntax ---
hi Comment         guifg=#6272a4  guibg=NONE     gui=italic
hi Constant        guifg=#bd93f9  guibg=NONE     gui=NONE
```

## Anti-Patterns

### Don't: Align Option Values with `set`

```vim
" WRONG — `set` does not accept aligned spacing
set tabstop  =4
set shiftwidth=4

" CORRECT
set tabstop=4
set shiftwidth=4
```

**Exception**: `hi` (highlight) definitions — see
[Highlight Definitions](#highlight-definitions).

### Don't: Use Global Variables for Internal State

```vim
" WRONG
let g:config_path = '/vim'

" CORRECT — script-local scope
let s:config_path = '/vim'
```

### Don't: Omit `abort` on Functions

```vim
" WRONG — continues after error
function! s:Foo()
    call s:bar()
endfunction

" CORRECT — stops on first error
function! s:Foo() abort
    call s:bar()
endfunction
```

### Don't: Use Recursive Mappings Without Reason

```vim
" WRONG — potential infinite recursion
nnoremap j gj
nnoremap gj j

" CORRECT — use noremap to avoid recursion
nnoremap gj j
```

### Don't: End-of-Line Comments

Avoid end-of-line `" comment` annotations after a command.  Move the
explanation to a separate `"` comment line above the setting — end-of-line
comments are easily missed during review and typically restate the obvious.

```vim
" WRONG — end-of-line annotation restates the obvious
set number  " Show line numbers

" CORRECT — separate line explains reasoning
" Line numbers help navigate compiler error messages
set number
```

## Security

### Secrets Management

Never hardcode API keys, tokens, or passwords in vimrc.  Vim has no native
secret management — use environment variables accessed via `$VAR`.

```vim
" WRONG — hardcoded token
let g:plugin_token = 'sk-1234567890'

" CORRECT — read from environment
let s:plugin_token = $PLUGIN_TOKEN
```

**Sensitive types**: API keys, tokens, passwords, private keys, certificates.

## References

1. [Vim Documentation](https://vimhelp.org/)
2. [Google Vimscript Style Guide](https://google.github.io/styleguide/vimscriptguide.xml)
3. [vim-galore](https://github.com/mhinz/vim-galore)
4. [XDG Base Directory — Vim](https://wiki.archlinux.org/title/XDG_Base_Directory#Vim)
5. [Dracula Theme Spec](https://spec.draculatheme.com)

## Validation

```bash
# Static lint (no side effects)
vint ~/.config/vim/vimrc

# Check for syntax errors (Ex mode, silent)
vim -e -c 'source ~/.config/vim/vimrc' -c 'q' 2>&1

# Verify specific option values
vim -e -c 'set tabstop?' -c 'q'

# Validate loaded scripts
:scriptnames
```
