---
paths:
  - "**/*.vim"
  - "**/vimrc"
---

# Vim Configuration Standards

> This file extends [coding-style.md](./coding-style.md) with vim-specific standards.

## Header Format

Vim configuration follows the [root coding-style.md](./coding-style.md) with vim-specific adjustments:

```vim
" vimrc
" =============================================================================
" Vim Configuration - Minimal, Plugin-Free Setup
"
" Location: ~/.config/vim/vimrc
" XDG: Loaded via VIMINIT environment variable
" =============================================================================
```

## Comment Character

Vim uses `"` for comments:

```vim
" This is a comment
set number  " This is also a comment
```

## Alignment Spaces Exception

Vim color scheme configuration MAY use alignment spaces for readability. This is an exception to the repository-wide alignment space prohibition.

**Scope:** This exception applies ONLY to:
- `vim/.config/vim/vimrc` color scheme section (lines ~286-400)
- Any `*.vim` files defining color schemes

**ALLOWED - Color palette table in comments (readability):**
```vim
"   bg       #282c34    bg-alt   #21242b    fg       #bbc2cf
"   red      #ff6c6b    orange   #da8548    green    #98be65
```

**ALLOWED - Highlight commands with column alignment (color scheme only):**
```vim
hi Normal       guifg=#bbc2cf guibg=#282c34 gui=NONE
hi CursorLine   guifg=NONE    guibg=#23272e gui=NONE
hi Visual        guifg=NONE    guibg=#3e4454 gui=NONE
```

**When uncertain:** Ask user to confirm whether alignment spaces are intentional formatting or should be removed. Do NOT auto-modify without user approval.

## Key Patterns

1. **No plugins**: This configuration is intentionally minimal. For advanced editing, use Emacs.
2. **Doom One theme**: Custom color scheme matching the repository theme
3. **XDG compliance**: All state/data stored in XDG paths
4. **Sensible defaults**: Modern editing preferences without external dependencies

## XDG Paths

Vim is configured via the `VIMINIT` environment variable set in `zsh/.config/zsh/conf.d/00-env.zsh`:

```bash
export VIMINIT="source ${XDG_CONFIG_HOME}/vim/vimrc"
```

| Path | Purpose |
|------|---------|
| `$XDG_CONFIG_HOME/vim/vimrc` | Main configuration |
| `$XDG_DATA_HOME/vim/undo` | Persistent undo history |
| `$XDG_STATE_HOME/vim/viminfo` | Vim state file |
