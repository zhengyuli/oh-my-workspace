# Shell Startup Cache Design

## Problem

Four tool initialization commands run `eval "$(tool init zsh)"` on every shell
start, spawning a subprocess each time. The repo already caches UV and carapace
completions to disk with binary-age invalidation — these four are the
remaining uncached calls.

| Tool | File | Current code |
|------|------|-------------|
| gdircolors | `00-env.zsh:137` | `eval "$(gdircolors -b)"` |
| direnv | `40-plugins.zsh:198` | `eval "$(direnv hook zsh)"` |
| starship | `50-prompt.zsh:26` | `eval "$(starship init zsh)"` |
| zoxide | `70-tools.zsh:87` | `eval "$(zoxide init zsh --cmd z)"` |

Each outputs static shell code (function definitions, hook registrations). The
output only changes when the binary itself is updated.

## Approach

Apply the same disk-cache pattern already used for UV and carapace:

```zsh
_cache="$XDG_CACHE_HOME/zsh/<name>.zsh"
if [[ ! -f "$_cache" ]] || [[ "$(command -v <binary>)" -nt "$_cache" ]]; then
  mkdir -p "${_cache:h}"
  <binary> <args> >! "$_cache"
fi
source "$_cache"
unset _cache
```

### Cache invalidation

`[[ "$(command -v <binary>)" -nt "$_cache" ]]` — regenerates when the binary
file is newer than the cache. This covers `brew upgrade`, manual reinstall,
and version managers. No timestamp, version-string, or hash tracking needed.

### Cache location

All caches go under `$XDG_CACHE_HOME/zsh/` (same directory as `zcompdump`,
`uv-completion.zsh`, `carapace-completion.zsh`).

| Tool | Cache file name |
|------|----------------|
| gdircolors | `gdircolors.zsh` |
| direnv | `direnv-hook.zsh` |
| starship | `starship-init.zsh` |
| zoxide | `zoxide-init.zsh` |

## File Changes

### 1. `00-env.zsh` — gdircolors

**Before:**
```zsh
if command -v gdircolors &>/dev/null; then
  eval "$(gdircolors -b)"
else
  ...
fi
```

**After:**
```zsh
if command -v gdircolors &>/dev/null; then
  _gdircolors_cache="$XDG_CACHE_HOME/zsh/gdircolors.zsh"
  if [[ ! -f "$_gdircolors_cache" ]] || \
     [[ "$(command -v gdircolors)" -nt "$_gdircolors_cache" ]]; then
    mkdir -p "${_gdircolors_cache:h}"
    gdircolors -b >! "$_gdircolors_cache"
  fi
  source "$_gdircolors_cache"
  unset _gdircolors_cache
else
  ...
fi
```

### 2. `40-plugins.zsh` — direnv

**Before:**
```zsh
if command -v direnv &>/dev/null; then
  export DIRENV_LOG_FORMAT=""
  eval "$(direnv hook zsh)"
fi
```

**After:**
```zsh
if command -v direnv &>/dev/null; then
  export DIRENV_LOG_FORMAT=""
  _direnv_cache="$XDG_CACHE_HOME/zsh/direnv-hook.zsh"
  if [[ ! -f "$_direnv_cache" ]] || \
     [[ "$(command -v direnv)" -nt "$_direnv_cache" ]]; then
    mkdir -p "${_direnv_cache:h}"
    direnv hook zsh >! "$_direnv_cache"
  fi
  source "$_direnv_cache"
  unset _direnv_cache
fi
```

### 3. `50-prompt.zsh` — starship

**Before:**
```zsh
if command -v starship &>/dev/null; then
  eval "$(starship init zsh)"
  ...
```

**After:**
```zsh
if command -v starship &>/dev/null; then
  _starship_cache="$XDG_CACHE_HOME/zsh/starship-init.zsh"
  if [[ ! -f "$_starship_cache" ]] || \
     [[ "$(command -v starship)" -nt "$_starship_cache" ]]; then
    mkdir -p "${_starship_cache:h}"
    starship init zsh >! "$_starship_cache"
  fi
  source "$_starship_cache"
  unset _starship_cache
  ...
```

### 4. `70-tools.zsh` — zoxide

**Before:**
```zsh
if command -v zoxide &>/dev/null; then
  eval "$(zoxide init zsh --cmd z)"
  unalias zi 2>/dev/null
fi
```

**After:**
```zsh
if command -v zoxide &>/dev/null; then
  _zoxide_cache="$XDG_CACHE_HOME/zsh/zoxide-init.zsh"
  if [[ ! -f "$_zoxide_cache" ]] || \
     [[ "$(command -v zoxide)" -nt "$_zoxide_cache" ]]; then
    mkdir -p "${_zoxide_cache:h}"
    zoxide init zsh --cmd z >! "$_zoxide_cache"
  fi
  source "$_zoxide_cache"
  unset _zoxide_cache
  unalias zi 2>/dev/null
fi
```

## Non-goals

- **Lazy-loading completions**: Bun/UV/carapace completions are already
  cached to disk. The `source` cost is negligible.
- **zsh-defer dependency**: Adds complexity and a new dependency for marginal
  gain over disk caching.
- **Zinit trigger-load**: Only works for zinit-managed plugins, not
  Homebrew-installed tools.

## Testing

1. `zsh -n` syntax check on all 4 modified files
2. Delete cache files, open new shell — verify tools work and caches are created
3. `brew upgrade <tool>` — verify cache regenerates on next shell start
4. Benchmark: `time zsh -i -c exit` before and after
