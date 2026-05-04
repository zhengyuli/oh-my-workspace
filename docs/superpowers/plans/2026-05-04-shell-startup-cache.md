# Shell Startup Cache Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Cache all 4 `eval "$(tool init zsh)"` calls to disk, eliminating subprocess spawns on every shell start.

**Architecture:** Replace each `eval "$(tool init)"` with the disk-cache pattern already used for UV/carapace in `70-tools.zsh`. Cache files live under `$XDG_CACHE_HOME/zsh/`, invalidated when the tool binary is newer than the cache.

**Tech Stack:** Zsh, GNU coreutils (gdircolors), Homebrew-installed CLI tools

---

## File Structure

| File | Responsibility | Change type |
|------|---------------|-------------|
| `shell/zsh/.config/zsh/conf.d/00-env.zsh` | Core env vars, LS_COLORS | Modify: gdircolors cache |
| `shell/zsh/.config/zsh/conf.d/40-plugins.zsh` | Zinit plugins, direnv | Modify: direnv hook cache |
| `shell/zsh/.config/zsh/conf.d/50-prompt.zsh` | Starship prompt | Modify: starship init cache |
| `shell/zsh/.config/zsh/conf.d/70-tools.zsh` | Tool integrations | Modify: zoxide init cache |

---

### Task 1: Cache gdircolors in 00-env.zsh

**Files:**
- Modify: `shell/zsh/.config/zsh/conf.d/00-env.zsh:136-141`

- [ ] **Step 1: Apply the cache pattern**

Replace lines 136-141 in `00-env.zsh`. The `else` branch (fallback LS_COLORS) stays unchanged.

Before:
```zsh
if command -v gdircolors &>/dev/null; then
  eval "$(gdircolors -b)"
else
```

After:
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
```

- [ ] **Step 2: Syntax check**

Run: `zsh -n shell/zsh/.config/zsh/conf.d/00-env.zsh`
Expected: No output (exit 0)

- [ ] **Step 3: Commit**

```bash
git add shell/zsh/.config/zsh/conf.d/00-env.zsh
git commit -m "perf(zsh): cache gdircolors output to avoid subprocess per shell start"
```

---

### Task 2: Cache direnv hook in 40-plugins.zsh

**Files:**
- Modify: `shell/zsh/.config/zsh/conf.d/40-plugins.zsh:195-199`

- [ ] **Step 1: Apply the cache pattern**

Replace the direnv block (lines 195-199 in `40-plugins.zsh`).

Before:
```zsh
if command -v direnv &>/dev/null; then
  # Suppress "direnv: loading..." / "direnv: export..." log clutter
  export DIRENV_LOG_FORMAT=""
  eval "$(direnv hook zsh)"
fi
```

After:
```zsh
if command -v direnv &>/dev/null; then
  # Suppress "direnv: loading..." / "direnv: export..." log clutter
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

- [ ] **Step 2: Syntax check**

Run: `zsh -n shell/zsh/.config/zsh/conf.d/40-plugins.zsh`
Expected: No output (exit 0)

- [ ] **Step 3: Commit**

```bash
git add shell/zsh/.config/zsh/conf.d/40-plugins.zsh
git commit -m "perf(zsh): cache direnv hook to avoid subprocess per shell start"
```

---

### Task 3: Cache starship init in 50-prompt.zsh

**Files:**
- Modify: `shell/zsh/.config/zsh/conf.d/50-prompt.zsh:25-26`

- [ ] **Step 1: Apply the cache pattern**

Replace the `eval` line inside the starship `if` block (lines 25-26 in `50-prompt.zsh`). Everything after the init (OSC title hook, etc.) stays unchanged.

Before:
```zsh
if command -v starship &>/dev/null; then
  eval "$(starship init zsh)"
```

After:
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
```

- [ ] **Step 2: Syntax check**

Run: `zsh -n shell/zsh/.config/zsh/conf.d/50-prompt.zsh`
Expected: No output (exit 0)

- [ ] **Step 3: Commit**

```bash
git add shell/zsh/.config/zsh/conf.d/50-prompt.zsh
git commit -m "perf(zsh): cache starship init to avoid subprocess per shell start"
```

---

### Task 4: Cache zoxide init in 70-tools.zsh

**Files:**
- Modify: `shell/zsh/.config/zsh/conf.d/70-tools.zsh:86-90`

- [ ] **Step 1: Apply the cache pattern**

Replace the zoxide block (lines 86-90 in `70-tools.zsh`). Keep the `unalias zi` after sourcing.

Before:
```zsh
if command -v zoxide &>/dev/null; then
  eval "$(zoxide init zsh --cmd z)"
  # zinit's alias zi=zinit shadows zoxide's zi() — remove it.
  unalias zi 2>/dev/null
fi
```

After:
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
  # zinit's alias zi=zinit shadows zoxide's zi() — remove it.
  unalias zi 2>/dev/null
fi
```

- [ ] **Step 2: Syntax check**

Run: `zsh -n shell/zsh/.config/zsh/conf.d/70-tools.zsh`
Expected: No output (exit 0)

- [ ] **Step 3: Commit**

```bash
git add shell/zsh/.config/zsh/conf.d/70-tools.zsh
git commit -m "perf(zsh): cache zoxide init to avoid subprocess per shell start"
```

---

### Task 5: Final Validation

- [ ] **Step 1: Syntax check all 4 files**

```bash
zsh -n shell/zsh/.config/zsh/conf.d/00-env.zsh && \
zsh -n shell/zsh/.config/zsh/conf.d/40-plugins.zsh && \
zsh -n shell/zsh/.config/zsh/conf.d/50-prompt.zsh && \
zsh -n shell/zsh/.config/zsh/conf.d/70-tools.zsh && \
echo "ALL PASS"
```

Expected: `ALL PASS`

- [ ] **Step 2: Verify no remaining uncached eval calls**

```bash
grep -n 'eval "\$(' shell/zsh/.config/zsh/conf.d/*.zsh
```

Expected: No output (all `eval "$(` patterns eliminated)

- [ ] **Step 3: Verify cache files are consistent**

Check that cache variable names follow the pattern `_<tool>_cache` and all are unset after use:

```bash
grep -n '_cache=' shell/zsh/.config/zsh/conf.d/*.zsh
grep -n 'unset _.*_cache' shell/zsh/.config/zsh/conf.d/*.zsh
```

Expected: 4 assignments, 4 unsets (one per tool)
