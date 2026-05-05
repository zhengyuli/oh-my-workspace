#!/usr/bin/env bats
# config-validation.bats — syntax validation for declarative config files

REPO_ROOT="${BATS_TEST_DIRNAME}/.."

# ---------------------------------------------------------------------------
# TOML Files
# ---------------------------------------------------------------------------

@test "starship.toml is valid TOML" {
  run python3 -c "
import tomllib, sys
with open(sys.argv[1], 'rb') as f:
    tomllib.load(f)
" "${REPO_ROOT}/shell/starship/.config/starship.toml"
  (( status == 0 ))
}

@test "uv.toml is valid TOML" {
  run python3 -c "
import tomllib, sys
with open(sys.argv[1], 'rb') as f:
    tomllib.load(f)
" "${REPO_ROOT}/lang/python/uv/.config/uv/uv.toml"
  (( status == 0 ))
}

@test "bunfig.toml is valid TOML" {
  run python3 -c "
import tomllib, sys
with open(sys.argv[1], 'rb') as f:
    tomllib.load(f)
" "${REPO_ROOT}/lang/typescript/bun/.config/.bunfig.toml"
  (( status == 0 ))
}

@test "yazi/yazi.toml is valid TOML" {
  run python3 -c "
import tomllib, sys
with open(sys.argv[1], 'rb') as f:
    tomllib.load(f)
" "${REPO_ROOT}/tool/yazi/.config/yazi/yazi.toml"
  (( status == 0 ))
}

@test "yazi/keymap.toml is valid TOML" {
  run python3 -c "
import tomllib, sys
with open(sys.argv[1], 'rb') as f:
    tomllib.load(f)
" "${REPO_ROOT}/tool/yazi/.config/yazi/keymap.toml"
  (( status == 0 ))
}

@test "yazi/theme.toml is valid TOML" {
  run python3 -c "
import tomllib, sys
with open(sys.argv[1], 'rb') as f:
    tomllib.load(f)
" "${REPO_ROOT}/tool/yazi/.config/yazi/theme.toml"
  (( status == 0 ))
}

@test "yazi/package.toml is valid TOML" {
  run python3 -c "
import tomllib, sys
with open(sys.argv[1], 'rb') as f:
    tomllib.load(f)
" "${REPO_ROOT}/tool/yazi/.config/yazi/package.toml"
  (( status == 0 ))
}

@test "yazi/catppuccin-mocha flavor.toml is valid TOML" {
  run python3 -c "
import tomllib, sys
with open(sys.argv[1], 'rb') as f:
    tomllib.load(f)
" "${REPO_ROOT}/tool/yazi/.config/yazi/flavors/catppuccin-mocha.yazi/flavor.toml"
  (( status == 0 ))
}

# ---------------------------------------------------------------------------
# YAML Files
# ---------------------------------------------------------------------------

@test "lazygit config.yml is valid YAML" {
  run python3 -c "
import yaml, sys
with open(sys.argv[1]) as f:
    yaml.safe_load(f)
" "${REPO_ROOT}/tool/lazygit/.config/lazygit/config.yml"
  (( status == 0 ))
}

# ---------------------------------------------------------------------------
# Git Config
# ---------------------------------------------------------------------------

@test "git/config is parseable by git" {
  run git config --file "${REPO_ROOT}/tool/git/.config/git/config" --list
  (( status == 0 ))
}

@test "git/config.local is parseable by git" {
  run git config --file "${REPO_ROOT}/tool/git/.config/git/config.local" --list
  (( status == 0 ))
}

@test "git/ignore contains common patterns" {
  run cat "${REPO_ROOT}/tool/git/.config/git/ignore"
  (( status == 0 ))
  [[ "$output" == *".DS_Store"* ]]
}

# ---------------------------------------------------------------------------
# Ghostty Config
# ---------------------------------------------------------------------------

@test "ghostty/config has no empty key assignments" {
  # Ghostty uses 'key = value' format. Empty values are invalid.
  run grep -nE '^[a-z_-]+ *= *$' "${REPO_ROOT}/term/ghostty/.config/ghostty/config"
  # grep returns 1 when no matches (good — means no empty assignments)
  (( status == 1 ))
}

@test "ghostty/config has no duplicate keys (excluding repeatable)" {
  # font-feature and keybind are intentionally repeated (array-like syntax)
  run bash -c "
    grep -E '^[a-z_-]+ *=' '${REPO_ROOT}/term/ghostty/.config/ghostty/config' |
    sed 's/ *=.*//' |
    grep -v -E '^(keybind|font-feature|palette)$' |
    sort |
    uniq -d
  "
  [[ -z "$output" ]]
}

# ---------------------------------------------------------------------------
# Shell Syntax
# ---------------------------------------------------------------------------

@test "setup.sh passes bash -n syntax check" {
  run bash -n "${REPO_ROOT}/setup.sh"
  (( status == 0 ))
}

@test "pre-setup.sh passes bash -n syntax check" {
  run bash -n "${REPO_ROOT}/claude/pre-setup.sh"
  (( status == 0 ))
}

@test "defaults.sh passes bash -n syntax check" {
  run bash -n "${REPO_ROOT}/platform/darwin/defaults.sh"
  (( status == 0 ))
}

@test "all zsh conf.d files pass zsh -n syntax check" {
  local failed=0
  for f in "${REPO_ROOT}"/shell/zsh/.config/zsh/conf.d/*.zsh; do
    if ! zsh -n "$f" 2>/dev/null; then
      echo "FAIL: $f" >&2
      failed=1
    fi
  done
  (( failed == 0 ))
}
