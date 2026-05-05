#!/usr/bin/env bats
# zsh-05-path.bats — tests for conf.d/05-path.zsh

load zsh-helper

setup() {
  setup_zsh_env
  mkdir -p "${HOME}/.local/bin"
  mkdir -p "${HOME}/.config/zsh/functions"
  mkdir -p "${HOME}/.config/zsh/completions"
}
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/05-path.zsh"

_run_path() {
  local expr="$1"
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export CARGO_HOME=\"${HOME}/.local/share/cargo\"
    export GOPATH=\"${HOME}/.local/share/go\"
    export BUN_INSTALL=\"${HOME}/.local/share/bun\"
    export HOMEBREW_PREFIX=\"/opt/homebrew\"
    export PATH=\"${BATS_TEST_DIRNAME}/mocks/zsh:/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\" \"\$XDG_STATE_HOME/zsh\"
    source \"${MODULE}\"
    ${expr}
  "
}

@test "HOME/.local/bin is in PATH" {
  _run_path 'print -l "${path[@]}"'
  [[ "$output" == *"${HOME}/.local/bin"* ]]
}

@test "PATH is deduplicated" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export CARGO_HOME=\"${HOME}/.local/share/cargo\"
    export GOPATH=\"${HOME}/.local/share/go\"
    export BUN_INSTALL=\"${HOME}/.local/share/bun\"
    export HOMEBREW_PREFIX=\"/opt/homebrew\"
    export PATH=\"/usr/bin:/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\" \"\$XDG_STATE_HOME/zsh\"
    source \"${MODULE}\"
    print -l \"\${path[@]}\" | grep -c '^/usr/bin\$'
  "
  [[ "$output" == "1" ]]
}

@test "non-existent dirs excluded from PATH" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export CARGO_HOME=\"${HOME}/.local/share/cargo\"
    export GOPATH=\"${HOME}/.local/share/go\"
    export BUN_INSTALL=\"${HOME}/.local/share/bun\"
    export HOMEBREW_PREFIX=\"/nonexistent_homebrew\"
    export PATH=\"${BATS_TEST_DIRNAME}/mocks/zsh:/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\" \"\$XDG_STATE_HOME/zsh\"
    source \"${MODULE}\"
    print -l \"\${path[@]}\"
  "
  [[ "$output" != *"/nonexistent_homebrew/bin"* ]]
}

@test "FPATH includes custom functions dir" {
  _run_path 'print -l "${fpath[@]}"'
  [[ "$output" == *"/.config/zsh/functions"* ]]
}

@test "autoload registers functions from functions dir" {
  echo 'echo hello' > "${HOME}/.config/zsh/functions/testfunc"
  _run_path 'whence -w testfunc'
  [[ "$output" == *"function"* ]]
}
