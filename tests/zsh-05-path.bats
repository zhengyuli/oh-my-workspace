#!/usr/bin/env bats
# zsh-05-path.bats — tests for conf.d/05-path.zsh

load zsh-helper

setup() {
  setup_zsh_env
  mkdir -p "${HOME}/.local/bin"
  mkdir -p "${HOME}/.config/zsh/functions"
}
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/conf.d/05-path.zsh"

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

@test "CARGO_HOME/bin in PATH when exists" {
  mkdir -p "${HOME}/.local/share/cargo/bin"
  _run_path 'print -l "${path[@]}"'
  [[ "$output" == *"/.local/share/cargo/bin"* ]]
}

@test "GOPATH/bin in PATH when exists" {
  mkdir -p "${HOME}/.local/share/go/bin"
  _run_path 'print -l "${path[@]}"'
  [[ "$output" == *"/.local/share/go/bin"* ]]
}

@test "BUN_INSTALL/bin in PATH when exists" {
  mkdir -p "${HOME}/.local/share/bun/bin"
  _run_path 'print -l "${path[@]}"'
  [[ "$output" == *"/.local/share/bun/bin"* ]]
}

@test "MANPATH includes system man dirs" {
  _run_path 'print -l "${manpath[@]}"'
  [[ "$output" == *"/usr/share/man"* ]]
}

@test "path array is unique typed" {
  _run_path 'print ${(t)path}'
  [[ "$output" == *"unique"* ]]
}

@test "fpath array is unique typed" {
  _run_path 'print ${(t)fpath}'
  [[ "$output" == *"unique"* ]]
}

@test "HOME/.local/bin has highest priority" {
  mkdir -p "${HOME}/.local/share/cargo/bin"
  _run_path 'print "${path[1]}"'
  [[ "$output" == *"/.local/bin" ]]
}

@test "empty functions dir does not error" {
  rm -f "${HOME}/.config/zsh/functions/"*
  _run_path 'true'
  (( status == 0 ))
}
