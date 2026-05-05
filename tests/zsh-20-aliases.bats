#!/usr/bin/env bats
# zsh-20-aliases.bats — tests for conf.d/20-aliases.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/20-aliases.zsh"

@test "ls uses eza when available" {
  run_zsh "$MODULE" 'alias ls'
  [[ "$output" == *"eza"* ]]
}

@test "ls falls back to ls -G on darwin without eza" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export OSTYPE=\"darwin22.0\"
    export PATH=\"/usr/bin:/bin\"
    source \"${MODULE}\"
    alias ls
  "
  [[ "$output" == *"-G"* ]]
}

@test "safety alias cp includes -iv" {
  run_zsh "$MODULE" 'alias cp'
  [[ "$output" == *"-iv"* ]]
}

@test "safety alias rm includes -iv" {
  run_zsh "$MODULE" 'alias rm'
  [[ "$output" == *"-iv"* ]]
}

@test "editor alias v is nvim" {
  run_zsh "$MODULE" 'alias v'
  [[ "$output" == *"nvim"* ]]
}

@test "grep has --color=auto" {
  run_zsh "$MODULE" 'alias grep'
  [[ "$output" == *"--color=auto"* ]]
}

@test "navigation alias .. defined" {
  run_zsh "$MODULE" 'alias ..'
  [[ "$output" == *"cd .."* ]]
}

@test "reload-zsh alias defined" {
  run_zsh "$MODULE" 'alias reload-zsh'
  [[ "$output" == *"exec zsh"* ]]
}
