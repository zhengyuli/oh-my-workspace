#!/usr/bin/env bats
# zsh-20-aliases.bats — tests for conf.d/20-aliases.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/conf.d/20-aliases.zsh"

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

@test "safety alias mv includes -iv" {
  run_zsh "$MODULE" 'alias mv'
  [[ "$output" == *"-iv"* ]]
}

@test "mkdir includes -pv" {
  run_zsh "$MODULE" 'alias mkdir'
  [[ "$output" == *"-pv"* ]]
}

@test "navigation alias ... goes up two levels" {
  run_zsh "$MODULE" 'alias ...'
  [[ "$output" == *"cd ../.."* ]]
}

@test "navigation alias - goes to previous dir" {
  run_zsh "$MODULE" 'alias -- -'
  [[ "$output" == *"cd -"* ]]
}

@test "editor alias vi is nvim" {
  run_zsh "$MODULE" 'alias vi'
  [[ "$output" == *"nvim"* ]]
}

@test "editor alias vim is nvim" {
  run_zsh "$MODULE" 'alias vim'
  [[ "$output" == *"nvim"* ]]
}

@test "vimdiff alias uses nvim -d" {
  run_zsh "$MODULE" 'alias vimdiff'
  [[ "$output" == *"nvim -d"* ]]
}

@test "emacs alias e uses -nw" {
  run_zsh "$MODULE" 'alias e'
  [[ "$output" == *"emacs -nw"* ]]
}

@test "path alias prints path entries" {
  run_zsh "$MODULE" 'alias path'
  [[ "$output" == *"print"* ]]
}

@test "cat uses bat when available" {
  run_zsh "$MODULE" 'alias cat'
  [[ "$output" == *"bat"* ]]
}

@test "df alias includes -h" {
  run_zsh "$MODULE" 'alias df'
  [[ "$output" == *"-h"* ]]
}

@test "du alias includes -sh" {
  run_zsh "$MODULE" 'alias du'
  [[ "$output" == *"-sh"* ]]
}

@test "egrep has --color=auto" {
  run_zsh "$MODULE" 'alias egrep'
  [[ "$output" == *"--color=auto"* ]]
}

@test "fgrep has --color=auto" {
  run_zsh "$MODULE" 'alias fgrep'
  [[ "$output" == *"--color=auto"* ]]
}

@test "la alias defined with eza" {
  run_zsh "$MODULE" 'alias la'
  [[ "$output" == *"eza"* ]] || [[ "$output" == *"-A"* ]]
}

@test "ll alias defined" {
  run_zsh "$MODULE" 'alias ll'
  [[ "$output" == *"eza"* ]] || [[ "$output" == *"-l"* ]]
}

@test "lt alias defined for tree view" {
  run_zsh "$MODULE" 'alias lt'
  [[ "$output" == *"tree"* ]]
}

@test "reload-conf alias sources zshrc" {
  run_zsh "$MODULE" 'alias reload-conf'
  [[ "$output" == *".zshrc"* ]]
}
