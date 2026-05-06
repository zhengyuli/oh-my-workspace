#!/usr/bin/env bats
# zsh-30-completion.bats — tests for conf.d/30-completion.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/conf.d/30-completion.zsh"

_run_comp() {
  local expr="$1"
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    source \"${MODULE}\"
    ${expr}
  "
}

@test "compinit runs without error" {
  _run_comp 'true'
  (( status == 0 ))
}

@test "zcompdump created in XDG cache" {
  _run_comp '[[ -f "$XDG_CACHE_HOME/zsh/zcompdump" ]]'
  (( status == 0 ))
}

@test "COMPDUMP_MAX_AGE_HOURS defaults to 20" {
  _run_comp 'print $COMPDUMP_MAX_AGE_HOURS'
  [[ "$output" == "20" ]]
}

@test "completion cache path uses XDG" {
  _run_comp 'zstyle -L ":completion:*" cache-path'
  [[ "$output" == *"/.cache/zsh/completion-cache"* ]]
}

@test "matcher-list includes smart case" {
  _run_comp 'zstyle -L ":completion:*" matcher-list'
  [[ "$output" == *"m:{a-z}={A-Za-z}"* ]]
}

@test "use-cache enabled" {
  _run_comp 'zstyle -L ":completion:*" use-cache'
  [[ "$output" == *"yes"* ]]
}

@test "rehash enabled" {
  _run_comp 'zstyle -L ":completion:*" rehash'
  [[ "$output" == *"true"* ]]
}

@test "list-dirs-first enabled" {
  _run_comp 'zstyle -L ":completion:*" list-dirs-first'
  [[ "$output" == *"true"* ]]
}

@test "squeeze-slashes enabled" {
  _run_comp 'zstyle -L ":completion:*" squeeze-slashes'
  [[ "$output" == *"true"* ]]
}

@test "ignored-patterns includes node_modules" {
  _run_comp 'zstyle -L ":completion:*" ignored-patterns'
  [[ "$output" == *"node_modules"* ]]
}

@test "completer order includes _complete and _ignored" {
  _run_comp 'zstyle -L ":completion:*" completer'
  [[ "$output" == *"_complete"* ]]
  [[ "$output" == *"_ignored"* ]]
}

@test "warnings format shows no matches" {
  _run_comp 'zstyle -L ":completion:*:warnings" format'
  [[ "$output" == *"no matches"* ]]
}

@test "list-colors includes directory coloring" {
  _run_comp 'zstyle -L ":completion:*" list-colors'
  [[ "$output" == *"di="* ]]
}

@test "cd ignores parent" {
  _run_comp 'zstyle -L ":completion:*:cd:*" ignore-parents'
  [[ "$output" == *"parent pwd"* ]]
}
