#!/usr/bin/env bats
# zsh-60-keybinds.bats — tests for conf.d/60-keybinds.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/zsh/conf.d/60-keybinds.zsh"

_run_keys() {
  local expr="$1"
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    source \"${MODULE}\"
    ${expr}
  "
}

@test "emacs keymap selected" {
  _run_keys 'bindkey -lL main'
  [[ "$output" == *"emacs"* ]]
}

@test "KEYTIMEOUT set to 10" {
  _run_keys 'print $KEYTIMEOUT'
  [[ "$output" == "10" ]]
}

@test "sudo-command-line widget registered" {
  _run_keys 'zle -lL | grep sudo-command-line'
  (( status == 0 ))
  [[ "$output" == *"sudo-command-line"* ]]
}

@test "Ctrl+P bound to history-search-backward" {
  _run_keys 'bindkey "^P"'
  [[ "$output" == *"history-search-backward"* ]]
}
