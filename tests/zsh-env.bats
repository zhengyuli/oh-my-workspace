#!/usr/bin/env bats
# zsh-env.bats — tests for shell/zsh/zshenv

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/zshenv"

@test "XDG_CONFIG_HOME defaults to HOME/.config" {
  run zsh -c "
    unset XDG_CONFIG_HOME XDG_CACHE_HOME XDG_DATA_HOME XDG_STATE_HOME ZDOTDIR
    export HOME=\"${HOME}\"
    source \"${MODULE}\"
    print \$XDG_CONFIG_HOME
  "
  [[ "$output" == "${HOME}/.config" ]]
}

@test "XDG_CACHE_HOME defaults to HOME/.cache" {
  run zsh -c "
    unset XDG_CONFIG_HOME XDG_CACHE_HOME XDG_DATA_HOME XDG_STATE_HOME ZDOTDIR
    export HOME=\"${HOME}\"
    source \"${MODULE}\"
    print \$XDG_CACHE_HOME
  "
  [[ "$output" == "${HOME}/.cache" ]]
}

@test "XDG_DATA_HOME defaults to HOME/.local/share" {
  run zsh -c "
    unset XDG_CONFIG_HOME XDG_CACHE_HOME XDG_DATA_HOME XDG_STATE_HOME ZDOTDIR
    export HOME=\"${HOME}\"
    source \"${MODULE}\"
    print \$XDG_DATA_HOME
  "
  [[ "$output" == "${HOME}/.local/share" ]]
}

@test "XDG_STATE_HOME defaults to HOME/.local/state" {
  run zsh -c "
    unset XDG_CONFIG_HOME XDG_CACHE_HOME XDG_DATA_HOME XDG_STATE_HOME ZDOTDIR
    export HOME=\"${HOME}\"
    source \"${MODULE}\"
    print \$XDG_STATE_HOME
  "
  [[ "$output" == "${HOME}/.local/state" ]]
}

@test "pre-existing XDG_CONFIG_HOME is not overwritten" {
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"/custom/config\"
    source \"${MODULE}\"
    print \$XDG_CONFIG_HOME
  "
  [[ "$output" == "/custom/config" ]]
}

@test "ZDOTDIR set to XDG_CONFIG_HOME/zsh" {
  run zsh -c "
    unset XDG_CONFIG_HOME XDG_CACHE_HOME XDG_DATA_HOME XDG_STATE_HOME ZDOTDIR
    export HOME=\"${HOME}\"
    source \"${MODULE}\"
    print \$ZDOTDIR
  "
  [[ "$output" == "${HOME}/.config/zsh" ]]
}
