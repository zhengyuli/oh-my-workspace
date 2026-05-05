#!/usr/bin/env bats
# zsh-70-tools.bats — tests for conf.d/70-tools.zsh

load zsh-helper

setup() { setup_zsh_env; }
teardown() { teardown_zsh_env; }

MODULE="${BATS_TEST_DIRNAME}/../shell/zsh/.config/zsh/conf.d/70-tools.zsh"

# Helper that loads compinit before the module (required by 70-tools)
_source_with_compinit() {
  local expr="$1"
  run zsh -c "
    export HOME=\"${HOME}\"
    export XDG_CONFIG_HOME=\"${HOME}/.config\"
    export XDG_CACHE_HOME=\"${HOME}/.cache\"
    export XDG_DATA_HOME=\"${HOME}/.local/share\"
    export XDG_STATE_HOME=\"${HOME}/.local/state\"
    export ZDOTDIR=\"${HOME}/.config/zsh\"
    export BUN_INSTALL=\"${HOME}/.local/share/bun\"
    export HOMEBREW_PREFIX=\"/opt/homebrew\"
    export PATH=\"${BATS_TEST_DIRNAME}/zsh-bin:/usr/bin:/bin\"
    mkdir -p \"\$XDG_CACHE_HOME/zsh\"
    autoload -Uz compinit && compinit -C -d \"\$XDG_CACHE_HOME/zsh/zcompdump\"
    source \"${MODULE}\"
    ${expr}
  "
}

@test "git wrapper: non-config commands pass through" {
  _source_with_compinit 'whence -w git'
  [ "$status" -eq 0 ]
  [[ "$output" == *"function"* ]]
}

@test "_git_check_flag: --get returns r" {
  _source_with_compinit 'print $(_git_check_flag --get)'
  [ "$status" -eq 0 ]
  [ "$output" = "r" ]
}

@test "_git_check_flag: --add returns w" {
  _source_with_compinit 'print $(_git_check_flag --add)'
  [ "$status" -eq 0 ]
  [ "$output" = "w" ]
}

@test "_git_check_flag: unknown flag returns empty" {
  _source_with_compinit 'print $(_git_check_flag --unknown)'
  [ "$status" -eq 0 ]
  [ "$output" = "" ]
}

@test "_git_classify_config_op: two positional args = write" {
  _source_with_compinit 'print $(_git_classify_config_op user.name "John")'
  [ "$status" -eq 0 ]
  [ "$output" = "w" ]
}

@test "_git_classify_config_op: one positional arg = read" {
  _source_with_compinit 'print $(_git_classify_config_op user.name)'
  [ "$status" -eq 0 ]
  [ "$output" = "r" ]
}

@test "_git_classify_config_op: --list flag = read" {
  _source_with_compinit 'print $(_git_classify_config_op --list)'
  [ "$status" -eq 0 ]
  [ "$output" = "r" ]
}

@test "uv completion cache created" {
  _source_with_compinit '[[ -f "$XDG_CACHE_HOME/zsh/uv-completion.zsh" ]]'
  [ "$status" -eq 0 ]
}

@test "zoxide cache created" {
  _source_with_compinit '[[ -f "$XDG_CACHE_HOME/zsh/zoxide-init.zsh" ]]'
  [ "$status" -eq 0 ]
}
